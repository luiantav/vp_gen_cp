# Script to fit models to data 

required_packages = c("here", "nloptr", "tibble", "optparse", "dplyr")
invisible(lapply(required_packages, require, character.only = TRUE))
#renv::restore()
here::i_am("flag_project_root.R")

##Source everything
source_path = file.path(here::here('phd_models/ML/Models'), fsep = .Platform$file.sep)
source_files = list.files(source_path, pattern = "[.][rR]$", full.names = TRUE, recursive = TRUE)
invisible(lapply(source_files, function(x) source(x)))
source(here::here('modeling', 'ML', "helper_func.R"))
source(here::here('modeling', 'ML', "Models", "all_models.R"))

#### Basic config.
nSubj = 4 # n of ratings x stimulus 
iter = 30 # iterations 
nStim = 9 # n of stimuli 
nPerc = 2 # n PU levels 
pulevels = c(0.4, 0.2)
xmin = -4
xmax = 4

xs = seq(xmin, xmax, by=((xmax - (xmin))/(nStim - 1))) #stimulus space -4:4
task = c(rep(0, 24), rep(1, 36)) # n trial x learning and generalisation task 
df_template <- data.frame(matrix(NA, nrow = iter, ncol = 14))

# Prep for saving
names(df_template) <- c("sub", "model", "ou", "pu", "iter", "rho", "omega","lambda", "offset","ll", "AIC", "AICc", "BIC","k")
df <- data.frame()

# Read in data
df_exp<- read.csv(paste0(here::here(), "/modeling/ML/Data/df_forfitting.csv" ),header = TRUE)

# To run on cluster ... 
run_on_cluster = 1
if (run_on_cluster == 1) {
  # Create options to pass to script
  option_list = list(
    optparse::make_option(c('-s', '--subject'), type='character', default = NULL, help = 'sub', metavar = 'subject'))
  
  # provide options in list to be callable by script
  opt_parser = optparse::OptionParser(option_list = option_list)
  opt = optparse::parse_args(opt_parser)
  subject <- opt$subject
}else{
  subject = c("gnd1v5zo5x7qjad", "brsvshywlogvxcq") # example subjects 
}


for (sub in c(subject)) {
print(sub)
df_temp <- df_template
df_temp$sub <- sub

# models to fit 
models = c('value_alt_offset', 'perc_offset', 'full_offset','value_sig')
for (gm in models){ 
df_temp[,] <- NA
for (cond in c(0:5)){ #task conditions

#get subject & generalisation data
dfs = df_exp[df_exp$subject %in% sub & df_exp$condition %in% cond,] 
df_sub = dfs[dfs$task %in% 1,]
df_sub$total_n = c(1: nrow(df_sub))
rrr = unique(df_sub$rr) # rr condition 
ppp = unique(df_sub$pu) # discriminability condition 

#get last learning rating and use as starting values for gen
df_learn_info = dfs[dfs$task %in% 0 & dfs$n_total == 24, ]
df_learn_info <- df_learn_info$response
data = list('nSubj' =length(unique(df_exp$sub)), 'sub' = sub, 'reward' = df_sub$reward, 'trial_sequence' = df_sub$stimulus, 'pu_vec' = df_sub$pu, 'ou_vec' = df_sub$ou, 
            'current_n' = df_sub$n_total, 'pepr' = NA, 'xs' = xs,
            'task' = task, response = df_sub$response, 'total_n' = df_sub$total_n, 'condition' = df_sub$condition, 'learn_info' = df_learn_info)

# Prepare for fitting 
mod_gen = gm
print(mod_gen)
conf = list('model_name'= mod_gen,'nStim' =nStim, 'cspos' = 5,'get_loglik' = 1, 'nStimNC'= nStim*2)

if(mod_gen == "value_alt_offset"){
  conf$params2estimate <- c("omega","lambda","offset")
  conf$params = list('omega'=NA,'lambda'=NA, 'offset'=NA)
} else if(mod_gen == "full_offset"){
  conf$params2estimate <- c("rho", "omega","lambda","offset")
  conf$params = list('rho'=NA, 'omega'=NA,'lambda'=NA, 'offset'=NA)
} else if(mod_gen == "perc_offset"){
  conf$params2estimate <- c("rho","offset")
  conf$params = list('rho'=NA, 'offset'=NA)
} else if(mod_gen == "value_sig"){
  conf$params2estimate <- c("omega","lambda","offset","k")
  conf$params = list('omega'=NA,'lambda'=NA, 'offset'=NA, 'k'=NA)
}


# Fit to data 
for (it in seq(iter)) {
  print(it)
  conf$start_vals <- NA 
  opts1 = list('algorithm'='NLOPT_GN_DIRECT_L', 'xtol_rel'=1.0e-4, 'maxeval'= 5000) #check for alternatives
  
  if(mod_gen == "value_alt_offset" | mod_gen == "norm_model") {lb = c(0,0,-1); ub = c(1,10,1); conf$start_vals <- c(0.5,2,0.01)
  } else if(mod_gen == "full_offset") {lb = c(0,0,0,-1); ub = c(1,1,10,1); conf$start_vals <- c(0.3,0.5,2,0.01)
  } else if(mod_gen == "perc_offset") {lb = c(0,-1); ub = c(1,1); conf$start_vals <- c(0.3,0.01)
  } else if(mod_gen == "value_sig") {lb = c(0,0,-1,1); ub = c(1,10,1,30); conf$start_vals <- c(0.5,2,0.01,5)
  }
  
  fit_stat2 = nloptr::nloptr(x0=conf$start_vals,
                            # Minimize neg LL
                            eval_f=loglik_fit_gen,
                            # Lower bound of parameters (e.g. c(0,0,1)) #
                            #lb=c(0,0,0),
                            lb = lb,
                            # Upper bound of parameters (e.g. c(1,1,10)) 
                            #ub=c(1,15,1),
                            ub = ub,
                            # Minimizer options
                            opts=opts1,
                            # Inputs to LL function
                            data=data, 
                            conf=conf)
  
  
  # Prepare parameters for saving 
  if(mod_gen == "value_alt_offset"){
    param_names <- c("omega", "lambda", "offset") 
  }else if(mod_gen == "full_offset"){
    param_names <- c("rho", "omega","lambda","offset") 
  }else if(mod_gen == "perc_offset" | mod_gen == "perc_offset_norm"){
    param_names <- c("rho","offset") 
  } else if(mod_gen == "value_sig"){
    param_names <- c("omega", "lambda", "offset","k") 
  }

  params<- fit_stat2$solution
  names(params) <- param_names
  params <- as.list(params)
  
  # Get measures of fit 
  fit_stat2$ll <- loglik_fit_gen(fit_stat2$solution, data, conf)
  fit_stat2$AIC <- 2*length(conf$params2estimate) + 2*fit_stat2$ll
  k = length(conf$params2estimate)
  n = length(data$trial_sequence)
  fit_stat2$AICc <- fit_stat2$AIC + (2*(k^2) + 2*k) / (n - k - 1)
  fit_stat2$BIC <- 2*fit_stat2$ll + log(length(data$trial_sequence))*length(conf$params2estimate)
  
  # Save
  df_temp$iter[it] = it
  df_temp$sub[it] = sub
  df_temp$ou[it] = rrr
  df_temp$pu[it] = ppp
  df_temp$model[it] = mod_gen
  
  if (mod_gen == "value_alt_offset"){
    df_temp$offset[it] <- params$offset
    df_temp$omega[it] <- params$omega
    df_temp$lambda[it] <- params$lambda
  } else if (mod_gen == "full_offset"){
    df_temp$rho[it] <- params$rho
    df_temp$offset[it] <- params$offset
    df_temp$omega[it] <- params$omega
    df_temp$lambda[it] <- params$lambda
  } else if (mod_gen == "perc_offset"){
    df_temp$rho[it] <- params$rho
    df_temp$offset[it] <- params$offset
  } else if (mod_gen == "value_sig"){
    df_temp$offset[it] <- params$offset
    df_temp$omega[it] <- params$omega
    df_temp$lambda[it] <- params$lambda
    df_temp$k[it] <- params$k
  }
  
  df_temp$ll[it] <- fit_stat2$ll
  df_temp$AIC[it] <- fit_stat2$AIC
  df_temp$AICc[it] <- fit_stat2$AICc
  df_temp$BIC[it] <- fit_stat2$BIC
  
}
df<- rbind(df, df_temp)
#print(df)
write.csv(df, here::here('outputs', 'fitting', 'todata', paste0("fits_", toString(sub),"_ft.csv")))
}
}

}
write.csv(df, here::here('outputs', 'fitting', 'todata', paste0("fits_", toString(sub),"_ft.csv")))
