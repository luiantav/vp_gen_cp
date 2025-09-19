# Script to fit learning models to learning task data 

required_packages = c("here", "nloptr", "tibble", "optparse")
invisible(lapply(required_packages, require, character.only = TRUE))
#renv::restore()
here::i_am("flag_project_root.R")

##Source everything
source_path = file.path(here::here('phd_models/ML/Models'), fsep = .Platform$file.sep)
source_files = list.files(source_path, pattern = "[.][rR]$", full.names = TRUE, recursive = TRUE)
invisible(lapply(source_files, function(x) source(x)))
source(here::here('modeling', 'ML', "helper_func.R"))
source(here::here('modeling', 'ML', "Models", "learning_models.R"))


#### Basic config.
nSubj = 4 # n of ratings x stimulus 
iter = 1 # iterations 
nStim = 9 # n of stimuli 
nPerc = 2 # n PU levels 
pulevels = c(0.4, 0.2)
xmin = -4
xmax = 4

xs = seq(xmin, xmax, by=((xmax - (xmin))/(nStim - 1))) #stimulus space -4:4
task = c(rep(0, 24), rep(1, 36)) # trial x task 
df_template <- data.frame(matrix(,nrow=iter))
df <- data.frame()

# Read in learning data
df_learn<- read.csv(paste0(here::here(), "/modeling/ML/Data/learningtrials.csv" ),header = TRUE)

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
  #subject = c("gnd1v5zo5x7qjad", "brsvshywlogvxcq") # example subjects 
  subject = c("gnd1v5zo5x7qjad", "brsvshywlogvxcq") 
}


for (sub in c(subject)) {

df_temp <- df_template
df_temp$sub <- sub

# Define models to fit 
models = c('rw_base_learn','rw_fix_learn')

for (gm in models){ 
for (cond in c(0:5)){ #task conditions
cat(sprintf("Sub %s | Model %s | Cond %d\n", sub, gm, cond))

#get subject & generalisation data
dfs = df_learn[df_learn$sub %in% sub & df_learn$condition %in% cond,] 
df_sub = dfs
df_sub$total_n = c(1: nrow(df_sub)) #here not sure as we have halfway and end
rrr = unique(df_sub$rr) # rr condition 
ppp = unique(df_sub$pu) # discriminability condition 

data = list('nSubj' =length(unique(df_learn$sub)), 'sub' = sub, 'reward' = df_sub$reward, 'trial_sequence' = df_sub$stimulus, 'pu_vec' = df_sub$pu, 'ou_vec' = df_sub$ou, 
            'current_n' = df_sub$n, response = df_sub$response, 'total_n' = df_sub$total_n, 'condition' = df_sub$condition)

# Prepare for fitting 
mod_gen = gm
conf = list('model_name'= mod_gen,'nStim' =nStim, 'cspos' = 5,'get_loglik' = 1)

if(mod_gen == "rw_base_learn" | mod_gen == "rw_ph_learn"){
  conf$params2estimate <- c("v0", "alpha")
  conf$params = list('v0' = NA)
  conf$params = list('alpha' = NA)
} else if(mod_gen == "rw_fix_learn"){
  conf$params2estimate <- c("alpha")
  conf$params = list('alpha'=NA)
} 

# Fit to data 

  conf$start_vals <- NA 
  opts1 = list('algorithm'='NLOPT_GN_DIRECT_L', 'xtol_rel'=1.0e-4, 'maxeval'= 5000) #check for alternatives
  
  if(mod_gen == "rw_base_learn" | mod_gen == "rw_ph_learn"){lb = c(0,0); ub = c(1,1); conf$start_vals <- c(0,0.5)
  } else if(mod_gen == "rw_fix_learn") {lb = c(0); ub = c(1); conf$start_vals <- c(0.5)
  } 
  
  fit_stat2 = nloptr::nloptr(x0=conf$start_vals,
                            # Minimize neg LL
                            eval_f=loglik_fit_learn,
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
  if(mod_gen == "rw_base_learn" | mod_gen == "rw_ph_learn"){
    param_names <- c("v0", "alpha") 
  }else if(mod_gen == "rw_fix_learn"){
    param_names <- c("alpha") 
  } 

  params<- fit_stat2$solution
  names(params) <- param_names
  params <- as.list(params)
  
  # Get measures of fit 
  fit_stat2$ll <- loglik_fit_learn(fit_stat2$solution, data, conf)
  fit_stat2$AIC <- 2*length(conf$params2estimate) + 2*fit_stat2$ll
  k = length(conf$params2estimate)
  n = length(data$trial_sequence)
  fit_stat2$AICc <- fit_stat2$AIC + (2*(k^2) + 2*k) / (n - k - 1)
  fit_stat2$BIC <- 2*fit_stat2$ll + log(length(data$trial_sequence))*length(conf$params2estimate)
  
  # Save
  if (mod_gen == "rw_base_learn" | mod_gen == "rw_ph_learn") {
    v0_col <- paste0("v0_", mod_gen, "R", rrr)
    alpha_col <- paste0("alpha_", mod_gen, "R", rrr)
    
    df_temp[[v0_col]] <- params$v0
    df_temp[[alpha_col]] <- params$alpha
    
    if (v0_col %in% names(df_temp)) {
      df_temp[[paste0(v0_col, "X")]] <- params$v0
    }
    if (alpha_col %in% names(df_temp)) {
      df_temp[[paste0(alpha_col, "X")]] <- params$alpha
    }
    
  } else if (mod_gen == "rw_fix_learn") {
    alpha_col <- paste0("alpha_", mod_gen, "R", rrr)
    
    df_temp[[alpha_col]] <- params$alpha
    
    if (alpha_col %in% names(df_temp)) {
      df_temp[[paste0(alpha_col, "X")]] <- params$alpha
    }
  }
  

  # Define column names
  ll_col   <- paste0("ll_", mod_gen, "R", rrr)
  AIC_col  <- paste0("AIC_", mod_gen, "R", rrr)
  AICc_col <- paste0("AICc_", mod_gen, "R", rrr)
  BIC_col  <- paste0("BIC_", mod_gen, "R", rrr)
  
  # Assign values with fallback to "X" column if needed
  df_temp[[if (ll_col %in% names(df_temp)) ll_col else paste0(ll_col, "X")]]     <- fit_stat2$ll
  df_temp[[if (AIC_col %in% names(df_temp)) AIC_col else paste0(AIC_col, "X")]]  <- fit_stat2$AIC
  df_temp[[if (AICc_col %in% names(df_temp)) AICc_col else paste0(AICc_col, "X")]] <- fit_stat2$AICc
  df_temp[[if (BIC_col %in% names(df_temp)) BIC_col else paste0(BIC_col, "X")]]  <- fit_stat2$BIC
}
}
df<- rbind(df, df_temp)
write.csv(df, here::here('outputs', 'fitting', 'learning', paste0("fits_", toString(sub),"full_set.csv")))
}
