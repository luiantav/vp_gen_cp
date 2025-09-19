# Script for model recovery 
# Simulates data for all models based on fitted parameters 
# & fits all models to simulated data

required_packages = c("here", "nloptr", "tibble", "optparse", "truncnorm","dplyr")
invisible(lapply(required_packages, require, character.only = TRUE))
#renv::restore()
here::i_am("flag_project_root.R")

#### Source everything
source_path = file.path(here::here('modeling/ML/Models'), fsep = .Platform$file.sep)
source_files = list.files(source_path, pattern = "[.][rR]$", full.names = TRUE, recursive = TRUE)
invisible(lapply(source_files, function(x) source(x)))
source(here::here('modeling', 'ML', "helper_func.R"))
source(here::here('modeling', 'ML', "Models", "all_models.R"))

# Get parameters from fitting and find quantiles 
#lambda
fittedparameters <- read.csv(paste0(here::here(), "/modeling/ML/Data/mrec/fittedparameters_valuemodel.csv"),header = TRUE)
quantiles_l_lin <- quantile(fittedparameters$lambda[fittedparameters$omega < 0.5], probs = c(0.05, 0.95))
quantiles_l_gauss <- quantile(fittedparameters$lambda[fittedparameters$omega >= 0.5], probs = c(0.05, 0.95))
lambda_lin <- seq(quantiles_l_lin[1], quantiles_l_lin[2], by=0.1)
lambda_gauss <- seq(quantiles_l_gauss[1], quantiles_l_gauss[2], by=0.1)

#offset
quantiles_o_lin <- quantile(fittedparameters$offset[fittedparameters$omega < 0.5], probs = c(0.05, 0.95))
quantiles_o_gauss <- quantile(fittedparameters$offset[fittedparameters$omega >= 0.5], probs = c(0.05, 0.95))
offset_lin <- seq(quantiles_o_lin[1], quantiles_o_lin[2], by=0.01)
offset_gauss <- seq(quantiles_o_gauss[1], quantiles_o_gauss[2], by=0.01)

#rho & offset 
fittedparameters <- read.csv(paste0(here::here(), "/modeling/ML/Data/mrec/fittedparameters_perceptualmodel.csv"),header = TRUE)
quantiles_r <- quantile(fittedparameters$rho, probs = c(0.05, 0.95))
quantiles_o_perc <- quantile(fittedparameters$offset, probs = c(0.05, 0.95))
rho <- seq(quantiles_r[1], quantiles_r[2], by=0.01)
offset_perc <- seq(quantiles_o_perc[1], quantiles_o_perc[2], by=0.01)


#### set general 
nSubj = 1
iter = 30 #iter for perceptual confusion
nOutc = 3 # n outcome uncertainty levels
nPerc = 2 # n discriminability levels
oulevels = c(0.25, 0.5, 0.75)
pu_ideal = c(0.4, 0.2)
nSamples =4 # how many samples x stimulus 
mean = 0
xmin = -4
xmax = 4
NLtrials =24 # learning trials
NGtrials= 36 # generalisation trials 
cspos = 5
nStim = 9 # n stimuli in stimulus space

xs = seq(xmin, xmax, by=((xmax - (xmin))/(nStim - 1))) #stim -4:4
s = c(1:9) #stimulus index
nTrial = NLtrials + NGtrials #total trials
n = c(1:nTrial)
task = c(rep(0, NLtrials), rep(1, NGtrials))

# Setup df for saving 
df_temp<- data.frame(matrix(NA, nrow = iter, ncol = 17))
names(df_temp) <- c("model_sim", "model_fit", "sub", "iter", "ll", "AIC", "AICc", "BIC","k","offsetIN","omegaIN", "lambdaIN", "rhoIN", "offsetOUT", "omegaOUT", "lambdaOUT", "rhoOUT")
df <- data.frame()


# Run locally or on cluster
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
  subject = "1"
}


for (sub in c(subject)) {
  
  #generate trial sequence for "participant"
  sequ = generate_seq(nTrial,nOutc, nPerc, oulevels, NLtrials,NGtrials, xs, nSamples)
  
  df_sim = data.frame(task = sequ$task, response = c(rep(NA,360)), stimulus = sequ$trial_sequence, mix = c(rep(NA,360)), rr = sequ$ou_vec, pu = sequ$pu_vec, sub = c(rep(sub,360)), 
                      lambda = c(rep(NA,360)), omega = c(rep(NA,360)), alpha = c(rep(NA,360)), alpha_extinction = c(rep(NA,360)), n = sequ$current_n, reward = sequ$reward, logll = c(rep(NA,360)), 
                      condition = sequ$condition, subject = sub, total_n = sequ$current_n)
  
  # Define models for simulation & recovery
  models = c('perc_offset','full_offset','value_alt_offset')
  

  for (gm in models){
    print(gm)
    sample_cond = sample(c(0:5),1)
    for (cond in sample_cond){
      
      # Get generalisation trials structure
      df_sub = df_sim[df_sim$subject %in% sub & df_sim$condition %in% cond,]
      df_sub = df_sub[df_sub$task %in% 1,]
      df_sub$total_n = c(1: nrow(df_sub))
      df_sub$current_n = c(1: nrow(df_sub))
      rrr = unique(df_sub$rr)
      ppp = unique(df_sub$pu)
      print(rrr)
      
      # Sim learning data (in fitting of data, this takes the real outcome expectancies after learning)
      df_learn_info = oulevels[unique(df_sub$rr)] 
      
      mod_sim = gm
      conf = list('model_name'= mod_sim,'nStim' =nStim, 'cspos' = 5,'get_loglik' = 0, 'nStimNC'= nStim*2)
      
      # Set parameters
      omega_pick = sample(c(0,1),1)
        if (mod_sim == "perc_offset"){
            conf$params2estimate <- c("rho","offset")
            params = list('rho' = sample(rho,1),'offset'=sample(offset_perc ,1))
          
        } else if(mod_sim == "value_alt_offset"){
          conf$params2estimate <- c("omega","lambda","offset")
          if(omega_pick == 1){ params = list('omega'=omega_pick, 'lambda'=sample(lambda_gauss,1),'offset'= sample(offset_gauss ,1))
          } else{ params = list('omega'=omega_pick, 'lambda'=sample(lambda_lin,1),'offset'=sample(offset_lin ,1))
          
          }
        } else if(mod_sim == "full_offset"){
          conf$params2estimate <- c("rho","omega","lambda","offset")
          if(omega_pick == 1){ params = list('rho' = sample(rho,1),'omega'=omega_pick, 'lambda'=sample(lambda_gauss,1),'offset'= sample(offset_gauss ,1))
          } else{ params = list('rho' = sample(rho,1), 'omega'=omega_pick, 'lambda'=sample(lambda_lin,1),'offset'=sample(offset_lin ,1))
          
          }
        } 
        
      data = list('nSubj' =length(unique(df_sim$sub)), 'sub' = sub, 'reward' = df_sub$reward, 'trial_sequence' = df_sub$stimulus, 'pu_vec' = df_sub$pu, 'ou_vec' = df_sub$ou, 'current_n' = df_sub$n, 'pepr' = NA, 'xs' = xs,
                  'task' = sequ$task, response = df_sub$response, 'total_n' = df_sub$n, 'learn_info' = df_learn_info)
      
      
      # Sim data
      if(conf$model_name == 'perc_offset'){
        gen_sim = rw_perc_est_offset(data, params, conf, pepr)
      } else if(conf$model_name == 'value_alt_offset'){
        gen_sim = rw_value_alt_offset_gen(data, params, conf, pepr)
      } else if(conf$model_name == 'full_offset'){
        gen_sim = rw_full_alt_offset_gen(data, params, conf, pepr)
      } 


      # Set up recovery 
      for (gm2 in models){
        df_temp[,] <- NA
        df_temp$model_sim = gm
        df_temp$model_fit = gm2
        
        # Save parameters 
        if (mod_sim == "value_alt_offset"){
          df_temp$offsetIN <- params$offset
          df_temp$omegaIN<- params$omega
          df_temp$lambdaIN <- params$lambda
          df_temp$rhoIN <- NA
        } else if (mod_sim == "full_offset"){
          df_temp$rhoIN <- params$rho
          df_temp$offsetIN<- params$offset
          df_temp$omegaIN <- params$omega
          df_temp$lambdaIN<- params$lambda
        } else if (mod_sim == "perc_offset"){
          df_temp$rhoIN<- params$rho
          df_temp$offsetIN<- params$offset
          df_temp$lambdaIN <- NA
          df_temp$omegaIN <- NA
        } 
        
        df_sub$current_n = c(1: nrow(df_sub))
        data = list('nSubj' =length(unique(df_sim$sub)), 'sub' = sub, 'reward' = df_sub$reward, 'trial_sequence' = df_sub$stimulus, 'pu_vec' = df_sub$pu, 'ou_vec' = df_sub$ou, 
                    'current_n' = df_sub$n, 'pepr' = NA, 'xs' = xs,
                    'task' = sequ$task, response = gen_sim$y, 'total_n' = df_sub$n, 'condition' = df_sub$condition, 'learn_info' = df_learn_info)
        
        mod_gen = gm2
        print(mod_gen)
        
        conf = list('model_name'= mod_gen,'nStim' =nStim, 'cspos' = 5,'get_loglik' = 1, 'nStimNC'= nStim*2)
        
        # Define parameter to fit
        if(mod_gen == "perc_offset"){
          conf$params2estimate <- c("rho", "offset")
          conf$params = list('rho'=NA, 'offset'=NA)
        } else if(mod_gen == "value_alt_offset"){
          conf$params2estimate <- c("omega","lambda","offset")
          conf$params = list('omega'=NA,'lambda'=NA,'offset'=NA)
        } else if(mod_gen == "full_offset"){
          conf$params2estimate <- c("rho","omega","lambda","offset")
          conf$params = list('rho'=NA,'omega'=NA,'lambda'=NA,'offset'=NA)
        } 

        
        for (it in seq(iter)) {
          print(it)
          
          conf$start_vals <- NA
          opts1 = list('algorithm'='NLOPT_GN_DIRECT_L', 'xtol_rel'=1.0e-4, 'maxeval'= 5000) #check for alternatives
          
          if(mod_gen == "perc_offset"){lb = c(0,-1); ub = c(1,1); conf$start_vals <- c(0.3,0)
          } else if(mod_gen == "value_alt_offset") {lb = c(0,0,-1); ub = c(1,10,1); conf$start_vals <- c(0.5,2,0)
          } else if(mod_gen == "full_offset") {lb = c(0,0,0,-1); ub = c(1,1,10,1); conf$start_vals <- c(0.3,0.5,2,0)
          }

          # Fit model 
          fit_stat2 = nloptr::nloptr(x0=conf$start_vals,
                                     # Minimize neg LL
                                     eval_f=loglik_fit_gen,
                                     # Lower bound of parameters (e.g. c(0,0,1))
                                     lb=lb, #c(0, 0, 0),
                                     # Upper bound of parameters (e.g. c(1,1,10))
                                     ub=ub, #c(1,15,1),
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
          }else if(mod_gen == "perc_offset"){
            param_names <- c("rho","offset") 
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

          
          if (mod_gen == "value_alt_offset"){
            df_temp$offsetOUT[it] <- params$offset
            df_temp$omegaOUT[it] <- params$omega
            df_temp$lambdaOUT[it] <- params$lambda
            df_temp$rhoOUT[it] <- NA
          } else if (mod_gen == "full_offset"){
            df_temp$rhoOUT[it] <- params$rho
            df_temp$offsetOUT[it] <- params$offset
            df_temp$omegaOUT[it] <- params$omega
            df_temp$lambdaOUT[it] <- params$lambda
          } else if (mod_gen == "perc_offset"){
            df_temp$rhoOUT[it] <- params$rho
            df_temp$offsetOUT[it] <- params$offset
            df_temp$omegaOUT[it] <- NA
            df_temp$lambdaOUT[it] <- NA
          } 
          
          df_temp$ll[it] <- fit_stat2$ll
          df_temp$AIC[it] <- fit_stat2$AIC
          df_temp$AICc[it] <- fit_stat2$AICc
          df_temp$BIC[it] <- fit_stat2$BIC
          
        } 

        df <- bind_rows(df, df_temp)
       
      } 
      write.csv(df, here::here('outputs', 'model_recovery', paste0("recovery_", toString(sub),"_", gm, "_", gm2,".csv")))
    }
    df <- df_temp[0, ]
  } 
} 


