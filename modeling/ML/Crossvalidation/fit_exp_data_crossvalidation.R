# Script to fit models to data 

required_packages = c("here", "nloptr", "tibble", "optparse")
invisible(lapply(required_packages, require, character.only = TRUE))
#renv::restore()
here::i_am("flag_project_root.R")

##Source everything
source_path = file.path(here::here('phd_models/ML/Models'), fsep = .Platform$file.sep)
source_files = list.files(source_path, pattern = "[.][rR]$", full.names = TRUE, recursive = TRUE)
invisible(lapply(source_files, function(x) source(x)))
source(here::here('modeling', 'ML', "helper_func.R"))
source(here::here('modeling', 'ML', "Models", "all_models.R"))

library(Metrics)

#Function to predict responses based on model 
model_predict <- function(params, data, conf) {

  data$current_n[1] = 25 #for how model is built, first entry needs to be indexed as 25 to pass on value
  if(conf$model_name == 'perc_est'){
    predicted = rw_perc_est_gen(data, params, conf, pepr)
  } else if(conf$model_name == 'value_alt_offset'){
    predicted = rw_value_alt_offset_gen(data, params, conf, pepr)
  } else if(conf$model_name == 'full'){
    predicted = rw_full_alt_offset_gen(data, params, conf, pepr)
  } 

  return(predicted)
}




#### Basic config.
nSubj = 4 # n of ratings x stimulus 
iter = 30 # iterations 
nStim = 9 # n of stimuli 
nPerc = 2 # n PU levels 
pulevels = c(0.4, 0.2)
xmin = -4
xmax = 4
# Number of CV folds
K <- 5

xs = seq(xmin, xmax, by=((xmax - (xmin))/(nStim - 1))) #stimulus space -4:4
task = c(rep(0, 24), rep(1, 36)) # trial x task 
df_template <- data.frame(matrix(,nrow=iter*K))
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



#set.seed(42)

for (sub in c(subject)) {
  
df_temp <- df_template
df_temp$sub <- sub
  
# Define models to fit 
models = c('full', 'value_alt_offset','perc_est') #'full', 
  
for (gm in models) {
for (cond in 0:5) {  # task conditions
      
# Get subject & generalisation data
dfs = df_exp[df_exp$subject %in% sub & df_exp$condition %in% cond, ] 
df_sub = dfs[dfs$task %in% 1, ]
df_sub$total_n = c(1: nrow(df_sub))
rrr = unique(df_sub$rr)
ppp = unique(df_sub$pu)
      
# Get learning rating (used as prior)
df_learn_info = dfs[dfs$task %in% 0 & dfs$n_total == 24, ]
df_learn_info <- df_learn_info$response
      
      
data = list('nSubj' =length(unique(df_exp$sub)), 'sub' = sub, 'reward' = df_sub$reward, 'trial_sequence' = df_sub$stimulus, 'pu_vec' = df_sub$pu, 'ou_vec' = df_sub$ou, 
            'current_n' = df_sub$n_total, 'pepr' = NA, 'xs' = xs,
            'task' = task, response = df_sub$response, 'total_n' = df_sub$total_n, 'condition' = df_sub$condition, 'learn_info' = df_learn_info)



# Prepare config
mod_gen = gm
conf = list('model_name' = mod_gen, 'nStim' = nStim, 'cspos' = 5, 'get_loglik' = 1, 'nStimNC' = nStim*2)

if(mod_gen == "perc_est"){
  conf$params2estimate <- c("rho")
  conf$params = list('rho' = NA)
} else if(mod_gen == "value_alt_offset"){
  conf$params2estimate <- c("omega","lambda","offset")
  conf$params = list('omega'=NA,'lambda'=NA, 'offset'=NA)
} else if(mod_gen == "full"){
  conf$params2estimate <- c("rho", "omega","lambda","offset")
  conf$params = list('rho'=NA, 'omega'=NA,'lambda'=NA, 'offset'=NA)
} 

for (fold in 1:K) {

  
opts1 = list('algorithm' = 'NLOPT_GN_DIRECT_L', 'xtol_rel' = 1e-4, 'maxeval' = 5000)

if(mod_gen == "perc_est"){lb = c(0); ub = c(1); conf$start_vals <- c(0.3)
} else if(mod_gen == "value_alt_offset") {lb = c(0,0,-1); ub = c(1,10,1); conf$start_vals <- c(0.5,2,0.01)
} else if(mod_gen == "full") {lb = c(0,0,0,-1); ub = c(1,1,10,1); conf$start_vals <- c(0.3,0.5,2,0.01)
} 

# Prepare folds
n_trials <- length(data$trial_sequence)
fold_ids <- sample(rep(1:K, length.out = n_trials))
trial_keys <- c("trial_sequence", "reward", "pu_vec", "task", 
                "response", "total_n", "condition", "ou_vec", "current_n")

cv_results <- vector("list", K)
all_predictions <- c()
all_observed <- c()

for (it in seq(iter)) {
  cat(sprintf("Sub %s | Model %s | Cond %d | Fold %d | Iter %d\n", sub, mod_gen, cond, fold, it))
  
  
  test_idx <- which(fold_ids == fold)
  train_idx <- setdiff(seq_len(n_trials), test_idx)
  
  data_train <- data
  data_test  <- data

  for (key in trial_keys) {
    data_train[[key]] <- data[[key]][train_idx]
    data_test[[key]]  <- data[[key]][test_idx]
  }
  
  
  # Fit model on training set
  fit <- nloptr::nloptr(x0 = conf$start_vals,
                        eval_f = loglik_fit_gen,
                        lb = lb,
                        ub = ub,
                        opts = opts1,
                        data = data_train,
                        conf = conf)
  
  # Name parameters
  if (mod_gen == "perc_est") {
    param_names <- c("rho")
  } else if (mod_gen == "value_alt_offset") {
    param_names <- c("omega", "lambda", "offset")
  } else if (mod_gen == "full") {
    param_names <- c("rho", "omega", "lambda", "offset")
  }
  
  params <- fit$solution
  names(params) <- param_names
  params <- as.list(params)
  
  #get prediction on test set

  #print("check par")
  #print(params)
  preds <- model_predict(params, data_test, conf)  #sim data based on fitted parameters 
  obs <- data_test$response #obs ppt response 
  #print("out of sample")
  #print(preds$y)
  #print(obs)
  
  # Store predictions for later summary
  all_predictions <- c(all_predictions, preds)
  all_observed <- c(all_observed, obs)
  
  # Calculate fold-level metrics

  #print(head(obs))
  #print(head(preds$y))
  fold_mae <- mae(obs, preds$y)
  fold_rmse <- rmse(obs, preds$y)
  fold_corr <- cor(obs, preds$y, method = "spearman")
  fold_errors <- preds$y - obs
  
  


#} #fold
#Print res
cat(sprintf("MAE: %.4f\n", cv_results[[5]]$mae))
cat(sprintf("RMSE: %.4f\n", cv_results[[5]]$rmse))
cat(sprintf("SPEARMAN: %.4f\n", cv_results[[5]]$spearman))
  

  # Save
  fidx = (iter*fold  + it)- iter#fold index
  #print(fidx)
  
  df_temp$iter[fidx] = it
  df_temp$fold[fidx] = fold

  if(mod_gen == "perc_est"){
    df_temp[fidx, paste(c("rho_", mod_gen, "R", rrr, "P", ppp), collapse = "")] <- params$rho
  } else if (mod_gen == "value_alt_offset"){
    df_temp[fidx, paste(c("offset_", mod_gen, "R", rrr, "P", ppp), collapse = "")] <- params$offset
    df_temp[fidx, paste(c("omega_", mod_gen, "R", rrr, "P", ppp), collapse = "")] <- params$omega
    df_temp[fidx, paste(c("lambda_", mod_gen, "R", rrr, "P", ppp), collapse = "")] <- params$lambda
  } else if (mod_gen == "full"){
    df_temp[fidx, paste(c("rho_", mod_gen, "R", rrr, "P", ppp), collapse = "")] <- params$rho
    df_temp[fidx, paste(c("offset_", mod_gen, "R", rrr, "P", ppp), collapse = "")] <- params$offset
    df_temp[fidx, paste(c("omega_", mod_gen, "R", rrr, "P", ppp), collapse = "")] <- params$omega
    df_temp[fidx, paste(c("lambda_", mod_gen, "R", rrr, "P", ppp), collapse = "")] <- params$lambda
  } 
  
  df_temp[fidx,paste(c("mae_", mod_gen, "R", rrr, "P", ppp), collapse = "")] <- fold_mae
  df_temp[fidx,paste(c("rmse_", mod_gen, "R", rrr, "P", ppp), collapse = "")] <- fold_rmse
  df_temp[fidx,paste(c("spearman_", mod_gen, "R", rrr, "P", ppp), collapse = "")] <- fold_corr

  
} #iter

} #fold 

} #cond
} #mod
df<- rbind(df, df_temp)
#write.csv(df, here::here('outputs', 'fitting', 'ITER30', paste0("fits_", toString(sub),"full_set.csv")))
write.csv(df, here::here('outputs', 'fitting', 'crossvalidationFULL', paste0("cvfits", toString(sub),"full_set.csv")))
#df <- data.frame()
} #sub
#write.csv(df, here::here('outputs', 'fitting', 'crossvalidation', paste0("cvfits","full_set.csv")))
