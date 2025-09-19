## Learning models for learning task

#~~~~~~~~~~~~~~~~~~
#Fixed staring val
#Fit starting val
#Pearce Hall


# estimate v0
rw_base_learn <- function(data, params, conf, pepr) {
  
  v <- matrix(0, conf$nStim,length(data$trial_sequence)+1) #value
  y <- vector() #answer given/ model choice 
  logll = vector()
  
  for (x in 1:length(data$trial_sequence)) {
    if (data$current_n[x]==1){ v[,x] = params$v0}
    
    #answer based on learned value 
    if (data$current_n[x]== 12 | data$current_n[x] == 24 ){
      y[x] <- v[conf$cspos,x] 
    }else{
      y[x] <- 0
    }
    
    #learning
    v[conf$cspos,x+1] <- v[conf$cspos,x] + (params$alpha * (data$reward[x] - v[conf$cspos,x]))
  }
  
  out <- data.frame(y= y, stimulus=data$trial_sequence, mix = rep(NA,length(data$trial_sequence)), 
                    rr = rep(NA,length(data$trial_sequence)), pu=data$pu_vec, sub=rep(sub,length(data$trial_sequence)), 
                    n = data$current_n, task = rep(0, length(data$trial_sequence)),reward = data$reward, response = data$response, logll = NA, total_n = data$total_n)   

}


# v0 fixed at 0.5
rw_fix_learn <- function(data, params, conf, pepr) {
  
  v <- matrix(0, conf$nStim,length(data$trial_sequence)+1) #value
  y <- vector() #answer given/ model choice 
  logll = vector()
  
  for (x in 1:length(data$trial_sequence)) {
    if (data$current_n[x]==1){ v[,x] = 0.5} # fixed starting value at 0.5
    
    if (data$current_n[x]== 12 | data$current_n[x] == 24 ){
      y[x] <- v[conf$cspos,x] 
    }else{
      y[x] <- 0
    }
    
    #learning
    v[conf$cspos,x+1] <- v[conf$cspos,x] + (params$alpha * (data$reward[x] - v[conf$cspos,x]))
  }
  
  out <- data.frame(y= y, stimulus=data$trial_sequence, mix = rep(NA,length(data$trial_sequence)), 
                    rr = rep(NA,length(data$trial_sequence)), pu=data$pu_vec, sub=rep(sub,length(data$trial_sequence)), 
                    n = data$current_n, task = rep(0, length(data$trial_sequence)),reward = data$reward, response = data$response, logll = NA, total_n = data$total_n)   

}


# Pearce Hall - non-constant learning rate and estimated v0
rw_ph_learn <- function(data, params, conf, pepr) {
  
  v <- matrix(0, conf$nStim,length(data$trial_sequence)+1) 
  y <- vector() #answer given/ model choice 
  logll = vector()
  alpha = vector()
  
  for (x in 1:length(data$trial_sequence)) {
    if (data$current_n[x]==1){ v[,x] = params$v0} # fixed starting value at 0.5
    
    if (data$current_n[x]== 12 | data$current_n[x] == 24 ){
      y[x] <- v[conf$cspos,x] 
    }else{
      y[x] <- 0
    }
    
    #learning
    v[conf$cspos,x+1] <- v[conf$cspos,x] + (params$alpha[x] * (data$reward[x] - v[conf$cspos,x]))
    alpha[x+1] = eta*abs(data$reward[x] - v[conf$cspos,x]) + (1-eta)*alpha[x]
  }
  
  out <- data.frame(y= y, stimulus=data$trial_sequence, mix = rep(NA,length(data$trial_sequence)), 
                    rr = rep(NA,length(data$trial_sequence)), pu=data$pu_vec, sub=rep(sub,length(data$trial_sequence)), 
                    n = data$current_n, task = rep(0, length(data$trial_sequence)),reward = data$reward, response = data$response, logll = NA, total_n = data$total_n)   
  
}
