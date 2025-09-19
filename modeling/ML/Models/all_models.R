## Generalisation models
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Value
#Perception
#Hybrid
#Sigmoid


# Value model parametrised by lambda, omega and offset 
rw_value_alt_offset_gen <- function(data, params, conf, pepr) {
  v <- matrix(0, conf$nStim, length(data$trial_sequence) + 1)
  y <- vector()
  G <- matrix(0, conf$nStim, 1) # init generalisation function

  for (x in 1:length(data$trial_sequence)) {
    if (data$current_n[x] == 25) { #25 is first generalisation trial
      v[conf$cspos, ] = data$learn_info #set last learning value as first gen value
      
      # value generalisation 
      for (s in 1:conf$nStim) {
        if (params$omega >= 0.5) { #fit depending on pattern
          G[s, 1] = 1 * exp(-(((xs)[s]^2) / (2 * params$lambda^2)))
        } else {
          G[s, 1] = 1 * exp(- ((xs)[s]^2) / (2 * params$lambda^2))
          if(s <5){ G[s,1]= 1 + abs(1 - G[s,1])
          }
        }
        v[s, ] <- (v[conf$cspos, x] * (G[s, 1]))  #generalise to set basis
      }
      #add offset 
      v[,] <- pmin(pmax(v[,] + params$offset, 0), 1)
    }
    #response
    y[x] <- v[(data$trial_sequence[x] + conf$cspos), x]
  }
  
  out2 <-
    data.frame(
      y = y,
      stimulus = data$trial_sequence,
      mix = rep(NA, length(data$trial_sequence)),
      rr = rep(NA, length(data$trial_sequence)),
      pu = data$pu_vec,
      sub = rep(sub, length(data$trial_sequence)),
      n = data$current_n,
      task = rep(1, length(data$trial_sequence)),
      reward = data$reward,
      response = data$response,
      logll = rep(NA, length(data$trial_sequence)),
      total_n = data$total_n
    )
}


#Hybrid model: Value + Perceptual 

rw_full_alt_offset_gen <- function(data, params, conf, pepr) {

  v <- matrix(0, conf$nStim, length(data$trial_sequence) + 1) #value
  y <- vector() #answer given/ model choice
  mixing <- vector() #perceived stimulus/ perceptual uncertainty
  G <- matrix(0, conf$nStim, 1) # generalisation function
  logll = vector()
  
  for (x in 1:length(data$trial_sequence)) {
    if (data$current_n[x] == 25){ #25 is the first generalisation trial!!
      v[conf$cspos, ] = data$learn_info #set last learning value as first gen value
      
      # value generalisation 
      for (s in 1:conf$nStim) {
        if (params$omega >= 0.5) {
          G[s, 1] = 1 * exp(-(((xs)[s]^2) / (2 * params$lambda^2)))
        } else {
          G[s, 1] = 1 * exp(- ((xs)[s]^2) / (2 * params$lambda^2))
          if(s <5){ G[s,1]= 1 + abs(1 - G[s,1])
          }
        }
        v[s, ] <- (v[conf$cspos, x] * (G[s, 1]))  #generalise to set basis
      }
      
      v[,] <- pmin(pmax(v[,] + params$offset, 0), 1) #add offset
    }
    
    # build perceptual probabilities for stimulus continuum based on distance from presented stim
    pepa = params$rho^(abs(xs))
    pepa[5] = 1-params$rho
    
    if ((data$trial_sequence[x] + conf$cspos) >5){
      pepa = c(rep(0, abs(5-(data$trial_sequence[x] + conf$cspos))), pepa)
      pepa <- pepa[1:9]
    } else if((data$trial_sequence[x] + conf$cspos)<5){
      pepa = pepa[-(1:(abs(5-(data$trial_sequence[x] + conf$cspos))))]
      pepa = c(pepa, rep(0, abs(5-(data$trial_sequence[x] + conf$cspos))))
    }
    
    #perceptual uncertainty
    mixing[x] = sample(data$xs, size = 1, replace = FALSE, prob = pepa)
    y[x] <- v[(mixing[x] + conf$cspos), x]
    v[conf$cspos, x + 1] <- v[conf$cspos, x]
  }
  
  out2 <-
    data.frame(
      y = y,
      stimulus = data$trial_sequence,
      mix = rep(NA,length(data$trial_sequence)),
      rr = rep(NA, length(data$trial_sequence)),
      pu = data$pu_vec,
      sub = rep(data$sub, length(data$trial_sequence)),
      n = data$current_n,
      task = rep(1, length(data$trial_sequence)),
      reward = data$reward,
      response = data$response,
      logll = rep(NA, length(data$trial_sequence)),
      total_n = data$total_n
    )
}


# Perceptual model 
rw_perc_est_offset <- function(data, params, conf, pepr) {
  v <- matrix(0, conf$nStim, length(data$trial_sequence) + 1) 
  y <- vector() 
  mixing <- vector() 
  logll = vector()
  
  for (x in 1:length(data$trial_sequence)) {
    if (data$current_n[x] == 25){  #first generalisation trial
      v[conf$cspos, x] = data$learn_info #set last learning value as first gen value
    }
    
    v[, x] <- pmin(pmax(v[, x] + params$offset, 0), 1) #add offset
    
    # build perceptual probabilities for stimulus continuum based on distance from presented stim
    pepa = params$rho^(abs(xs))
    pepa[5] = 1-params$rho
    
    if ((data$trial_sequence[x] + conf$cspos) >5){
      pepa = c(rep(0, abs(5-(data$trial_sequence[x] + conf$cspos))), pepa)
      pepa <- pepa[1:9]
    } else if((data$trial_sequence[x] + conf$cspos)<5){
      pepa = pepa[-(1:(abs(5-(data$trial_sequence[x] + conf$cspos))))]
      pepa = c(pepa, rep(0, abs(5-(data$trial_sequence[x] + conf$cspos))))
    }
    
    # draw perceived stimulus according to distribution 
    mixing[x] = sample(data$xs, size = 1, replace = FALSE, prob = pepa)
    y[x] <- v[(mixing[x] + conf$cspos), x]
    v[conf$cspos, x + 1] <- v[conf$cspos, x]
    
  }
  
  # save all relevant information
  out2 <-
    data.frame(
      y = y,
      stimulus = data$trial_sequence,
      mix = rep(NA, length(data$trial_sequence)),
      rr = rep(NA, length(data$trial_sequence)),
      pu = data$pu_vec,
      sub = rep(sub, length(data$trial_sequence)),
      n = data$current_n,
      task = rep(1, length(data$trial_sequence)),
      reward = data$reward,
      response = data$response,
      logll = rep(NA, length(data$trial_sequence)),
      total_n = data$total_n
    )
}





#Sigmoid - value 
rw_value_sig <- function(data, params, conf, pepr) {
  v <- matrix(0, conf$nStim, length(data$trial_sequence) + 1)
  y <- vector()
  G <- matrix(0, conf$nStim, 1) # init generalisation function
  
  for (x in 1:length(data$trial_sequence)) {
    if (data$current_n[x] == 25) { #25 is first generalisation trial
      v[conf$cspos, ] = data$learn_info #set last learning value as first gen value
      
      # value generalisation 
      for (s in 1:conf$nStim) {
        if (params$omega >= 0.5) { #fit depending on pattern
          G[s, 1] = 1 * exp(-(((xs)[s]^2) / (2 * params$lambda^2)))
        } else {
          G[s, 1] = 1 * exp(- ((xs)[s]^2) / (2 * params$lambda^2))
          if(s <5){ G[s,1]= 1 + abs(1 - G[s,1])
          }
        }
        v[s, ] <- (v[conf$cspos, x] * (G[s, 1]))  #generalise to set basis
      }

      v[,] <- pmin(pmax(v[,] + params$offset, 0), 1) #add offset 
    }

    #response: sigmoid trans
    y_trans = 1 / (1 + exp(-params$k * ((v[(data$trial_sequence[x] + conf$cspos), x]) - 0.5))) #k = slope
    y[x] <- y_trans
  }
  
  out2 <-
    data.frame(
      y = y,
      stimulus = data$trial_sequence,
      mix = rep(NA, length(data$trial_sequence)),
      rr = rep(NA, length(data$trial_sequence)),
      pu = data$pu_vec,
      sub = rep(sub, length(data$trial_sequence)),
      n = data$current_n,
      task = rep(1, length(data$trial_sequence)),
      reward = data$reward,
      response = data$response,
      logll = rep(NA, length(data$trial_sequence)),
      total_n = data$total_n
    )
}

