

get_parameters2 <- function(df) {
  
  all_combi <- c("rmse_perc_estR", "rmse_value_alt_offsetR")

  for (s in unique(df$sub)) {
    for (ic in all_combi) {
      
      fold_min_vals <- list()
      fold_min_idxs <- list()
      
      for (f in unique(df$fold[df$sub == s])) {
        dftemp <- subset(df, sub == s & fold == f)
        df2 <- dftemp[, grepl(ic, names(dftemp))]
        
        #find best fitting iter x fold. 
        fold_min_vals[[as.character(f)]] <- apply(df2, 2, min, na.rm = TRUE) #min iter
        fold_min_idxs[[as.character(f)]] <- sapply(df2, which.min) #corresp index
      }
      
      # Get corresponding parameters to best fitting iteration 
      if (ic == "rmse_perc_estR") {
        rho_vals_all <- list()
        for (f in names(fold_min_idxs)) {
          dftemp <- subset(df, sub == s & fold == as.numeric(f))
          rho_cols <- dftemp[, grepl("rho_perc_est", names(dftemp))]
          idxs <- fold_min_idxs[[f]]
          rho_vals <- mapply(function(col, idx) if (!is.na(idx)) col[idx] else NA,
                             as.data.frame(rho_cols), idxs)
          rho_vals_all[[f]] <- rho_vals
        }
        
        # average over folds 
        rho_matrix <- do.call(rbind, rho_vals_all)
        mean_rhos <- colMeans(rho_matrix, na.rm = TRUE)
        
        for (i in seq_along(mean_rhos)) {
          df[[paste0("RHO_perc", i)]][df$sub == s] <- mean_rhos[i]
        }
        
        
        
      } else if (ic == "rmse_value_alt_offsetR") {
        lambda_all <- list()
        omega_all <- list()
        offset_all <- list()
        
        for (f in names(fold_min_idxs)) {
          dftemp <- subset(df, sub == s & fold == as.numeric(f))
          idxs <- fold_min_idxs[[f]]
          
          lambda_cols <- dftemp[, grepl("lambda_value_alt_offset", names(dftemp))]
          omega_cols  <- dftemp[, grepl("omega_value_alt_offset", names(dftemp))]
          offset_cols <- dftemp[, grepl("offset_value_alt_offset", names(dftemp))]
          
          lambda_all[[f]] <- sapply(seq_along(idxs), function(i) lambda_cols[idxs[i], i])
          omega_all[[f]]  <- sapply(seq_along(idxs), function(i) omega_cols[idxs[i], i])
          offset_all[[f]] <- sapply(seq_along(idxs), function(i) offset_cols[idxs[i], i])
        }
        
        mean_lambda <- colMeans(do.call(rbind, lambda_all), na.rm = TRUE)
        mean_omega  <- colMeans(do.call(rbind, omega_all), na.rm = TRUE)
        mean_offset <- colMeans(do.call(rbind, offset_all), na.rm = TRUE)
        
        df[df$sub == s, paste0("LDA_value_alt_offset", 1:6)] <- matrix(rep(mean_lambda, each = sum(df$sub == s)), ncol = 6, byrow = TRUE)
        df[df$sub == s, paste0("OM_value_alt_offset", 1:6)]  <- matrix(rep(mean_omega, each = sum(df$sub == s)),  ncol = 6, byrow = TRUE)
        df[df$sub == s, paste0("OFF_value_alt_offset", 1:6)] <- matrix(rep(mean_offset, each = sum(df$sub == s)), ncol = 6, byrow = TRUE)
        
      } else if (ic == "rmse_fullR") {
        lambda_all <- list()
        omega_all <- list()
        offset_all <- list()
        rho_all <- list()
        
        for (f in names(fold_min_idxs)) {
          dftemp <- subset(df, sub == s & fold == as.numeric(f))
          idxs <- fold_min_idxs[[f]]
          
          lambda_cols <- dftemp[, grepl("lambda_full", names(dftemp))]
          omega_cols  <- dftemp[, grepl("omega_full", names(dftemp))]
          offset_cols <- dftemp[, grepl("offset_full", names(dftemp))]
          rho_cols <- dftemp[, grepl("rho_full", names(dftemp))]
          
          lambda_all[[f]] <- sapply(seq_along(idxs), function(i) lambda_cols[idxs[i], i])
          omega_all[[f]]  <- sapply(seq_along(idxs), function(i) omega_cols[idxs[i], i])
          offset_all[[f]] <- sapply(seq_along(idxs), function(i) offset_cols[idxs[i], i])
          rho_all[[f]] <- sapply(seq_along(idxs), function(i) rho_cols[idxs[i], i])
        }
        
        mean_lambda <- colMeans(do.call(rbind, lambda_all), na.rm = TRUE)
        mean_omega  <- colMeans(do.call(rbind, omega_all), na.rm = TRUE)
        mean_offset <- colMeans(do.call(rbind, offset_all), na.rm = TRUE)
        mean_rho <- colMeans(do.call(rbind, rho_all), na.rm = TRUE)
        
        df[df$sub == s, paste0("LDA_full", 1:6)] <- matrix(rep(mean_lambda, each = sum(df$sub == s)), ncol = 6, byrow = TRUE)
        df[df$sub == s, paste0("OM_full", 1:6)]  <- matrix(rep(mean_omega, each = sum(df$sub == s)),  ncol = 6, byrow = TRUE)
        df[df$sub == s, paste0("OFF_full", 1:6)] <- matrix(rep(mean_offset, each = sum(df$sub == s)), ncol = 6, byrow = TRUE)
        df[df$sub == s, paste0("RHO_full", 1:6)] <- matrix(rep(mean_rho, each = sum(df$sub == s)), ncol = 6, byrow = TRUE)
      }
    }
  }
  
  return(df)
}

