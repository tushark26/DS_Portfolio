###################################
# NNETAR Function
###################################


##############AK#########################
mape <- function(real, pred){
  return(100 * mean(abs((real - pred)/(real+0.00000001))))
} 
#######################################



FFNN<- function(data_train = all_data){
  
  # Train Test Split   ## Need to clean in future.
  X_train <- data_train[train_period, ..x_vars]
  Y_train <- data_train[train_period,c('TOTAL_REVENUE')]
  X_test  <- data_train[test_period, ..x_vars]
  Y_test  <- data_train[test_period,c('TOTAL_REVENUE')]
  
  # Convert  train to ts
  Y_train_ts <- ts(all_data[['TOTAL_REVENUE']],start=start_date,end=end_train,frequency = 12 ) 
  
  # Convert full Y to ts
  Y_ts <- ts(all_data[['TOTAL_REVENUE']],start=start_date,end=end_date,frequency = 12 )
  
  # Auto NNETAR W/ Grid Search Result
  set.seed(2018)
  
  # Optimal Parameters Candidates
  param1 <- seq(from = 1, to = 61, by = 4) #size
  param2 <- seq(from = 0.1, to = 0.7, by = 0.05) #decay
  param3 <- seq(from = 5, to = 50, by = 5) #repeats 
  param4 <- c(TRUE, FALSE)   #scale
  
  # Create fit and pred mape arrays
  
  par1_par2_mat <- matrix( 0, nrow = length(param1), ncol = length(param2))
  rownames(par1_par2_mat) <- param1 # 
  colnames(par1_par2_mat) <- param2 # 
  
  par1_par2_vec <- as.numeric(vector(length = testing_horizon))
  
  # Tune Parameters
  tune_time_start <- Sys.time()
  for(i in seq_along(param1)){ 
    for(j in seq_along(param2)){ 
      print(paste('Trying Size : ',param1[i], ' and Decay : ', param2[j]))
      set.seed(2018)
      
      # Without Ex Reg
      if(data_train$LOD[1] %in% c(eval(H_LVL_2),eval(H_LVL_3),eval(H_LVL_4))){
        print("Without External Variables")
        fit1 <- nnetar(Y_train_ts, 
                       decay=param2[j],
                       size = param1[i],
                       MaxNWts= 2000)
        par1_par2_vec <- forecast(fit1,h = forecast_horizon)$mean
      }
      # With Ex Reg
      else if(data_train$LOD[1] == H_LVL_1){
        fit1 <- nnetar(Y_train_ts, 
                       xreg=data.table(X_train),
                       decay=param2[j],
                       size = param1[i],
                       MaxNWts= 2000)
        par1_par2_vec <- forecast(fit1,xreg=data.table(X_test))$mean
      }
      par1_par2_mat[i,j] <- mape(Y_test[['TOTAL_REVENUE']],as.vector(par1_par2_vec))
    }
  }
  
  # Get the index es of min MAPE forecasted
  param1_arg <- (dimnames(par1_par2_mat)[[1]][which.min(apply(par1_par2_mat, MARGIN = 1,min))])
  param2_arg <- (dimnames(par1_par2_mat)[[2]][which.min(apply(par1_par2_mat, MARGIN = 2,min))])
  param_pred_MAPE_PRED <- round(par1_par2_mat[param1_arg,param2_arg],2)
  print(paste("min predicted MAPE  ", param_pred_MAPE_PRED))
  
  # Scale vs size
  par3_par4_mat <- matrix( 0, nrow = length(param3), ncol = length(param4))
  rownames(par3_par4_mat) <- param3 
  colnames(par3_par4_mat) <- param4 
  
  par3_par4_vec <- as.numeric(vector(length = testing_horizon))
  
  
  for(i in seq_along(param3)){ 
    for(j in seq_along(param4)){ 
      print(paste('Trying Repeats : ',param3[i], ' and Scale : ', param4[j]))
      set.seed(2018)
      
      # Without X Reg
      if(data_train$LOD[1] %in% c(eval(H_LVL_2),eval(H_LVL_3),eval(H_LVL_4))){
        print("Without External Variables")
        fit1 <- nnetar(Y_train_ts, 
                       repeats = param3[i],
                       scale.inputs = param4[j],
                       decay = as.numeric(param2_arg),
                       size  = as.numeric(param1_arg),
                       MaxNWts= 2000
        )
        par3_par4_vec <- forecast(fit1,h = forecast_horizon)$mean
      }
      # With X Reg
      else if(data_train$LOD[1] == H_LVL_1){
        print("With External Variables")
        fit1 <- nnetar(Y_train_ts, 
                       xreg=X_train,
                       repeats = param3[i],
                       scale.inputs = param4[j],
                       decay = as.numeric(param2_arg),
                       size  = as.numeric(param1_arg),
                       MaxNWts= 2000
        )
        par3_par4_vec <- forecast(fit1,xreg=X_test)$mean
      }
      
      par3_par4_mat[i,j] <- mape(Y_test[['TOTAL_REVENUE']],as.vector(par3_par4_vec))
    }
  }
  
  # Get the index es of min MAPE forecasted
  param3_arg <- (dimnames(par3_par4_mat)[[1]][which.min(apply(par3_par4_mat, MARGIN = 1,min))])
  param4_arg <- (dimnames(par3_par4_mat)[[2]][which.min(apply(par3_par4_mat, MARGIN = 2,min))])
  param_pred_MAPE_PRED <- round(par3_par4_mat[param3_arg,param4_arg],2)
  print(paste("min predicted MAPE  ", param_pred_MAPE_PRED))
  
  tune_time_total <- Sys.time() - tune_time_start
  paste('Tuning for Size, Decay, Scale and Repeats took ', tune_time_total)
  
  
  # Fit the model with tuned parameters 
  
  # Without X Reg 
  if(data_train$LOD[1] %in% c(eval(H_LVL_2),eval(H_LVL_3),eval(H_LVL_4))){
    
    # Fit on full data
    fit_uni <- nnetar( Y_train_ts, 
                       repeats      = as.numeric(param3_arg),
                       scale.inputs = eval(parse(text=param4_arg)),
                       decay        = as.numeric(param2_arg),
                       size         = as.numeric(param1_arg),
                       MaxNWts= 2000
    )
    # Components of the list to be returned
    val <- as.vector(forecast(fit_uni, h = testing_horizon)$mean)    
    fit <- as.vector(fit_uni$fitted)
    
    #forecast out 2018
    nnetar_fcst <- nnetar(y            = Y_ts,
                          model        = fit_uni,
                          repeats      = as.numeric(param3_arg),
                          scale.inputs = eval(parse(text=param4_arg)),
                          decay        = as.numeric(param2_arg),
                          size         = as.numeric(param1_arg),
                          MaxNWts= 2000)
    fcst      <- as.vector((forecast(nnetar_fcst, h=forecast_horizon))$mean)  
  }
  
  # With X Reg 
  else if(data_train$LOD[1] == H_LVL_1){
    fit_multi <- nnetar( Y_train_ts, 
                         xreg         = X_train,
                         repeats      = as.numeric(param3_arg),
                         scale.inputs = eval(parse(text=param4_arg)),
                         decay        = as.numeric(param2_arg),
                         size         = as.numeric(param1_arg),
                         MaxNWts= 2000
    )
    
    # Components of the list to be returned
    val <- as.vector(forecast(fit_multi, h=testing_horizon, xreg = X_test)$mean)    
    fit <- as.vector(fit_multi$fitted)
    
    #forecast out 2018
    nnetar_fcst <- nnetar(y = Y_ts,
                          xreg = as.data.table(data_train)[, ..x_vars],
                          model = fit_multi,
                          repeats = as.numeric(param3_arg),
                          scale.inputs = eval(parse(text=param4_arg)),
                          decay = as.numeric(param2_arg),
                          size  = as.numeric(param1_arg),
                          MaxNWts= 2000
    )
    
    fcst      <- as.vector((forecast(nnetar_fcst, h=forecast_horizon, xreg = xreg_fcst))$mean)   
  }
  
  details <- paste('NNAR(',nnetar_fcst$p,',',
                   nnetar_fcst$P,',',
                   nnetar_fcst$size,')[12]', sep='')
  
  return(list(val,fit,fcst,details))
  print('nnetar complete.')
}

###################################################################
nnetar_output <- function(all_data = all_data){ 
  print("NNETAR")
  test_NNETAR<- FFNN(all_data)
  
  # Store final results in dataframe
  ID         <- Seg_num
  DATE       <- forecast_dates
  TEST_FORECAST <- round(as.numeric(test_NNETAR[[1]]),2)
  MODEL_NAME <- "NNETAR"
  FORECASTRESULT   <- round(as.numeric(test_NNETAR[[3]]),2)
  DETAILS    <- test_NNETAR[[4]]
  MAPE<- round(as.numeric(ALL_Q(all_data[['TOTAL_REVENUE']][test_period],TEST_FORECAST)[[2]]),2)
  RESULT_NNETAR <- data.frame(ID,DATE,MODEL_NAME,FORECASTRESULT,DETAILS,MAPE,TEST_FORECAST)
  return(RESULT_NNETAR)
}