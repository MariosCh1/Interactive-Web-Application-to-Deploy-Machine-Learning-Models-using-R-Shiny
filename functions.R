##function for removing the column
removecolumn <- function(df, nameofthecolumn){
  df[ , -which(names(df) %in% nameofthecolumn)]
}



xgb_gs_cv_regression <- function(xgb_train, subsample_choice, colsample_bytree_choice, max_depth_choice, min_child_weight_choice, eta_choice, n_rounds_choice, n_fold_choice){
  
  searchGridSubCol <- expand.grid(
                                  subsample = subsample_choice, #c(0.6, 0.65, 0.7, 0.75, 0.8), (0,1]
                                  colsample_bytree = colsample_bytree_choice, #c(0.6, 0.65,0.7, 0.75, 0.8), (0,1]
                                  max_depth = max_depth_choice, #c(8, 9, 10, 11), [0,∞]
                                  min_child_weight = min_child_weight_choice, #seq(1, 2, 3), [0,∞]
                                  eta = eta_choice, #c(0.06, 0.07, 0.08), [0,1]
                                  n_rounds = n_rounds_choice, #c(100,150,250,1000), cv parameter
                                  n_fold = n_fold_choice #c(4,5,6,10) cv parameter
                                  )

  rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, 
                                     
      function(parameterList){
      
        #Extract Parameters to test
        currentSubsampleRate <- parameterList[["subsample"]]
        currentColsampleRate <- parameterList[["colsample_bytree"]]
        currentDepth <- parameterList[["max_depth"]]
        currentEta <- parameterList[["eta"]]
        currentMinChildWeight <- parameterList[["min_child_weight"]]
        currentNRounds <- parameterList[["n_rounds"]]
        currentNFold <- parameterList[["n_fold"]]
        
        #https://www.rdocumentation.org/packages/xgboost/versions/1.6.0.1/topics/xgb.cv
        #https://xgboost.readthedocs.io/en/latest/parameter.html
        
        xgboostModelCV <- xgb.cv(objective = "reg:squarederror", #xgb parameter
                                 data =  xgb_train, 
                                 booster = "gbtree", #xgb parameter
                                 showsd = TRUE, #xgb parameter whether to show standard deviation of cross validation
                                 #metrics = "rmse",#k-folds cv parameter
                                 verbose = TRUE, #xgb print the statistics during the process
                                 print_every_n = 10, #k-folds cv parameter
                                 early_stopping_rounds = 10, #k-folds cv parameter
                                 eval_metric = "rmse", #xgb parameter
                                 "nrounds" = currentNRounds, #k-folds cv parameter
                                 "nfold" = currentNFold, #k-folds cv parameter
                                 "max_depth" = currentDepth, 
                                 "eta" = currentEta, 
                                 "subsample" = currentSubsampleRate, 
                                 "colsample_bytree" = currentColsampleRate, 
                                 "min_child_weight" = currentMinChildWeight 
                                 )
        
        xgb_cv_xvalidationScores <- xgboostModelCV$evaluation_log
        
        #best score
        test_rmse <- tail(xgb_cv_xvalidationScores$test_rmse_mean, 1)
        train_rmse <- tail(xgb_cv_xvalidationScores$train_rmse_mean,1)
        
        gs_results_output <- c(test_rmse, train_rmse, currentSubsampleRate, currentColsampleRate, currentDepth, currentEta, currentMinChildWeight, currentNRounds, currentNFold)
        
        return(gs_results_output)
        
      }
  
  )

  gs_results_varnames <- c("TestRMSE", "TrainRMSE", "SubSampRate", "ColSampRate", "Depth", "eta", "currentMinChildWeight", "nrounds", "nfold")
  t_rmseErrorsHyperparameters <- as.data.frame(t(rmseErrorsHyperparameters))
  names(t_rmseErrorsHyperparameters)<- gs_results_varnames

  return(t_rmseErrorsHyperparameters)

}


