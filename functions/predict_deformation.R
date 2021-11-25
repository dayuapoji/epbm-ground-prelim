predict_deformation <- function(df_train, df_test, ID) {
  
  # split data
  # df_train <- df_segment %>% .[str_detect(.$ID, ID, negate = T), ]
  df_test <- df_test %>% .[str_detect(.$ID, ID), ]
  
  # linear model
  model_lm <- lm(DeltaValue ~ ., data = df_train[ , 6:ncol(df_train)])
  pred_lm <- predict(model_lm, newdata = df_test[ , 6:ncol(df_test)])
  
  # random forests
  model_rf <- ranger(DeltaValue ~ ., data = df_train[ , 6:ncol(df_train)])
  pred_rf <- predict(model_rf, data = df_test[ , 6:ncol(df_test)])
  
  # results
  results <- data.frame(ChainageHead = df_test$HeadDistance,
                        Actual = df_test$DeltaValue, 
                        PredLM = pred_lm - pred_lm[1],
                        PredRF = pred_rf$predictions - pred_rf$predictions[1])
  # ErrorLM = abs(df_test$DeltaValue - pred_lm),
  # ErrorRF = abs(df_test$DeltaValue - pred_rf$predictions))
  
  return(results)
}
