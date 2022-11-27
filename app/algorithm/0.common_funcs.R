


library(tidyverse)
library(lubridate)
library(data.table)
library(dtplyr)
library(tictoc)
library(glue)
library(pROC)
library(caret)
library(Metrics)
library(xgboost)
options(dplyr.summarise.inform = FALSE)







trainer_func <- function(train_set,
                         validation_set,
                         explanatory_variables,
                         hypergrid)
{
  target_variable <- "label"
  best_val_rmse <- 1000000
  best_val_predictions <- NULL
  train_features <-
    train_set %>% select(all_of(explanatory_variables)) %>% data.matrix()
  train_labels   <- train_set[[target_variable]]
  val_features   <-
    validation_set %>% select(all_of(explanatory_variables)) %>% data.matrix()
  val_labels     <- validation_set[[target_variable]]
  print(glue('Hyperparameter tuning begins...'))
  for (i in 1:nrow(hypergrid)) {
    xgb <-
      xgb.train(
        data        = train_features %>% xgboost::xgb.DMatrix(label = train_labels),
        eta         = hypergrid[['eta']][i],
        max_depth   = hypergrid[['max_depth']][i],
        nrounds     = hypergrid[['nrounds']][i],
        objective   = "reg:squarederror",
        eval_metric = 'rmse',
        watchlist   = list(
          train = train_features %>% xgboost::xgb.DMatrix(label = train_labels),
          test  = val_features   %>% xgboost::xgb.DMatrix(label = val_labels)
        ),
        early_stopping_rounds = 30,
        verbose     = 0
      )
    
    val_predictions <-
      predict(xgb, data.matrix(validation_set %>% select(all_of(
        explanatory_variables
      ))))
    suppressMessages({
      hypergrid[['rmse']][i] <- rmse(val_labels, val_predictions)
    })
    
    print(
      glue(
        "[{now()}]  eta={hypergrid[['eta']][i]}, max_depth={hypergrid[['max_depth']][i]}, nrounds={xgb$best_iteration}, rmse={hypergrid[['rmse']][i] %>% round(4)}"
      )
    )
    hypergrid[['nrounds']][i] <- xgb$best_iteration
    
    if (best_val_rmse > hypergrid[['rmse']][i]) {
      best_val_rmse         <- hypergrid[['rmse']][i]
      best_val_predictions <- val_predictions
    }
    
  }
  best_params <-
    hypergrid %>%
    arrange(desc(rmse)) %>%
    head(1) %>%
    as.list()
  toc()
  
  
  stuff <- list()
  stuff$mdl <- xgb
  stuff$hypergrid <- hypergrid
  stuff$best_params <- best_params
  stuff$val_pred_vs_actual <-
    tibble(prediction = best_val_predictions,
           actual = val_labels)
  
  return(stuff)
  
  
  
}


ohe_encoder <- function(df) {
  dummy <- dummyVars(" ~ .", data = df)
  dummy
}




tester_func <- function(mdl, test_set) {
  test_features <- test_set %>% select(all_of(mdl$feature_names))
  test_predictions <- predict(mdl, data.matrix(test_features))
  
  results <- list()
  results[['test_predictions']] <-
    tibble(prediction = test_predictions)
  
  results
  
  
  
}


## *************************















calc_mode <- function(x) {
  # List the distinct / unique values
  distinct_values <- unique(x)
  
  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  
  # Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}
