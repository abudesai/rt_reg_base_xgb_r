

## ---- Initialising libraries ----
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(lubridate)))
suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(httr)))
suppressWarnings(suppressMessages(library(glue)))

## Load model
trained_model  <- read_rds('/opt/ml_vol/model/artifacts/model.rds')
variables_to_encode   <- trained_model$variables_to_encode
id_column             <- trained_model$id_column
exp_vars              <- trained_model$exp_vars
encodings             <- trained_model$encodings
variables_numeric     <- trained_model$variables_numeric
id_column             <- trained_model$id_column




prediction_scorer <- function(row) {
  ## Function to get data and return probability
  
  ## initialize scores
  score  <- 0
  
  ## Encode categorical features with number of training encoding
  if (length(variables_to_encode) != 0)
  {
    full_data_categorical <-
      row  %>% select(variables_to_encode) %>%
      mutate(across(everything(), ~ replace_na(.x, calc_mode(.x))))
    
    for (i in variables_to_encode) {
      #define original categorical labels
      encoding <- encodings[[i]]
      #convert labels to numeric values
      full_data_categorical[[i]] = transform(encoding, full_data_categorical[[i]])
    }
    row <-
      cbind(id, full_data_numeric, full_data_categorical)
    
  } else{
    row <-
      cbind(id, full_data_numeric)
    
  }
  
  model <- trained_model$mdl
  
  ## Getting probability
  score <-
    predict(model, data.matrix(row %>% select(all_of(
      model$feature_names
    ))))
  
  score
}




#* @post /infer
function(req) {
  ## grab the request body 'req' and put it into the variable 'row'
  row <- jsonlite::fromJSON(req$postBody) %>% as_tibble()
  row %>% glimpse()
  
  ## placeholder for JSON string to be printed at the end
  result <-
    tibble(prediction = 0,
           warnings = '')
  
  ## parameters that we need
  necessary_params <- exp_vars
  
  ## if we do NOT have all we need...
  if (!all(necessary_params %in% names(row))) {
    result$prediction <- 0
    result$warnings <- 'Some necessary features are missing'
    
  } else {
    ## keep only the necessary parameters
    row <- row[necessary_params]
    
    ## if any of the necessary parameters are null...
    if (row %>% sapply(is.null) %>% any()) {
      result$prediction <- 0
      result$warnings <-
        paste('The following required parameters were NULL:',
              null_parameters)
      
    } else {
      prediction <- prediction_scorer(row)
      
      
    }
    
    c(result$prediction,
      result$warnings)
  }
  
}



#* @get /ping
#* @serializer json list(auto_unbox=TRUE)
endpoint.healthz <- function(req) {
  return("it's working perfectly")
}
