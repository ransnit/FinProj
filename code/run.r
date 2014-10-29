source("init_predictors.r")
source("init_response.r")
require("randomForest")

source("delete_bad_rows.r")

train.rf.model <- function(data, test_proportion = 0.3)
{
  # Initialize structures:
  predictors <- create.predictors(data)
  response <- create.response(data)
  
  stopifnot(nrow(predictors) == length(response))
  nrows <- nrow(predictors)
  
  # Save files:
  saveRDS(predictors, "predictors.rds")
  saveRDS(response, "response.rds")
  
  # Truncate structurs:
  to_delete <- rows.to.delete(nrows)
  predictors <- predictors[-to_delete,]
  response <- response[-to_delete]
  
  # Assert validity:
  stopifnot(all(!is.na(predictors)), 
            all(!is.na(response)), 
            all(!is.infinite(predictors)),
            all(!is.infinite(response)))
  
  # Separate data into test and train:
  TRAIN <- ceiling(test_proportion*nrow(data))
  predictors_t <- tail(predictors, -TRAIN)
  response_t <- tail(response, -TRAIN)
  predictors <- head(predictors, TRAIN)
  response <- head(response, TRAIN)
  
  # Train model:
  model <- randomForest(x = predictors, y = response, xtest = predictors_t, ytest = response_t, 
                        na.action = na.omit, keep.forest=T, importance = T)
  return (model)
}

rf_model <- train.rf.model(data)
saveRDS(rf_model, "rf_model.rds")
print(rf_model)
