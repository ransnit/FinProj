source("init_predictors.r")
source("init_response.r")
require("randomForest")

source("delete_bad_rows.r")

train.rf.model <- function(data, test_proportion = 0.3)
{
  t1 <- Sys.time()
  cat("Started Processising...\n")
  
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
  predictors <- predictors[!is.na(response),]
  response <- response[!is.na(response)]
  
  # Assert validity:
  stopifnot(all(!is.na(predictors)), 
            all(!is.na(response)), 
            all(!is.infinite(predictors)),
            all(!is.infinite(response)))
  
  # Separate data into test and train:
  TRAIN <- ceiling((1-test_proportion)*nrow(predictors))
  
  predictors_test <- tail(predictors, -TRAIN)
  response_test <- tail(response, -TRAIN)
  predictors_train <- head(predictors, TRAIN)
  response_train <- head(response, TRAIN)
  
  rm(predictors, response)
  
  cat("Done creating predictors & response. Time duration:", Sys.time() - t1,"mins.\n")
  cat("No. of rows in train-data:", nrow(predictors_train), "\n")
  cat("No. of rows in test-data:", nrow(predictors_test), "\n")
  cat("Training RF-model...\n")
  
  # Train model:
  model <- randomForest(x = predictors_train, y = response_train, 
                        xtest = predictors_test, ytest = response_test, ntree = 10,
                        na.action = na.omit, keep.forest=T, importance = T)
  cat("Done training model. Time duration:", Sys.time() - t1,"mins.\n")
  return (model)
}

rf_model <- train.rf.model(data)
saveRDS(rf_model, "rf_model.rds")
print(rf_model)
