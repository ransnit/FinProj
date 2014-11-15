source("init_predictors.r")
source("init_response.r")
source("delete_bad_rows.r")
require("randomForest")

train.rf.model <- function(predictors, response, test_proportion = 0.3)
{
  t <- Sys.time()
  nrows <- nrow(predictors)  

	# Separate data into test and train:
	TRAIN <- ceiling((1-test_proportion)*ndays(predictors))	* ROWS_PER_DAY

  predictors_test <- tail(predictors, -TRAIN)
  response_test <- tail(response, -TRAIN)
  predictors_train <- head(predictors, TRAIN)
  response_train <- head(response, TRAIN)
  
  rm(predictors, response)

  # Truncate structurs:
  to_delete <- rows.to.delete(nrow(predictors_test))
  predictors_test <- predictors_test[-to_delete,]
  response_test <- response_test[-to_delete]

	to_delete <- rows.to.delete(nrow(predictors_train))
  predictors_train <- predictors_train[-to_delete,]
  response_train <- response_train[-to_delete]
  
  cat("No. of NA response found in train data:", length(which(is.na(response_train))), "\n")
  predictors_train <- predictors_train[!is.na(response_train),]
  response_train <- response_train[!is.na(response_train)]

  cat("No. of NA response found in test data:", length(which(is.na(response_test))), "\n")
  predictors_test <- predictors_test[!is.na(response_test),]
  response_test <- response_test[!is.na(response_test)]
  
  # Assert validity:
  stopifnot(all(!is.na(predictors_test)), 
            all(!is.na(response_test)), 
            all(!is.infinite(predictors_test)),
            all(!is.infinite(response_test)))

  stopifnot(all(!is.na(predictors_train)), 
            all(!is.na(response_train)), 
            all(!is.infinite(predictors_train)),
            all(!is.infinite(response_train)))
  
  train_proportions <- c(length(which(response_train==-1)), length(which(response_train==0)), length(which(response_train==1))) / length(response_train)
  test_proportions <- c(length(which(response_test==-1)), length(which(response_test==0)), length(which(response_test==1))) / length(response_test)
  
  cat("No. of rows in train-data:", nrow(predictors_train), "\n")
  cat("Train-data proportions (-1, 0, 1):", train_proportions, "\n")
  cat("No. of rows in test-data:", nrow(predictors_test), "\n")
  cat("Test-data proportions (-1, 0, 1):", test_proportions, "\n")
  cat("Training RF-model...\n")
  
  # Train model:
  model <- randomForest(x = predictors_train, y = response_train, 
                        xtest = predictors_test, ytest = response_test, ntree = 1500, # TODO
                        na.action = na.omit, keep.forest=T, importance = T)
  cat("Done training model. Time duration:", Sys.time() - t,"mins.\n")
  return (model)
}

process.data.and.train.model <- function(data)
{
  t <- Sys.time()
  cat("Started Processing at", t,"...\n")
  
  # Initialize structures:
  predictors <- create.predictors(data)
  response <- create.response(data)
  
  stopifnot(nrow(predictors) == length(response))
  
  # Save files:
  saveRDS(predictors, "predictors.rds")
  saveRDS(response, "response.rds")
  
  cat("Done creating predictors & response. Time duration:", Sys.time() - t,"mins.\n")
  
  model <- train.rf.model(predictors, response)
  
  cat("Done Everything. Total Time duration:", Sys.time() - t,"mins.\n")
  
  return (model)
}
