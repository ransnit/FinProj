source("train_model.r")

data_IBM <- read.csv("../data/IBM.txt")
data_SPY <- read.csv("../data/SPY.txt")
data_XLK <- read.csv("../data/XLK.txt")

p_IBM <- create.predictors(data_IBM)
p_SPY <- create.predictors(data_SPY)
p_XLK <- create.predictors(data_XLK)

predictors <- cbind(p_SPY, p_XLK[,-1], p_IBM[,-1])
response <- create.response(data_IBM)

rm(data_IBM, data_SPY, data_XLK, p_IBM, p_SPY, p_XLK)

model <- train.rf.model(predictors, response)

show(model)
plot(model)
plot(model$importance[,4])