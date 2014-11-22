source("train_model.r")

data_IBM <- read.csv("../data/IBM.txt")
data_SPY <- read.csv("../data/SPY.txt")
data_XLK <- read.csv("../data/XLK.txt")

p_IBM <- create.predictors(data_IBM)
p_SPY <- create.predictors(data_SPY)
p_XLK <- create.predictors(data_XLK)

p_IBM[,11] <- p_IBM[,10] + p_IBM[,11] # aggregate proportions of 1s and 0s
p_IBM <- p_IBM[,-10]

predictors <- cbind(p_SPY[, -c(10,11,12)], p_XLK[,-c(1,10,11,12)], p_IBM[,-1])
response <- create.response(data_IBM)
response[response == 1] <- 0
levels(response)[3] <- 0

rm(data_IBM, data_SPY, data_XLK, p_IBM, p_SPY, p_XLK)

model1 <- train.rf.model(predictors, response)
show(model1)
saveRDS(model1, "model1.rds")
i <- model1$importance[,3]
plot(i)
text(i, labels = colnames(predictors), pos = 3)
