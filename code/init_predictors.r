source("init_all.r")

create.predictors <- function(data)
{
  data <- preprocess.data(data)
  n <- nrow(data)
  m <- 1 + 4 + 2 + length(PRICE_EMA_INDICES) + 1#length(VOLUME_EMA_INDICES)# + 2 #features_num
  days_num <- n / ROWS_PER_DAY
  predictors <- matrix(0, n, m)
  
  # Minute indices
  predictors[,1] <- rep(seq(0, 390, 1), days_num)
  
  # Normalized distances between now's price and yesterday's prices
  predictors[,2] <- (data$CLOSE - data$YC) / data$SD
  predictors[,3] <- (data$CLOSE - data$YO) / data$SD
  predictors[,4] <- (data$CLOSE - data$YH) / data$SD
  predictors[,5] <- (data$CLOSE - data$YL) / data$SD
  
  # Normalized distances between now's price and today's extermum prices
  predictors[,6] <- (data$DH - data$CLOSE) / data$SD
  predictors[,7] <- (data$CLOSE - data$DL) / data$SD
  
  # Normalized distances between now's price and today's EMAs
  col_index <- 8
  for (i in PRICE_EMA_INDICES)
  {
    val_lable <- paste("EMA", i, "CLOSE", sep="_")
    std_lable <- paste("EMSTD", i, "CLOSE", sep="_")
    predictors[,col_index] <- (data$CLOSE - data[, val_lable]) / data[, std_lable]
    col_index <- col_index + 1
  }

  predictors[,col_index] <- data$VOLUME
  col_index <- col_index + 1
  
  # Normalized distances between now's volume and today's EMAs
#   for (i in VOLUME_EMA_INDICES)
#   {
#     val_lable <- paste("EMA", i, "VOLUME", sep="_")
#     std_lable <- paste("EMSTD", i, "VOLUME", sep="_")
#     predictors[,col_index] <- (data$VOLUME - data[, val_lable]) / data[, std_lable]
#     col_index <- col_index + 1
#   }
#   
#   # Candle-tail's ratios
#   predictors[,col_index] <- data$CTD
#   col_index <- col_index + 1
#   
#   predictors[,col_index] <- data$CTU
#   col_index <- col_index + 1
  
  return (predictors)
}