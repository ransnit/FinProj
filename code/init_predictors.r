source("init_all.r")

create.predictors <- function(data)
{
  data <- preprocess.data(data)
  n <- nrow(data)
  m <- 17 + length(PRICE_EMA_INDICES) + length(MONEY_RATIO_INDICES) #features_num
  days_num <- n / ROWS_PER_DAY
  predictors <- matrix(0, n, m)
  
  # Minute indices
  predictors[,1] <- rep(seq(0, 390, 1), days_num)
  
  # Normalized distances between now's close-price and yesterday's prices
  predictors[,2] <- (data$CLOSE - data$YC) / data$SD
  predictors[,3] <- (data$CLOSE - data$YO) / data$SD
  predictors[,4] <- (data$CLOSE - data$YH) / data$SD
  predictors[,5] <- (data$CLOSE - data$YL) / data$SD
  predictors[,6] <- (data$CLOSE - data$YT) / data$SD
  
  # Normalized distances between now's close-price and today's extermum prices
  predictors[,7] <- (data$DH - data$CLOSE) / data$SD
  predictors[,8] <- (data$CLOSE - data$DL) / data$SD
  predictors[,9] <- (data$DT - data$CLOSE) / data$SD

	# Normalized distances between now's typical-price and yesterday's prices
  predictors[,10] <- (data$TYPICAL - data$YC) / data$SDT
  predictors[,11] <- (data$TYPICAL - data$YO) / data$SDT
  predictors[,12] <- (data$TYPICAL - data$YH) / data$SDT
  predictors[,13] <- (data$TYPICAL - data$YL) / data$SDT
  predictors[,14] <- (data$TYPICAL - data$YT) / data$SDT
  
  # Normalized distances between now's typical-price and today's extermum prices
  predictors[,15] <- (data$DH - data$TYPICAL) / data$SDT
  predictors[,16] <- (data$TYPICAL - data$DL) / data$SDT
  predictors[,17] <- (data$DT - data$TYPICAL) / data$SDT
  
  # Normalized distances between now's close-price and today's EMAs
  col_index <- 18
  for (i in PRICE_EMA_INDICES)
  {
    val_lable <- paste("EMA", i, "CLOSE", sep="_")
    std_lable <- paste("EMSTD", i, "CLOSE", sep="_")
    predictors[,col_index] <- (data$CLOSE - data[, val_lable]) / data[, std_lable]
    col_index <- col_index + 1
  }

	# Money-ratios:
	for (i in MONEY_RATIO_INDICES)
  {
    val_lable <- paste("MR", i, sep="_")
		predictors[,col_index] <- data[, val_lable]
		col_index <- col_index + 1
  }
	
  return (predictors)
}
