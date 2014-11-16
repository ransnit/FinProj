

preprocess.data <- function(data)
{
	data$TYPICAL <- (data$CLOSE + data$HIGH + data$LOW) / 3

	## yesterday's values:
  data$YH <- initialize.yesterday.high(data)
  data$YL <- initialize.yesterday.low(data)
  data$YO <- initialize.yesterday.open(data)
  data$YC <- initialize.yesterday.close(data)
	data$YT <- (data$YH + data$YL + data$YC) / 3
  
  ## today's values:
  data$DH <- initialize.daily.high(data)
  data$DL <- initialize.daily.low(data)
	data$DT <- (data$DH + data$DL + data$CLOSE) / 3

  data$SD <- initialize.daily.sd(data, "CLOSE")
  data$SDT <- initialize.daily.sd(data, "TYPICAL")
  
#   ## EMAs (CLOSE):
#   for (i in PRICE_EMA_INDICES)  
#   {
#     lable_name <- paste("EMA",i , "CLOSE", sep="_")
#     data[, lable_name] <- init.ema(data, i, "CLOSE")
#     
#     lable_name <- paste("EMSTD",i , "CLOSE", sep="_")
#     data[, lable_name] <- init.emstd(data, i, "CLOSE")
#   }

  ## EMAs (TYPICAL):
  for (i in PRICE_EMA_INDICES)  
  {
    lable_name <- paste("EMA",i , "TYPICAL", sep="_")
    data[, lable_name] <- init.ema(data, i, "TYPICAL")
    
    lable_name <- paste("EMSTD",i , "TYPICAL", sep="_")
    data[, lable_name] <- init.emstd(data, i, "TYPICAL")
  }

	# Money-ratios:
	for (i in MONEY_RATIO_INDICES)
  {
    lable_name <- paste("MR", i, sep="_")
		data[, lable_name] <- initialize.money.ratio(data, i)
  }

  return (data)
}
