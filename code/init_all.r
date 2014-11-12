source("emas.r")
source("init_daily_vals.r")
source("init_yester_vals.r")
source("candle_tail_ratios.r")

preprocess.data <- function(data)
{
  ## yesterday's values:
  data$YH <- initialize.yesterday.high(data)
  data$YL <- initialize.yesterday.low(data)
  data$YO <- initialize.yesterday.open(data)
  data$YC <- initialize.yesterday.close(data)
  
  ## today's values:
  data$DH <- initialize.daily.high(data)
  data$DL <- initialize.daily.low(data)
  data$SD <- initialize.daily.sd(data)
  
#   ## candle tails' ratios:
#   data$CTD <- init.candle.tail.down(data)
#   data$CTU <- init.candle.tail.up(data)
  
  ## EMAs:
  for (i in PRICE_EMA_INDICES)  
  {
    lable_name <- paste("EMA",i , "CLOSE", sep="_")
    data[, lable_name] <- init.ema(data, i, "CLOSE")
    
    lable_name <- paste("EMSTD",i , "CLOSE", sep="_")
    data[, lable_name] <- init.emstd(data, i, "CLOSE")
  }
  
  ## Volume EMAs:
#   for (i in VOLUME_EMA_INDICES)  
#   {
#     lable_name <- paste("EMA",i , "VOLUME", sep="_")
#     data[, lable_name] <- init.ema(data, i, "VOLUME")
#     
#     lable_name <- paste("EMSTD",i , "VOLUME", sep="_")
#     data[, lable_name] <- init.emstd(data, i, "VOLUME")
#   }

  return (data)
}