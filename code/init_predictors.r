source("init_all.r")
source("init_class_props.r")

create.predictors <- function(data)
{
#  data <- preprocess.data(data)
  n <- nrow(data)
  m <- 12 + length(PRICE_EMA_INDICES) + length(MONEY_RATIO_INDICES) #features_num
  days_num <- n / ROWS_PER_DAY
  predictors <- matrix(NA, n, m)
  
  # Minute indices
  typical <- (data$CLOSE + data$HIGH + data$LOW) / 3
  sdt <- initialize.daily.val(typical, sd)
  yh <- initialize.yesterday.val(data$HIGH, max)
  yl <- initialize.yesterday.val(data$LOW, min)
  yo <- initialize.yesterday.val(data$OPEN, first)
  yc <- initialize.yesterday.val(data$CLOSE, last)
  yt <- (yh + yl + yc) / 3
  dh <- initialize.daily.val(data$HIGH, max)
  dl <- initialize.daily.val(data$LOW, min)
  dt <- (dh + dl + data$CLOSE) / 3
  
  predictors[,1] <- rep(seq(0, 390, 1), days_num)
	# Normalized distances between now's typical-price and yesterday's prices
  predictors[,2] <- (typical - yh) / sdt
  predictors[,3] <- (typical - yl) / sdt
  predictors[,4] <- (typical - yo) / sdt
  predictors[,5] <- (typical - yc) / sdt
  predictors[,6] <- (typical - yt) / sdt
  
  # Normalized distances between now's typical-price and today's extermum prices
  predictors[,7] <- (dh - typical) / sdt
  predictors[,8] <- (typical - dl) / sdt
  predictors[,9] <- (dt - typical) / sdt
  
  if (F)
    cps <- initialize.class.proportion(data)
  else
    cps <- readRDS("class_props_IBM_02.rds")

  predictors[,10] <- cps[, 1]
  predictors[,11] <- cps[, 2]
  predictors[,12] <- cps[, 3]

  # Normalized distances between now's close-price and today's EMAs
  col_index <- 13
#   for (i in PRICE_EMA_INDICES)
#   {
#     val_lable <- paste("EMA", i, "CLOSE", sep="_")
#     std_lable <- paste("EMSTD", i, "CLOSE", sep="_")
#     predictors[,col_index] <- (data$CLOSE - data[, val_lable]) / data[, std_lable]
#     col_index <- col_index + 1
#   }

  for (i in PRICE_EMA_INDICES)
  {
    ema <- init.ema(i, typical)
    emstd <- init.emstd(i, typical, ema)
    predictors[,col_index] <- (typical - ema) / emstd
    col_index <- col_index + 1
  }

	# Money-ratios:
	for (i in MONEY_RATIO_INDICES)
  {
		predictors[,col_index] <- initialize.money.ratio(i, typical, data$VOLUME)
		col_index <- col_index + 1
  }
	
  return (predictors)
}
