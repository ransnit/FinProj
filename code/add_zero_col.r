ROWS_PER_DAY <- 391
PRICE_EMA_INDICES <- c(100, seq(250, 5000, 250))
MONEY_RATIO_INDICES <- c(10, 14, 50)
DELTA <- 0.2 # in dollars
TIMEOUT_THRESH <- 180 # in mins
NROWS_FROM_BEGINNING <- 20
NROWS_FROM_ENDING <- 45

ndays <- function(data) { return (nrow(data) / ROWS_PER_DAY) }
 # Sergio's first commit
