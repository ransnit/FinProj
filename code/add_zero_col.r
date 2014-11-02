ROWS_PER_DAY <- 391
PRICE_EMA_INDICES <- seq(25, 500, 25)
VOLUME_EMA_INDICES <- seq(100, 1000, 100)
DELTA <- 0.3 # in dollars
TIMEOUT_THRESH <- 180 # in mins
NROWS_FROM_BEGINNING <- 30
NROWS_FROM_ENDING <- 120

ndays <- function(data) { return (nrow(data) / ROWS_PER_DAY) }
 # Sergio's first commit
