ROWS_PER_DAY <- 391
PRICE_EMA_INDICES <- seq(2, 200, 2)
VOLUME_EMA_INDICES <- seq(2, 200, 2)
DELTA <- 0.2 # in dollars
TIMEOUT_THRESH <- 180 # in mins
NROWS_FROM_BEGINNING <- 30
NROWS_FROM_ENDING <- 30

ndays <- function(data) { return (nrow(data) / ROWS_PER_DAY) }
 # Sergio's first commit
