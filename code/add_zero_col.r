ROWS_PER_DAY <- 391
PRICE_EMA_INDICES <- c(100, 250, 500, 750, 1000, 1500, 2000, 2500, 3000, 3500, 5000)
MONEY_RATIO_INDICES <- c(250, 275, 300, 325, 350, 375, 400)
DELTA <- 0.2 # in dollars
TIMEOUT_THRESH <- 180 # in mins
NROWS_FROM_BEGINNING <- 20
NROWS_FROM_ENDING <- 45

ndays <- function(data) 
{ 
  if (is.null(dim(data)))
    return (length(data) / ROWS_PER_DAY)

  return (nrow(data) / ROWS_PER_DAY) 
}
