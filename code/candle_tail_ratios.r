source("add_zero_col.r")

init.candle.tail.up <- function(data)
{
  data_open_close <- data.frame(data$OPEN, data$CLOSE)
  temp <- apply(data_open_close, 1, max)
  result <- (data$HIGH - temp) / (data$HIGH - data$LOW)
  result[is.nan(result)] <- 0
  return (result)
}

init.candle.tail.down <- function(data)
{
  data_open_close <- data.frame(data$OPEN, data$CLOSE)
  temp <- apply(data_open_close, 1, min)
  result <- (temp - data$LOW) / (data$HIGH - data$LOW)
  result[is.nan(result)] <- 0
  return (result)
}
