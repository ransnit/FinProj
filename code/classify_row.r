source("config.r")

classify.specific.row <- function(data, row_ind, timeout_row_ind = NA)
{
  if (is.na(timeout_row_ind))
    timeout_row_ind <- row_ind + ROWS_PER_DAY - (row_ind%%ROWS_PER_DAY)
  else
    if (row_ind > timeout_row_ind)
      return (NA)
  
  enter_price <- data$OPEN[row_ind+1]
  
  h <- (data$HIGH[(row_ind+1):timeout_row_ind] - enter_price) >= DELTA
  l <- (enter_price - data$LOW[(row_ind+1):timeout_row_ind]) >= DELTA
  
  if(!any(h) && !any(l))
    return (0)
  
  if(!any(h))
    return (-1)
  
  if(!any(l))
    return (1)
  
  if(which(h)[1] < which(l)[1])
    return (1)
  
  if(which(h)[1] > which(l)[1])
    return (-1)
  
  return (NA)
}