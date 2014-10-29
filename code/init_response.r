source("add_zero_col.r")

create.response <- function(data)
{
  response.for.line <- function(row) # row is the row-index of the minute to be classified
  {
    if (row%%ROWS_PER_DAY == 0)
      return (NA)
    
    rows_left_to_end_of_day <- ROWS_PER_DAY - (row%%ROWS_PER_DAY)
    enter_price <- data$OPEN[row+1]
    for (i in (row+1):(row+rows_left_to_end_of_day))
    {
      if((data$HIGH[i] - enter_price >= DELTA) && (enter_price - data$LOW[i] >= DELTA))
        return (NA)
      
      if(data$HIGH[i] - enter_price >= DELTA)
        return (1)
      
      if(enter_price - data$LOW[i] >= DELTA)
        return(-1)
    }
    return (0)
  }
  
  return (factor(sapply(1:nrow(data), response.for.line)))
}
