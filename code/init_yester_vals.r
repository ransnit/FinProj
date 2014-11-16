source("config.r")

last <- function(x) { return (tail(x,1)) }
first <- function(x) { return (head(x,1)) }

initialize.yesterday.val <- function(values, func)
{
  yester.val.at.day.index <- function(index)
  {
    if (index == 1)
      return (rep(NA, ROWS_PER_DAY))
    
    yesterday_first_row_ind <- (index-2)*ROWS_PER_DAY+1
    curr_day_first_row_ind <- (index-1)*ROWS_PER_DAY+1
    yv <- func(values[yesterday_first_row_ind:(yesterday_first_row_ind+ROWS_PER_DAY-1)])
    
    return (rep(yv, ROWS_PER_DAY))
  }
  
  return (c(sapply(1:ndays(values), yester.val.at.day.index)))
}
