source("config.r")

initialize.daily.val <- function(values, func)
{
  init.daily.val.for.single.day <- function(day_ind)
  {
    curr_day_first_row_ind <- (day_ind-1)*ROWS_PER_DAY+1
    curr_day_last_row_ind <- curr_day_first_row_ind+ROWS_PER_DAY-1
    init.daily.val.for.single.row <- function(row_ind) { return (func(values[curr_day_first_row_ind:row_ind])) }
    
    return (sapply(curr_day_first_row_ind:curr_day_last_row_ind, init.daily.val.for.single.row))
  }
  
  return (c(sapply(1:ndays(values), init.daily.val.for.single.day)))
}
