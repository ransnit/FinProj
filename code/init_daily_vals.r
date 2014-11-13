source("add_zero_col.r")

initialize.daily.low <- function(data)
{
  init.daily.low.for.single.day <- function(day_ind)
  {
    curr_day_first_row_ind <- (day_ind-1)*ROWS_PER_DAY+1
    curr_day_last_row_ind <- curr_day_first_row_ind+ROWS_PER_DAY-1
    init.daily.low.for.single.row <- function(row_ind) { return (min(data$LOW[curr_day_first_row_ind:row_ind])) }
    
    return (sapply(curr_day_first_row_ind:curr_day_last_row_ind, init.daily.low.for.single.row))
  }
  
  return (c(sapply(1:ndays(data), init.daily.low.for.single.day)))
}

initialize.daily.high <- function(data)
{
  init.daily.high.for.single.day <- function(day_ind)
  {
    curr_day_first_row_ind <- (day_ind-1)*ROWS_PER_DAY+1
    curr_day_last_row_ind <- curr_day_first_row_ind+ROWS_PER_DAY-1
    init.daily.high.for.single.row <- function(row_ind) { return (max(data$HIGH[curr_day_first_row_ind:row_ind])) }
                                                               
    return (sapply(curr_day_first_row_ind:curr_day_last_row_ind, init.daily.high.for.single.row))
  }
  
  return (c(sapply(1:ndays(data), init.daily.high.for.single.day)))
}

initialize.daily.sd <- function(data, val_lable)
{
  init.daily.sd.for.single.day <- function(day_ind)
  {
    curr_day_first_row_ind <- (day_ind-1)*ROWS_PER_DAY+1
    curr_day_last_row_ind <- curr_day_first_row_ind+ROWS_PER_DAY-1
    
    compute.sd.per.row <- function(row_ind) { return (sd(data[curr_day_first_row_ind:row_ind, val_lable])) }
    return (sapply(curr_day_first_row_ind:curr_day_last_row_ind, compute.sd.per.row))
  }
  
  return (c(sapply(1:ndays(data), init.daily.sd.for.single.day)))
}

