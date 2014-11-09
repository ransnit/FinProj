source("add_zero_col.r")

# init.ema <- function(data, l, val_lable)
# {
#   alpha <- 2 / (l+1)
#   
#   init.ema.per.day <- function(day_ind)
#   {
#     curr_day_first_row_ind <- (day_ind-1)*ROWS_PER_DAY+1
#     curr_day_last_row_ind <- curr_day_first_row_ind+ROWS_PER_DAY-1
#     vals <- data[curr_day_first_row_ind:curr_day_last_row_ind, val_lable]
#     result <- rep(NA, ROWS_PER_DAY)
#     
#     result[1] <- vals[1]
#   
#     for (j in 2:ROWS_PER_DAY)
#       result[j] <- (1-alpha)*result[j-1] + alpha*vals[j] 
#     
#     return (result)
#   }
#   
#   return (c(sapply(1:ndays(data), init.ema.per.day)))
# }
# 
# init.emstd <- function(data, l, val_lable)
# {
#   lable_name <- paste("EMSTD", l, val_lable, sep="_")
#   ema_lable_name <- paste("EMA",l , val_lable, sep="_")
#   alpha <- 2 / (l+1)
#   n_days <- nrow(data) / ROWS_PER_DAY
#   
#   
#   init.emstd.per.day <- function(day_ind)
#   {
#     curr_day_first_row_ind <- (day_ind-1)*ROWS_PER_DAY+1
#     curr_day_last_row_ind <- curr_day_first_row_ind+ROWS_PER_DAY-1
#     
#     vals <- data[curr_day_first_row_ind:curr_day_last_row_ind, val_lable]
#     means <- data[curr_day_first_row_ind:curr_day_last_row_ind, ema_lable_name]
#     result <- rep(NA, ROWS_PER_DAY)
#     
#     result[1] <- 0
#     
#     for (j in 2:ROWS_PER_DAY)
#     {
#       prev_var <- result[j-1]^2
#       prev_mean <- means[j-1]
#       curr_mean <- means[j]
#       curr_val <- vals[j]
#       
#       result[j] <- sqrt((1-alpha)*prev_var + alpha*(curr_val - prev_mean)*(curr_val - curr_mean))
#     }
#     
#     return (result)
#   }
#   
#   return (c(sapply(1:ndays(data), init.emstd.per.day)))
# }

# source("add_zero_col.r")

init.ema <- function(data, l, val_lable)
{
  n <- nrow(data)
  alpha <- 2 / (l+1)

  vals <- data[, val_lable]
  result <- rep(NA, n)
  
  result[1] <- vals[1]
  
  for (j in 2:n)
    result[j] <- (1-alpha)*result[j-1] + alpha*vals[j] 
  
  return (result)
}

init.emstd <- function(data, l, val_lable)
{
  n <- nrow(data)
  lable_name <- paste("EMSTD", l, val_lable, sep="_")
  ema_lable_name <- paste("EMA",l , val_lable, sep="_")
  alpha <- 2 / (l+1)
  
  vals <- data[, val_lable]
  means <- data[, ema_lable_name]
  result <- rep(NA, n)
  
  result[1] <- 0
  
  for (j in 2:n)
  {
    prev_var <- result[j-1]^2
    prev_mean <- means[j-1]
    curr_mean <- means[j]
    curr_val <- vals[j]
    
    result[j] <- sqrt((1-alpha)*prev_var + alpha*(curr_val - prev_mean)*(curr_val - curr_mean))
  }
  
  return (result)
}
