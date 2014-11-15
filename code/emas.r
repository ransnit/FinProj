source("add_zero_col.r")

init.ema <- function(l, values)
{
  alpha <- 2 / (l+1)
  result <- rep(NA, length(values))
  result[1] <- values[1]
  
  for (j in 2:length(values))
    result[j] <- (1-alpha)*result[j-1] + alpha*values[j] 
  
  return (result)
}

init.emstd <- function(l, values, means)
{
  stopifnot(length(values) == length(means))
  
  alpha <- 2 / (l+1)
  result <- rep(NA, length(values))
  result[1] <- 0
  
  for (j in 2:length(values))
  {
    prev_var <- result[j-1]^2
    prev_mean <- means[j-1]
    curr_mean <- means[j]
    curr_val <- values[j]
    
    result[j] <- sqrt((1-alpha)*prev_var + alpha*(curr_val - prev_mean)*(curr_val - curr_mean))
  }
  
  return (result)
}
