source("classify_row.r")

initialize.class.proportion <- function(data)
{
  proportions.for.line <- function(timeout_row)
  {
    if ((timeout_row%%ROWS_PER_DAY) == 1)
      return (rep(NA, 3))
    
    classify.specific.row.with.timeout <- function(row) { return (classify.specific.row(data, row, timeout_row)) }
    
    begin_of_day <- timeout_row - ((timeout_row-1)%%ROWS_PER_DAY)
    classes <- sapply(begin_of_day:(timeout_row-1), classify.specific.row.with.timeout)
    
    t <- c(length(which(classes == 1)), length(which(classes == 0)), length(which(classes == -1)))
    
    return (t / length(classes))
  }
  
  return (t(sapply(1:nrow(data), proportions.for.line)))
}