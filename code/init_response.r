source("classify_row.r")

create.response <- function(data)
{
  response.for.line <- function(row) # row is the row-index of the minute to be classified
  {
    if (row%%ROWS_PER_DAY == 0)
      return (NA)
    
    return (classify.specific.row(data, row))
  }
  
  return (factor(sapply(1:nrow(data), response.for.line)))
}
