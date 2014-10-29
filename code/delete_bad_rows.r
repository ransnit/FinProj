source("add_zero_col.r")

rows.to.delete <- function(nrows)
{
  f <- function(i) return (seq(i, nrows, ROWS_PER_DAY))
  to_delete <- c(sapply(1:NROWS_FROM_BEGINNING, f), sapply(391:(391-NROWS_FROM_ENDING), f), 1:ROWS_PER_DAY)
  
  return (unique(to_delete)) #remove duplicate elements
}
