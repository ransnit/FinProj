source("config.r")

initialize.money.ratio <- function(l, price, volume)
{
	stopifnot(length(price) == length(volume))
  
  current.typical.price.class <- function(row)
	{
		if (row%%ROWS_PER_DAY == 1)
			return (NA)

		if (price[row-1] > price[row])
			return (-1)

		if (price[row-1] < price[row])
			return (1)

		return (0)
	}

	tp_classes <- sapply(1:length(price), current.typical.price.class)
	money_flow <- volume * price

	money.ratio.per.row <- function(row)
	{
		if (row < l)
			return (NA)
		
		mf <- money_flow[(row-l+1):row]
		tpc <- tp_classes[(row-l+1):row]

		positives_sum <- sum(mf[which(tpc == 1)])
		negatives_sum <- sum(mf[which(tpc == -1)])
    
    if (positives_sum == 0 || negatives_sum == 0)
      return (0)
		
		return (positives_sum / negatives_sum)
	}

	return (sapply(1:length(price), money.ratio.per.row))
}
