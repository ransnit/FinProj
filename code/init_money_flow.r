source("add_zero_col.r")

initialize.money.ratio <- function(data, l)
{
	current.typical.price.class <- function(row)
	{
		if (row%%ROWS_PER_DAY == 1)
			return (NA)

		if (data$TYPICAL[row-1] > data$TYPICAL[row])
			return (-1)

		if (data$TYPICAL[row-1] < data$TYPICAL[row])
			return (1)

		return (0)
	}

	tp_classes <- sapply(1:nrow(data), current.typical.price.class)
	money_flow <- data$VOLUME * data$TYPICAL

	money.ratio.per.row <- function(row)
	{
		if (row < l)
			return (NA)
		
		mf <- money_flow[(row-l+1):row]
		tpc <- tp_classes[(row-l+1):row]

		positives_sum <- sum(mf[which(tpc == 1)])
		negatives_sum <- sum(mf[which(tpc == -1)])
		
		return (positives_sum / negatives_sum)
	}

	return (sapply(1:nrow(data), money.ratio.per.row))
}
