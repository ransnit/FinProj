candle.stick.chart <- function(data, ylim=NA)
{
  n <- nrow(data)
  
  if (n > 400)
    cat("You're trying to display more than 400 bars, things might look a little messy...\n")
  
  closes <- data$CLOSE
  opens <- data$OPEN
  highs <- data$HIGH
  lows <- data$LOW
  
  xlim <- c(1, n)

  if (is.na(ylim))
  {
    hb <- max(highs[!is.na(highs)])
    lb <- min(lows[!is.na(lows)])
    p <- (hb-lb)/10
    ylim <- c(lb-p, hb+p)
  }
  
  positives <- closes >= opens
  
  green_bars <- rep(NA, n)
  red_bars <- rep(NA, n)
  green_white_bars <- rep(NA, n)
  red_white_bars <- rep(NA, n)
  
  not.na.and.val <- function(x)
  {
    return (!is.na(x) & x)
  }
  
  green_bars[not.na.and.val(positives)] <- closes[not.na.and.val(positives)]
  red_bars[not.na.and.val(!positives)] <- opens[not.na.and.val(!positives)]
  green_white_bars[not.na.and.val(positives)] <- opens[not.na.and.val(positives)]
  red_white_bars[not.na.and.val(!positives)] <- closes[not.na.and.val(!positives)]
  
  plot(highs, type='h', col='black', lwd=1, xlim=xlim, ylim=ylim, ylab='', xlab='')
  par(new=T)
  plot(green_bars, type='h', col='dark green', lwd=5, xlim=xlim, ylim=ylim, ylab='', xlab='')
  par(new=T)
  plot(red_bars, type='h', col='dark red', lwd=5, xlim=xlim, ylim=ylim, ylab='', xlab='')
  par(new=T)
  plot(green_white_bars, type='h', col='white', lwd=5, xlim=xlim, ylim=ylim, ylab='', xlab='')
  par(new=T)
  plot(green_white_bars, type='h', col='black', lwd=1, xlim=xlim, ylim=ylim, ylab='', xlab='')
  par(new=T)
  plot(red_white_bars, type='h', col='white', lwd=5, xlim=xlim, ylim=ylim, ylab='', xlab='')
  par(new=T)
  plot(red_white_bars, type='h', col='black', lwd=1, xlim=xlim, ylim=ylim, ylab='', xlab='')
  par(new=T)
  plot(lows, type='h', col='white', lwd=2, xlim=xlim, ylim=ylim, ylab='', xlab='')
  par(new=T)
  
  plot(red_bars, pch=20, col='dark red', xlim=xlim, ylim=ylim, ylab='', xlab='')
  par(new=T)
  plot(green_bars, pch=20, col='dark green', xlim=xlim, ylim=ylim, ylab='', xlab='')
  par(new=T)
  plot(red_white_bars, col='dark red', pch=20, xlim=xlim, ylim=ylim, ylab='', xlab='')  
  par(new=T)
  plot(green_white_bars, col='dark green', pch=20, xlim=xlim, ylim=ylim, ylab='', xlab='')
}
