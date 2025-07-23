
.plot_errorbars <- function(k,k.sd,bins,horiz=T){
  epsilon <- diff(bins)[1]*.15
  for(i in 1:length(bins)) {
    x <- c(rev(k)[i],rev(k)[i]+rev(k.sd)[i])
    y <- c(bins[i],bins[i])
    segments(x[1],y[1],x[2],y[2],xpd=T)
    segments(x[2],y[2]-epsilon,x[2],y[2]+epsilon,xpd=T)
  }
}