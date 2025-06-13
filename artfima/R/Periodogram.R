Periodogram <- function(z) {
  spec.pgram(z, fast=FALSE, detrend=FALSE, plot=FALSE, taper=0, log="no", 
             type="h")$spec
}
