
# Helper function to calculate the mode, not exported

getMode <- function(x) {
  dens <- densityFolded(x)
  return(dens$x[which.max(dens$y)])
}

