Ltrans <- function(tnsr) {

  if (tnsr@num_modes != 3)
    stop("LTAR only implemented for 3d so far")

  modes <- tnsr@modes
  n1 <- modes[1]
  n2 <- modes[2]
  n3 <- modes[3]

  dtformz <- aperm(apply(tnsr@data, MARGIN = 1:2, dct), c(2,3,1))

  return(dtformz)
}
