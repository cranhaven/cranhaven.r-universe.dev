LTAR <- function(p,tnsr,type = c("const", "trend", "both", "none"),
                 season=NULL) {
  # input: p = number of lags
  # tnsr = a 3 mode tensor, with time on mode 2

  tform <- "dct" # use discrete cosine transform
  itform <- "idct"

  if (tnsr@num_modes != 3)
    stop("LTAR only implemented for 3d so far")

  modes <- tnsr@modes
  l <- modes[1]
  m <- modes[2]
  d <- modes[3]

  # Time is along mode 2.  Transform will be taken
  # along mode 3.
  dtformz <- Ltrans(tnsr)

  ## Determine coefficient tensors
  Aten <- array(NA,dim=c(l,l*p,d))
  Cten <- array(NA,dim=c(l,1,d))

  for (j in 1:d) {
    y <- ts(aperm(dtformz[,,j],c(2,1)))
    m1 <- VAR(y,p,type,season)
    A <- Bcoef(m1)
    Aten[,,j] <- A[,1:(l*p)]
    Cten[,,j] <- A[,(p+1)]
  }


  Aten <- aperm(apply(array(Aten,c(l,l*p,d)),
                   MARGIN=1:2,itform),c(2,3,1))
  Cten <- array(Cten[,1,],dim=c(l,1,d))
  Cten <- apply(Cten,3,itform)
  invisible(list(A = Aten, C = Cten))
}







