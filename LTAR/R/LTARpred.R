LTARpred <- function(p,tnsr,h,type = c("const", "trend", "both", "none"),
                     season=NULL){

  # Input:
  # tnsr: 3 mode tensor
  # p: Number of lags
  # h: Number of steps to predict

  tform <- "dct" # use discrete cosine transform

  # Extract modes
  modes <- tnsr@modes
  l <- modes[1]
  m <- modes[2]
  d <- modes[3]

  if (tnsr@num_modes != 3)
    stop("LTAR only implemented for 3d so far")
  is.natural <- function(x)
    {
      x>0 && identical(round(x), x)
    }
  if(!is.natural(h))
    stop("Forecast steps must be a natural number")
  if (h>m)
    stop("Not enough historical data to forecast that far ahead")

  # forecast next step ahead
  for (i in 1:h) {
    model <- LTAR(p,tnsr,type,season)

    # Convert A and C to tensors
    A <- as.tensor(model$A)
    C <- rand_tensor(c(l,1,d))
    C[,1,] <- model$C

    yt1 <- C
    pos <- 1

    for (j in seq(1,l*p, by=l)) {
      At <- A[,j:(j+l-1),]
      y <- array(tnsr[,pos,]@data,c(l,1,d))
      y <- as.tensor(y)
      yt1 <- yt1 + tmult(At,y,tform)
      pos <- pos+1
    }

    dims <- dim(tnsr)
    dims[2] <- dims[2]+1
    new_tnsr <- array(NA,dim=dims)
    yt1_array <- array(yt1@data,c(5,1,6))
    new_tnsr[,1,] <- yt1_array
    new_tnsr[,2:dims[2],] <- tnsr@data
    tnsr <- as.tensor(new_tnsr)
  }
  ypred <- tnsr[,1:h,]
  invisible(list(ypred = ypred))
}
