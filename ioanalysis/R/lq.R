lq <- function(io){
  # Preliminaries
  if(!"InputOutput" %in% class(io)) stop("io must be of class InputOutput. See ?as.inputoutput")
  A <- io$A
  X <- io$X
  RS_label <- io$RS_label
  # Updating
  Xn <- sum(X)
  regions <- unique(RS_label[, 1])
  location <- 0
  lq <- rep(1/100, length(X))
  for(r in 1:length(regions)){
    i <- which(RS_label[, 1] %in% regions[r])
    Xr <- sum(X[i])
    sectors <- unique(RS_label[i, 2])
    for(s in 1:length(sectors)){
      j <- which( RS_label[i, 2] %in% sectors[s])
      Xri <- sum(X[i[j]])
      k <- which(RS_label[, 2] %in% sectors[s])
      Xni <- sum(X[k])
      top <- Xri/Xr
      bot <- Xni/Xn
      location <- location + 1
      lq[location] <- top/bot
    }
  }
  Slq <- matrix(lq, ncol = length(X), nrow = length(X))
  i <- which(Slq >= 1)
  Slq[i] <- 1
  upd <- A * Slq
  upd
}
