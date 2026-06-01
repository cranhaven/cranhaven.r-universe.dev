senmscores <-
function (y, z, mset, inner = 0, trim = 3, lambda = 1/2) 
{
  # returns the scores used by senm in sensitivitymult
  tau<-0
  alternative <- "greater"
  TonT <- FALSE
  
  stopifnot((inner >= 0) & (inner <= trim))
  stopifnot((lambda > 0) & (lambda < 1))
  stopifnot(is.vector(y) & is.vector(z) & is.vector(mset))
  stopifnot((length(z) == length(y)))
  stopifnot((length(z) == length(mset)))
  stopifnot(all(!is.na(y)))
  stopifnot(all((z == 0) | (z == 1)))
  tbcheck <- table(z, mset)
  ck <- all(tbcheck[2, ] == 1) & all(tbcheck[1, ] >= 1)
  ck <- ck&(0 == sd(tbcheck[2,]))
  if (!ck) {
    warning("Every matched set must contain one treated 
            subject and the same fixed number of controls.")
    stopifnot(ck)
  }
  mset <- as.integer(mset)
  restore<-1:length(z)
  o <- order(mset, 1 - z)
  restore<-restore[o]
  y <- y[o]
  z <- z[o]
  mset <- mset[o]
  tb <- table(mset)
  nset <- length(tb)
  setsize <- max(tb)
  makeymat <- function(yj) {
    ymat <- t(matrix(NA, setsize, nset))
    m <- 0
    for (i in 1:nset) {
      ymat[i, 1:tb[i]] <- yj[(m + 1):(m + tb[i])]
      m <- m + tb[i]
    }
    ymat
  }
  ymat <- makeymat(y)
  if (alternative == "less") {
    ymat <- (-ymat)
    tau <- (-tau)
  }
  if (!(tau == 0)) 
    ymat[, 1] <- ymat[, 1] - tau
  ms <- sensitivitymult::mscorev(ymat, inner = inner, trim = trim, qu = lambda, 
                TonT = TonT)
  sc<-as.vector(t(ms))[order(restore)]
  sc
}
