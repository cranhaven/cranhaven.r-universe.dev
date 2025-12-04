closedCapMhJK <-
function(CH, ci=0.95) {
  # CH is a 1/0 capture history matrix, animals x occasions, OR
  #  a vector of capture frequencies of length equal to the number
  #  of occasions - trailing zeros are required.
  # ci is the required confidence interval
  
  # B&O = Burnham, K.P. & Overton, W.S. (1979) Robust estimation of population size
  #   when capture probabilities vary among animals. Ecology, 60, 927-936.
  # R&B = Rexstad, E; K Burnham 1992. User's guide for interactive program CAPTURE.
  #   USGS Patuxent. 

  if (is.matrix(CH) || is.data.frame(CH)) {
    n.occ <- ncol(CH)
    freq <- tabulate(rowSums(CH), nbins=n.occ)
  } else {
    freq <- round(CH)
    n.occ <- length(freq)
  }

  crit <- fixCI(ci)[2]

   N.cap <- sum(freq)  # Number of animals captured
   n.snap <- sum(freq * seq_along(freq)) # Total number of capture events
   n.jack <- min(5, n.occ) # Number of jackknife estimations

  out.mat <- matrix(NA_real_, 2, 3)
  colnames(out.mat) <- c("est", "lowCI", "uppCI")
  rownames(out.mat) <- c("Nhat", "pHat")

#  if(sum(freq[-1]) > 1)  {  # Do checks here
  if(sum(freq[-1]) > 0)  {  # Do checks here
    # Remove trailing zeros:
    while(freq[length(freq)] == 0)
      freq <- freq[-length(freq)]
     ### Create matrix of jackknife coefficients following B&O Table 1 p928
     # CAPTURE uses a simplified form for n.occ > 30.
     if(n.occ > 30) {
        A <- matrix(c(
               2, 1, 1, 1, 1,
               3, 0, 1, 1, 1,
               4,-2, 2, 1, 1,
               5,-5, 5, 0, 1,
               6,-9,11,-4, 2), nrow=5, ncol=5, byrow=TRUE)
     } else {
      # We do a 5x5 matrix, which will contain NaNs if n.occ < 5, then adjust it
      #   afterwards. Equations are from B&O.
      tmp <- suppressWarnings(factorial(n.occ)/factorial(n.occ-(1:5)))
      A2 <- rbind(
        c(n.occ-1, 0,0,0,0) / tmp,
        c(2*n.occ-3,  -(n.occ-2)^2, 0, 0, 0) / tmp,
        c(3*n.occ-6,  -(3*n.occ^2-15*n.occ+19),   (n.occ-3)^3, 0,0) /tmp,
        c(4*n.occ-10, -(6*n.occ^2-36*n.occ+55),   4*n.occ^3-42*n.occ^2+148*n.occ-175,
               -(n.occ-4)^4, 0)/tmp,
        c(5*n.occ-15, -(10*n.occ^2-70*n.occ+125),
                10*n.occ^3-120*n.occ^2+485*n.occ-660,
                -((n.occ-4)^5-(n.occ-5)^5), (n.occ-5)^5) / tmp)
      A <- A2[1:n.jack, 1:n.jack] + 1  
    }

     # Adjust length(freq) or ncol(A) so that they match:
     if(length(freq) < n.jack)
        freq <- c(freq, rep(0, n.jack - length(freq)))
     if(length(freq) > n.jack)
        A <- cbind(A, matrix(1, nrow=n.jack, ncol=length(freq)-n.jack))

     # Calculate jackknife estimates and SE (but see revised SE below)
     Nj <- freq %*% t(A)
     var.Nj <- freq %*% t(A^2) - Nj # could be negative
     #SEj <- sqrt(pmax(0, var.Nj))
     
     # Calculate test statistic for differences in Nj's
     diffs <- diff(Nj[1,])
     # The coefficients labelled b[i] by B&O p929:
     b <- apply(A, 2, diff)
     b2 <- b^2
     # Variance of the differences
     var.diff <- (freq %*% t(b2) - diffs^2/N.cap) * N.cap / (N.cap-1)
     Chi2 <- diffs^2 / var.diff
     # For some values of n.jack < 5, diffs = var.diff = 0, causing NaNs.
     Chi2[is.nan(Chi2)] <- 0
     # Last Chi2 = 0
     Chi2 <- c(Chi2, 0)

     ### Revised calculation of SE
     # See R&B for theory. CAPTURE source code uses Chi2 - 1 for the power 
     #   calculation, or power = 0.5 if Chi2 < 1:
     lambda <- pmax(0, Chi2 - 1)[-length(Chi2)]
     powr <- pchisq(3.8415, 1, lambda, lower.tail=FALSE)
     powr[lambda == 0] <- 0.5
     pi <- c(1-powr, 1) * c(1,cumprod(powr))
     E.Nj <- sum(pi * Nj)
     var.rev <- sum( pi * var.Nj + pi * (Nj - E.Nj)^2) # Sometimes negative.
     SE.rev <- sqrt(max(0,var.rev))

     ### Interpolation
     # See B&O bottom of p933. CAPTURE uses Chi2 to calculate
     #   weights, not the p-values as in B&O (they are NOT equivalent).
     # Select jackknife which is not significantly different from next higher
     #   order jackknife:
     m <- which(Chi2 < 3.8415)[1]
     if(m > 1) {
        c <- (Chi2[m-1]-3.84)/(Chi2[m-1]-Chi2[m])
        d <- c*A[m,] + (1-c)*A[m-1,] 
        N.hat <- sum(d * freq)
        if(N.hat < Nj[1]) {
           N.hat <- Nj[1]
        }
     } else {
        # CAPTURE interpolates between N(1) and N.cap if appropriate.
        xtest <- Nj[1]*freq[1] / (Nj[1] - freq[1])
        if (xtest > 3.84) {                       # interpolation is okay
           c <- (xtest-3.84)/(xtest-Chi2[1])
           d <- c*A[1,] + (1-c) 
           N.hat <- sum(d * freq)
        } else {
           N.hat <- Nj[1]
  #         SE.rev <- NA
        }
     }

     ### Calculate 95% CI based on log-normal distribution. See R&B.
     f0 <- N.hat - N.cap
     if(f0 < .Machine$double.eps) {
        CI <- c(N.cap, N.cap)
     } else {
        C <- exp(crit*sqrt(log(1 + SE.rev^2/f0^2)))
        CI <- c(N.cap + f0/C, N.cap + f0*C)
     }
    # Prepare output
    out.mat[1, ] <- c(N.hat, CI)
    out.mat[2, ] <- n.snap / (out.mat[1, c(1,3,2)] * n.occ)
  }
  out <- list(call = match.call(),
          # beta = beta.mat,
          real = out.mat,
          logLik = c(logLik=NA, df=NA, nobs=N.cap * n.occ))
  class(out) <- c("wiqid", "list")
  return(out)
}
