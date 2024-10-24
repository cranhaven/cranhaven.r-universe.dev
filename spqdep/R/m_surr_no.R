m_surr_no <- function(mdtfull, m = m, r = 1, initobs = 1,
                      control_knn = 0, coor = NULL) {
  if ((r < 1) || (r  > (m-1)))
    stop("minimum degree of overlap is 1 and less than the amplitude of the m-story")
  N <- nrow(mdtfull)
  mdtfull <- matrix(mdtfull, nrow = nrow(mdtfull),
                    ncol = ncol(mdtfull))
  rownames(mdtfull) <- colnames(mdtfull) <- 1:N
  nnlist <- matrix(0, N, m - 1)  # Matrix with list of nearest neighbors
  #nn1 <- cbind(1:N,sqrt((x[1,1] - x[,1])^2 + (x[1,2] - x[,2])^2),x)
  nn1 <- cbind(1:N, mdtfull[initobs,])
  nn1 <- nn1[order(nn1[, 2]),]
  nnlist[initobs, ] <- nn1[2:m, 1]
  R <- trunc((N - m)/(m - r)) + 1
  list <- rep(0, R)
  list[1] = initobs
  if (m > (r + 1)) {
    blacklist <- c(initobs, nnlist[initobs, 1:(m - (r + 1))])
  } else {
    blacklist <- initobs
  }
  mdtnew <- mdtfull[-blacklist, -blacklist]
  t <- initobs
  for (v in 2:R) {
    list[v] = nnlist[t, m - r]
    h <- list[v]
    #nn1 <- nn1[!nn1[,1] %in% blacklist,]
    #nn1[,2] <-  sqrt((nn1[1,3] - nn1[,3])^2 + (nn1[1,4] - nn1[,4])^2)
    vdt <- mdtnew[rownames(mdtnew) == h, , drop = FALSE]
    nn1 <- cbind(as.integer(colnames(vdt)), vdt[1, ])
    nn1 <- nn1[order(nn1[,2]),]
    nnlist[h,] <- nn1[2:(m), 1]
    t = h
    if (m > (r + 1)) {
      blacklist <- c(blacklist, h, nnlist[h, 1:(m - (r + 1))])
    } else {
      blacklist <- c(blacklist, h)
    }
    mdtnew <- mdtfull[-blacklist, -blacklist]
  }
  nnlist <- cbind(1:N, nnlist)
  nnlist1 <- cbind(nnlist[which(!nnlist[,2] == 0),])
  # control deberia ser una lista
  # Se eliminan aquellas m-historias que contengan vecinos que no estén
  # dentro de los k vecinos mas proximos
  # if (is.null(control_knn)==FALSE){
  if ((control_knn != 0) && !is.null(coor)){
    knn <- cbind(1:N,knearneigh(coor, control_knn)$nn)
    int <- numeric()
    for (i in 1:dim(nnlist1)[1]){
      int[i] <- length(intersect(nnlist1[i,],knn[nnlist1[i,1],]))
    }
    nnlist1 <- nnlist1[int == m,]
    # nnlist2 <-  nnlist1[int != m,]
    # nnlist2 <- sort(unique(matrix(nnlist2, ncol = 1)))
    # x2 <- coor[nnlist2,]
    # mh2 <- m.surround(x = x2, m = m, r = r)
  }
  return(nnlist1)
}
