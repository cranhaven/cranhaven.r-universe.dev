######################   begin create.U    ######################################
create.U <- function(X) {
  #
  #   Author: Joseph B. Lang, Univ of Iowa
  #   Created:  8/19/01  (last update: 3/30/04)
  #
  # This program creates a full-column rank matrix, U, with column space
  # equal to the orthogonal complement of the column space of X.  That is,
  # U has column space equal to the null space of X^T.
  #
  # This program is used in mph.fit.
  #
  #  Input:  X must be of full column rank
  #
  nrowX <- nrow(X)
  u <- nrowX - ncol(X)
  if (u == 0) {U <- 0}
  else {w.mat <- matrix(runif(nrowX*u,1,10),nrowX,u)
  U <- w.mat - X%*%solve(t(X)%*%X)%*%t(X)%*%w.mat
  }
  U
}
######################   end create.U     ######################################