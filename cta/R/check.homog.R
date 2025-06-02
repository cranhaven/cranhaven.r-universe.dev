######################  begin check.homog  #####################################
check.homog <- function(h.fct,Z,tol=NULL)  {
  #
  #   Author:  Joseph B. Lang,  Univ of Iowa
  #   Created: 4/29/09
  #
  #   This program checks whether the constraint function h(.) satisfies
  #   a necessary condition for Z homogeneity.
  #
  #   This program is used in mph.fit, versions 3.0 and above.
  #
  #   The main idea...
  #
  #   h(.) is Z homogeneous if h(D(Zgamma)x) = G(gamma)h(x),  where G is a diagonal
  #   matrix with gamma elements raised to some power.
  #
  #   As a check,  if h(.) is homogeneous then h(D(Zgamma)x1)/h(D(Zgamma)x2) = h(x1)/h(x2);
  #   That is, diff = h(D(Zgamma)x1)*h(x2) - h(D(Zgamma)x2)*h(x1)= 0.  [Here, the division
  #   and multiplication is taken element-wise.]
  #
  #   This program randomly generates gamma, x1, and x2 and
  #   computes norm(diff).   It returns a warning if norm(diff) is too far from 0.
  #
  if (is.null(tol)) {tol <- 1e-9}
  nr <- nrow(Z)
  nc <- ncol(Z)
  x1 <- runif(nr,1,10); x2 <- runif(nr,1,10)
  gam <- runif(nc,1,10)
  gx1 <- x1*c((Z%*%gam))  #= diag(Z%*%gam)%*%x1
  gx2 <- x2*c((Z%*%gam))
  diff <- h.fct(gx1)*h.fct(x2) - h.fct(gx2)*h.fct(x1)
  norm.diff <- sqrt(   sum(diff*diff)  )
  chk <- ""
  if (norm.diff >  tol) {chk <- paste("h(m) is not Z homogeneous [based on tol=",tol,"]!")}
  chk
}
######################  end check.homog  #####################################