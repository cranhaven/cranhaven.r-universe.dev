######################  begin check.HLP  #####################################

check.HLP <- function(L.fct,Z,tol=NULL) {
  #
  #   Author:  Joseph B. Lang,  Univ of Iowa
  #   Created: 4/29/09 (updated:  5/20/09)
  #
  #   This program checks whether the link function L(.) is a candidate HLP link
  #   function.   Specifically, this program checks whether L(.) satisfies
  #   certain necessary conditions that follow from a sufficient condition
  #   for HLP link status.
  #
  #   If the necessary conditions are satisfied then there is corroborating
  #   evidence that L(.) has HLP link status.  If the necessary conditions
  #   are not satisfied, then the sufficient condition for HLP link status
  #   is not satisfied, so L(.) may or may not have HLP link status.
  #
  #   This program is used in mph.fit, versions 3.0 and above.
  #
  #   The main idea:
  #
  #   The model L(m) = X beta is an HLP model if L(.) is a smooth link function that
  #   satisfies the  HLP conditions with respect to Z (i.e. strata s) and X.
  #   That is,
  #      1.  L(.) has HLP link status wrt Z   AND
  #      2.  The implied constraint function h(m)=U'L(m) is Z homogeneous.
  #            Here,  null(U') = span(X).
  #
  #      L(.) has HLP link status wrt Z if, for m = D(Z g)p, equivalently,
  #           for g = Z'm  and p = D^{-1}(ZZ'm)m,
  #
  #      1(a)  L(m) = a(g) + L(p),  where a(g1/g2)-a(1) = a(g1) - a(g2),
  #                                  i.e. a(g) has the form  C log nu + constant.
  #                OR
  #
  #       (b)  L(m) = G(g)L(p),   where G(g) is a diagonal matrix with diagonal
  #                               elements that are powers of the g elements,
  #                               i.e. L(.) is Z homogeneous (see Lang 2004).
  #                OR
  #
  #       (c)  The components of L(.) are a mixture of types (a) and (b):
  #            L[j](m) = a[j](g) + L[j](p)  or  L[j](m) = G[j](g)L[j](p),
  #            j=1,...,l.
  #
  #   N.B.  Lang (JASA 2005) defined HLP models as those satisfying 1(a) and 2.
  #         Version 3.1+ of mph.fit uses a broader definition of HLP model.
  #         Specifically, models satisfying 1(b) and 2 or 1(c) and 2 are also
  #         considered HLP models.
  #
  #   Conditions 1(b) and 2 can be checked using the 'check.homog' function.
  #   Condition 1(c) is not checked.
  #
  #   This function, 'check.HLP',  is concerned with sufficient condition 1(a) only.
  #
  #   If L(.) satisfies 1(a)  then
  #
  #      (i)   diff1 = [L(D(g1)p1) - L(D(g2)p1)] - [L(D(g1/g2)p1) - L(p1)] = 0 and
  #      (ii)  diff2 = [L(D(g1)p1) - L(D(g1)p2)] - [L(p1) - L(p2)] = 0
  #               Here pi = D^{-1}(ZZ'mi)mi,  where mi = D(Z gi)pi, i=1,2.
  #
  #   This program randomly generates g1, g2, p1, p2  and
  #   computes norm(diff) = sqrt(norm(diff1)^2 + norm(diff2)^2).
  #   It returns a warning if norm(diff) is too far from 0.
  #
  if (is.null(tol)) {tol <- 1e-9}
  nr <- nrow(Z); nc <- ncol(Z)
  g1 <- runif(nc,1,10)
  g2 <- runif(nc,1,10)
  g1.over.g2 <- g1/g2
  x1 <- runif(nr,1,10)
  x2 <- runif(nr,1,10)
  p1 <- x1*c(1/Z%*%t(Z)%*%x1)
  p2 <- x2*c(1/Z%*%t(Z)%*%x2)
  g1p1 <- p1*c(Z%*%g1)  #= diag(Z%*%g1)%*%p1
  g1p2 <- p2*c(Z%*%g1)
  g2p1 <- p1*c(Z%*%g2)
  g1.over.g2p1 <- p1*c(Z%*%g1.over.g2)
  diff1 <- L.fct(g1p1) - L.fct(g2p1) - (L.fct(g1.over.g2p1) - L.fct(p1))
  diff2 <- L.fct(g1p1) - L.fct(g1p2) - (L.fct(p1) - L.fct(p2))
  norm.diff <- sqrt(sum(diff1*diff1) + sum(diff2*diff2))
  chk <- ""
  if (norm.diff >  tol) {chk <- paste("L(m) may not be an HLP link [based on tol=",tol,"]!")}
  chk
}

######################  end check.HLP  ##############################################
