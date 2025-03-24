#' Last kPC Algorithm Step: Extend Object with Skeleton to Completed PDAG
#'
#' This function performs the last (generalised transitive) step in the \link{kpc} algorithm. It transforms an object of the class "pcAlgo" containing a skeleton and corresponding conditional independence information into a weakly additive noise directed acyclic graph (CPDAG). The functions first determine the v-structures in the collider step, and then performs the Generalised Transitive Step as described in Tillman et al (2009) to orient as many of the remaining edges as possible.
#'
#'
#' @param gInput "pcAlgo"-object containing skeleton and conditional indepedence information.
#' @param suffStat a list of sufficient statistics, containing all necessary elements for the conditional independence decisions in the function indepTest.
#' @param indepTest A function for testing conditional independence. It is internally called as indepTest(x,y,S,suffStat). Default is \link{kernelCItest}.
#' @param alpha significance level (number in (0,1) for the individual conditional independence tests.
#' @param verbose 0: No output; 1: Details
#' @param unfVect vector containing numbers that encode ambiguous triples (as returned by pc.cons.intern). This is needed in the conservative and majority rule PC algorithms.
#' @param solve.confl	if TRUE, the orientation of the v-structures and the orientation rules work with lists for candidate sets and allow bi-directed edges to resolve conflicting edge orientations. Note that therefore the resulting object is order-independent but might not be a PDAG because bi-directed edges can be present.
#' @param orientCollider if TRUE, collider are oriented.
#' @param rules Array of length 3 containing TRUE or FALSE for each rule. TRUE in position i means that rule i (Ri) will be applied. By default, all rules are used.gInput
#' @importFrom utils combn
#' @importFrom pcalg skeleton triple2numb
#' @importMethodsFrom graph numEdges
#' @import methods
#' @export
#' @details First we perform a collider step, that is orienting triples a-b-c as a->b<-c iff b is not in separating set of a and c. Then we orient edges a-S as a->S if b_r is independent of a set S, where b_r are the residuals of b non parametrically regressed on S and parents of b and none of the edges S_i-a can be oriented as S_i->a, that is residuals S_i_r would be independent of a.
#'
#' @return An oriented object of class "pcAlgo".
#'
#' @references Tillman, R. E., Gretton, A. and Spirtes, P. (2009). Nonlinear directed acyclic structure learning with weakly additive noise model. NIPS 22, Vancouver.
## simulate data
#' @examples
#' \dontrun{
#' library(pcalg)
#' set.seed(4)
#' n <- 300
#' data <- NULL
#' x1 <- 2*(runif(n)-0.5)
#' x2 <- x1 + runif(n)-0.5
#' x3 <- x1^2 + 0.6*runif(n)
#' x4 <- rnorm(n)
#' x5 <- x3 + x4^2 + 2*runif(n)
#' x6 <- 10*(runif(n)-0.5)
#' x7 <- x6^2 + 10*runif(n)
#' x8 <- 2*x7^2 + rnorm(n)
#' x9 <- x7 + 5*runif(n)
#' data <- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9)
#' true <- matrix(0,9,9)
#' true[c(1),c(2,3)]<-true[c(3,4),5]<-true[c(6),c(7)]<-true[c(7),c(8)]<-true[7,9]<-1

#' ## estimate skeleton
#' resU1 <- skeleton(suffStat = list(data=data, ic.method="dcc.perm", p=200),
#'                   indepTest = kernelCItest,
#'                   verbose = TRUE, alpha = 0.1, p=9)
#'
#' resU2 <- skeleton(suffStat = list(data=data, ic.method="hsic.gamma",
#'                              sig=1, numCol = 50),
#'                   indepTest = kernelCItest,
#'                   verbose = TRUE, alpha = 0.1, p=9)
#'
#' resU3 <- skeleton(suffStat = list(data=data, ic.method="hsic.perm",
#'                              sig=1, numCol = 50, p=200),
#'                   indepTest = kernelCItest,
#'                   verbose = TRUE, alpha = 0.1, p=9)
#'
#' resU4 <- skeleton(suffStat = list(data=data, ic.method="hsic.clust",
#'                              p=200, sig=1, numCluster=100, numCol = 50,
#'                              eps = 0.1, paral = 1),
#'                   indepTest = kernelCItest,
#'                   verbose = TRUE, alpha = 0.1, p=9)
#'
#' resU5 <- skeleton(suffStat = list(C = cor(data), n = n),
#'                   indepTest = gaussCItest,
#'                   verbose = TRUE, alpha = 0.1, p=9)
#'
#' if (require(Rgraphviz)) {
#'  par(mfrow=c(2,3))
#'  plot(resU1,main="dpc")
#'  plot(resU2,main="kpc-resid-gamma")
#'  plot(resU3,main="kpc-resid-perm")
#'  plot(resU4,main="kpc-clust")
#'  plot(resU5,main="pc")
#'  plot(as(true,"graphNEL"),main="True DAG")
#' }
#'
#' ## orient edges using three different methods
#' resD1 <- udag2wanpdag(gInput = resU1,
#'                       suffStat = list(data=data, ic.method="dcc.perm", sig=1, numCol = 50, p=200),
#'                       indepTest = kernelCItest,
#'                       verbose = TRUE, alpha = 0.1)
#' resD2 <- udag2wanpdag(gInput = resU1,
#'                       suffStat = list(data=data, ic.method="hsic.gamma", sig=1, numCol = 50),
#'                       indepTest = kernelCItest,
#'                       verbose = TRUE, alpha = 0.1)
#' resD3 <- udag2wanpdag(gInput = resU1,
#'                       suffStat = list(data=data, ic.method="hsic.perm", sig=1, numCol = 50, p=200),
#'                       indepTest = kernelCItest,
#'                       verbose = TRUE, alpha = 0.1)
#' resD4 <- udag2pdagRelaxed(gInput = resU1, verbose = T)
#' if (require(Rgraphviz)) {
#'  par(mfrow=c(2,3))
#'  plot(resD1,main="dpc")
#'  plot(resD2,main="kpc-resid-gamma")
#'  plot(resD3,main="kpc-resid-perm")
#'  plot(resD4,main="pc")
#'  plot(as(true,"graphNEL"),main="True DAG")
#' }
#' }

udag2wanpdag <- function (       gInput,
                                 suffStat,
                                 indepTest=kernelCItest,
                                 alpha = 0.2,
                                 verbose = FALSE,
                                 unfVect = NULL,
                                 solve.confl = FALSE,
                                 orientCollider = TRUE,
                                 rules = rep(TRUE, 3)
                                 ){
  rule1 <- function(pdag, solve.confl = FALSE, unfVect = NULL) {
    ind <- which(pdag == 1 & t(pdag) == 0, arr.ind = TRUE)
    for (i in seq_len(nrow(ind))) {
      a <- ind[i, 1]
      b <- ind[i, 2]
      isC <- which(pdag[b, ] == 1 & pdag[, b] == 1 & pdag[a, ] == 0 & pdag[, a] == 0)
      if (length(isC) > 0) {
        for (ii in seq_along(isC)) {
          c <- isC[ii]
          if (!solve.confl | (pdag[b, c] == 1 & pdag[c, b] == 1)) {
            if (!is.null(unfVect)) {
              if (!any(unfVect == triple2numb(p, a, b, c), na.rm = TRUE) && !any(unfVect == triple2numb(p, c, b, a), na.rm = TRUE)) {
                pdag[b, c] <- 1
                pdag[c, b] <- 0
              }
            }
            else {
              pdag[b, c] <- 1
              pdag[c, b] <- 0
            }
            if (verbose)
              cat("\nRule 1':", a, "->", b, " and ",
                  b, "-", c, " where ", a, " and ", c,
                  " not connected and ", a, b, c, " faithful triple: ",
                  b, "->", c, "\n")
          }
          else if (pdag[b, c] == 0 & pdag[c, b] == 1) {
            if (!is.null(unfVect)) {
              if (!any(unfVect == triple2numb(p, a, b, c), na.rm = TRUE) && !any(unfVect == triple2numb(p, c, b, a), na.rm = TRUE)) {
                pdag[b, c] <- 2
                pdag[c, b] <- 2
                if (verbose)
                  cat("\nRule 1':", a, "->", b, "<-",
                      c, " but ", b, "->", c, "also possible and",
                      a, b, c, " faithful triple: ", a,
                      "->", b, "<->", c, "\n")
              }
            }
            else {
              pdag[b, c] <- 2
              pdag[c, b] <- 2
              if (verbose)
                cat("\nRule 1':", a, "->", b, "<-", c,
                    " but ", b, "->", c, "also possible and",
                    a, b, c, " faithful triple: ", a, "->",
                    b, "<->", c, "\n")
            }
          }
        }
      }
      if (!solve.confl)
        pdag <- pdag
    }
    pdag
  }
  ################################################################################
  rule2 <- function(pdag, solve.confl = FALSE) {
    ind <- which(pdag == 1 & t(pdag) == 1, arr.ind = TRUE)
    for (i in seq_len(nrow(ind))) {
      a <- ind[i, 1]
      b <- ind[i, 2]
      isC <- which(pdag[a, ] == 1 & pdag[,a] == 0 & pdag[, b] == 1 & pdag[b,] == 0)
      for (ii in seq_along(isC)) {
        c <- isC[ii]
        if (!solve.confl | (pdag[a, b] == 1 & pdag[b,a] == 1)) {
          pdag[a, b] <- 1
          pdag[b, a] <- 0
          if (verbose)
            cat("\nRule 2: Chain ", a, "->", c, "->",b, ":", a, "->", b, "\n")
        }
        else if (pdag[a, b] == 0 & pdag[b, a] == 1) {
          pdag[a, b] <- 2
          pdag[b, a] <- 2
          if (verbose)
            cat("\nRule 2: Chain ", a, "->", c, "->",b, ":", a, "<->", b, "\n")
        }
      }
      if (!solve.confl)
        pdag <- pdag
    }
    pdag
  }
  ################################################################################
  rule3 <- function(pdag, solve.confl = FALSE, unfVect = NULL) {
    ind <- which(pdag == 1 & t(pdag) == 1,
                 arr.ind = TRUE)
    for (i in seq_len(nrow(ind))) {
      a <- ind[i, 1]
      b <- ind[i, 2]
      c <- which(pdag[a, ] == 1 & pdag[,a] == 1 & pdag[, b] == 1 & pdag[b,] == 0)
      if (length(c) >= 2) {
        cmb.C <- combn(c, 2)
        cC1 <- cmb.C[1, ]
        cC2 <- cmb.C[2, ]
        for (j in seq_along(cC1)) {
          c1 <- cC1[j]
          c2 <- cC2[j]
          if (pdag[c1, c2] == 0 && pdag[c2, c1] == 0) {
            if (!is.null(unfVect)) {
              if (!any(unfVect == triple2numb(p, c1,a, c2), na.rm = TRUE) && !any(unfVect == triple2numb(p, c2, a, c1), na.rm = TRUE)) {
                if (!solve.confl | (pdag[a, b] == 1 &
                                    pdag[b, a] == 1)) {
                  pdag[a, b] <- 1
                  pdag[b, a] <- 0
                  if (!solve.confl)
                    pdag <- pdag
                  if (verbose)
                    cat("\nRule 3':", a, c1, c2, "faithful triple: ",a, "->", b, "\n")
                  break
                }
                else if (pdag[a, b] == 0 & pdag[b, a] ==
                         1) {
                  pdag[a, b] <- pdag[b, a] <- 2
                  if (verbose)
                    cat("\nRule 3':", a, c1, c2, "faithful triple: ",a, "<->", b, "\n")
                  break
                }
              }
            }
            else {
              if (!solve.confl | (pdag[a, b] == 1 & pdag[b, a] == 1)) {
                pdag[a, b] <- 1
                pdag[b, a] <- 0
                if (!solve.confl)
                  pdag <- pdag
                if (verbose)
                  cat("\nRule 3':", a, c1, c2, "faithful triple: ",a, "->", b, "\n")
                break
              }
              else if (pdag[a, b] == 0 & pdag[b, a] == 1) {
                pdag[a, b] <- pdag[b, a] <- 2
                if (verbose)
                  cat("\nRule 3':", a, c1, c2, "faithful triple: ",a, "<->", b, "\n")
                break
              }
            }
          }
        }
      }
    }
    pdag
  }
  ################################################################################
  orientConflictCollider <- function(pdag, x, y, z) {
    if (pdag[x, y] == 1) {
      pdag[y, x] <- 0
    }
    else {
      pdag[x, y] <- pdag[y, x] <- 2
    }
    if (pdag[z, y] == 1) {
      pdag[y, z] <- 0
    }
    else {
      pdag[z, y] <- pdag[y, z] <- 2
    }
    pdag
  }
  ################################################################################
  checkImmor <- function(pdag,
                         V,
                         S,
                         ...){
    output <- TRUE
    udag <- pmin(pdag+t(pdag),1)
    parentsV <- which(pdag[,V]==1 & pdag[V,]==0, arr.ind = T)
    if (length(S)>1){
      test.dag1 <- udag[S,S] + diag(length(S))
      if (length(which(test.dag1==0 & t(test.dag1)==0 , arr.ind=T)) > 0) {output <- FALSE}
    }
    test.dag2 <- udag[c(parentsV),S]
    if (sum(test.dag2) < (length(S) * length(parentsV)) ) {output <- FALSE}
    output

  }
  ################################################################################

  if (numEdges(gInput@graph) == 0)
    return(gInput)
  g <- as(gInput@graph, "matrix")
  p <- nrow(g)
  pdag <- g
  if (orientCollider) {
    ind <- which(g == 1, arr.ind = TRUE)
    for (i in seq_len(nrow(ind))) {
      x <- ind[i, 1]
      y <- ind[i, 2]
      allZ <- setdiff(which(g[y, ] == 1), x)
      for (z in allZ) {
        if (g[x, z] == 0 && !((y %in% gInput@sepset[[x]][[z]]) || (y %in% gInput@sepset[[z]][[x]]))) {
          if (length(unfVect)  == 0) {
            if (!solve.confl) {
              pdag[x, y] <- pdag[z, y] <- 1
              pdag[y, x] <- pdag[y, z] <- 0
            }
            else {
              pdag <- orientConflictCollider(pdag, x, y, z)
            }
          }
          else {
            if (!any(unfVect == triple2numb(p, x, y, z), na.rm = TRUE) && !any(unfVect == triple2numb(p, z, y, x), na.rm = TRUE)) {
              if (!solve.confl) {
                pdag[x, y] <- pdag[z, y] <- 1
                pdag[y, x] <- pdag[y, z] <- 0
              }
              else {
                pdag <- orientConflictCollider(pdag, x, y, z)
              }
            }
          }
        }
      }
    }
  }

  s <- 1
  n <- ncol(suffStat$data)

  undi.nbhd <- list()
  ind <- which(pdag==1 & t(pdag)==1, arr.ind = T)
  colnames(ind)<-NULL
  for (i in 1:n){
    undi.nbhd[[i]] <- ind[ind[,1]==i,2]
  }
  size.undi.nbhd <- sapply(undi.nbhd,length)
  nbhd.updt <- rep(0,n)

  while (max(size.undi.nbhd) >= s){
    if(verbose) cat("Checking undirected neighbourhoods of size ",s,"\n")
    for (i in 1:n){
      V <- i
      if(verbose) cat("V: ",V,". Size of neighbourhood of V: ",size.undi.nbhd[V],"\n")
      s2 <- s
      if ( (size.undi.nbhd[V]==s2) | ( size.undi.nbhd[V]<s2 & nbhd.updt[V]) ) {
        s2 <- size.undi.nbhd[V]
        while (s2 > 0){
          if(verbose) cat("Nbhd of V: ",undi.nbhd[[V]],"\n")
          if (length(undi.nbhd[[V]])==1){sub.nbhd.V <- as.matrix(undi.nbhd[[V]])}
          else {sub.nbhd.V <- as.matrix(combn(undi.nbhd[[V]],s2),nrow=s2)}
          for (j in 1:ncol(sub.nbhd.V)){
            to.update <- TRUE
            if (checkImmor(pdag,V,sub.nbhd.V[,j])){
              S <- sub.nbhd.V[,j]
              parentsV <- which(pdag[,V]==1 & pdag[V,]==0, arr.ind = T)
              pval1 <- regrVonPS(G=pdag, V=V, S=S, suffStat=suffStat, indepTest=indepTest, alpha=alpha)
              if (pval1 > 0) {
                if(verbose) cat("Reject orientation",S,"->",V,", as",V,"cannot be regressed to independence on its parents",parentsV,"and subset",S,".\n")
                to.update <- FALSE
              }
              else {
                if(verbose) cat(V,"can be regressed to independence on its parents",parentsV,"and subset",S,".\n")
                for (k in 1:length(S)){
                  W <- S[k]
                  if(verbose) cat("W: ",W,"\n")
                  s3 <- size.undi.nbhd[W]
                  if(verbose) cat("Nbhd of W: ",undi.nbhd[[W]],"\n")
                  to.continue <- TRUE
                  while (to.continue & s3 > 0){
                    if (length(undi.nbhd[[W]])==1){sub.nbhd.W <- as.matrix(undi.nbhd[[W]])}
                    else {sub.nbhd.W <- as.matrix(combn(undi.nbhd[[W]],s3),nrow=s3)}
                    for (j2 in 1:ncol(sub.nbhd.W)){
                      if (checkImmor(pdag,W,sub.nbhd.W[,j2])){
                        S2 <- sub.nbhd.W[,j2]
                        if (sum(S2==V)==1){
                          parentsV <- which(pdag[,W]==1 & pdag[W,]==0, arr.ind = T)
                          pval2 <- regrVonPS(G=pdag, V=W, S=S2, suffStat=suffStat, indepTest=indepTest, alpha=alpha)
                          if (pval2 == 0) {
                            if(verbose) cat("Reject orientation",S,"->",V,", as",W,"can be regressed to independence on its parents",parentsV,"and subset",S2,".\n")
                            to.update <- FALSE
                            to.continue <- FALSE
                          }
                          else {if(verbose) cat(W,"cannot be regressed to independence on its parents",parentsV,"and subset",S2,".\n")}
                        }
                      }
                      else {if(verbose) cat("Reject orientation",sub.nbhd.W[,j2],"->",W,", as it would create immorality.\n")}
                    }
                    s3 <- s3 - 1
                  }

                }
              }
              if (to.update){
                if(verbose) cat("Accept orientation",S,"->",V,".\n")
                pdag[V,S] <- 0
                pdag[S,V] <- 1
                if ( length(setdiff(undi.nbhd[[V]],S)) != 0 ){
                  pdag[setdiff(undi.nbhd[[V]],S),V] <- 0
                  pdag[V,setdiff(undi.nbhd[[V]],S)] <- 1
                }
                s2 <- 0
                repeat {
                  old_pdag <- pdag
                  if (rules[1]) {
                    pdag <- rule1(pdag, solve.confl = solve.confl, unfVect = unfVect)
                  }
                  if (rules[2]) {
                    pdag <- rule2(pdag, solve.confl = solve.confl)
                  }
                  if (rules[3]) {
                    pdag <- rule3(pdag, solve.confl = solve.confl, unfVect = unfVect)
                  }
                  if (all(pdag == old_pdag))
                    break
                }
                undi.nbhd <- list()
                ind <- which(pdag==1 & t(pdag)==1, arr.ind = T)
                colnames(ind)<-NULL
                for (i in 1:n){
                  undi.nbhd[[i]] <- ind[ind[,1]==i,2]
                }
                nbhd.updt[V] <- TRUE
                size.undi.nbhd <- sapply(undi.nbhd,length)
              }
            }
          }
          s2 <- s2 - 1
        }
      }
    }
    s <- s + 1
  }
  gInput@graph <- as(pdag, "graphNEL")
  gInput
}
