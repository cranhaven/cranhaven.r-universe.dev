#' Estimate the WAN-PDAG using the kPC Algorithm
#'
#' Estimates the weakly additive noise partially directed acyclic graph (WAN-PDAG) from observational data, using the kPC algorithm. This is a version of \code{\link{pc}} from pcalg package, that uses HSIC (\link{hsic.gamma}, \link{hsic.perm} or \link{hsic.clust}) or distance covariance (\code{\link{dcov.test}} or \link{dcov.gamma})  independence tests and \link{udag2wanpdag} instead of \code{\link{udag2pdag}} in the last step.
#'
#' @param suffStat a \link{list} of sufficient statistics, containing all necessary elements for the conditional independence decisions in the function indepTest
#' @param indepTest A function for testing conditional independence. It is internally called as indepTest(x,y,S,suffStat), and tests conditional independence of x and y given S. Here, x and y are variables, and S is a (possibly empty) vector of variables (all variables are denoted by their column numbers in the adjacency matrix). suffStat is a list, see the argument above. The return value of indepTest is the p-value of the test for conditional independence. Default is \link{kernelCItest}.
#' @param alpha significance level (number in (0,1) for the individual conditional independence tests.
#' @param labels (optional) character vector of variable (or "node") names. Typically preferred to specifying p.
#' @param p (optional) number of variables (or nodes). May be specified if labels are not, in which case labels is set to 1:p.
#' @param verbose If TRUE, detailed output is provided.
#' @param fixedGaps A logical matrix of dimension p*p. If entry [i,j] or [j,i] (or both) are TRUE, the edge i-j is removed before starting the algorithm. Therefore, this edge is guaranteed to be absent in the resulting graph.
#' @param fixedEdges A logical matrix of dimension p*p. If entry [i,j] or [j,i] (or both) are TRUE, the edge i-j is never considered for removal. Therefore, this edge is guaranteed to be present in the resulting graph.
#' @param NAdelete If indepTest returns NA and this option is TRUE, the corresponding edge is deleted. If this option is FALSE, the edge is not deleted.
#' @param m.max	Maximal size of the conditioning sets that are considered in the conditional independence tests.
#' @param u2pd String specifying the method for dealing with conflicting information when trying to orient edges (see details below).
#' @param skel.method	Character string specifying method; the default, "stable" provides an order-independent skeleton, see skeleton.
#' @param conservative Logical indicating if the conservative PC is used. In this case, only option u2pd = "relaxed" is supported. Note that therefore the resulting object might not be extendable to a DAG. See details for more information.
#' @param maj.rule	Logical indicating that the triples shall be checked for ambiguity using a majority rule idea, which is less strict than the conservative PC algorithm. For more information, see details.
#' @param solve.confl	If TRUE, the orientation of the v-structures and the orientation rules work with lists for candidate sets and allow bi-directed edges to resolve conflicting edge orientations. In this case, only option u2pd = relaxed is supported. Note, that therefore the resulting object might not be a CPDAG because bi-directed edges might be present. See details for more information.
#' @details For more information: \code{\link{pc}}.
#'
#' @importFrom pcalg skeleton pc.cons.intern pcAlgo
#' @importClassesFrom graph graphNEL
#' @import methods
#'
#' @export
#' @return An object of class "pcAlgo" (see \code{\link{pcAlgo}}) containing an estimate of the equivalence class of the underlying DAG.
#' @author Petras Verbyla (\email{petras.verbyla@mrc-bsu.cam.ac.uk})
#'
#' @references Tillman, R. E., Gretton, A. and Spirtes, P. (2009). Nonlinear directed acyclic structure learning with weakly additive noise model. NIPS 22, Vancouver.
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
#' x7 <- x6^2 + 5*runif(n)
#' x8 <- 2*x7^2 + 1.5*rnorm(n)
#' x9 <- x7 + 4*runif(n)
#' data <- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9)
#' true <- matrix(0,9,9)
#' true[c(1),c(2,3)]<-true[c(3,4),5]<-true[c(6),c(7)]<-true[c(7),c(8)]<-true[7,9]<-1
#'
#' pc <- pc(suffStat = list(C = cor(data), n = 9),
#'          indepTest = gaussCItest,
#'          alpha = 0.9,
#'          labels = colnames(data),
#'          u2pd = "relaxed",
#'          skel.method = "stable",
#'          verbose = TRUE)
#' kpc1 <- kpc(suffStat = list(data=data, ic.method="dcc.perm"),
#'             indepTest = kernelCItest,
#'             alpha = 0.1,
#'             labels = colnames(data),
#'             u2pd = "relaxed",
#'             skel.method = "stable",
#'             verbose = TRUE)
#' kpc2 <- kpc(suffStat = list(data=data, ic.method="hsic.gamma"),
#'             indepTest = kernelCItest,
#'             alpha = 0.1,
#'             labels = colnames(data),
#'             u2pd = "relaxed",
#'             skel.method = "stable",
#'             verbose = TRUE)
#' kpc3 <- kpc(suffStat = list(data=data, ic.method="hsic.perm"),
#'             indepTest = kernelCItest,
#'             alpha = 0.1,
#'             labels = colnames(data),
#'             u2pd = "relaxed",
#'             skel.method = "stable",
#'             verbose = TRUE)
#' kpc4 <- kpc(suffStat = list(data=data, ic.method="hsic.clust"),
#'             indepTest = kernelCItest,
#'             alpha = 0.1,
#'             labels = colnames(data),
#'             u2pd = "relaxed",
#'             skel.method = "stable",
#'             verbose = TRUE)
#'
#' if (require(Rgraphviz)) {
#'  par(mfrow=c(2,3))
#'  plot(pc,main="pc")
#'  plot(kpc1,main="dpc.perm")
#'  plot(kpc2,main="kpc.gamma")
#'  plot(kpc3,main="kpc.perm")
#'  plot(kpc4,main="kpc.clust")
#'  plot(as(true,"graphNEL"),main="True DAG")
#' }
#' }

kpc <- function (suffStat, indepTest, alpha, labels, p,
                 fixedGaps = NULL, fixedEdges = NULL, NAdelete = TRUE, m.max = Inf,
                 u2pd = c("relaxed","rand", "retry"),
                 skel.method = c("stable", "original","stable.fast"),
                 conservative = FALSE, maj.rule = FALSE,
                 solve.confl = FALSE, verbose = FALSE)
{
  cl <- match.call()
  if (!missing(p))
    stopifnot(is.numeric(p), length(p <- as.integer(p)) ==
                1, p >= 2)
  if (missing(labels)) {
    if (missing(p))
      stop("need to specify 'labels' or 'p'")
    labels <- as.character(seq_len(p))
  }
  else {
    stopifnot(is.character(labels))
    if (missing(p)) {
      p <- length(labels)
    }
    else if (p != length(labels))
      stop("'p' is not needed when 'labels' is specified, and must match length(labels)")
    else message("No need to specify 'p', when 'labels' is given")
  }
  u2pd <- match.arg(u2pd)
  skel.method <- match.arg(skel.method)
  if (u2pd != "relaxed") {
    if (conservative || maj.rule)
      stop("Conservative PC and majority rule PC can only be run with 'u2pd = relaxed'")
    if (solve.confl)
      stop("Versions of PC using lists for the orientation rules (and possibly bi-directed edges)\n can only be run with 'u2pd = relaxed'")
  }
  if (conservative && maj.rule)
    stop("Choose either conservative PC or majority rule PC!")
  skel <- skeleton(suffStat, indepTest, alpha, labels = labels,
                   method = skel.method, fixedGaps = fixedGaps, fixedEdges = fixedEdges,
                   NAdelete = NAdelete, m.max = m.max, verbose = verbose)
  skel@call <- cl
  if (!conservative && !maj.rule) {
    wanpdag <- udag2wanpdag(gInput = skel, suffStat=suffStat, indepTest = indepTest, alpha = alpha, verbose = verbose, solve.confl = solve.confl)
  }
  else {
    pc. <- pc.cons.intern(skel, suffStat, indepTest, alpha, version.unf = c(2, 1), maj.rule = maj.rule, verbose = verbose)
    print(pc.$sk)
    print(pc.$unfTripl)
    wanpdag <- udag2wanpdag(pc.$sk, suffStat=suffStat, indepTest = indepTest, alpha = alpha, verbose = verbose, unfVect = pc.$unfTripl, solve.confl = solve.confl)
  }
  wanpdag
}
