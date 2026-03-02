#' Sums of the Absolute Value of the IPC Scores
#'
#' \code{SIPC.AMMI} computes the Sums of the Absolute Value of the IPC Scores
#' (ASI) \insertCite{sneller_repeatability_1997}{ammistability} considering all
#' significant interaction principal components (IPCs) in the AMMI model. Using
#' SIPC, the Simultaneous Selection Index for Yield and Stability (SSI) is also
#' calculated according to the argument \code{ssi.method}. \loadmathjax
#'
#' The Sums of the Absolute Value of the IPC Scores (\mjseqn{SIPC})
#' \insertCite{sneller_repeatability_1997}{ammistability} is computed as
#' follows:
#'
#' \mjsdeqn{SIPC = \sum_{n=1}^{N'} \left | \lambda_{n}^{0.5}\gamma_{in} \right
#' |}
#'
#' OR
#'
#' \mjsdeqn{SIPC = \sum_{n=1}^{N'}\left | PC_{n} \right |}
#'
#' Where, \mjseqn{N'} is the number of significant IPCs (number of IPC that were
#' retained in the AMMI model via F tests); \mjseqn{\lambda_{n}} is the singular
#' value for \mjseqn{n}th IPC and correspondingly \mjseqn{\lambda_{n}^{2}} is
#' its eigen value; \mjseqn{\gamma_{in}} is the eigenvector value for
#' \mjseqn{i}th genotype; and \mjseqn{PC_{1}}, \mjseqn{PC_{2}}, \mjseqn{\cdots},
#' \mjseqn{PC_{n}} are the scores of 1st, 2nd, ..., and \mjseqn{n}th IPC.
#'
#' The closer the SIPC scores are to zero, the more stable the genotypes are
#' across test environments.
#'
#' @inheritParams MASV.AMMI
#'
#' @return A data frame with the following columns:  \item{SIPC}{The SIPC
#'   values.} \item{SSI}{The computed values of simultaneous selection index for
#'   yield and stability.} \item{rSIPC}{The ranks of SIPC values.} \item{rY}{The
#'   ranks of the mean yield of genotypes.} \item{means}{The mean yield of the
#'   genotypes.}
#'
#'   The names of the genotypes are indicated as the row names of the data
#'   frame.
#'
#' @importFrom methods is
#' @importFrom stats aggregate
#' @importFrom agricolae AMMI
#' @export
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @seealso \code{\link[agricolae]{AMMI}}, \code{\link[ammistability]{SSI}}
#'
#' @examples
#' library(agricolae)
#' data(plrv)
#'
#' # AMMI model
#' model <- with(plrv, AMMI(Locality, Genotype, Rep, Yield, console = FALSE))
#'
#' # ANOVA
#' model$ANOVA
#'
#' # IPC F test
#' model$analysis
#'
#' # Mean yield and IPC scores
#' model$biplot
#'
#' # G*E matrix (deviations from mean)
#' array(model$genXenv, dim(model$genXenv), dimnames(model$genXenv))
#'
#' # With default n (N') and default ssi.method (farshadfar)
#' SIPC.AMMI(model)
#'
#' # With n = 4 and default ssi.method (farshadfar)
#' SIPC.AMMI(model, n = 4)
#'
#' # With default n (N') and ssi.method = "rao"
#' SIPC.AMMI(model, ssi.method = "rao")
#'
#' # Changing the ratio of weights for Rao's SSI
#' SIPC.AMMI(model, ssi.method = "rao", a = 0.43)
#'
SIPC.AMMI <- function(model, n, alpha = 0.05,
                      ssi.method = c("farshadfar", "rao"), a = 1) {

  # Check model class
  if (!is(model, "AMMI")) {
    stop('"model" is not of class "AMMI"')
  }

  # Check alpha value
  if (!(0 < alpha && alpha < 1)) {
    stop('"alpha" should be between 0 and 1 (0 < alpha < 1)')
  }

  # Find number of significant IPCs according to F test
  if (missing(n) || is.null(n)) {
    n <- sum(model$analysis$Pr.F <= alpha, na.rm = TRUE)
  }

  # Check for n
  if (n %% 1 != 0 && length(n) != 1) {
    stop('"n" is not an integer vector of unit length')
  }

  # Check if n > N
  if (n > nrow(model$analysis)) {
    stop('"n" is greater than the number of IPCs in "model"')
  }

  ssi.method <- match.arg(ssi.method)

  # Fetch response (Yield)
  yresp <- setdiff(colnames(model$means), c("ENV", "GEN", "RESIDUAL"))

# cova <- cov(model$genXenv)
# values <- eigen(cova)
# SIPC1 <- (sqrt(values$values[1])*model$biplot[,3])
# SIPC2 <- (sqrt(values$values[2])*model$biplot[,4])
# SIPC3 <- (sqrt(values$values[3])*model$biplot[,5])
# SIPC <- SIPC1+SIPC2+SIPC3
# rs <- rank(SIPC)
# rSIPC <- data.frame(SIPC1,SIPC2,SIPC3,SIPC,rs)
# rSIPC

  # # GxE matrix
  # ge <- array(model$genXenv, dim(model$genXenv), dimnames(model$genXenv))
  # # SVD
  # svdge <- svd(ge)
  # lambda.n <- svdge$d[1:n]
  # gamma.n <- svdge$u[,1:n]
  # A <- gamma.n %*% diag(sqrt(lambda.n))

  A <- model$biplot
  A <- A[A[, 1] == "GEN", -c(1, 2)]
  A <- A[, 1:n] # Fetch only n IPCs

  SIPC <- unname(rowSums(apply(A, 2, FUN = abs)))

  B <- model$means
  W <- aggregate(B[, yresp], by = list(model$means$GEN), FUN = mean, na.rm = TRUE)
  SSI_SIPC <- SSI(y = W$x, sp = SIPC, gen = W$Group.1,
                  method = ssi.method, a = a)
  ranking <- SSI_SIPC
  colnames(ranking) <- c("SIPC", "SSI", "rSIPC", "rY", "means")

  return(ranking)

}
