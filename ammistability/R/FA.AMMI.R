#' Stability Measure Based on Fitted AMMI Model
#'
#' \code{FA.AMMI} computes the Stability Measure Based on Fitted AMMI Model (FA)
#' \insertCite{raju_study_2002}{ammistability} considering all significant
#' interaction principal components (IPCs) in the AMMI model. Using FA, the
#' Simultaneous Selection Index for Yield and Stability (SSI) is also calculated
#' according to the argument \code{ssi.method}. \loadmathjax
#'
#' The Stability Measure Based on Fitted AMMI Model (\mjseqn{FA})
#' \insertCite{raju_study_2002}{ammistability} is computed as follows:
#'
#' \mjsdeqn{FA = \sum_{n=1}^{N'}\lambda_{n}^{2}\gamma_{in}^{2}}
#'
#' Where, \mjseqn{N'} is the number of significant IPCs (number of IPC that were
#' retained in the AMMI model via F tests); \mjseqn{\lambda_{n}} is the singular
#' value for \mjseqn{n}th IPC and correspondingly \mjseqn{\lambda_{n}^{2}} is
#' its eigen value; and \mjseqn{\gamma_{in}} is the eigenvector value for
#' \mjseqn{i}th genotype.
#'
#' When \mjseqn{N'} is replaced by 1 (only first IPC axis is considered for
#' computation), then the parameter \mjseqn{FP} can be estimated
#' \insertCite{zali_evaluation_2012}{ammistability}.
#'
#' \mjsdeqn{FP = \lambda_{1}^{2}\gamma_{i1}^{2}}
#'
#' When \mjseqn{N'} is replaced by 2 (only first two IPC axes are considered for
#' computation), then the parameter \mjseqn{B} can be estimated
#' \insertCite{zali_evaluation_2012}{ammistability}.
#'
#' \mjsdeqn{B = \sum_{n=1}^{2}\lambda_{n}^{2}\gamma_{in}^{2}}
#'
#' When \mjseqn{N'} is replaced by \mjseqn{N} (All the IPC axes are considered
#' for computation), then the parameter estimated is equivalent to Wricke's
#' ecovalence (\mjseqn{W_{(AMMI)}})
#' \insertCite{wricke_method_1962,zali_evaluation_2012}{ammistability}.
#'
#' \mjsdeqn{W_{(AMMI)} = \sum_{n=1}^{N}\lambda_{n}^{2}\gamma_{in}^{2}}
#'
#' @inheritParams MASV.AMMI
#'
#' @return A data frame with the following columns:  \item{FA}{The FA values.}
#'   \item{SSI}{The computed values of simultaneous selection index for yield
#'   and stability.} \item{rFA}{The ranks of FA values.} \item{rY}{The ranks of
#'   the mean yield of genotypes.} \item{means}{The mean yield of the
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
#' FA.AMMI(model)
#'
#' # With n = 4 and default ssi.method (farshadfar)
#' FA.AMMI(model, n = 4)
#'
#' # With default n (N') and ssi.method = "rao"
#' FA.AMMI(model, ssi.method = "rao")
#'
#' # Changing the ratio of weights for Rao's SSI
#' FA.AMMI(model, ssi.method = "rao", a = 0.43)
#'
FA.AMMI <- function(model, n, alpha = 0.05,
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

# cova<-cov(model$genXenv)
# values<-eigen(cova)
# D1<-sqrt((values$values[1]*model$biplot[,3])^2)
# D2<-sqrt((values$values[2]*model$biplot[,4])^2)
# D3<-sqrt((values$values[3]*model$biplot[,5])^2)
# D<-D1+D2+D3
# FA<-D1^2+D2^2+D3^2
# rf<-rank(FA)
# rFA<-data.frame(FA,rf)
# rFA

  # GxE matrix
  ge <- array(model$genXenv, dim(model$genXenv), dimnames(model$genXenv))
  # SVD
  svdge <- svd(ge)
  gamma.n <- svdge$u[, 1:n]
  lambda.n <- svdge$d[1:n]

  FA <- rowSums((gamma.n^2) %*% diag(lambda.n^2))

  B <- model$means
  W <- aggregate(B[, yresp], by = list(model$means$GEN), FUN = mean, na.rm = TRUE)
  SSI_FA <- SSI(y = W$x, sp = FA, gen = W$Group.1,
                method = ssi.method, a = a)
  ranking <- SSI_FA
  colnames(ranking) <- c("FA", "SSI", "rFA", "rY", "means")

  return(ranking)

}
