#' Averages of the Squared Eigenvector Values
#'
#' \code{EV.AMMI} computes the Sums of the Averages of the Squared Eigenvector
#' Values (EV) \insertCite{zobel_stress_1994}{ammistability} considering all
#' significant interaction principal components (IPCs) in the AMMI model. Using
#' EV, the Simultaneous Selection Index for Yield and Stability (SSI) is also
#' calculated according to the argument \code{ssi.method}. \loadmathjax
#'
#' The Averages of the Squared Eigenvector Values (\mjseqn{EV})
#' \insertCite{zobel_stress_1994}{ammistability} is computed as follows:
#'
#' \mjsdeqn{EV = \sum_{n=1}^{N'}\frac{\gamma_{in}^2}{N'}}
#'
#' Where, \mjseqn{N'} is the number of significant IPCs (number of IPC that were
#' retained in the AMMI model via F tests); and \mjseqn{\gamma_{in}} is the
#' eigenvector value for \mjseqn{i}th genotype.
#'
#' @inheritParams MASV.AMMI
#'
#' @return A data frame with the following columns:  \item{EV}{The EV values.}
#'   \item{SSI}{The computed values of simultaneous selection index for yield
#'   and stability.} \item{rEV}{The ranks of EV values.} \item{rY}{The ranks of
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
#' \insertRef{zobel_stress_1994}{ammistability}
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
#' EV.AMMI(model)
#'
#' # With n = 4 and default ssi.method (farshadfar)
#' EV.AMMI(model, n = 4)
#'
#' # With default n (N') and ssi.method = "rao"
#' EV.AMMI(model, ssi.method = "rao")
#'
#' # Changing the ratio of weights for Rao's SSI
#' EV.AMMI(model, ssi.method = "rao", a = 0.43)
#'
EV.AMMI <- function(model, n, alpha = 0.05,
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

# EV1<-model$biplot[,3]^2
# EV2<-model$biplot[,4]^2
# EV3<-model$biplot[,5]^2
# EV<-EV1+EV2+EV3
# rEV<-rank(EV)
# rEV<-data.frame(EV1,EV2,EV3,EV,rEV)
# rEV

  # GxE matrix
  ge <- array(model$genXenv, dim(model$genXenv), dimnames(model$genXenv))
  # SVD
  svdge <- svd(ge)
  gamma.n <- svdge$u[, 1:n]

  EV <- rowSums(gamma.n^2 / n)

  B <- model$means
  W <- aggregate(B[, yresp], by = list(model$means$GEN), FUN = mean, na.rm = TRUE)
  SSI_EV <- SSI(y = W$x, sp = EV, gen = W$Group.1,
                method = ssi.method, a = a)
  ranking <- SSI_EV
  colnames(ranking) <- c("EV", "SSI", "rEV", "rY", "means")

  return(ranking)

}
