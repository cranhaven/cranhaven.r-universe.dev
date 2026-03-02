#' Absolute Value of the Relative Contribution of IPCs to the Interaction
#'
#' \code{ZA.AMMI} computes the Absolute Value of the Relative Contribution of
#' IPCs to the Interaction (\mjseqn{\textrm{Z}_{\textrm{a}}})
#' \insertCite{zali_evaluation_2012}{ammistability} considering all significant
#' interaction principal components (IPCs) in the AMMI model. Using
#' \mjseqn{\textrm{Z}_{\textrm{a}}}, the Simultaneous Selection Index for Yield
#' and Stability (SSI) is also calculated according to the argument
#' \code{ssi.method}. \loadmathjax
#'
#' The Absolute Value of the Relative Contribution of IPCs to the Interaction
#' (\mjseqn{Za}) \insertCite{zali_evaluation_2012}{ammistability} is computed as
#' follows:
#'
#' \mjsdeqn{Za = \sum_{i=1}^{N'}\left | \theta_{n}\gamma_{in} \right |}
#'
#' Where, \mjseqn{N'} is the number of significant IPCAs (number of IPC that
#' were retained in the AMMI model via F tests); \mjseqn{\gamma_{in}} is the
#' eigenvector value for \mjseqn{i}th genotype; and \mjseqn{\theta_{n}} is the
#' percentage sum of squares explained by the \mjseqn{n}th principal component
#' interaction effect..
#'
#' @inheritParams MASV.AMMI
#'
#' @return A data frame with the following columns:  \item{Za}{The Za values.}
#'   \item{SSI}{The computed values of simultaneous selection index for yield
#'   and stability.} \item{rZa}{The ranks of Za values.} \item{rY}{The ranks of
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
#' ZA.AMMI(model)
#'
#' # With n = 4 and default ssi.method (farshadfar)
#' ZA.AMMI(model, n = 4)
#'
#' # With default n (N') and ssi.method = "rao"
#' ZA.AMMI(model, ssi.method = "rao")
#'
#' # Changing the ratio of weights for Rao's SSI
#' ZA.AMMI(model, ssi.method = "rao", a = 0.43)
#'
ZA.AMMI <- function(model, n, alpha = 0.05,
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

  # Za1 = model$biplot[,3]*model$analysis[1,1]
  # Za1<-Za1[-c(53:56)]
  # Za2 = model$biplot[,4]*model$analysis[2,1]
  # Za2<-Za2[-c(53:56)]
  # Za3 = model$biplot[,5]*model$analysis[3,1]
  # Za3<-Za3[-c(53:56)]
  # Za = Za1+Za2+Za3
  # rZ<-rank(Z)
  # rZi<-data.frame(Za1,Za2,Za3,Za,rZa)
  # rZi

  # GxE matrix
  ge <- array(model$genXenv, dim(model$genXenv), dimnames(model$genXenv))
  # SVD
  svdge <- svd(ge)
  gamma.n <- svdge$u[, 1:n]

  theta.n <- model$analysis[1:n, ]$percent / 100

  Za <- rowSums(abs(gamma.n %*% diag(theta.n)))

  B <- model$means
  W <- aggregate(B[, yresp], by = list(model$means$GEN), FUN = mean, na.rm = TRUE)
  SSI_Za <- SSI(y = W$x, sp = Za, gen = W$Group.1,
                method = ssi.method, a = a)
  ranking <- SSI_Za
  colnames(ranking) <- c("Za", "SSI", "rZa", "rY", "means")

  return(ranking)

}
