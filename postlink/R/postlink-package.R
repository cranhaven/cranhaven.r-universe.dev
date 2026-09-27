#' @title postlink: Post-Linkage Data Analysis Under Record Linkage Errors
#'
#' @description
#' The \code{postlink} package provides a unified suite of statistical tools
#' designed to rigorously account for record linkage errors in downstream modeling.
#'
#' Record linkage is often error-prone. When datasets are merged using noisy or non-unique
#' identifiers, mismatches (false links) are inadvertently introduced. Ignoring these errors
#' acts as a contaminant in regression analysis, typically leading to attenuated
#' estimates and biased statistical inference. \code{postlink} equips researchers with
#' methodologies to propagate linkage uncertainty into their models, in particular accommodating
#' "secondary analysis" workflows where direct access to the primary, unlinked files is restricted.
#'
#' @details
#' \strong{A Two-Phase Workflow}
#'
#' The package is built on a modular, object-oriented S3 architecture to
#' provide a familiar, standard formula-based modeling interface. The
#' specification of linkage error is decoupled from the substantive statistical modeling.
#'
#' \strong{Phase 1: Adjustment Specification}
#'
#' The analyst encapsulates the linked data and the chosen error-adjustment methodology
#' by using one of the \code{adj*} constructor functions. These functions validate the
#' data and return a structured adjustment object:
#' \itemize{
#'   \item \code{\link{adjELE}()}: Specifies the Exchangeable Linkage Error (ELE) model,
#'   which corrects for bias using known or audited mismatch rates.
#'   \item \code{\link{adjMixture}()}: Specifies a frequentist mixture model approach
#'   that treats match status as a latent variable, estimating error rates directly from
#'   data (e.g., using match scores) via the EM algorithm.
#'   \item \code{\link{adjMixBayes}()}: Specifies a Bayesian mixture model approach,
#'   enabling parameter estimation and multiple imputation of latent match statuses
#'   using Stan.
#' }
#'
#' \strong{Phase 2: Estimation & Inference}
#'
#' The adjustment object is subsequently passed to a standard modeling wrapper,
#' integrating the error correction into the familiar R modeling syntax:
#' \itemize{
#'   \item \code{\link{plglm}()}: Generalized Linear Models (linear, logistic, Poisson, Gamma).
#'   \item \code{\link{plcoxph}()}: Cox Proportional Hazards models.
#'   \item \code{\link{plctable}()}: Contingency table analyses.
#'   \item \code{\link{plsurvreg}()}: Parametric survival models.
#' }
#' As a note, estimation and inference supported for each type of adjustment
#' object vary. Refer to the \code{adj*} documentation for models currently supported.
#'
#' Post-estimation tools function as expected in standard R workflows (e.g.,
#' \code{summary()}, \code{predict()}, \code{vcov()}, and \code{confint()}).
#' These methods specially  are derived to account for the additional steps
#' introduced by the linkage error adjustment.
#'
#' \strong{Note:}
#' While the two-phase workflow is recommended for standard analyses, the package's
#' architecture intentionally isolates the core logic of each method into independent
#' internal routines. Advanced users, developers, or researchers running large-scale
#' simulations can bypass the wrapper functions and formula-parsing overhead entirely.
#' By supplying pre-computed design matrices and response vectors, the underlying
#' computational functions can be directly used if preferred (e.g., \code{\link{coxphELE}()},
#'  \code{\link{glmMixture}()}, \code{\link{survregMixBayes}()}).
#'
#' @references
#' Chambers, R. (2009). Regression analysis of probability-linked data.
#' \emph{Official Statistics Research Series}, 4, 1-15.
#'
#' Chambers, R. L., Fabrizi, E., Ranalli, M. G., Salvati, N., & Wang, S. (2023).
#' Robust regression using probabilistically linked data. \emph{Wiley Interdisciplinary
#' Reviews: Computational Statistics}, 15(2), e1596. \doi{10.1002/wics.1596}
#'
#' Gutman, R., Sammartino, C., Green, T., & Montague, B. (2016). Error adjustments for file
#' linking methods using encrypted unique client identifier (eUCI) with application to recently
#' released prisoners who are HIV+. \emph{Statistics in Medicine}, 35(1), 115–129. \doi{10.1002/sim.6586}
#'
#' Slawski, M., West, B. T., Bukke, P., Wang, Z., Diao, G., &
#' Ben-David, E. (2025). A general framework for regression with mismatched
#' data based on mixture modelling. \emph{Journal of the Royal Statistical Society
#' Series A: Statistics in Society}, 188(3), 896-919. \doi{10.1093/jrsssa/qnae083}
#'
#' Vo, T. H., Garès, V., Zhang, L. C., Happe, A., Oger, E., Paquelet, S., & Chauvet, G. (2024).
#' Cox regression with linked data. \emph{Statistics in Medicine}, 43(2), 296-314. \doi{10.1002/sim.9960}
#'
#' @useDynLib postlink, .registration = TRUE
#' @import Rcpp
#' @import methods
#' @importFrom rstantools rstan_config
#' @importFrom RcppParallel RcppParallelLibs
"_PACKAGE"
