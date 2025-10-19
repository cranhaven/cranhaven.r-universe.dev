#' @title Repeated-Measures Lin's Concordance Correlation Coefficient (CCC)
#'
#' @description
#' Computes all pairwise Lin's Concordance Correlation Coefficients (CCC)
#' across multiple methods (L \eqn{\geq} 2) for repeated-measures data.
#' Each subject must be measured by all methods across the same set of time
#' points or replicates.
#'
#' CCC measures both accuracy (how close measurements are to the line of
#' equality) and precision (Pearson correlation). Confidence intervals are
#' optionally computed using a U-statistics-based estimator with Fisher's Z
#' transformation
#'
#' @param data A data frame containing the repeated-measures dataset.
#' @param response Character. Name of the numeric outcome column.
#' @param method Character. Name of the method column (factor with L
#' \eqn{\geq} 2 levels).
#' @param time Character or NULL. Name of the time/repetition column. If NULL,
#' one time point is assumed.
#' @param Dmat Optional numeric weight matrix (T \eqn{\times} T) for
#' timepoints. Defaults to identity.
#' @param delta Numeric. Power exponent used in the distance computations
#' between method trajectories
#' across time points. This controls the contribution of differences between
#' measurements:
#' \itemize{
#'   \item \code{delta = 1} (default) uses **absolute differences**.
#'   \item \code{delta = 2} uses **squared differences**, more sensitive to
#'   larger deviations.
#'   \item \code{delta = 0} reduces to a **binary distance** (presence/absence
#'   of disagreement), analogous to a repeated-measures version of the kappa
#'   statistic.
#' }
#' The choice of \code{delta} should reflect the penalty you want to assign to
#' measurement disagreement.
#
#' @param ci Logical. If TRUE, returns confidence intervals (default FALSE).
#' @param conf_level Confidence level for CI (default 0.95).
#' @param n_threads Integer (\eqn{\geq} 1). Number of OpenMP threads to use for computation.
#'   Defaults to \code{getOption("matrixCorr.threads", 1L)}.
#' @param verbose Logical. If TRUE, prints diagnostic output (default FALSE).
#'
#' @return
#' If \code{ci = FALSE}, a symmetric matrix of class \code{"ccc"} (estimates only).
#' If \code{ci = TRUE}, a list of class \code{"ccc"}, \code{"ccc_ci"} with elements:
#' \itemize{
#'   \item \code{est}: CCC estimate matrix
#'   \item \code{lwr.ci}: Lower bound matrix
#'   \item \code{upr.ci}: Upper bound matrix
#' }
#'
#' @details
#' This function computes pairwise Lin's Concordance Correlation Coefficient
#' (CCC) between methods in a repeated-measures design using a
#' U-statistics-based nonparametric estimator proposed by
#' Carrasco et al. (2013). It is computationally efficient and robust,
#' particularly for large-scale or balanced longitudinal designs.
#'
#' Lin's CCC is defined as
#' \deqn{
#' \rho_c = \frac{2 \cdot \mathrm{cov}(X, Y)}{\sigma_X^2 + \sigma_Y^2 +
#' (\mu_X - \mu_Y)^2}
#' }{
#' CCC = 2 * cov(X, Y) / [var(X) + var(Y) + (mean(X) - mean(Y))^2]
#' }
#' where:
#' \itemize{
#'   \item \eqn{X} and \eqn{Y} are paired measurements from two methods.
#'   \item \eqn{\mu_X}, \eqn{\mu_Y} are means, and \eqn{\sigma_X^2},
#'   \eqn{\sigma_Y^2} are variances.
#' }
#'
#' ## U-statistics Estimation
#' For repeated measures across \eqn{T} time points and \eqn{n} subjects we
#' assume
#' \itemize{
#'   \item all \eqn{n(n-1)} pairs of subjects are considered to compute a
#'   U-statistic estimator for within-method and cross-method distances.
#'   \item if `delta > 0`, pairwise distances are raised to a power before
#'   applying a time-weighted kernel matrix \eqn{D}.
#'   \item if `delta = 0`, the method reduces to a version similar to a
#'   repeated-measures kappa.
#' }
#'
#' ## Confidence Intervals
#' Confidence intervals are constructed using a **Fisher Z-transformation**
#' of the CCC. Specifically,
#' \itemize{
#'   \item The CCC is transformed using
#'   \eqn{Z = 0.5 \log((1 + \rho_c) / (1 - \rho_c))}.
#'   \item Standard errors are computed from the asymptotic variance of the
#'   U-statistic.
#'   \item Normal-based intervals are computed on the Z-scale and then
#'   back-transformed to the CCC scale.
#' }
#'
#' ## Assumptions
#' \itemize{
#'   \item The design must be **balanced**, where all subjects must have
#'   complete observations for all methods and time points.
#'   \item The method is **nonparametric** and does not require assumptions
#'   of normality or linear mixed effects.
#'   \item Weights (`Dmat`) allow differential importance of time points.
#' }
#'
#' For **unbalanced** or **complex hierarchical** data (e.g.,
#' missing timepoints, covariate adjustments), consider using
#' \code{\link{ccc_lmm_reml}}, which uses a variance components approach
#' via linear mixed models.
#'
#' @references
#' Lin L (1989). A concordance correlation coefficient to evaluate reproducibility.
#' \emph{Biometrics}, 45: 255-268.
#'
#' Lin L (2000). A note on the concordance correlation coefficient.
#' \emph{Biometrics}, 56: 324-325.
#'
#' Carrasco JL, Jover L (2003). Estimating the concordance correlation coefficient:
#' a new approach. \emph{Computational Statistics & Data Analysis}, 47(4): 519-539.
#'
#' @seealso \code{\link{ccc}}, \code{\link{ccc_lmm_reml}},
#' \code{\link{plot.ccc}}, \code{\link{print.ccc}}
#'
#' @examples
#' set.seed(123)
#' df <- expand.grid(subject = 1:10,
#'                   time = 1:2,
#'                   method = c("A", "B", "C"))
#' df$y <- rnorm(nrow(df), mean = match(df$method, c("A", "B", "C")), sd = 1)
#'
#' # CCC matrix (no CIs)
#' ccc1 <- ccc_pairwise_u_stat(df, response = "y", method = "method", time = "time")
#' print(ccc1)
#' summary(ccc1)
#' plot(ccc1)
#'
#' # With confidence intervals
#' ccc2 <- ccc_pairwise_u_stat(df, response = "y", method = "method", time = "time", ci = TRUE)
#' print(ccc2)
#' summary(ccc2)
#' plot(ccc2)
#'
#' #------------------------------------------------------------------------
#' # Choosing delta based on distance sensitivity
#' #------------------------------------------------------------------------
#' # Absolute distance (L1 norm) - robust
#' ccc_pairwise_u_stat(df, response = "y", method = "method", time = "time", delta = 1)
#'
#' # Squared distance (L2 norm) - amplifies large deviations
#' ccc_pairwise_u_stat(df, response = "y", method = "method", time = "time", delta = 2)
#'
#' # Presence/absence of disagreement (like kappa)
#' ccc_pairwise_u_stat(df, response = "y", method = "method", time = "time", delta = 0)
#'
#' @author Thiago de Paula Oliveira
#' @export
ccc_pairwise_u_stat <- function(data,
                        response,
                        method,
                        time = NULL,
                        Dmat = NULL,
                        delta = 1,
                        ci = FALSE,
                        conf_level = 0.95,
                        n_threads = getOption("matrixCorr.threads", 1L),
                        verbose = FALSE) {
  df <- as.data.frame(data)
  df[[method]] <- factor(df[[method]])
  method_levels <- levels(df[[method]])
  L <- length(method_levels)

  if (L < 2) stop("Need at least two methods (levels in method)")

  if (is.null(time)) {
    df$time_i <- 0
    ntime <- 1
  } else {
    df[[time]] <- factor(df[[time]])
    df$time_i <- as.integer(df[[time]]) - 1
    ntime <- length(levels(df[[time]]))
  }

  if (is.null(Dmat)) {
    Dmat <- diag(ntime)
  } else {
    Dmat <- as.matrix(Dmat)
    if (!all(dim(Dmat) == c(ntime, ntime))) stop("Dmat dimension mismatch")
  }

  cccr_est <- matrix(1, L, L, dimnames = list(method_levels, method_levels))
  cccr_lwr <- matrix(NA_real_, L, L, dimnames = list(method_levels, method_levels))
  cccr_upr <- matrix(NA_real_, L, L, dimnames = list(method_levels, method_levels))

  n_threads <- max(1L, as.integer(n_threads))
  set_omp_threads(n_threads)
  if (verbose) cat("Using", get_omp_threads(), "OpenMP threads\n")

  # Loop over all method pairs
  for (i in 1:(L - 1)) {
    for (j in (i + 1):L) {
      m1 <- method_levels[i]
      m2 <- method_levels[j]

      df_sub <- df[df[[method]] %in% c(m1, m2), , drop = FALSE]
      df_sub$met_i <- as.integer(factor(df_sub[[method]], levels = c(m1, m2))) - 1

      ns <- nrow(df_sub) / (2 * ntime)
      if (ns != floor(ns))
        stop(sprintf("Data inconsistent for method pair %s vs %s", m1, m2))

      res <- cccUst_rcpp(df_sub[[response]],
                         df_sub$met_i,
                         df_sub$time_i,
                         0, 1,
                         ntime, ns,
                         Dmat, delta,
                         conf_level)

      cccr_est[i, j] <- cccr_est[j, i] <- res$CCC
      cccr_lwr[i, j] <- cccr_lwr[j, i] <- res$LL_CI
      cccr_upr[i, j] <- cccr_upr[j, i] <- res$UL_CI
    }
  }

  if (ci) {
    result <- list(est = cccr_est, lwr.ci = cccr_lwr, upr.ci = cccr_upr)
    attr(result, "method") <- "Repeated-measures Lin's concordance"
    attr(result, "description") <- "Repeated-measures CCC (pairwise) with confidence intervals"
    attr(result, "package") <- "matrixCorr"
    attr(result, "conf.level")  <- conf_level
    class(result) <- c("ccc", "ccc_ci")
  } else {
    result <- cccr_est
    attr(result, "method") <- "Repeated-measures Lin's concordance"
    attr(result, "description") <- "Repeated-measures CCC (pairwise matrix)"
    attr(result, "package") <- "matrixCorr"
    class(result) <- c("ccc", "matrix")
  }

  return(result)
}

#' @title Concordance Correlation via REML (Linear Mixed-Effects Model)
#'
#' @description
#' Compute Lin's Concordance Correlation Coefficient (CCC) from a linear
#' mixed-effects model fitted by REML. The fixed-effects part can include
#' \code{method} and/or \code{time} (optionally their interaction), with a
#' subject-specific random intercept to capture between-subject variation.
#' Large \eqn{n \times n} inversions are avoided by solving small per-subject
#' systems.
#'
#' \strong{Assumption:} time levels are treated as \emph{regular, equally spaced}
#' visits indexed by their order within subject. The AR(1) residual model is
#' in discrete time on the visit index (not calendar time). \code{NA} time codes
#' break the serial run. Gaps in the factor levels are \emph{ignored} (adjacent
#' observed visits are treated as lag-1).
#'
#' @param data A data frame.
#' @param response Character. Response variable name.
#' @param rind Character. Subject ID variable name (random intercept).
#' @param method Character or \code{NULL}. Optional column name of method factor
#'   (added to fixed effects).
#' @param time Character or \code{NULL}. Optional column name of time factor
#'   (added to fixed effects).
#' @param interaction Logical. Include \code{method:time} interaction?
#'   (default \code{FALSE}).
#' @param max_iter Integer. Maximum iterations for variance-component updates
#'   (default \code{100}).
#' @param tol Numeric. Convergence tolerance on parameter change
#'   (default \code{1e-6}).
#'
#' @param Dmat Optional \eqn{n_t \times n_t} numeric matrix to weight/aggregate
#'   time-specific fixed biases in the \eqn{S_B} quadratic form. If supplied, it
#'   is used (after optional mass rescaling; see \code{Dmat_rescale}) whenever at
#'   least two \emph{present} time levels exist; otherwise it is ignored. \strong{If
#'   \code{Dmat} is \code{NULL}}, a canonical kernel \eqn{D_m} is \emph{constructed}
#'   from \code{Dmat_type} and \code{Dmat_weights} (see below). \code{Dmat} should be
#'   symmetric positive semidefinite; small asymmetries are symmetrized internally.
#'
#' @param Dmat_type Character, one of \code{c("time-avg","typical-visit",
#'   "weighted-avg","weighted-sq")}. Only used when \code{Dmat = NULL}.
#'   It selects the aggregation target for time-specific fixed biases in
#'   \eqn{S_B}. Options are:
#'
#'   \itemize{
#'     \item \code{"time-avg"}: square of the time-averaged bias, \eqn{D_m=(1/n_t)\,11^\top}.
#'     \item \code{"typical-visit"}: average of squared per-time biases, \eqn{D_m=I_{n_t}}.
#'     \item \code{"weighted-avg"}: square of a weighted average, \eqn{D_m=n_t\,w\,w^\top} with \eqn{\sum w=1}.
#'     \item \code{"weighted-sq"}: weighted average of squared biases, \eqn{D_m=n_t\,\mathrm{diag}(w)} with \eqn{\sum w=1}.
#'   }
#'   Pick \code{"time-avg"} for CCC targeting the time-averaged measurement; pick
#'   \code{"typical-visit"} for CCC targeting a randomly sampled visit (typical occasion).
#'   Default \code{"time-avg"}.
#'
#' @param Dmat_weights Optional numeric weights \eqn{w} used when
#'   \code{Dmat_type \%in\% c("weighted-avg","weighted-sq")}. Must be nonnegative and
#'   finite. If \code{names(w)} are provided, they should match the \emph{full} time
#'   levels in \code{data}; they are aligned to the \emph{present} time subset per fit.
#'   If unnamed, the length must equal the number of present time levels. In all cases
#'   \eqn{w} is internally normalized to sum to 1.
#'
#' @param Dmat_rescale Logical. When \code{TRUE} (default), the supplied/built
#'   \eqn{D_m} is rescaled to satisfy the simple mass rule
#'   \eqn{1^\top D_m 1 = n_t}. This keeps the \eqn{S_B} denominator invariant and
#'   harmonizes with the \eqn{\kappa}-shrinkage used for variance terms.
#'
#' @param ci Logical. If \code{TRUE}, return a CI container; limits are computed
#'   by a large-sample delta method for CCC (see \strong{CIs} note below).
#' @param conf_level Numeric in \eqn{(0,1)}. Confidence level when
#'   \code{ci = TRUE} (default \code{0.95}).
#' @param ci_mode Character scalar; one of \code{c("auto","raw","logit")}.
#'   Controls how confidence intervals are computed when \code{ci = TRUE}.
#'   If \code{"raw"}, a Wald CI is formed on the CCC scale and truncated to
#'   \code{[0,1]}. If \code{"logit"}, a Wald CI is computed on the
#'   \eqn{\mathrm{logit}(\mathrm{CCC})} scale and back-transformed to the
#'   original scale (often more stable near 0 or 1). If \code{"auto"}
#'   (default), the method is chosen per estimate based on simple diagnostics
#'   (e.g., proximity to the \code{[0,1]} boundary / numerical stability),
#'   typically preferring \code{"logit"} near the boundaries and \code{"raw"}
#'   otherwise.
#' @param verbose Logical. If \code{TRUE}, prints a structured summary of the
#'   fitted variance components and \eqn{S_B} for each fit. Default \code{FALSE}.
#' @param digits Integer \eqn{(\ge 0)}. Number of decimal places to use in the
#'   printed summary when \code{verbose = TRUE}. Default \code{4}.
#' @param use_message Logical. When \code{verbose = TRUE}, choose the printing
#'   mechanism, where \code{TRUE} uses \code{message()} (respects \code{sink()},
#'   easily suppressible via \code{suppressMessages()}), whereas \code{FALSE}
#'   uses \code{cat()} to \code{stdout}. Default \code{TRUE}.
#'
#' @param ar Character. Residual correlation structure: \code{"none"} (iid) or
#'   \code{"ar1"} for subject-level AR(1) correlation within contiguous time
#'   runs. Default \code{c("none")}.
#' @param ar_rho Numeric in \eqn{(-0.999,\,0.999)} or \code{NA}.
#'   If \code{ar = "ar1"} and \code{ar_rho} is finite, it is treated as fixed.
#'   If \code{ar = "ar1"} and \code{ar_rho = NA}, \eqn{\rho} is estimated by
#'   profiling a 1-D objective (REML when available; an approximation otherwise).
#'   Default \code{NA_real_}.
#' @param slope Character. Optional extra random-effect design \eqn{Z}.
#'   With \code{"subject"} a single random slope is added (one column in \eqn{Z});
#'   with \code{"method"} one column per method level is added; with
#'   \code{"custom"} you provide \code{slope_Z} directly. Default
#'   \code{c("none","subject","method","custom")}.
#' @param slope_var For \code{slope \%in\% c("subject","method")}, a character
#'   string giving the name of a column in \code{data} used as the slope regressor
#'   (e.g., centered time). It is looked up inside \code{data}; do not
#'   pass the vector itself. NAs are treated as zeros in \eqn{Z}.
#' @param slope_Z For \code{slope = "custom"}, a numeric matrix with \eqn{n}
#'   rows (same order as \code{data}) providing the full extra random-effect
#'   design \eqn{Z}. \strong{Each column of \code{slope_Z} has its own variance
#'   component} \eqn{\sigma_{Z,j}^2}; columns are treated as \emph{uncorrelated}
#'   (diagonal block in \eqn{G}). Ignored otherwise.
#' @param drop_zero_cols Logical. When \code{slope = "method"}, drop all-zero
#'   columns of \eqn{Z} after subsetting (useful in pairwise fits). Default
#'   \code{TRUE}.
#'
#' @param vc_select Character scalar; one of \code{c("auto","none")}.
#'   Controls how the subject by method \eqn{\sigma^2_{A\times M}} ("subj_method") and
#'   subject by time \eqn{\sigma^2_{A\times T}} ("subj_time") variance components are
#'   included. If \code{"auto"} (default), the function performs boundary-aware
#'   REML likelihood-ratio tests (LRTs; null on the boundary at zero with a
#'   half-\eqn{\chi^2_1} reference) to decide whether to retain each component,
#'   in the order given by \code{vc_test_order}. If \code{"none"}, no testing
#'   is done and inclusion is taken from \code{include_subj_method}/\code{include_subj_time}
#'   (or, if \code{NULL}, from the mere presence of the corresponding factor in
#'   the design).  In pairwise fits, the decision is made independently for each
#'   method pair.
#'
#' @param vc_alpha Numeric scalar in \eqn{(0,1)}; default \code{0.05}.
#'   Per-component significance level for the boundary-aware REML LRTs used when
#'   \code{vc_select = "auto"}. The tests are one-sided for variance components
#'   on the boundary and are *not* multiplicity-adjusted.
#'
#' @param vc_test_order Character vector (length 2) with a permutation of
#'   \code{c("subj_time","subj_method")}; default \code{c("subj_time","subj_method")}. Specifies the order
#'   in which the two variance components are tested when \code{vc_select = "auto"}.
#'   The component tested first may be dropped before testing the second. If a
#'   factor is absent in the design (e.g., no time factor so "subj_time" is undefined),
#'   the corresponding test is skipped.
#'
#' @param include_subj_method,include_subj_time Logical scalars or \code{NULL}.
#'   When \code{vc_select = "none"}, these control whether the
#'   \eqn{\sigma^2_{A\times M}} ("subj_method") and \eqn{\sigma^2_{A\times T}} ("subj_time")
#'   random effects are included (\code{TRUE}) or excluded (\code{FALSE}) in the model.
#'   If \code{NULL} (default), inclusion defaults to the presence of the
#'   corresponding factor in the data (i.e., at least two method/time levels).
#'   When \code{vc_select = "auto"}, these arguments are ignored (automatic
#'   selection is used instead).
#'
#' @param sb_zero_tol Non-negative numeric scalar; default \code{1e-10}.
#'   Numerical threshold for the fixed-effect dispersion term \eqn{S_B}.
#'   After computing \eqn{\widehat{S_B}} and its delta-method variance, if
#'   \eqn{\widehat{S_B} \le} \code{sb_zero_tol} or non-finite, the procedure
#'   treats \eqn{S_B} as fixed at zero in the delta step. It sets
#'   \eqn{d_{S_B}=0} and \eqn{\mathrm{Var}(\widehat{S_B})=0}, preventing
#'   numerical blow-ups of \code{SE(CCC)} when \eqn{\widehat{S_B}\to 0} and the
#'   fixed-effects variance is ill-conditioned for the contrast. This stabilises
#'   inference in rare boundary cases; it has no effect when
#'   \eqn{\widehat{S_B}} is comfortably above the threshold.

#'
#' @details
#' For measurement \eqn{y_{ij}} on subject \eqn{i} under fixed
#' levels (method, time), we fit
#' \deqn{ y = X\beta + Zu + \varepsilon,\qquad
#'        u \sim N(0,\,G),\ \varepsilon \sim N(0,\,R). }
#' Notation: \eqn{m} subjects, \eqn{n=\sum_i n_i} total rows;
#' \eqn{nm} method levels; \eqn{nt} time levels; \eqn{q_Z} extra
#' random-slope columns (if any); \eqn{r=1+nm+nt} (or \eqn{1+nm+nt+q_Z} with slopes).
#' Here \eqn{Z} is the subject-structured random-effects design and \eqn{G} is
#' block-diagonal at the subject level with the following \emph{per-subject}
#' parameterisation. Specifically,
#' \itemize{
#'   \item one random intercept with variance \eqn{\sigma_A^2};
#'   \item optionally, \emph{method} deviations (one column per method level)
#'         with a common variance \eqn{\sigma_{A\times M}^2} and zero
#'         covariances across levels (i.e., multiple of an identity);
#'   \item optionally, \emph{time} deviations (one column per time level)
#'         with a common variance \eqn{\sigma_{A\times T}^2} and zero
#'         covariances across levels;
#'   \item optionally, an \emph{extra} random effect aligned with \eqn{Z}
#'         (random slope), where each \emph{column} has its own variance
#'         \eqn{\sigma_{Z,j}^2} and columns are uncorrelated.
#' }
#' The fixed-effects design is \code{~ 1 + method + time} and, if
#' \code{interaction=TRUE}, \code{+ method:time}.
#'
#' \strong{Residual correlation \eqn{R} (regular, equally spaced time).}
#' Write \eqn{R_i=\sigma_E^2\,C_i(\rho)}. With \code{ar="none"}, \eqn{C_i=I}.
#' With \code{ar="ar1"}, within-subject residuals follow a \emph{discrete} AR(1)
#' process along the visit index after sorting by increasing time level. Ties
#' retain input order, and any \code{NA} time code breaks the series so each
#' contiguous block of non-\code{NA} times forms a run. The correlation
#' between \emph{adjacent observed visits} in a run is \eqn{\rho}; we do not use
#' calendar-time gaps. Internally we work with the \emph{precision} of the AR(1)
#' correlation: for a run of length \eqn{L\ge 2}, the tridiagonal inverse has
#' \deqn{ (C^{-1})_{11}=(C^{-1})_{LL}=\frac{1}{1-\rho^2},\quad
#'        (C^{-1})_{tt}=\frac{1+\rho^2}{1-\rho^2}\ (2\le t\le L-1),\quad
#'        (C^{-1})_{t,t+1}=(C^{-1})_{t+1,t}=\frac{-\rho}{1-\rho^2}. }
#' The working inverse is \eqn{\,R_i^{-1}=\sigma_E^{-2}\,C_i(\rho)^{-1}}.
#'
#' \strong{Per-subject Woodbury system.} For subject \eqn{i} with \eqn{n_i}
#' rows, define the per-subject random-effects design \eqn{U_i} (columns:
#' intercept, method indicators, time indicators; dimension
#' \eqn{\,r=1+nm+nt\,}). The core never forms
#' \eqn{V_i = R_i + U_i G U_i^\top} explicitly. Instead,
#' \deqn{ M_i \;=\; G^{-1} \;+\; U_i^\top R_i^{-1} U_i, }
#' and accumulates GLS blocks via rank-\eqn{r} corrections using
#' \eqn{\,V_i^{-1} = R_i^{-1}-R_i^{-1}U_i M_i^{-1}U_i^\top R_i^{-1}\,}:
#' \deqn{ X^\top V^{-1} X \;=\; \sum_i \Big[
#'        X_i^\top R_i^{-1}X_i \;-\; (X_i^\top R_i^{-1}U_i)\,
#'        M_i^{-1}\,(U_i^\top R_i^{-1}X_i) \Big], }
#' \deqn{ X^\top V^{-1} y \;=\; \sum_i \Big[
#'        X_i^\top R_i^{-1}y_i \;-\; (X_i^\top R_i^{-1}U_i)\,M_i^{-1}\,
#'        (U_i^\top R_i^{-1}y_i) \Big]. }
#' Because \eqn{G^{-1}} is diagonal with positive entries, each \eqn{M_i} is
#' symmetric positive definite; solves/inversions use symmetric-PD routines with
#' a small diagonal ridge and a pseudo-inverse if needed.
#'
#' \strong{Random-slope \eqn{Z}.}
#' Besides \eqn{U_i}, the function can include an extra design \eqn{Z_i}.
#' \itemize{
#'   \item \code{slope="subject"}: \eqn{Z} has one column (the regressor in
#'         \code{slope_var}); \eqn{Z_{i}} is the subject-\eqn{i} block, with its own
#'         variance \eqn{\sigma_{Z,1}^2}.
#'   \item \code{slope="method"}: \eqn{Z} has one column per method level;
#'         row \eqn{t} uses the slope regressor if its method equals level \eqn{\ell},
#'         otherwise 0; all-zero columns can be dropped via
#'         \code{drop_zero_cols=TRUE} after subsetting. Each column has its own
#'         variance \eqn{\sigma_{Z,\ell}^2}.
#'   \item \code{slope="custom"}: \eqn{Z} is provided fully via \code{slope_Z}.
#'         Each column is an independent random effect with its own variance
#'         \eqn{\sigma_{Z,j}^2}; cross-covariances among columns are set to 0.
#' }
#' Computations simply augment \eqn{\tilde U_i=[U_i\ Z_i]} and the corresponding
#' inverse-variance block. The EM updates then include, for each column \eqn{j=1,\ldots,q_Z},
#' \deqn{ \sigma_{Z,j}^{2\,(new)} \;=\; \frac{1}{m}
#'       \sum_{i=1}^m \Big( b_{i,\text{extra},j}^2 +
#'       (M_i^{-1})_{\text{extra},jj} \Big)
#'       \quad (\text{if } q_Z>0). }
#' \emph{Interpretation:} the \eqn{\sigma_{Z,j}^2} represent additional within-subject
#' variability explained by the slope regressor(s) in column \eqn{j} and are \emph{not} part of the CCC
#' denominator (agreement across methods/time).
#'
#' \strong{EM-style variance-component updates.} With current \eqn{\hat\beta},
#' form residuals \eqn{r_i = y_i - X_i\hat\beta}. The BLUPs and conditional
#' covariances are
#' \deqn{ b_i \;=\; M_i^{-1}\,(U_i^\top R_i^{-1} r_i), \qquad
#'       \mathrm{Var}(b_i\mid y) \;=\; M_i^{-1}. }
#' Let \eqn{e_i=r_i-U_i b_i}. Expected squares then yield closed-form updates:
#' \deqn{ \sigma_A^{2\,(new)} \;=\; \frac{1}{m}\sum_i \Big( b_{i,0}^2 +
#' (M_i^{-1})_{00} \Big), }
#' \deqn{ \sigma_{A\times M}^{2\,(new)} \;=\; \frac{1}{m\,nm}
#'       \sum_i \sum_{\ell=1}^{nm}\!\Big( b_{i,\ell}^2 +
#'       (M_i^{-1})_{\ell\ell} \Big)
#'       \quad (\text{if } nm>0), }
#' \deqn{ \sigma_{A\times T}^{2\,(new)} \;=\; \frac{1}{m\,nt}
#'       \sum_i \sum_{t=1}^{nt}
#'       \Big( b_{i,t}^2 + (M_i^{-1})_{tt} \Big)
#'       \quad (\text{if } nt>0), }
#' \deqn{ \sigma_E^{2\,(new)} \;=\; \frac{1}{n} \sum_i
#'       \Big( e_i^\top C_i(\rho)^{-1} e_i \;+\;
#'       \mathrm{tr}\!\big(M_i^{-1}U_i^\top C_i(\rho)^{-1} U_i\big) \Big), }
#' together with the per-column update for \eqn{\sigma_{Z,j}^2} given above.
#' Iterate until the \eqn{\ell_1} change across components is \eqn{<}\code{tol}
#' or \code{max_iter} is reached.
#'
#' \strong{Fixed-effect dispersion \eqn{S_B}: choosing the time-kernel \eqn{D_m}.}
#'
#' Let \eqn{d = L^\top \hat\beta} stack the within-time, pairwise method differences,
#' grouped by time as \eqn{d=(d_1^\top,\ldots,d_{n_t}^\top)^\top} with
#' \eqn{d_t \in \mathbb{R}^{P}} and \eqn{P = n_m(n_m-1)}. The symmetric
#' positive semidefinite kernel \eqn{D_m \succeq 0} selects which functional of the
#' bias profile \eqn{t \mapsto d_t} is targeted by \eqn{S_B}. Internally, the code
#' rescales any supplied/built \eqn{D_m} to satisfy \eqn{1^\top D_m 1 = n_t} for
#' stability and comparability.
#'
#' \itemize{
#'   \item \code{Dmat_type = "time-avg"} (square of the time-averaged bias).
#'     Let \deqn{ w \;=\; \frac{1}{n_t}\,\mathbf{1}_{n_t}, \qquad
#'                D_m \;\propto\; I_P \otimes (w\,w^\top), }
#'     so that
#'     \deqn{ d^\top D_m d \;\propto\; \sum_{p=1}^{P}
#'            \left( \frac{1}{n_t}\sum_{t=1}^{n_t} d_{t,p} \right)^{\!2}. }
#'     Methods have equal \eqn{\textit{time-averaged}}
#'     means within subject, i.e. \eqn{\sum_{t=1}^{n_t} d_{t,p}/n_t = 0} for all
#'     \eqn{p}. Appropriate when decisions depend on an average over time and
#'     opposite-signed biases are allowed to cancel.
#'
#'   \item \code{Dmat_type = "typical-visit"} (average of squared per-time biases).
#'     With equal visit probability, take
#'     \deqn{ D_m \;\propto\; I_P \otimes
#'            \mathrm{diag}\!\Big(\tfrac{1}{n_t},\ldots,\tfrac{1}{n_t}\Big), }
#'     yielding
#'     \deqn{ d^\top D_m d \;\propto\; \frac{1}{n_t}
#'            \sum_{t=1}^{n_t}\sum_{p=1}^{P} d_{t,p}^{\,2}. }
#'     Methods agree on a \eqn{\textit{typical}}
#'     occasion drawn uniformly from the visit set. Use when each visit matters
#'     on its own; alternating signs \eqn{d_{t,p}} do not cancel.
#'
#'   \item \code{Dmat_type = "weighted-avg"} (square of a weighted time average).
#'     For user weights \eqn{a=(a_1,\ldots,a_{n_t})^\top} with \eqn{a_t \ge 0}, set
#'     \deqn{ w \;=\; \frac{a}{\sum_{t=1}^{n_t} a_t}, \qquad
#'            D_m \;\propto\; I_P \otimes (w\,w^\top), }
#'     so that
#'     \deqn{ d^\top D_m d \;\propto\; \sum_{p=1}^{P}
#'            \left( \sum_{t=1}^{n_t} w_t\, d_{t,p} \right)^{\!2}. }
#'     Methods have equal \eqn{\textit{weighted
#'     time-averaged}} means, i.e. \eqn{\sum_{t=1}^{n_t} w_t\, d_{t,p} = 0} for all
#'     \eqn{p}. Use when some visits (e.g., baseline/harvest) are a priori more
#'     influential; opposite-signed biases may cancel according to \eqn{w}.
#'
#'   \item \code{Dmat_type = "weighted-sq"} (weighted average of squared per-time biases).
#'     With the same weights \eqn{w}, take
#'     \deqn{ D_m \;\propto\; I_P \otimes \mathrm{diag}(w_1,\ldots,w_{n_t}), }
#'     giving
#'     \deqn{ d^\top D_m d \;\propto\; \sum_{t=1}^{n_t} w_t
#'            \sum_{p=1}^{P} d_{t,p}^{\,2}. }
#'     Methods agree at visits sampled with
#'     probabilities \eqn{\{w_t\}}, counting each visit’s discrepancy on its own.
#'     Use when per-visit agreement is required but some visits should be
#'     emphasised more than others.
#' }
#'
#' \strong{Time-averaging for CCC (regular visits).}
#' The reported CCC targets agreement of the \emph{time-averaged} measurements
#' per method within subject by default (\code{Dmat_type="time-avg"}). Averaging over \eqn{T}
#' non-\code{NA} visits shrinks time-varying components by
#' \deqn{ \kappa_T^{(g)} \;=\; 1/T, \qquad
#'       \kappa_T^{(e)} \;=\; \{T + 2\sum_{k=1}^{T-1}(T-k)\rho^k\}/T^2, }
#' with \eqn{\kappa_T^{(e)}=1/T} when residuals are i.i.d. With unbalanced \eqn{T}, the
#' implementation averages the per-(subject,method) \eqn{\kappa} values across the
#' pairs contributing to CCC and then clamps \eqn{\kappa_T^{(e)}} to
#' \eqn{[10^{-12},\,1]} for numerical stability. Choosing
#' \code{Dmat_type="typical-visit"} makes \eqn{S_B} match the interpretation of a
#' randomly sampled occasion instead.
#'
#' \strong{Concordance correlation coefficient.} The CCC used is
#' \deqn{ \mathrm{CCC} \;=\;
#'       \frac{\sigma_A^2 + \kappa_T^{(g)}\,\sigma_{A\times T}^2}
#'            {\sigma_A^2 + \sigma_{A\times M}^2 +
#'             \kappa_T^{(g)}\,\sigma_{A\times T}^2 + S_B +
#'             \kappa_T^{(e)}\,\sigma_E^2}. }
#' Special cases: with no method factor, \eqn{S_B=\sigma_{A\times M}^2=0}; with
#' a single time level, \eqn{\sigma_{A\times T}^2=0} (no \eqn{\kappa}-shrinkage).
#' When \eqn{T=1} or \eqn{\rho=0}, both \eqn{\kappa}-factors equal 1. The \emph{extra}
#' random-effect variances \eqn{\{\sigma_{Z,j}^2\}} (if used) are \emph{not} included.
#'
#' \strong{CIs / SEs (delta method for CCC).}
#' Let
#' \deqn{ \theta \;=\; \big(\sigma_A^2,\ \sigma_{A\times M}^2,\
#' \sigma_{A\times T}^2,\ \sigma_E^2,\ S_B\big)^\top, }
#' and write \eqn{\mathrm{CCC}(\theta)=N/D} with
#' \eqn{N=\sigma_A^2+\kappa_T^{(g)}\sigma_{A\times T}^2} and
#' \eqn{D=\sigma_A^2+\sigma_{A\times M}^2+\kappa_T^{(g)}\sigma_{A\times T}^2+S_B+\kappa_T^{(e)}\sigma_E^2}.
#' The gradient components are
#' \deqn{ \frac{\partial\,\mathrm{CCC}}{\partial \sigma_A^2}
#'       \;=\; \frac{\sigma_{A\times M}^2 + S_B + \kappa_T^{(e)}\sigma_E^2}{D^2}, }
#' \deqn{ \frac{\partial\,\mathrm{CCC}}{\partial \sigma_{A\times M}^2}
#'       \;=\; -\,\frac{N}{D^2}, \qquad
#'        \frac{\partial\,\mathrm{CCC}}{\partial \sigma_{A\times T}^2}
#'       \;=\; \frac{\kappa_T^{(g)}\big(\sigma_{A\times M}^2 + S_B +
#'                                     \kappa_T^{(e)}\sigma_E^2\big)}{D^2}, }
#' \deqn{ \frac{\partial\,\mathrm{CCC}}{\partial \sigma_E^2}
#'       \;=\; -\,\frac{\kappa_T^{(e)}\,N}{D^2}, \qquad
#'        \frac{\partial\,\mathrm{CCC}}{\partial S_B}
#'       \;=\; -\,\frac{N}{D^2}. }
#'
#' \emph{Estimating \eqn{\mathrm{Var}(\hat\theta)}.}
#' The EM updates write each variance component as an average of per-subject
#' quantities. For subject \eqn{i},
#' \deqn{ t_{A,i} \;=\; b_{i,0}^2 + (M_i^{-1})_{00},\qquad
#'        t_{M,i} \;=\; \frac{1}{nm}\sum_{\ell=1}^{nm}
#'                        \Big(b_{i,\ell}^2 + (M_i^{-1})_{\ell\ell}\Big), }
#' \deqn{ t_{T,i} \;=\; \frac{1}{nt}\sum_{j=1}^{nt}
#'                        \Big(b_{i,j}^2 + (M_i^{-1})_{jj}\Big),\qquad
#'        s_i \;=\; \frac{e_i^\top C_i(\rho)^{-1} e_i +
#'        \mathrm{tr}\!\big(M_i^{-1}U_i^\top C_i(\rho)^{-1} U_i\big)}{n_i}, }
#' where \eqn{b_i = M_i^{-1}(U_i^\top R_i^{-1} r_i)} and
#' \eqn{M_i = G^{-1} + U_i^\top R_i^{-1} U_i}.
#' With \eqn{m} subjects, form the empirical covariance of the stacked
#' subject vectors and scale by \eqn{m} to approximate the covariance of the
#' means:
#' \deqn{ \widehat{\mathrm{Cov}}\!\left(
#'       \begin{bmatrix} t_{A,\cdot} \\ t_{M,\cdot} \\ t_{T,\cdot} \end{bmatrix}
#'       \right)
#'       \;\approx\; \frac{1}{m}\,
#'        \mathrm{Cov}_i\!\left(
#'       \begin{bmatrix} t_{A,i} \\ t_{M,i} \\ t_{T,i} \end{bmatrix}\right). }
#' (Drop rows/columns as needed when \code{nm==0} or \code{nt==0}.)
#'
#' The residual variance estimator is a weighted mean
#' \eqn{\hat\sigma_E^2=\sum_i w_i s_i} with \eqn{w_i=n_i/n}. Its variance is
#' approximated by the variance of a weighted mean of independent terms,
#' \deqn{ \widehat{\mathrm{Var}}(\hat\sigma_E^2)
#'       \;\approx\; \Big(\sum_i w_i^2\Big)\,\widehat{\mathrm{Var}}(s_i), }
#' where \eqn{\widehat{\mathrm{Var}}(s_i)} is the sample variance across
#' subjects. The method-dispersion term uses the quadratic-form delta already
#' computed for \eqn{S_B}:
#' \deqn{ \widehat{\mathrm{Var}}(S_B)
#'       \;=\; \frac{2\,\mathrm{tr}\!\big((A_{\!fix}\,\mathrm{Var}(\hat\beta))^2\big)
#'              \;+\; 4\,\hat\beta^\top A_{\!fix}\,\mathrm{Var}(\hat\beta)\,
#'              A_{\!fix}\,\hat\beta}
#'                    {\big[nm\,(nm-1)\,\max(nt,1)\big]^2}, }
#' with \eqn{A_{\!fix}=L\,D_m\,L^\top}.
#'
#' \emph{Putting it together.} Assemble
#' \eqn{\widehat{\mathrm{Var}}(\hat\theta)} by combining the
#' \eqn{(\sigma_A^2,\sigma_{A\times M}^2,\sigma_{A\times T}^2)} covariance
#' block from the subject-level empirical covariance, add the
#' \eqn{\widehat{\mathrm{Var}}(\hat\sigma_E^2)} and
#' \eqn{\widehat{\mathrm{Var}}(S_B)} terms on the diagonal,
#' and ignore cross-covariances across these blocks (a standard large-sample
#' simplification). Then
#' \deqn{ \widehat{\mathrm{se}}\{\widehat{\mathrm{CCC}}\}
#'       \;=\; \sqrt{\,\nabla \mathrm{CCC}(\hat\theta)^\top\,
#'                     \widehat{\mathrm{Var}}(\hat\theta)\,
#'                     \nabla \mathrm{CCC}(\hat\theta)\,}. }
#'
#' A two-sided \eqn{(1-\alpha)} normal CI is
#' \deqn{ \widehat{\mathrm{CCC}} \;\pm\; z_{1-\alpha/2}\,
#'       \widehat{\mathrm{se}}\{\widehat{\mathrm{CCC}}\}, }
#' truncated to \eqn{[0,1]} in the output for convenience. When \eqn{S_B} is
#' truncated at 0 or samples are very small/imbalanced, the normal CI can be
#' mildly anti-conservative near the boundary; a logit transform for CCC or a
#' subject-level (cluster) bootstrap can be used for sensitivity analysis.
#'
#' \strong{Choosing \eqn{\rho} for AR(1).}
#' When \code{ar="ar1"} and \code{ar_rho = NA}, \eqn{\rho} is estimated by
#' profiling the REML log-likelihood at \eqn{(\hat\beta,\hat G,\hat\sigma_E^2)}.
#' With very few visits per subject, \eqn{\rho} can be weakly identified; consider
#' sensitivity checks over a plausible range.
#'
#' @section Notes on stability and performance:
#' All per-subject solves are \eqn{\,r\times r} with \eqn{r=1+nm+nt+q_Z}, so cost
#' scales with the number of subjects and the fixed-effects dimension rather
#' than the total number of observations. Solvers use symmetric-PD paths with
#' a small diagonal ridge and pseudo-inverse,
#' which helps for very small/unbalanced subsets and near-boundary estimates.
#' For \code{AR(1)}, observations are ordered by time within subject; \code{NA} time codes
#' break the run, and gaps between factor levels are treated as regular steps
#' (elapsed time is not used).
#'
#' \emph{Heteroscedastic slopes across \eqn{Z} columns are supported.}
#' Each \eqn{Z} column has its own variance component \eqn{\sigma_{Z,j}^2}, but
#' cross-covariances among \eqn{Z} columns are set to zero (diagonal block). Column
#' rescaling changes the implied prior on \eqn{b_{i,\text{extra}}} but does not
#' introduce correlations.
#'
#' @seealso \code{build_L_Dm_Z_cpp}
#' for constructing \eqn{L}/\eqn{D_m}/\eqn{Z}; \code{\link{ccc_pairwise_u_stat}}
#' for a U-statistic alternative; and \pkg{cccrm} for a reference approach via
#' \pkg{nlme}.
#'
#' @references
#' Lin L (1989). A concordance correlation coefficient to evaluate reproducibility.
#' \emph{Biometrics}, 45: 255-268.
#'
#' Lin L (2000). A note on the concordance correlation coefficient.
#' \emph{Biometrics}, 56: 324-325.
#'
#' Carrasco, J. L. et al. (2013). Estimation of the concordance
#' correlation coefficient for repeated measures using SAS and R.
#' \emph{Computer Methods and Programs in Biomedicine}, 109(3), 293–304.
#' \doi{10.1016/j.cmpb.2012.09.002}
#'
#' King et al. (2007). A Class of Repeated Measures Concordance
#' Correlation Coefficients.
#' \emph{Journal of Biopharmaceutical Statistics}, 17(4).
#' \doi{10.1080/10543400701329455}
#'
#' @examples
#' # ====================================================================
#' # 1) Two methods (no time): baseline CCC
#' # ====================================================================
#' set.seed(1)
#' n_subj <- 30
#' meth   <- factor(rep(c("A","B"), each = n_subj))
#' id     <- factor(rep(seq_len(n_subj), times = 2))
#' sigA <- 1.0; sigE <- 0.5
#' u  <- rnorm(n_subj, 0, sqrt(sigA))
#' y  <- c(u + rnorm(n_subj, 0, sqrt(sigE)),
#'          u + 0.2 + rnorm(n_subj, 0, sqrt(sigE)))
#' dat <- data.frame(y, id, method = meth)
#'
#' ccc_rm1 <- ccc_lmm_reml(dat, response = "y", rind = "id", method = "method")
#' print(ccc_rm1)
#' summary(ccc_rm1)
#'
#' # 95% CI container
#' ccc_rm2 <- ccc_lmm_reml(dat, response = "y", rind = "id", method = "method", ci = TRUE)
#' ccc_rm2
#'
#' # ====================================================================
#' # 2) Subject x METHOD variance present, no time
#' #     y_{i,m} = mu + b_m + u_i + w_{i,m} + e_{i,m}
#' #     with u_i ~ N(0, s_A^2), w_{i,m} ~ N(0, s_{AxM}^2)
#' # ====================================================================
#' set.seed(102)
#' n_subj <- 60
#' n_time <- 8
#'
#' id     <- factor(rep(seq_len(n_subj), each = 2 * n_time))
#' time   <- factor(rep(rep(seq_len(n_time), times = 2), times = n_subj))
#' method <- factor(rep(rep(c("A","B"),    each  = n_time), times = n_subj))
#'
#' sigA  <- 0.6   # subject
#' sigAM <- 0.3   # subject × method
#' sigAT <- 0.5   # subject × time
#' sigE  <- 0.4   # residual
#' # Expected estimate S_B = 0.2^2 = 0.04
#' biasB <- 0.2   # fixed method bias
#'
#' # random effects
#' u_i <- rnorm(n_subj, 0, sqrt(sigA))
#' u   <- u_i[as.integer(id)]
#'
#' sm      <- interaction(id, method, drop = TRUE)
#' w_im_lv <- rnorm(nlevels(sm), 0, sqrt(sigAM))
#' w_im    <- w_im_lv[as.integer(sm)]
#'
#' st      <- interaction(id, time, drop = TRUE)
#' g_it_lv <- rnorm(nlevels(st), 0, sqrt(sigAT))
#' g_it    <- g_it_lv[as.integer(st)]
#'
#' # residuals & response
#' e <- rnorm(length(id), 0, sqrt(sigE))
#' y <- (method == "B") * biasB + u + w_im + g_it + e
#'
#' dat_both <- data.frame(y, id, method, time)
#'
#' # Both sigma2_subject_method and sigma2_subject_time are identifiable here
#' fit_both <- ccc_lmm_reml(dat_both, "y", "id", method = "method", time = "time",
#'                          vc_select = "auto", verbose = TRUE)
#' summary(fit_both)
#'
#' # ====================================================================
#' # 3) Subject x TIME variance present (sag > 0) with two methods
#' #     y_{i,m,t} = mu + b_m + u_i + g_{i,t} + e_{i,m,t}
#' #     where g_{i,t} ~ N(0, s_{AxT}^2) shared across methods at time t
#' # ====================================================================
#' set.seed(202)
#' n_subj <- 60; n_time <- 14
#' id     <- factor(rep(seq_len(n_subj), each = 2 * n_time))
#' method <- factor(rep(rep(c("A","B"), each = n_time), times = n_subj))
#' time   <- factor(rep(rep(seq_len(n_time), times = 2), times = n_subj))
#'
#' sigA  <- 0.7
#' sigAT <- 0.5
#' sigE  <- 0.5
#' biasB <- 0.25
#'
#' u   <- rnorm(n_subj, 0, sqrt(sigA))[as.integer(id)]
#' gIT <- rnorm(n_subj * n_time, 0, sqrt(sigAT))
#' g   <- gIT[ (as.integer(id) - 1L) * n_time + as.integer(time) ]
#' y   <- (method == "B") * biasB + u + g + rnorm(length(id), 0, sqrt(sigE))
#' dat_sag <- data.frame(y, id, method, time)
#'
#' # sigma_AT should be retained; sigma_AM may be dropped (since w_{i,m}=0)
#' fit_sag <- ccc_lmm_reml(dat_sag, "y", "id", method = "method", time = "time",
#'                         vc_select = "auto", verbose = TRUE)
#' summary(fit_sag)
#'
#' # ====================================================================
#' # 4) BOTH components present: sab > 0 and sag > 0 (2 methods x T times)
#' #     y_{i,m,t} = mu + b_m + u_i + w_{i,m} + g_{i,t} + e_{i,m,t}
#' # ====================================================================
#' set.seed(303)
#' n_subj <- 60; n_time <- 4
#' id     <- factor(rep(seq_len(n_subj), each = 2 * n_time))
#' method <- factor(rep(rep(c("A","B"), each = n_time), times = n_subj))
#' time   <- factor(rep(rep(seq_len(n_time), times = 2), times = n_subj))
#'
#' sigA  <- 0.8
#' sigAM <- 0.3
#' sigAT <- 0.4
#' sigE  <- 0.5
#' biasB <- 0.2
#'
#' u   <- rnorm(n_subj, 0, sqrt(sigA))[as.integer(id)]
#' # (subject, method) random deviations: repeat per (i,m) across its times
#' wIM <- rnorm(n_subj * 2, 0, sqrt(sigAM))
#' w   <- wIM[ (as.integer(id) - 1L) * 2 + as.integer(method) ]
#' # (subject, time) random deviations: shared across methods at time t
#' gIT <- rnorm(n_subj * n_time, 0, sqrt(sigAT))
#' g   <- gIT[ (as.integer(id) - 1L) * n_time + as.integer(time) ]
#' y   <- (method == "B") * biasB + u + w + g + rnorm(length(id), 0, sqrt(sigE))
#' dat_both <- data.frame(y, id, method, time)
#'
#' fit_both <- ccc_lmm_reml(dat_both, "y", "id", method = "method", time = "time",
#'                          vc_select = "auto", verbose = TRUE, ci = TRUE)
#' summary(fit_both)
#'
#' # If you want to force-include both VCs (skip testing):
#' fit_both_forced <-
#'  ccc_lmm_reml(dat_both, "y", "id", method = "method", time = "time",
#'               vc_select = "none", include_subj_method  = TRUE,
#'               include_subj_time  = TRUE, verbose = TRUE)
#' summary(fit_both_forced)
#'
#' # ====================================================================
#' # 5) D_m choices: time-averaged (default) vs typical visit
#' # ====================================================================
#' # Time-average
#' ccc_lmm_reml(dat_both, "y", "id", method = "method", time = "time",
#'              vc_select = "none", include_subj_method  = TRUE,
#'              include_subj_time  = TRUE, Dmat_type = "time-avg")
#'
#' # Typical visit
#' ccc_lmm_reml(dat_both, "y", "id", method = "method", time = "time",
#'              vc_select = "none", include_subj_method  = TRUE,
#'              include_subj_time  = TRUE, Dmat_type = "typical-visit")
#'
#' # ====================================================================
#' # 6) AR(1) residual correlation with fixed rho (larger example)
#' # ====================================================================
#' \donttest{
#' set.seed(10)
#' n_subj   <- 40
#' n_time   <- 10
#' methods  <- c("A", "B", "C", "D")
#' nm       <- length(methods)
#' id     <- factor(rep(seq_len(n_subj), each = n_time * nm))
#' method <- factor(rep(rep(methods, each = n_time), times = n_subj),
#'                  levels = methods)
#' time   <- factor(rep(rep(seq_len(n_time), times = nm), times = n_subj))
#'
#' beta0    <- 0
#' beta_t   <- 0.2
#' bias_met <- c(A = 0.00, B = 0.30, C = -0.15, D = 0.05)
#' sigA     <- 1.0
#' rho_true <- 0.6
#' sigE     <- 0.7
#'
#' t_num <- as.integer(time)
#' t_c   <- t_num - mean(seq_len(n_time))
#' mu    <- beta0 + beta_t * t_c + bias_met[as.character(method)]
#'
#' u_subj <- rnorm(n_subj, 0, sqrt(sigA))
#' u      <- u_subj[as.integer(id)]
#'
#' e <- numeric(length(id))
#' for (s in seq_len(n_subj)) {
#'   for (m in methods) {
#'     idx <- which(id == levels(id)[s] & method == m)
#'     e[idx] <- stats::arima.sim(list(ar = rho_true), n = n_time, sd = sigE)
#'   }
#' }
#' y <- mu + u + e
#' dat_ar4 <- data.frame(y = y, id = id, method = method, time = time)
#'
#' ccc_lmm_reml(dat_ar4,
#'              response = "y", rind = "id", method = "method", time = "time",
#'              ar = "ar1", ar_rho = 0.6, verbose = TRUE)
#' }
#'
#' # ====================================================================
#' # 7) Random slope variants (subject, method, custom Z)
#' # ====================================================================
#' \donttest{
#' ## By SUBJECT
#' set.seed(2)
#' n_subj <- 60; n_time <- 4
#' id  <- factor(rep(seq_len(n_subj), each = 2 * n_time))
#' tim <- factor(rep(rep(seq_len(n_time), times = 2), times = n_subj))
#' method <- factor(rep(rep(c("A","B"), each = n_time), times = n_subj))
#' subj <- as.integer(id)
#' slope_i <- rnorm(n_subj, 0, 0.15)
#' slope_vec <- slope_i[subj]
#' base <- rnorm(n_subj, 0, 1.0)[subj]
#' tnum <- as.integer(tim)
#' y <- base + 0.3*(method=="B") + slope_vec*(tnum - mean(seq_len(n_time))) +
#'      rnorm(length(id), 0, 0.5)
#' dat_s <- data.frame(y, id, method, time = tim)
#' dat_s$t_num <- as.integer(dat_s$time)
#' dat_s$t_c   <- ave(dat_s$t_num, dat_s$id, FUN = function(v) v - mean(v))
#' ccc_lmm_reml(dat_s, "y", "id", method = "method", time = "time",
#'              slope = "subject", slope_var = "t_c", verbose = TRUE)
#'
#' ## By METHOD
#' set.seed(3)
#' n_subj <- 60; n_time <- 4
#' id  <- factor(rep(seq_len(n_subj), each = 2 * n_time))
#' tim <- factor(rep(rep(seq_len(n_time), times = 2), times = n_subj))
#' method <- factor(rep(rep(c("A","B"), each = n_time), times = n_subj))
#' slope_m <- ifelse(method=="B", 0.25, 0.00)
#' base <- rnorm(n_subj, 0, 1.0)[as.integer(id)]
#' tnum <- as.integer(tim)
#' y <- base + 0.3*(method=="B") + slope_m*(tnum - mean(seq_len(n_time))) +
#'      rnorm(length(id), 0, 0.5)
#' dat_m <- data.frame(y, id, method, time = tim)
#' dat_m$t_num <- as.integer(dat_m$time)
#' dat_m$t_c   <- ave(dat_m$t_num, dat_m$id, FUN = function(v) v - mean(v))
#' ccc_lmm_reml(dat_m, "y", "id", method = "method", time = "time",
#'              slope = "method", slope_var = "t_c", verbose = TRUE)
#'
#' ## SUBJECT + METHOD random slopes (custom Z)
#' set.seed(4)
#' n_subj <- 50; n_time <- 4
#' id  <- factor(rep(seq_len(n_subj), each = 2 * n_time))
#' tim <- factor(rep(rep(seq_len(n_time), times = 2), times = n_subj))
#' method <- factor(rep(rep(c("A","B"), each = n_time), times = n_subj))
#' subj <- as.integer(id)
#' slope_subj <- rnorm(n_subj, 0, 0.12)[subj]
#' slope_B    <- ifelse(method=="B", 0.18, 0.00)
#' tnum <- as.integer(tim)
#' base <- rnorm(n_subj, 0, 1.0)[subj]
#' y <- base + 0.3*(method=="B") +
#'      (slope_subj + slope_B) * (tnum - mean(seq_len(n_time))) +
#'      rnorm(length(id), 0, 0.5)
#' dat_bothRS <- data.frame(y, id, method, time = tim)
#' dat_bothRS$t_num <- as.integer(dat_bothRS$time)
#' dat_bothRS$t_c   <- ave(dat_bothRS$t_num, dat_bothRS$id, FUN = function(v) v - mean(v))
#' MM <- model.matrix(~ 0 + method, data = dat_bothRS)
#' Z_custom <- cbind(
#'   subj_slope = dat_bothRS$t_c,
#'   MM * dat_bothRS$t_c
#' )
#' ccc_lmm_reml(dat_bothRS, "y", "id", method = "method", time = "time",
#'              slope = "custom", slope_Z = Z_custom, verbose = TRUE)
#' }
#'
#' @author Thiago de Paula Oliveira
#' @importFrom stats as.formula model.matrix setNames qnorm optimize
#' @export
ccc_lmm_reml <- function(data, response, rind,
                         method = NULL, time = NULL, interaction = FALSE,
                         max_iter = 100, tol = 1e-6,
                         Dmat = NULL,
                         Dmat_type = c("time-avg","typical-visit","weighted-avg","weighted-sq"),
                         Dmat_weights = NULL,
                         Dmat_rescale = TRUE,
                         ci = FALSE, conf_level = 0.95,
                         ci_mode = c("auto","raw","logit"),
                         verbose = FALSE, digits = 4, use_message = TRUE,
                         ar = c("none", "ar1"),
                         ar_rho = NA_real_,
                         slope = c("none", "subject", "method", "custom"),
                         slope_var = NULL,
                         slope_Z = NULL,
                         drop_zero_cols = TRUE,
                         vc_select = c("auto","none"),
                         vc_alpha = 0.05,
                         vc_test_order = c("subj_time","subj_method"),
                         include_subj_method = NULL,
                         include_subj_time = NULL,
                         sb_zero_tol = 1e-10) {

  ar         <- match.arg(ar)
  slope      <- match.arg(slope)
  Dmat_type  <- match.arg(Dmat_type)
  vc_select    <- match.arg(vc_select)
  vc_test_order<- match.arg(vc_test_order, several.ok = TRUE)
  ci_mode <- match.arg(ci_mode)
  ci_mode_int <- switch(ci_mode, raw = 0L, logit = 1L, auto = 2L)

  if (identical(ar, "ar1")) {
    if (length(ar_rho) != 1L) stop("ar_rho must be length 1 (or NA to estimate).")
    if (!is.na(ar_rho) && abs(ar_rho) >= 0.999) stop("ar_rho must be in (-0.999, 0.999).")
  }

  df <- as.data.frame(data)
  df[[response]]   <- as.numeric(df[[response]])
  df[[rind]] <- factor(df[[rind]])
  if (!is.null(method))  df[[method]]  <- factor(df[[method]])
  if (!is.null(time)) df[[time]] <- factor(df[[time]])
  all_time_lvls <- if (!is.null(time)) levels(df[[time]]) else character(0)

  rhs <- "1"
  if (!is.null(method))  rhs <- paste(rhs, "+", method)
  if (!is.null(time)) rhs <- paste(rhs, "+", time)
  if (!is.null(method) && !is.null(time) && interaction) rhs <- paste(rhs, "+", paste0(method, ":", time))
  fml <- as.formula(paste("~", rhs))

  extra_label <- switch(slope,
                        "subject" = "random slope (subject)",
                        "method"  = "random slope (by method)",
                        "custom"  = "custom random effect",
                        NULL)

  if (is.null(method) || nlevels(df[[method]]) < 2L) {
    stop("At least two method levels are required to compute pairwise CCC. ",
         "The 'overall' CCC has been removed. ",
         "Supply `method` with >= 2 levels.", call. = FALSE)
  }

  # Only pairwise path remains
  return(
    ccc_lmm_reml_pairwise(
      df                = df,
      fml               = fml,
      response          = response,
      rind              = rind,
      method            = method,
      time              = time,
      slope             = slope,
      slope_var         = slope_var,
      slope_Z           = slope_Z,
      drop_zero_cols    = drop_zero_cols,
      Dmat              = Dmat,
      ar                = ar,
      ar_rho            = ar_rho,
      max_iter          = max_iter,
      tol               = tol,
      conf_level        = conf_level,
      verbose           = verbose,
      digits            = digits,
      use_message       = use_message,
      extra_label       = extra_label,
      ci                = ci,
      ci_mode_int       = ci_mode_int,
      all_time_lvls     = all_time_lvls,
      Dmat_type         = Dmat_type,
      Dmat_weights      = Dmat_weights,
      Dmat_rescale      = Dmat_rescale,
      vc_select         = vc_select,
      vc_alpha          = vc_alpha,
      vc_test_order     = vc_test_order,
      include_subj_method = include_subj_method,
      include_subj_time   = include_subj_time,
      sb_zero_tol       = sb_zero_tol
    )
  )
}

#' @keywords internal
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' @title num_or_na
#' @description Helper to safely coerce a value to numeric or return NA if invalid.
#' @keywords internal
num_or_na <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1 || !is.finite(x)) NA_real_ else x
}

#' @title compute_ci_from_se
#' @description Compute confidence intervals from point estimate and standard error.
#' @keywords internal
compute_ci_from_se <- function(ccc, se, level) {
  if (!is.finite(ccc) || !is.finite(se)) return(c(NA_real_, NA_real_))
  z <- qnorm(1 - (1 - level)/2)
  c(max(0, min(1, ccc - z * se)), max(0, min(1, ccc + z * se)))
}

#' @keywords internal
#' @importFrom stats plogis qlogis
compute_ci_logit_from_se <- function(ccc, se, level) {
  if (!is.finite(ccc) || !is.finite(se) || ccc <= 0 || ccc >= 1) return(c(NA_real_, NA_real_))
  z <- qnorm(1 - (1 - level)/2)
  mu <- qlogis(ccc)
  se_logit <- se / (ccc * (1 - ccc))  # delta
  l <- plogis(mu - z * se_logit)
  u <- plogis(mu + z * se_logit)
  c(max(0, min(1, l)), max(0, min(1, u)))
}

#' @keywords internal
infer_ci_method <- function(ans, ci_mode_int, conf_level, tie_tol = 1e-8) {
  if (is.null(ans)) return(ans)

  # Respect explicit fields if backend already set them
  if (!is.null(ans$ci_method) || !is.null(ans$ci_mode_code)) {
    if (is.null(ans$conf_level)) ans$conf_level <- conf_level
    return(ans)
  }

  method <- ci_mode_name(ci_mode_int)   # "wald-raw" / "wald-logit" / "auto"
  code   <- ci_mode_int                 # 0 / 1 / 2

  if (ci_mode_int == 2L) {  # auto
    ccc <- num_or_na(ans$ccc)
    se  <- num_or_na(ans$se_ccc)
    lwr <- num_or_na(ans$lwr)
    upr <- num_or_na(ans$upr)

    if (is.finite(ccc) && is.finite(se)) {
      if (is.finite(lwr) && is.finite(upr)) {
        raw_ci   <- compute_ci_from_se(ccc, se, conf_level)
        logit_ci <- compute_ci_logit_from_se(ccc, se, conf_level)

        d_raw   <- sum(abs(raw_ci   - c(lwr, upr)))
        d_logit <- sum(abs(logit_ci - c(lwr, upr)))

        if (is.finite(d_raw) && is.finite(d_logit)) {
          if (abs(d_raw - d_logit) <= tie_tol) {
            # near tie: prefer logit near boundaries, else raw
            prefer_logit <- (ccc < 0.1 || ccc > 0.9)
            if (prefer_logit) { method <- "wald-logit"; code <- 1L }
            else              { method <- "wald-raw";   code <- 0L }
          } else if (d_logit < d_raw) {
            method <- "wald-logit"; code <- 1L
          } else {
            method <- "wald-raw";   code <- 0L
          }
        }
      } else {
        # R will build CI later with compute_ci_from_se -> raw
        method <- "wald-raw"; code <- 0L
      }
    }
  }

  ans$ci_method    <- method
  ans$ci_mode_code <- code
  if (is.null(ans$conf_level)) ans$conf_level <- conf_level
  ans
}

#' @title num_or_na_vec
#' @description Display variance component estimation details to the console.
#' @keywords internal
num_or_na_vec <- function(x) {
  if (is.null(x)) return(NULL)
  x <- suppressWarnings(as.numeric(x))
  if (!length(x)) return(NULL)
  x[!is.finite(x)] <- NA_real_
  x
}


#' @title .vc_message
#' @description Display variance component estimation details to the console.
#' @keywords internal
.vc_message <- function(ans, label, nm, nt, conf_level,
                        digits = 4, use_message = TRUE,
                        extra_label = NULL,
                        ar = c("none", "ar1"),
                        ar_rho = NA_real_) {
  ar <- match.arg(ar)

  fmt <- function(x) {
    if (is.null(x)) return("NA")
    x <- suppressWarnings(as.numeric(x))
    if (length(x) == 0 || all(!is.finite(x))) return("NA")
    if (length(x) == 1) {
      return(formatC(x, format = "f", digits = digits))
    }
    # vector: pretty-print as a comma-separated list
    paste0("[", paste(formatC(x, format = "f", digits = digits), collapse = ", "), "]")
  }

  colw <- 38L
  v <- function(s, x) sprintf("  %-*s : %s", colw, s, fmt(x))

  out <- c(
    sprintf("---- matrixCorr::ccc_lmm_reml - variance-components (%s) ----", label),
    sprintf("Design: methods nm = %d, times nt = %d", nm, nt)
  )
  if (identical(ar, "ar1")) {
    info <- if (is.na(ar_rho)) "AR(1) (rho estimated)" else sprintf("AR(1) with rho = %s", fmt(ar_rho))
    out <- c(out, paste("Residual correlation:", info))
  } else {
    out <- c(out, "Residual correlation: independent (iid)")
  }

  out <- c(out,
           "Estimates:",
           v("sigma_A^2 (subject)",             ans[["sigma2_subject"]]),
           v("sigma_A_M^2 (subject x method)",  ans[["sigma2_subject_method"]]),
           v("sigma_A_T^2 (subject x time)",    ans[["sigma2_subject_time"]]),
           v("sigma_E^2 (error)",               ans[["sigma2_error"]]))

  if (!is.null(ans[["sigma2_extra"]])) {
    lab <- if (is.null(extra_label)) "extra random effect" else extra_label
    out <- c(out, v(sprintf("sigma_Z^2 (%s)", lab), ans[["sigma2_extra"]]))
  }

  out <- c(out,
           v("S_B (fixed-effect dispersion)",   ans[["SB"]]),
           v("SE(CCC)",                         ans[["se_ccc"]]))

  has_bounds <- is.finite(num_or_na(ans$lwr)) && is.finite(num_or_na(ans$upr))
  if (!is.null(ans$ci_method) || has_bounds) {
    cm <- ans$ci_method %||% ci_mode_name(ans$ci_mode_code %||% NA_integer_)
    cl <- ans$conf_level %||% conf_level
    out <- c(out, sprintf("CI: %s (conf_level = %s)", cm, fmt(cl)))
  }
  out <- c(out,
           "--------------------------------------------------------------------------")

  if (use_message) message(paste(out, collapse = "\n")) else cat(paste(out, collapse = "\n"), "\n")
}

#' @title build_LDZ
#' @description Internal helper to construct L, Dm, and Z matrices for random effects.
#' @keywords internal
build_LDZ <- function(colnames_X, method_levels, time_levels, Dsub, df_sub,
                      method_name, time_name, slope, interaction, slope_var,
                      drop_zero_cols) {
  slope_mode_cpp <- switch(slope, none = "none", subject = "subject", method = "method", custom = "none")
  if (!identical(slope, "custom")) {
    build_L_Dm_Z_cpp(
      colnames_X      = colnames_X,
      rmet_name       = if (is.null(method_name)) NULL else method_name,
      rtime_name      = if (is.null(time_name)) NULL else time_name,
      method_levels   = if (is.null(method_name)) character(0) else method_levels,
      time_levels     = if (is.null(time_name)) character(0) else time_levels,
      has_interaction = interaction,
      Dmat_global     = Dsub,
      slope_mode      = slope_mode_cpp,
      slope_var       = if (!is.null(slope_var)) df_sub[[slope_var]] else NULL,
      method_codes    = if (!is.null(method_name)) as.integer(df_sub[[method_name]]) else NULL,
      drop_zero_cols  = drop_zero_cols
    )
  } else {
    build_L_Dm_cpp(
      colnames_X      = colnames_X,
      rmet_name       = if (is.null(method_name)) NULL else method_name,
      rtime_name      = if (is.null(time_name)) NULL else time_name,
      method_levels   = if (is.null(method_name)) character(0) else method_levels,
      time_levels     = if (is.null(time_name)) character(0) else time_levels,
      has_interaction = interaction,
      Dmat_global     = Dsub
    )
  }
}

#' @title run_cpp
#' @description Wrapper for calling 'C++' backend for CCC estimation.
#' @keywords internal
run_cpp <- function(Xr, yr, subject, method_int, time_int, Laux, Z,
                    use_ar1, ar1_rho, max_iter, tol, conf_level, ci_mode_int,
                    include_subj_method = TRUE, include_subj_time = TRUE,
                    sb_zero_tol = 1e-10, eval_single_visit = FALSE,
                    time_weights = NULL) {

  # Guard: you cannot include a random component whose dimension is 0
  include_subj_method <- isTRUE(include_subj_method) && isTRUE(Laux$nm > 0)
  include_subj_time   <- isTRUE(include_subj_time)   && isTRUE(Laux$nt > 0)

  ccc_vc_cpp(
    Xr = unname(Xr),
    yr = yr,
    subject = subject,
    method  = method_int,
    time    = time_int,
    nm = Laux$nm, nt = Laux$nt,
    max_iter = max_iter, tol = tol,
    conf_level = conf_level,
    ci_mode = ci_mode_int,
    Lr    = if (is.null(Laux$L))  NULL else unname(Laux$L),
    auxDr = if (is.null(Laux$Dm)) NULL else unname(Laux$Dm),
    Zr    = if (is.null(Z))       NULL else unname(Z),
    use_ar1 = use_ar1,
    ar1_rho = as.numeric(ar1_rho),
    include_subj_method = include_subj_method,
    include_subj_time   = include_subj_time,
    sb_zero_tol = as.numeric(sb_zero_tol),
    eval_single_visit = eval_single_visit,
    time_weights = if (is.null(time_weights)) NULL else as.numeric(time_weights)
  )
}

#' @keywords internal
ci_mode_name <- function(code) {
  switch(as.character(code),
         "0" = "wald-raw",
         "1" = "wald-logit",
         "2" = "auto",     # chosen per-estimate in C++
         "unknown")
}

#' @title estimate_rho
#' @description Estimate AR(1) correlation parameter rho by optimizing REML log-likelihood.
#' @keywords internal
estimate_rho <- function(Xr, yr, subject, method_int, time_int, Laux, Z,
                         rho_lo = -0.95, rho_hi = 0.95,
                         max_iter = 100, tol = 1e-6, conf_level = 0.95,
                         ci_mode_int,
                         include_subj_method = TRUE, include_subj_time = TRUE,
                         sb_zero_tol = 1e-10, eval_single_visit = FALSE,
                         time_weights = NULL) {  # <-- NEW
  obj <- function(r) {
    fit <- run_cpp(Xr, yr, subject, method_int, time_int, Laux, Z,
                   use_ar1 = TRUE, ar1_rho = r,
                   max_iter = max_iter, tol = tol, conf_level = conf_level,
                   ci_mode_int = ci_mode_int,
                   include_subj_method = include_subj_method,
                   include_subj_time   = include_subj_time,
                   sb_zero_tol = sb_zero_tol,
                   eval_single_visit = eval_single_visit,
                   time_weights = time_weights)
    ll <- suppressWarnings(as.numeric(fit[["reml_loglik"]]))
    if (!is.finite(ll)) return(Inf)
    -ll
  }
  oo <- optimize(obj, interval = c(rho_lo, rho_hi))
  list(rho = unname(oo$minimum), used_reml = TRUE)
}

#' @keywords internal
#' @importFrom stats pchisq
p_half_chisq1 <- function(lrt) 0.5 * pchisq(lrt, df = 1, lower.tail = FALSE)

#' @keywords internal
reml_lrt_select <- function(Xr, yr, subject, method_int, time_int, Laux, Z,
                            ar = c("none","ar1"), ar_rho = NA_real_,
                            max_iter = 100, tol = 1e-6, conf_level = 0.95,
                            ci_mode_int,
                            alpha = 0.05, test_order = c("subj_time","subj_method"),
                            sb_zero_tol = 1e-10,
                            eval_single_visit = FALSE,
                            time_weights = NULL) {
  ar <- match.arg(ar)
  can_subj_method <- isTRUE(Laux$nm > 0)
  can_subj_time   <- isTRUE(Laux$nt > 0)

  inc_subj_method <- can_subj_method
  inc_subj_time   <- can_subj_time

  # Function to get rho for a given (inc_subj_method, inc_subj_time)
  get_rho <- function(inc_subj_method, inc_subj_time) {
    if (!identical(ar, "ar1") || !is.na(ar_rho)) return(ar_rho)
    er <- estimate_rho(Xr, yr, subject, method_int, time_int, Laux, Z,
                       max_iter = max_iter, tol = tol, conf_level = conf_level,
                       ci_mode_int = ci_mode_int,
                       include_subj_method = inc_subj_method,
                       include_subj_time   = inc_subj_time,
                       sb_zero_tol = sb_zero_tol,
                       eval_single_visit = eval_single_visit,
                       time_weights = time_weights)  # <-- NEW
    er$rho
  }

  # Fit full (with rho profiled if needed)
  rho_full <- get_rho(inc_subj_method, inc_subj_time)
  fit_full <- run_cpp(Xr, yr, subject, method_int, time_int, Laux, Z,
                      use_ar1 = identical(ar, "ar1"),
                      ar1_rho = if (identical(ar, "ar1")) rho_full else 0,
                      max_iter = max_iter, tol = tol, conf_level = conf_level,
                      ci_mode_int = ci_mode_int,
                      include_subj_method = inc_subj_method,
                      include_subj_time   = inc_subj_time,
                      sb_zero_tol = sb_zero_tol,
                      eval_single_visit = eval_single_visit,
                      time_weights = time_weights)   # <-- NEW

  for (what in test_order) {
    if (what == "subj_time" && inc_subj_time) {
      rho0 <- get_rho(inc_subj_method, FALSE)
      fit0 <- run_cpp(Xr, yr, subject, method_int, time_int, Laux, Z,
                      use_ar1 = identical(ar, "ar1"),
                      ar1_rho = if (identical(ar, "ar1")) rho0 else 0,
                      max_iter = max_iter, tol = tol, conf_level = conf_level,
                      ci_mode_int = ci_mode_int,
                      include_subj_method = inc_subj_method, include_subj_time = FALSE,
                      sb_zero_tol = sb_zero_tol,
                      eval_single_visit = eval_single_visit,
                      time_weights = time_weights)   # <-- NEW
      lrt <- 2 * (as.numeric(fit_full$reml_loglik) - as.numeric(fit0$reml_loglik))
      p   <- p_half_chisq1(max(lrt, 0))
      if (is.finite(p) && p > alpha) {
        inc_subj_time <- FALSE
        fit_full <- fit0
        rho_full <- rho0
      }
    }
    if (what == "subj_method" && inc_subj_method) {
      rho0 <- get_rho(FALSE, inc_subj_time)
      fit0 <- run_cpp(Xr, yr, subject, method_int, time_int, Laux, Z,
                      use_ar1 = identical(ar, "ar1"),
                      ar1_rho = if (identical(ar, "ar1")) rho0 else 0,
                      max_iter = max_iter, tol = tol, conf_level = conf_level,
                      ci_mode_int = ci_mode_int,
                      include_subj_method = FALSE, include_subj_time = inc_subj_time,
                      sb_zero_tol = sb_zero_tol,
                      eval_single_visit = eval_single_visit,
                      time_weights = time_weights)   # <-- NEW
      lrt <- 2 * (as.numeric(fit_full$reml_loglik) - as.numeric(fit0$reml_loglik))
      p   <- p_half_chisq1(max(lrt, 0))
      if (is.finite(p) && p > alpha) {
        inc_subj_method <- FALSE
        fit_full <- fit0
        rho_full <- rho0
      }
    }
  }

  list(include_subj_method = inc_subj_method,
       include_subj_time   = inc_subj_time,
       rho = rho_full,
       fit = fit_full)
}

#' @title ccc_lmm_reml_pairwise
#' @description Internal function to handle pairwise CCC estimation for each method pair.
#' @keywords internal
ccc_lmm_reml_pairwise <- function(df, fml, response, rind, method, time,
                                  slope, slope_var, slope_Z, drop_zero_cols,
                                  Dmat, ar, ar_rho, max_iter, tol,
                                  conf_level, verbose, digits, use_message,
                                  extra_label, ci, ci_mode_int, all_time_lvls,
                                  Dmat_type = c("time-avg","typical-visit","weighted-avg","weighted-sq"),
                                  Dmat_weights = NULL,
                                  Dmat_rescale = TRUE,
                                  vc_select = c("auto","none"),
                                  vc_alpha = 0.05,
                                  vc_test_order = c("subj_time","subj_method"),
                                  include_subj_method = NULL,
                                  include_subj_time = NULL,
                                  sb_zero_tol = 1e-10) {

  Dmat_type <- match.arg(Dmat_type)
  eval_single_visit <- Dmat_type %in% c("typical-visit","weighted-sq")
  vc_select <- match.arg(vc_select)
  vc_test_order <- match.arg(vc_test_order, several.ok = TRUE)

  df[[method]] <- droplevels(df[[method]])
  method_levels <- levels(df[[method]])
  Lm <- length(method_levels)

  est_mat <- matrix(1,  Lm, Lm, dimnames = list(method_levels, method_levels))
  if (isTRUE(ci)) {
    lwr_mat <- matrix(NA_real_, Lm, Lm, dimnames = list(method_levels, method_levels))
    upr_mat <- matrix(NA_real_, Lm, Lm, dimnames = list(method_levels, method_levels))
  }

  # store rho per pair if estimated
  rho_mat <- matrix(NA_real_, Lm, Lm, dimnames = list(method_levels, method_levels))

  # variance component containers (per pair)
  vc_subject        <- matrix(NA_real_, Lm, Lm, dimnames = list(method_levels, method_levels))
  vc_subject_method <- matrix(NA_real_, Lm, Lm, dimnames = list(method_levels, method_levels))
  vc_subject_time   <- matrix(NA_real_, Lm, Lm, dimnames = list(method_levels, method_levels))
  vc_error          <- matrix(NA_real_, Lm, Lm, dimnames = list(method_levels, method_levels))
  vc_extra <- matrix(vector("list", Lm * Lm), Lm, Lm,
                     dimnames = list(method_levels, method_levels))
  vc_SB             <- matrix(NA_real_, Lm, Lm, dimnames = list(method_levels, method_levels))
  vc_se_ccc         <- matrix(NA_real_, Lm, Lm, dimnames = list(method_levels, method_levels))

  # AR1 diagnostics
  ar1_rho_lag1_mat <- matrix(NA_real_,    Lm, Lm, dimnames = list(method_levels, method_levels))
  ar1_pairs_mat   <- matrix(NA_integer_, Lm, Lm, dimnames = list(method_levels, method_levels))
  ar1_pval_mat    <- matrix(NA_real_,    Lm, Lm, dimnames = list(method_levels, method_levels))
  ar1_reco_mat    <- matrix(NA,          Lm, Lm, dimnames = list(method_levels, method_levels))

  for (i in 1:(Lm - 1L)) {
    for (j in (i + 1L):Lm) {
      m1 <- method_levels[i]; m2 <- method_levels[j]

      idx <- which(df[[method]] %in% c(m1, m2))
      subj_int   <- as.integer(df[[rind]][idx])
      y_sub      <- df[[response]][idx]
      met_fac    <- droplevels(df[[method]][idx])        # exactly 2 levels
      time_fac   <- if (!is.null(time)) droplevels(df[[time]][idx]) else NULL

      Xp <- model.matrix(fml, data = df[idx, , drop = FALSE])

      # Present time levels in this pair
      lev_time_sub <- if (!is.null(time_fac)) levels(time_fac) else character(0)

      # -------- Build/subset Dmat for this pair (only if ≥ 2 time levels) --------
      if (!is.null(time) && length(lev_time_sub) >= 2L) {
        if (!is.null(Dmat)) {
          # Subset the user-supplied Dmat to the present time levels
          Dfull <- as.matrix(Dmat)
          if (!is.null(all_time_lvls) &&
              nrow(Dfull) == length(all_time_lvls) && ncol(Dfull) == length(all_time_lvls)) {
            pos  <- match(lev_time_sub, all_time_lvls)
            Dsub <- Dfull[pos, pos, drop = FALSE]
          } else if (nrow(Dfull) == length(lev_time_sub) && ncol(Dfull) == length(lev_time_sub)) {
            # already in the pair's time order
            Dsub <- Dfull
          } else {
            stop("Dmat has incompatible dimension for present time levels in a pairwise fit.")
          }
          if (isTRUE(Dmat_rescale))
            Dsub <- .Dmat_normalise_mass(Dsub, length(lev_time_sub))
          # soft symmetrisation for safety
          Dsub <- 0.5 * (Dsub + t(Dsub))
        } else {
          # Construct from type/weights; align (named) weights from global to present levels
          w_sub <- .align_weights_to_levels(Dmat_weights, lev_time_sub, all_time_lvls)
          Dsub  <- .Dmat_build_kernel(length(lev_time_sub),
                                      type    = Dmat_type,
                                      w       = w_sub,
                                      rescale = Dmat_rescale)
        }
      } else {
        Dsub <- NULL
      }

      # --- NEW: per-pair time weights for kappa (only for weighted-avg target) ---
      time_weights_kappa <- NULL
      if (!is.null(time) && length(lev_time_sub) >= 2L && identical(Dmat_type, "weighted-avg")) {
        w_sub <- .align_weights_to_levels(Dmat_weights, lev_time_sub, all_time_lvls)
        if (is.null(w_sub)) w_sub <- rep(1 / length(lev_time_sub), length(lev_time_sub))
        sw <- sum(w_sub, na.rm = TRUE)
        if (!is.finite(sw) || sw <= 0) stop("Dmat_weights must sum to a positive finite number.")
        time_weights_kappa <- as.numeric(w_sub / sw)
      }

      # infer "has_interaction" from model matrix columns for this pair
      has_interaction <- any(grepl(":", colnames(Xp), fixed = TRUE))

      df_sub <- df[idx, , drop = FALSE]
      Laux <- build_LDZ(
        colnames_X      = colnames(Xp),
        method_levels   = levels(met_fac),
        time_levels     = lev_time_sub,
        Dsub            = Dsub,
        df_sub          = df_sub,
        method_name     = method,
        time_name       = time,
        slope           = slope,
        interaction     = has_interaction,
        slope_var       = slope_var,
        drop_zero_cols  = drop_zero_cols
      )

      method_int <- if (nlevels(met_fac)  >= 2L) as.integer(met_fac)  else integer(0)
      time_int   <- if (!is.null(time_fac) && nlevels(time_fac) >= 2L) as.integer(time_fac) else integer(0)
      Zp <- if (!identical(slope, "custom")) Laux$Z else {
        if (is.null(slope_Z)) NULL else as.matrix(slope_Z)[idx, , drop = FALSE]
      }

      ## Decide inclusions for this pair
      inc_pair <- if (identical(vc_select, "none")) {
        list(
          subj_method = if (!is.null(include_subj_method)) isTRUE(include_subj_method) else Laux$nm > 0,
          subj_time   = if (!is.null(include_subj_time))   isTRUE(include_subj_time)   else Laux$nt > 0
        )
      } else NULL

      if (is.null(inc_pair) && (Laux$nm > 0 || Laux$nt > 0) && identical(vc_select, "auto")) {
        # --- AUTO: boundary-aware REML LRTs with rho profiled consistently ---
        sel <- reml_lrt_select(
          Xp, y_sub, subj_int, method_int, time_int, Laux, Zp,
          ar = ar, ar_rho = ar_rho,
          max_iter = max_iter, tol = tol, conf_level = conf_level,
          ci_mode_int = ci_mode_int,
          alpha = vc_alpha, test_order = vc_test_order,
          sb_zero_tol = sb_zero_tol,
          eval_single_visit = eval_single_visit,
          time_weights = time_weights_kappa     # <-- NEW
        )
        ans <- sel$fit
        inc_subj_method_eff <- sel$include_subj_method
        inc_subj_time_eff   <- sel$include_subj_time
        rho_used            <- sel$rho
        ans <- infer_ci_method(ans, ci_mode_int, conf_level)
      } else {
        inc_subj_method_eff <- if (is.null(inc_pair)) (Laux$nm > 0) else inc_pair$subj_method
        inc_subj_time_eff   <- if (is.null(inc_pair)) (Laux$nt > 0) else inc_pair$subj_time

        rho_used <- if (identical(ar, "ar1") && is.na(ar_rho)) {
          er <- estimate_rho(
            Xp, y_sub, subj_int, method_int, time_int, Laux, Zp,
            max_iter = max_iter, tol = tol, conf_level = conf_level,
            ci_mode_int = ci_mode_int,
            include_subj_method = inc_subj_method_eff,
            include_subj_time   = inc_subj_time_eff,
            sb_zero_tol = sb_zero_tol,
            eval_single_visit = eval_single_visit,
            time_weights = time_weights_kappa    # <-- NEW
          )
          er$rho
        } else ar_rho

        ans <- tryCatch(
          run_cpp(
            Xp, y_sub, subj_int, method_int, time_int, Laux, Zp,
            use_ar1 = identical(ar, "ar1"),
            ar1_rho = if (identical(ar, "ar1")) rho_used else 0,
            max_iter = max_iter, tol = tol, conf_level = conf_level,
            ci_mode_int = ci_mode_int,
            include_subj_method = inc_subj_method_eff,
            include_subj_time   = inc_subj_time_eff,
            sb_zero_tol = sb_zero_tol,
            eval_single_visit = eval_single_visit,
            time_weights = time_weights_kappa      # <-- NEW
          ),
          error = function(e) {
            warning(sprintf("ccc_vc_cpp failed for pair (%s, %s): %s", m1, m2, conditionMessage(e)))
            NULL
          }
        )
        ans <- infer_ci_method(ans, ci_mode_int, conf_level)
      }

      # ---- record results (common to both branches) ----
      rho_mat[i, j] <- rho_mat[j, i] <- as.numeric(rho_used)

      val <- if (is.null(ans)) NA_real_ else unname(ans$ccc)
      est_mat[i, j] <- est_mat[j, i] <- val

      if (!is.null(ans)) {
        vc_subject[i, j]        <- vc_subject[j, i]        <- num_or_na(ans[["sigma2_subject"]])
        vc_subject_method[i, j] <- vc_subject_method[j, i] <- num_or_na(ans[["sigma2_subject_method"]])
        vc_subject_time[i, j]   <- vc_subject_time[j, i]   <- num_or_na(ans[["sigma2_subject_time"]])
        vc_error[i, j]          <- vc_error[j, i]          <- num_or_na(ans[["sigma2_error"]])

        extra_vec <- ans[["sigma2_extra"]]
        vc_extra[i, j] <- list(extra_vec)
        vc_extra[j, i] <- list(extra_vec)

        vc_SB[i, j]             <- vc_SB[j, i]             <- num_or_na(ans[["SB"]])
        vc_se_ccc[i, j]         <- vc_se_ccc[j, i]         <- num_or_na(ans[["se_ccc"]])

        # AR1 diagnostics
        ar1_rho_lag1_mat[i, j] <- ar1_rho_lag1_mat[j, i] <- num_or_na(ans[["ar1_rho_lag1"]])
        ar1_pairs_mat[i, j]   <- ar1_pairs_mat[j, i]   <- suppressWarnings(as.integer(ans[["ar1_pairs"]]))
        ar1_pval_mat[i, j]    <- ar1_pval_mat[j, i]    <- num_or_na(ans[["ar1_pval"]])
        ar1_reco_mat[i, j]    <- ar1_reco_mat[j, i]    <- isTRUE(ans[["use_ar1"]])

        if (isTRUE(verbose)) {
          .vc_message(ans, label = sprintf("Pair: %s vs %s", m1, m2),
                      nm = Laux$nm, nt = Laux$nt,
                      conf_level = conf_level, digits = digits,
                      use_message = use_message,
                      extra_label = extra_label, ar = ar,
                      ar_rho = if (identical(ar, "ar1")) rho_used else NA_real_)
        }
      }

      if (isTRUE(ci)) {
        lwr_cpp <- num_or_na(if (!is.null(ans)) ans[["lwr"]] else NA_real_)
        upr_cpp <- num_or_na(if (!is.null(ans)) ans[["upr"]] else NA_real_)
        if (is.na(lwr_cpp) || is.na(upr_cpp)) {
          se_cpp <- num_or_na(if (!is.null(ans)) ans[["se_ccc"]] else NA_real_)
          ci2 <- compute_ci_from_se(num_or_na(val), se_cpp, conf_level)
          lwr_cpp <- ci2[1]; upr_cpp <- ci2[2]
        }
        lwr_mat[i, j] <- lwr_mat[j, i] <- lwr_cpp
        upr_mat[i, j] <- upr_mat[j, i] <- upr_cpp
      }
    }
  }

  # Summarise AR(1) recommendation across pairs
  if (!identical(ar, "ar1")) {
    if (any(ar1_reco_mat == TRUE, na.rm = TRUE)) {
      message("AR(1) residual model recommended (lag-1 autocorrelation detected in at least one pair). ",
              "Use ar = \"ar1\" to account for serial correlation.\n")
    }
  }

  diag(est_mat) <- 1
  if (isTRUE(ci)) {
    diag(lwr_mat) <- NA_real_
    diag(upr_mat) <- NA_real_
    out <- list(est = est_mat, lwr.ci = lwr_mat, upr.ci = upr_mat)
    attr(out, "method")      <- "Variance Components REML - pairwise"
    attr(out, "description") <- "Lin's CCC per method pair from random-effects LMM"
    attr(out, "package")     <- "matrixCorr"
    attr(out, "conf.level")  <- conf_level
    if (identical(ar, "ar1")) attr(out, "ar_rho") <- rho_mat

    # attach variance-component matrices
    attr(out, "sigma2_subject")        <- vc_subject
    attr(out, "sigma2_subject_method") <- vc_subject_method
    attr(out, "sigma2_subject_time")   <- vc_subject_time
    attr(out, "sigma2_error")          <- vc_error
    attr(out, "sigma2_extra")          <- vc_extra
    attr(out, "SB")                    <- vc_SB
    attr(out, "se_ccc")                <- vc_se_ccc

    # AR1 diagnostics
    if (!identical(ar, "ar1")) {
      attr(out, "ar1_rho_lag1") <- ar1_rho_lag1_mat
      attr(out, "ar1_pairs")    <- ar1_pairs_mat
      attr(out, "ar1_pval")     <- ar1_pval_mat
      attr(out, "use_ar1")      <- ar1_reco_mat
    }

    class(out) <- c("ccc_lmm_reml", "matrixCorr_ccc_ci", "matrixCorr_ccc", "ccc")
    return(out)
  } else {
    out <- est_mat
    attr(out, "method")      <- "Variance Components REML - pairwise"
    attr(out, "description") <- "Lin's CCC per method pair from random-effects LMM"
    attr(out, "package")     <- "matrixCorr"
    if (identical(ar, "ar1")) attr(out, "ar_rho") <- rho_mat

    # attach variance-component matrices
    attr(out, "sigma2_subject")        <- vc_subject
    attr(out, "sigma2_subject_method") <- vc_subject_method
    attr(out, "sigma2_subject_time")   <- vc_subject_time
    attr(out, "sigma2_error")          <- vc_error
    attr(out, "sigma2_extra")          <- vc_extra
    attr(out, "SB")                    <- vc_SB
    attr(out, "se_ccc")                <- vc_se_ccc

    # AR1 diagnostics
    if (!identical(ar, "ar1")) {
      attr(out, "ar1_rho_lag1") <- ar1_rho_lag1_mat
      attr(out, "ar1_pairs")    <- ar1_pairs_mat
      attr(out, "ar1_pval")     <- ar1_pval_mat
      attr(out, "use_ar1")      <- ar1_reco_mat
    }

    class(out) <- c("ccc_lmm_reml", "matrixCorr_ccc", "ccc", "matrix")
    return(out)
  }
}


#' @keywords internal
.Dmat_normalise_mass <- function(D, target_mass) {
  if (is.null(D)) return(NULL)
  one <- rep(1, nrow(D))
  mass <- as.numeric(t(one) %*% D %*% one)
  # leave as is; 'C++' will guard S_B
  if (!is.finite(mass) || mass <= 0) return(D)
  D * (target_mass / mass)
}

#' @keywords internal
.Dmat_build_kernel <- function(nt, type = c("time-avg","typical-visit","weighted-avg","weighted-sq"),
                               w = NULL, rescale = TRUE) {
  type <- match.arg(type)
  if (nt < 2) return(NULL)
  if (type %in% c("weighted-avg","weighted-sq")) {
    if (is.null(w)) w <- rep(1/nt, nt)
    w <- as.numeric(w)
    if (length(w) != nt) stop("Dmat_weights length must equal the number of present time levels.")
    if (!all(is.finite(w)) || any(w < 0)) stop("Dmat_weights must be non-negative and finite.")
    sw <- sum(w); if (sw <= 0) stop("Dmat_weights sums to zero.")
    w <- w / sw
  }
  D <- switch(type,
              "time-avg"      = (1/nt) * matrix(1, nt, nt),
              "typical-visit" = diag(nt),
              "weighted-avg"  = nt * (w %o% w),
              "weighted-sq"   = nt * diag(w)
  )
  if (rescale) D <- .Dmat_normalise_mass(D, nt)
  # symmetrise softly for safety
  0.5 * (D + t(D))
}

#' Align (optional named) weights to a subset of time levels
#' @keywords internal
.align_weights_to_levels <- function(w, present_lvls, all_lvls) {
  if (is.null(w)) return(NULL)
  if (!is.null(names(w))) {
    idx <- match(present_lvls, all_lvls)
    if (anyNA(idx)) stop("Present time levels not found in all_time_lvls.")
    w_all <- rep(NA_real_, length(all_lvls))
    w_all[seq_along(all_lvls)] <- w[all_lvls]
    w_sub <- w_all[idx]
    if (anyNA(w_sub)) stop("Missing weights for some present time levels.")
    as.numeric(w_sub)
  } else {
    if (length(w) != length(present_lvls))
      stop("Unnamed Dmat_weights must have length equal to the number of present time levels.")
    as.numeric(w)
  }
}

#' Print method for matrixCorr CCC objects
#'
#' @param x A `matrixCorr_ccc` or `matrixCorr_ccc_ci` object.
#' @param digits Number of digits for CCC estimates.
#' @param ci_digits Number of digits for CI bounds.
#' @param show_ci One of `"auto"`, `"yes"`, `"no"`.
#' @param ... Passed to underlying printers.
#' @export
#' @method print matrixCorr_ccc
print.matrixCorr_ccc <- function(x,
                                 digits = 4,
                                 ci_digits = 4,
                                 show_ci = c("auto", "yes", "no"),
                                 ...) {
  show_ci <- match.arg(show_ci)
  is_ci_obj <- inherits(x, "matrixCorr_ccc_ci") ||
    (is.list(x) && all(c("est", "lwr.ci", "upr.ci") %in% names(x)))

  if (is_ci_obj) {
    est <- as.matrix(x$est)
    lwr <- as.matrix(x$lwr.ci)
    upr <- as.matrix(x$upr.ci)
  } else if (is.matrix(x)) {
    est <- as.matrix(x)
    lwr <- matrix(NA_real_, nrow(est), ncol(est), dimnames = dimnames(est))
    upr <- lwr
  } else {
    stop("Invalid object format for class 'ccc'.")
  }

  rn <- rownames(est); cn <- colnames(est)
  if (is.null(rn)) rn <- paste0("m", seq_len(nrow(est)))
  if (is.null(cn)) cn <- rn

  has_any_ci <- any(is.finite(lwr) | is.finite(upr))
  include_ci <- switch(show_ci, auto = has_any_ci, yes = TRUE, no = FALSE)

  cl <- suppressWarnings(as.numeric(attr(x, "conf.level")))
  if (include_ci && is.finite(cl)) {
    cat(sprintf("Concordance pairs (Lin's CCC, %g%% CI)\n\n", 100 * cl))
  } else {
    cat("Concordance pairs (Lin's CCC)\n\n")
  }

  if (nrow(est) == 1L && ncol(est) == 1L) {
    df <- data.frame(
      method1  = rn[1],
      method2  = cn[1],
      estimate = formatC(est[1,1], format = "f", digits = digits),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    if (include_ci) {
      df$lwr <- ifelse(is.na(lwr[1,1]), NA,
                       formatC(lwr[1,1], format = "f", digits = ci_digits))
      df$upr <- ifelse(is.na(upr[1,1]), NA,
                       formatC(upr[1,1], format = "f", digits = ci_digits))
    }
    print(df, row.names = FALSE, right = FALSE, ...)
    return(invisible(x))
  }

  rows <- vector("list", nrow(est) * (ncol(est) - 1L) / 2L); k <- 0L
  for (i in seq_len(nrow(est) - 1L)) {
    for (j in (i + 1L):ncol(est)) {
      k <- k + 1L
      row <- list(
        method1  = rn[i],
        method2  = cn[j],
        estimate = formatC(est[i, j], format = "f", digits = digits)
      )
      if (include_ci) {
        row$lwr <- ifelse(is.na(lwr[i, j]), NA,
                          formatC(lwr[i, j], format = "f", digits = ci_digits))
        row$upr <- ifelse(is.na(upr[i, j]), NA,
                          formatC(upr[i, j], format = "f", digits = ci_digits))
      }
      rows[[k]] <- row
    }
  }

  df <- do.call(rbind.data.frame, rows); rownames(df) <- NULL
  print(df, row.names = FALSE, right = FALSE, ...)
  invisible(x)
}

#' Print method for matrixCorr CCC objects with CIs
#'
#' @inheritParams print.matrixCorr_ccc
#' @export
#' @method print matrixCorr_ccc_ci
print.matrixCorr_ccc_ci <- function(x, ...) {
  print.matrixCorr_ccc(x, ...)
}

#' S3 print for legacy class `ccc_ci`
#'
#' For compatibility with objects that still carry class `"ccc_ci"`.
#' @inheritParams print.matrixCorr_ccc
#' @export
#' @method print ccc_ci
print.ccc_ci <- function(x, ...) {
  print.matrixCorr_ccc(x, ...)
}

#' @title Summary Method for `ccc_lmm_reml` Objects
#'
#' @description Produces a detailed summary of a `"ccc_lmm_reml"` object, including
#' Lin's CCC estimates and associated variance component estimates per method pair.
#'
#' @param object An object of class `"ccc_lmm_reml"`, as returned by [ccc_lmm_reml()].
#' @param digits Integer; number of decimal places to round CCC estimates and components.
#' @param ci_digits Integer; decimal places for confidence interval bounds.
#' @param show_ci Character string indicating whether to show confidence intervals:
#'   `"auto"` (default) shows only if non-NA CIs exist, `"yes"` always shows CIs,
#'   `"no"` never shows them.
#' @param ... Additional arguments (ignored).
#'
#' @return A data frame of class `"summary.ccc_lmm_reml"` with columns:
#'   \code{method1}, \code{method2}, \code{estimate}, and optionally \code{lwr}, \code{upr},
#'   as well as variance component estimates: \code{sigma2_subject}, \code{sigma2_subject_method},
#'   \code{sigma2_subject_time}, \code{sigma2_error}, \code{sigma2_extra}, \code{SB}, \code{se_ccc}.
#'
#' @export
#' @method summary ccc_lmm_reml
summary.ccc_lmm_reml <- function(object,
                                 digits = 4,
                                 ci_digits = 2,
                                 show_ci = c("auto", "yes", "no"),
                                 ...) {
  show_ci <- match.arg(show_ci)

  # Base CCC summary (handles CI and formatting choices)
  base_summary <- summary.ccc(object, digits = digits,
                              ci_digits = ci_digits, show_ci = show_ci)

  # Pull the estimate matrix to know the size/order of pairs
  est_mat <- if (is.list(object) && !is.null(object$est)) {
    as.matrix(object$est)
  } else {
    as.matrix(object)
  }

  rn <- rownames(est_mat); cn <- colnames(est_mat)
  if (is.null(rn)) rn <- as.character(seq_len(nrow(est_mat)))
  if (is.null(cn)) cn <- as.character(seq_len(ncol(est_mat)))

  n_pairs <- if (nrow(est_mat) == 1L && ncol(est_mat) == 1L) 1L else {
    nrow(est_mat) * (ncol(est_mat) - 1L) / 2L
  }

  # Helper: extract VC values in the same order as summary.ccc builds rows (numeric-only)
  extract_pairs_num <- function(val) {
    # Overall 1x1
    if (nrow(est_mat) == 1L && ncol(est_mat) == 1L) {
      if (is.null(val)) return(NA_real_)
      if (is.matrix(val)) return(suppressWarnings(as.numeric(val[1, 1])))
      return(suppressWarnings(as.numeric(val[1])))
    }
    # Pairwise
    out <- numeric(n_pairs)
    k <- 0L
    for (i in seq_len(nrow(est_mat) - 1L)) {
      for (j in (i + 1L):ncol(est_mat)) {
        k <- k + 1L
        if (is.null(val)) {
          out[k] <- NA_real_
        } else if (is.matrix(val)) {
          out[k] <- suppressWarnings(as.numeric(val[i, j]))
        } else {
          vv <- suppressWarnings(as.numeric(val))
          out[k] <- if (length(vv) == 1L) vv else NA_real_
        }
      }
    }
    out
  }

  # UPDATED: extract sigma2_extra as list of numeric vectors (one per pair / overall)
  extract_pairs_extra_list <- function(val) {
    # Overall 1x1
    if (nrow(est_mat) == 1L && ncol(est_mat) == 1L) {
      if (is.null(val)) return(list(NULL))
      if (is.matrix(val) && typeof(val) == "list") {
        return(list(val[[1, 1]]))
      }
      # could be a plain numeric vector
      return(list(suppressWarnings(as.numeric(if (is.matrix(val)) val[1, 1] else val))))
    }
    # Pairwise
    if (is.matrix(val) && typeof(val) == "list") {
      out <- vector("list", n_pairs)
      k <- 0L
      for (i in seq_len(nrow(est_mat) - 1L)) {
        for (j in (i + 1L):ncol(est_mat)) {
          k <- k + 1L
          out[[k]] <- val[[i, j]]
        }
      }
      return(out)
    } else if (is.null(val)) {
      return(vector("list", n_pairs))
    } else {
      # fallback: recycle a single numeric(vector) across pairs
      vv <- suppressWarnings(as.numeric(val))
      return(replicate(n_pairs, vv, simplify = FALSE))
    }
  }

  vc_names_num <- c("sigma2_subject",
                    "sigma2_subject_method",
                    "sigma2_subject_time",
                    "sigma2_error",
                    "SB",
                    "se_ccc")

  # Numeric VCs (same as before)
  vc_cols_num <- lapply(vc_names_num, function(nm) extract_pairs_num(attr(object, nm)))
  names(vc_cols_num) <- vc_names_num

  # sigma2_extra as list of vectors (may be NULL / varying lengths)
  extra_list <- extract_pairs_extra_list(attr(object, "sigma2_extra"))
  # Round each vector
  extra_list <- lapply(extra_list, function(v) if (is.null(v)) NULL else round(as.numeric(v), digits))

  # Determine max number of extra components across pairs/overall
  max_k <- 0L
  for (v in extra_list) if (!is.null(v)) max_k <- max(max_k, length(v))
  # Build extra columns sigma2_extra1..K (NA where not available)
  extra_cols <- list()
  if (max_k > 0L) {
    for (k in seq_len(max_k)) {
      colk <- rep(NA_real_, n_pairs)
      for (r in seq_len(n_pairs)) {
        v <- extra_list[[r]]
        if (!is.null(v) && length(v) >= k) colk[r] <- v[k]
      }
      extra_cols[[paste0("sigma2_extra", k)]] <- colk
    }
  }

  # Assemble output
  out <- base_summary

  # Attach numeric VC columns (rounded)
  for (nm in vc_names_num) {
    out[[nm]] <- as.numeric(round(vc_cols_num[[nm]], digits))
  }

  # Attach expanded extra columns
  if (length(extra_cols)) {
    for (nm in names(extra_cols)) {
      out[[nm]] <- as.numeric(extra_cols[[nm]])
    }
  } else {
    # keep a single column to signal absence, if you prefer:
    # out[["sigma2_extra1"]] <- NA_real_
  }

  extract_pairs_num_mat <- function(val) {
    # Overall 1x1
    if (nrow(est_mat) == 1L && ncol(est_mat) == 1L) {
      return(suppressWarnings(as.numeric(if (is.matrix(val)) val[1,1] else val)))
    }
    outv <- numeric(n_pairs); k <- 0L
    for (i in seq_len(nrow(est_mat) - 1L)) {
      for (j in (i + 1L):ncol(est_mat)) {
        k <- k + 1L
        outv[k] <- suppressWarnings(as.numeric(if (is.matrix(val)) val[i, j] else val))
      }
    }
    outv
  }
  # helper to pull logical per-pair
  extract_pairs_logi_mat <- function(val) {
    as_logi <- function(x) isTRUE(x)
    # Overall 1x1
    if (nrow(est_mat) == 1L && ncol(est_mat) == 1L) {
      v <- if (is.matrix(val)) val[1,1] else val
      return(as_logi(v))
    }
    outv <- logical(n_pairs); k <- 0L
    if (is.matrix(val)) {
      for (i in seq_len(nrow(est_mat) - 1L)) {
        for (j in (i + 1L):ncol(est_mat)) {
          k <- k + 1L
          outv[k] <- as_logi(val[i, j])
        }
      }
    } else {
      outv[] <- as_logi(val)
    }
    outv
  }

  has_attr <- function(obj, nm) !is.null(attr(obj, nm))

  ar1_cols <- list()

  if (has_attr(object, "ar_rho")) {
    ar1_rho_attr <- attr(object, "ar_rho")
    ar1_cols$ar1_rho <- extract_pairs_num_mat(ar1_rho_attr)
  }

  if (has_attr(object, "ar1_rho_lag1")) {
    ar1_rho_lag1_attr <- attr(object, "ar1_rho_lag1")
    ar1_cols$ar1_rho_lag1 <- extract_pairs_num_mat(ar1_rho_lag1_attr)
  }
  if (has_attr(object, "ar1_pairs")) {
    ar1_pairs_attr <- attr(object, "ar1_pairs")
    ar1_cols$ar1_pairs <- extract_pairs_num_mat(ar1_pairs_attr)
  }
  if (has_attr(object, "ar1_pval")) {
    ar1_pval_attr <- attr(object, "ar1_pval")
    ar1_cols$ar1_pval <- extract_pairs_num_mat(ar1_pval_attr)
  }
  if (has_attr(object, "use_ar1")) {
    ar1_reco_attr <- attr(object, "use_ar1")
    ar1_cols$use_ar1 <- extract_pairs_logi_mat(ar1_reco_attr)
  }

  # then, after you build `out`, add only what you have:
  if (!is.null(ar1_cols$ar1_rho)) {
    out[["ar1_rho"]] <- ifelse(is.finite(ar1_cols$ar1_rho),
                               round(ar1_cols$ar1_rho, digits), NA_real_)
  }
  if (!is.null(ar1_cols$ar1_rho_lag1)) {
    out[["ar1_rho_lag1"]] <- ifelse(is.finite(ar1_cols$ar1_rho_lag1),
                                    round(ar1_cols$ar1_rho_lag1, digits), NA_real_)
  }
  if (!is.null(ar1_cols$ar1_pairs)) {
    out[["ar1_pairs"]] <- ifelse(is.finite(ar1_cols$ar1_pairs),
                                 as.integer(ar1_cols$ar1_pairs), NA_integer_)
  }
  if (!is.null(ar1_cols$ar1_pval)) {
    out[["ar1_pval"]] <- ifelse(is.finite(ar1_cols$ar1_pval),
                                round(ar1_cols$ar1_pval, digits), NA_real_)
  }
  if (!is.null(ar1_cols$use_ar1)) {
    out[["use_ar1"]] <- ar1_cols$use_ar1
  }

  class(out) <- c("summary.ccc_lmm_reml", "data.frame")
  attr(out, "conf.level") <- attr(base_summary, "conf.level")
  attr(out, "has_ci")     <- attr(base_summary, "has_ci")
  attr(out, "digits")     <- digits
  attr(out, "ci_digits")  <- ci_digits
  out
}
