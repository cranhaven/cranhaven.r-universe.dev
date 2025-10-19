#' @title Bland-Altman for repeated measurements
#' @description
#' Repeated-measures Bland-Altman (BA) for method comparison based on a
#' mixed-effects model fitted to **subject-time matched paired differences**.
#' A subject-specific random intercept accounts for clustering, and (optionally)
#' an AR(1) process captures serial correlation across replicates within subject.
#' The function returns bias (mean difference), limits of agreement (LoA),
#' confidence intervals, and variance components, for either two methods or
#' all pairwise contrasts when \eqn{\ge}3 methods are supplied.
#'
#' **Required columns / vectors**
#' \itemize{
#'   \item \code{response}: numeric measurements.
#'   \item \code{subject}: subject identifier (integer/factor/numeric).
#'   \item \code{method}: method label with \eqn{\ge}2 levels (factor/character/integer).
#'   \item \code{time}: replicate index used to form pairs; only records where
#'         both methods are present for the same \code{subject} and \code{time}
#'         contribute to a given pairwise BA.
#' }
#'
#' @param data Optional `data.frame`/`data.table` containing required columns.
#' @param response Numeric vector (stacked outcomes) **or** a single character
#'   string giving the column name in `data`.
#' @param subject Subject ID (integer/factor/numeric) **or** a single character
#'   string giving the column name in `data`.
#' @param method Method label (factor/character/integer; N >= 2 levels) **or**
#'   a single character string giving the column name in `data`.
#' @param time Integer/numeric replicate/time index (pairs within subject) **or**
#'   a single character string giving the column name in `data`.
#' @param two Positive scalar; LoA multiple of SD (default 1.96).
#' @param conf_level Confidence level for CIs (default 0.95).
#' @param include_slope Logical. If \code{TRUE}, the model includes the pair mean
#'  as a fixed effect and estimates a proportional-bias slope.
#'   Look at details for deeper information. We recommend fit both with and without the slope. If \eqn{\beta_1} is
#'   materially non-zero over a wide level range, consider a scale transformation
#'   (e.g., log or percent-logit) and re-fit without the slope.
#'
#' @param use_ar1 Logical; AR(1) within-subject residual correlation.
#' @param ar1_rho AR(1) parameter (|rho|<1).
#' @param max_iter,tol EM control for the backend (defaults 200, 1e-6).
#' @param verbose Logical; print brief progress.
#'
#' @return
#' Either a \code{"ba_repeated"} object (exactly two methods) or a
#' \code{"ba_repeated_matrix"} object (pairwise results when \eqn{\ge}3 methods).
#'
#' \strong{If \code{"ba_repeated_matrix"} (N\eqn{\ge}3 methods)}, outputs are:
#' \itemize{
#'   \item \code{bias} \eqn{(m \times m)}; estimated mean difference (row - column).
#'          Diagonal is \code{NA}.
#'   \item \code{sd_loa} \eqn{(m \times m)}; estimated SD of a \emph{single new} paired
#'         difference for the (row, column) methods, accounting for the random
#'         subject intercept and (if enabled) AR(1) correlation.
#'   \item \code{loa_lower}, \code{loa_upper} \eqn{(m \times m)}; limits of agreement
#'         for a single measurement pair, computed as
#'         \eqn{\mathrm{bias} \pm \mathrm{two}\times \mathrm{sd\_loa}}.
#'         Signs follow the row - column convention
#'         (e.g., \code{loa_lower[j,i] = -loa_upper[i,j]}).
#'   \item \code{width} \eqn{(m \times m)}; LoA width,
#'         \code{loa_upper - loa_lower} (= \code{2 * two * sd_loa}).
#'   \item \code{n} \eqn{(m \times m)}; number of subject-time pairs used in each
#'         pairwise BA (complete cases where both methods are present).
#'
#'   \item \strong{CI matrices at nominal \code{conf_level}} (delta method):
#'         \itemize{
#'           \item \code{mean_ci_low}, \code{mean_ci_high}; CI for the bias.
#'           \item \code{loa_lower_ci_low}, \code{loa_lower_ci_high}; CI for the lower LoA.
#'           \item \code{loa_upper_ci_low}, \code{loa_upper_ci_high}; CI for the upper LoA.
#'         }
#'
#'   \item \code{slope} (\eqn{m \times m}; optional); proportional-bias slope (difference
#'         vs pair mean) when \code{include_slope = TRUE}.
#'
#'   \item \code{sigma2_subject} \eqn{(m \times m)}; estimated variance of the
#'         subject-level random intercept (on differences).
#'   \item \code{sigma2_resid} \eqn{(m \times m)}; estimated residual variance of a
#'         single difference after accounting for the random intercept (and AR(1), if used).
#'
#'   \item \code{use_ar1} \emph{(scalar logical)}; whether AR(1) modeling was requested.
#'   \item \code{ar1_rho} \emph{(scalar numeric or \code{NA})}; user-supplied \eqn{\rho}
#'         if a single common value was provided; \code{NA} otherwise.
#'   \item \code{ar1_rho_pair} (\eqn{m \times m}; optional); \eqn{\rho} actually used per pair
#'         (may be estimated from data or equal to the supplied value).
#'   \item \code{ar1_estimated} (\eqn{m \times m}; optional logical); for each pair, \code{TRUE}
#'         if \eqn{\rho} was estimated internally; \code{FALSE} if supplied.
#'
#'   \item \code{methods} \emph{(character)}; method level names; matrix rows/columns
#'         follow this order.
#'   \item \code{two} \emph{(scalar)}; LoA multiplier used (default \code{1.96}).
#'   \item \code{conf_level} \emph{(scalar)}; nominal confidence level used for CIs.
#'
#'   \item \code{data_long} \emph{(data.frame)}; the long data used for fitting
#'         (\code{response}, \code{subject}, \code{method}, \code{time}). Included to
#'         facilitate plotting/reproducibility; not required for summary methods.
#' }
#'
#' \strong{If \code{"ba_repeated"} (exactly two methods)}, outputs are:
#' \itemize{
#'   \item \code{mean.diffs} \emph{(scalar)}; estimated bias (method 2 - method 1).
#'   \item \code{lower.limit}, \code{upper.limit} \emph{(scalars)}; LoA
#'         \eqn{\mu \pm \mathrm{two}\times \mathrm{SD}} for a single new pair.
#'   \item \code{critical.diff} \emph{(scalar)}; \code{two * SD}; LoA half-width.
#'   \item \code{two}, \code{conf_level} \emph{(scalars)}; as above.
#'   \item \code{CI.lines} \emph{(named numeric)}; CI bounds for bias and both LoA
#'         (\code{*.ci.lower}, \code{*.ci.upper}) at \code{conf_level}.
#'   \item \code{means}, \code{diffs} \emph{(vectors)}; per-pair means and differences
#'         used by plotting helpers.
#'   \item \code{based.on} \emph{(integer)}; number of subject-time pairs used.
#'   \item \code{include_slope}, \code{beta_slope}; whether a proportional-bias slope
#'         was estimated and its value (if requested).
#'   \item \code{sigma2_subject}, \code{sigma2_resid}; variance components as above.
#'   \item \code{use_ar1}, \code{ar1_rho}, \code{ar1_estimated}; AR(1) settings/results
#'         as above (scalars for the two-method fit).
#' }
#'
#' @details
#' The function implements a repeated-measures Bland–Altman (BA) analysis for
#' two or more methods using a linear mixed model fitted to
#' \emph{subject–time matched paired differences}. For any selected pair of
#' methods \eqn{(a,b)}, let
#' \deqn{d_{it} = y_{itb} - y_{ita}, \qquad m_{it} = \tfrac{1}{2}(y_{itb} + y_{ita}),}
#' where \eqn{y_{itm}} denotes the observed value from method \eqn{m\in\{a,b\}} on
#' subject \eqn{i} at replicate/time \eqn{t}; \eqn{d_{it}} is the paired difference
#' (method \eqn{b} minus method \eqn{a}); and \eqn{m_{it}} is the corresponding pair mean.
#' Here \eqn{i=1,\ldots,S} indexes subjects and \eqn{t} indexes replicates/time within subject.
#' Only records with both methods present at the same \code{subject} and \code{time}
#' contribute to that pair.
#'
#' The fitted per-pair model is
#' \deqn{d_{it} = \beta_0 + \beta_1\, m_{it} + u_i + \varepsilon_{it},}
#' with random intercept \eqn{u_i \sim \mathcal{N}(0, \sigma_u^2)} and
#' within-subject residual vector \eqn{\varepsilon_i} having covariance
#' \eqn{\operatorname{Cov}(\varepsilon_i) = \sigma_e^2\, \mathbf{R}_i}.
#' When \code{use_ar1 = FALSE}, \eqn{\mathbf{R}_i = \mathbf{I}_{n_i}} (i.i.d.).
#' When \code{use_ar1 = TRUE}, \eqn{\mathbf{R}_i = \mathbf{C}_i^{-1}} and
#' \eqn{\mathbf{C}_i} encodes an AR(1) \emph{precision} structure over time
#' (see below). Setting \code{include_slope = FALSE} drops the regressor
#' \eqn{m_{it}} (i.e., \eqn{\beta_1 = 0}).
#'
#' \subsection{AR(1) within-subject correlation}{
#' For each subject, observations are first ordered by \code{time}. Contiguous
#' blocks satisfy \eqn{t_{k+1} = t_k + 1}; non-contiguous gaps and any
#' negative/NA times are treated as singletons (i.i.d.).
#'
#' For a contiguous block of length \eqn{L} and AR(1) parameter \eqn{\rho}
#' (clipped to \eqn{(-0.999, 0.999)}), the blockwise \emph{precision} matrix
#' \eqn{\mathbf{C}} has entries
#' \deqn{\mathbf{C} = \frac{1}{1-\rho^2}\,\begin{bmatrix}
#' 1 & -\rho &       &        &  \\
#' -\rho & 1+\rho^2 & -\rho  &        &  \\
#'       & \ddots   & \ddots & \ddots &  \\
#'       &          & -\rho  & 1+\rho^2 & -\rho \\
#'       &          &        & -\rho    & 1
#' \end{bmatrix}.}
#' The full \eqn{\mathbf{C}_i} is block-diagonal over contiguous segments, with
#' a small ridge added to the diagonal for numerical stability. Residual
#' covariance is \eqn{\sigma_e^2 \mathbf{R}_i = \sigma_e^2 \mathbf{C}_i^{-1}}.
#'
#' If \code{use_ar1 = TRUE} and \code{ar1_rho} is \code{NA}, \eqn{\rho} is
#' estimated in two passes (i) fit the i.i.d. model; (ii) compute detrended
#' residuals within each contiguous block (remove block-specific intercept and
#' linear time), form lag-1 correlations, apply a small-sample bias adjustment
#' \eqn{(1-\rho^2)/L}, pool with Fisher's \eqn{z} (weights \eqn{\approx L-3}),
#' and refit with the pooled \eqn{\hat\rho}.
#' }
#'
#' \subsection{Estimation by stabilised EM/GLS}{
#' Let \eqn{\mathbf{X}_i} be the fixed-effects design for subject \eqn{i}
#' (intercept, and optionally the pair mean). Internally, the pair mean
#' regressor is centred and scaled before fitting to stabilise the slope;
#' estimates are back-transformed to the original units afterwards.
#'
#' Given current \eqn{(\sigma_u^2, \sigma_e^2)}, the marginal precision of
#' \eqn{d_i} integrates \eqn{u_i} via a Woodbury/Sherman-Morrison identity:
#' \deqn{\mathbf{V}_i^{-1} \;=\; \sigma_e^{-2}\mathbf{C}_i \;-\;
#' \sigma_e^{-2}\mathbf{C}_i \mathbf{1}\,
#' \Big(\sigma_u^{-2} + \sigma_e^{-2}\mathbf{1}^\top \mathbf{C}_i \mathbf{1}\Big)^{-1}
#' \mathbf{1}^\top \mathbf{C}_i \sigma_e^{-2}.}
#' The GLS update is
#' \deqn{\hat\beta \;=\; \Big(\sum_i \mathbf{X}_i^\top \mathbf{V}_i^{-1} \mathbf{X}_i\Big)^{-1}
#' \Big(\sum_i \mathbf{X}_i^\top \mathbf{V}_i^{-1} d_i\Big).}
#' Writing \eqn{r_i = d_i - \mathbf{X}_i\hat\beta}, the E-step gives the BLUP of
#' the random intercept and its conditional second moment:
#' \deqn{M_i \;=\; \sigma_u^{-2} + \sigma_e^{-2}\mathbf{1}^\top \mathbf{C}_i \mathbf{1},\qquad
#' \tilde u_i \;=\; M_i^{-1}\,\sigma_e^{-2}\mathbf{1}^\top \mathbf{C}_i r_i,\qquad
#' \mathbb{E}[u_i^2\mid y] \;=\; \tilde u_i^2 + M_i^{-1}.}
#' The M-step updates the variance components by
#' \deqn{\hat\sigma_u^2 \;=\; \frac{1}{m}\sum_i \mathbb{E}[u_i^2\mid y], \qquad
#' \hat\sigma_e^2 \;=\; \frac{1}{n}\sum_i \Big\{(r_i - \mathbf{1}\tilde u_i)^\top
#' \mathbf{C}_i (r_i - \mathbf{1}\tilde u_i) + M_i^{-1}\,\mathbf{1}^\top \mathbf{C}_i \mathbf{1}\Big\}.}
#' The algorithm employs positive-definite inverses with ridge fallback,
#' trust-region damping of variance updates (ratio-bounded), and clipping of
#' parameters to prevent degeneracy.
#' }
#'
#' \subsection{Point estimates reported}{
#' The reported BA \emph{bias} is the \emph{subject-equal} mean of the paired
#' differences,
#' \deqn{\mu_0 \;=\; \frac{1}{m}\sum_{i=1}^m \bar d_i,\qquad
#' \bar d_i = \frac{1}{n_i}\sum_{t=1}^{n_i} d_{it},}
#' which is robust to unbalanced replicate counts (\eqn{n_i}) and coincides with
#' the GLS intercept when subjects contribute equally. If \code{include_slope =
#' TRUE}, the proportional-bias slope is estimated against the pair mean
#' \eqn{m_{it}}; internally \eqn{m_{it}} is centred and scaled and the slope is
#' returned on the original scale.
#' }
#'
#' \subsection{Proportional bias slope (\code{include_slope})}{
#' When \code{include_slope = TRUE}, the model augments the mean difference with
#' the pair mean \eqn{m_{it} = \tfrac{1}{2}(y_{itb}+y_{ita})}:
#' \deqn{d_{it} = \beta_0 + \beta_1 m_{it} + u_i + \varepsilon_{it}.}
#' Internally \eqn{m_{it}} is centred and scaled for numerical stability; the
#' reported \code{beta_slope} and intercept are back-transformed to the original scale.
#'
#' \eqn{\beta_1 \neq 0} indicates level-dependent (proportional)
#' bias, where the difference between methods changes with the overall measurement level.
#' The Bland–Altman limits of agreement produced by this function remain the
#' conventional, \emph{horizontal} bands centred at the subject-equal bias \eqn{\mu_0};
#' they are not regression-adjusted LoA.
#'
#' On an appropriate scale and when
#' the two methods have comparable within-subject variances, the null expectation is
#' \eqn{\beta_1 \approx 0}. However, because \eqn{m_{it}} contains measurement error
#' from both methods, unequal precisions can produce a spurious slope even if there is
#' no true proportional bias. Under a simple no-bias data-generating model
#' \eqn{y_{itm}=\theta_{it}+c_m+e_{itm}} with independent errors of variances
#' \eqn{\sigma_a^2,\sigma_b^2}, the expected OLS slope is approximately
#' \deqn{\mathbb{E}[\hat\beta_1] \;\approx\;
#' \dfrac{\tfrac{1}{2}(\sigma_b^2-\sigma_a^2)}
#'       {\mathrm{Var}(\theta_{it}) + \tfrac{1}{4}(\sigma_a^2+\sigma_b^2)},}
#' showing zero expectation when \eqn{\sigma_a^2=\sigma_b^2} and attenuation as the
#' level variability \eqn{\mathrm{Var}(\theta_{it})} increases.
#' }
#'
#' \subsection{Limits of Agreement (LoA)}{
#' A \emph{single new paired measurement} for a random subject has variance
#' \deqn{\mathrm{Var}(d^\star) \;=\; \sigma_u^2 + \sigma_e^2,}
#' so the LoA are
#' \deqn{\mathrm{LoA} \;=\; \mu_0 \;\pm\; \texttt{two}\,\sqrt{\sigma_u^2 + \sigma_e^2}.}
#' The argument \code{two} is the SD multiplier (default \eqn{z_{1-\alpha/2}}
#' implied by \code{conf_level}, e.g., \eqn{1.96} at \eqn{95\%}).
#' }
#'
#' \subsection{Wald/Delta confidence intervals}{
#' CIs for \emph{bias} and each LoA use a delta method around the EM estimates.
#' The standard error of \eqn{\mu_0} is based on the dispersion of subject
#' means \eqn{\mathrm{Var}(\mu_0) \approx \mathrm{Var}(\bar d_i)/m}. For
#' \eqn{\mathrm{sd}_{\mathrm{LoA}} = \sqrt{V}} with \eqn{V=\sigma_u^2+\sigma_e^2},
#' we can define
#' \deqn{\mathrm{Var}(\mathrm{sd}) \;\approx\; \frac{\mathrm{Var}(V)}{4V}, \quad
#' \mathrm{Var}(V) \;\approx\; \mathrm{Var}(\hat\sigma_u^2) + \mathrm{Var}(\hat\sigma_e^2)
#' + 2\,\mathrm{Cov}(\hat\sigma_u^2,\hat\sigma_e^2).}
#' The per-subject contributions used to approximate these terms are:
#' \eqn{A_i=\mathbb{E}[u_i^2\mid y]} and
#' \eqn{B_i=\{(r_i-\mathbf{1}\tilde u_i)^\top\mathbf{C}_i(r_i-\mathbf{1}\tilde u_i)
#' + M_i^{-1}\mathbf{1}^\top\mathbf{C}_i\mathbf{1}\}/n_i}. Empirical variances
#' of \eqn{A_i} and \eqn{B_i} (with replicate-size weighting for \eqn{B_i}) and
#' their covariance yield \eqn{\mathrm{Var}(\hat\sigma_u^2)},
#' \eqn{\mathrm{Var}(\hat\sigma_e^2)} and \eqn{\mathrm{Cov}(\hat\sigma_u^2,\hat\sigma_e^2)}.
#' For a LoA bound \eqn{L_\pm = \mu_0 \pm \texttt{two}\cdot \mathrm{sd}}, the
#' working variance is
#' \deqn{\mathrm{Var}(L_\pm) \;\approx\; \mathrm{Var}(\mu_0) + \texttt{two}^2\,\mathrm{Var}(\mathrm{sd}),}
#' and Wald CIs use the normal quantile at \code{conf_level}. These are
#' large-sample approximations; with very small \eqn{m} they may be conservative.
#' }
#'
#' \subsection{Three or more methods}{
#' When \eqn{\ge 3} methods are supplied, the routine performs the above fit for
#' each unordered pair \eqn{(j,k)} and reassembles matrices. Specifically,
#' \itemize{
#' \item \emph{Orientation} is defined to be \emph{row minus column}, i.e.,
#'       \eqn{\texttt{bias}[j,k]} estimates \eqn{\mathbb{E}(y_k - y_j)}.
#' \item \eqn{\texttt{bias}} is antisymmetric
#'       (\eqn{b_{jk} = -b_{kj}}); \eqn{\texttt{sd\_loa}} and \eqn{\texttt{width}}
#'       are symmetric; LoA obey \eqn{\texttt{loa\_lower}[j,k] = -\texttt{loa\_upper}[k,j]}.
#' \item \eqn{\texttt{n}[j,k]} is the number of subject-time pairs
#'       used for that contrast (complete cases where both methods are present).
#' }
#' }
#'
#' \subsection{Missingness, time irregularity, and safeguards}{
#' Pairs are formed only where both methods are observed at the same
#' \code{subject} and \code{time}; other records do not influence that pair.
#' AR(1) structure is applied only over contiguous time sequences within
#' subject; gaps break contiguity and revert those positions to i.i.d.
#' Numerically, inverses use a positive-definite solver with adaptive ridge and
#' pseudo-inverse fallback; variance updates are clamped and ratio-damped;
#' \eqn{\rho} is clipped to \eqn{(-0.999,0.999)}.
#' }
#'
#' @examples
#' # -------- Simulate repeated-measures data --------
#' set.seed(1)
#'
#' # design (no AR)
#' # subjects
#' S   <- 30L
#' # replicates per subject
#' Tm  <- 15L
#' subj <- rep(seq_len(S), each = Tm)
#' time <- rep(seq_len(Tm), times = S)
#'
#' # subject signal centered at 0 so BA "bias" won't be driven by the mean level
#' mu_s  <- rnorm(S, mean = 0, sd = 8)
#' # constant within subject across replicates
#' true  <- mu_s[subj]
#'
#' # common noise (no AR, i.i.d.)
#' sd_e <- 2
#' e0   <- rnorm(length(true), 0, sd_e)
#'
#' # --- Methods ---
#' # M1: signal + noise
#' y1 <- true + e0
#'
#' # M2: same precision as M1; here identical so M3 can be
#' #     almost perfectly the inverse of both M1 and M2
#' y2 <- y1 + rnorm(length(true), 0, 0.01)
#'
#' # M3: perfect inverse of M1 and M2
#' y3 <- -y1   # = -(true + e0)
#'
#' # M4: unrelated to all others (pure noise, different scale)
#' y4 <- rnorm(length(true), 3, 6)
#'
#' data <- rbind(
#'   data.frame(y = y1, subject = subj, method = "M1", time = time),
#'   data.frame(y = y2, subject = subj, method = "M2", time = time),
#'   data.frame(y = y3, subject = subj, method = "M3", time = time),
#'   data.frame(y = y4, subject = subj, method = "M4", time = time)
#' )
#' data$method <- factor(data$method, levels = c("M1","M2","M3","M4"))
#'
#' # quick sanity checks
#' with(data, {
#'   Y <- split(y, method)
#'   round(cor(cbind(M1 = Y$M1, M2 = Y$M2, M3 = Y$M3, M4 = Y$M4)), 3)
#' })
#'
#' # Run BA (no AR)
#' ba4 <- bland_altman_repeated(
#'   data = data,
#'   response = "y", subject = "subject", method = "method", time = "time",
#'   two = 1.96, conf_level = 0.95,
#'   include_slope = FALSE, use_ar1 = FALSE
#' )
#' summary(ba4)
#' plot(ba4)
#'
#' # -------- Simulate repeated-measures with AR(1) data --------
#' set.seed(123)
#' S <- 40L                      # subjects
#' Tm <- 50L                     # replicates per subject
#' methods <- c("A","B","C")     # N = 3 methods
#' rho <- 0.4                    # AR(1) within-subject across time
#'
#' ar1_sim <- function(n, rho, sd = 1) {
#'   z <- rnorm(n)
#'   e <- numeric(n)
#'   e[1] <- z[1] * sd
#'   if (n > 1) for (t in 2:n) e[t] <- rho * e[t-1] + sqrt(1 - rho^2) * z[t] * sd
#'   e
#' }
#'
#' # Subject baseline + time trend (latent "true" signal)
#' subj <- rep(seq_len(S), each = Tm)
#' time <- rep(seq_len(Tm), times = S)
#' # subject effects
#' mu_s  <- rnorm(S, 50, 7)
#' trend <- rep(seq_len(Tm) - mean(seq_len(Tm)), times = S) * 0.8
#' true  <- mu_s[subj] + trend
#'
#' # Method-specific biases (B has +1.5 constant; C has slight proportional bias)
#' bias  <- c(A = 0, B = 1.5, C = -0.5)
#' # proportional component on "true"
#' prop  <- c(A = 0.00, B = 0.00, C = 0.10)
#'
#' # Build long data: for each method, add AR(1) noise within subject over time
#' make_method <- function(meth, sd = 3) {
#'   e <- unlist(lapply(split(seq_along(time), subj),
#'                      function(ix) ar1_sim(length(ix), rho, sd)))
#'   y <- true * (1 + prop[meth]) + bias[meth] + e
#'   data.frame(y = y, subject = subj, method = meth, time = time,
#'              check.names = FALSE)
#' }
#'
#' data <- do.call(rbind, lapply(methods, make_method))
#' data$method <- factor(data$method, levels = methods)
#'
#' # -------- Repeated BA (pairwise matrix) ---------------------
#' baN <- bland_altman_repeated(
#'   response = data$y, subject = data$subject, method = data$method, time = data$time,
#'   two = 1.96, conf_level = 0.95,
#'   include_slope = FALSE,         # estimate proportional bias per pair
#'   use_ar1 = TRUE # model AR(1) within-subject
#' )
#'
#' # Matrices (row - column orientation)
#' print(baN)
#' summary(baN)
#'
#' # Faceted BA scatter by pair
#' plot(baN, smoother = "lm", facet_scales = "free_y")
#'
#' # -------- Two-method path (A vs B only) -----------------------------------
#' data_AB <- subset(data, method %in% c("A","B"))
#' baAB <- bland_altman_repeated(
#'   response = data_AB$y, subject = data_AB$subject,
#'   method = droplevels(data_AB$method), time = data_AB$time,
#'   include_slope = FALSE, use_ar1 = TRUE, ar1_rho = 0.4
#' )
#' print(baAB)
#' plot(baAB)
#'
#' @author Thiago de Paula Oliveira
#' @export
bland_altman_repeated <- function(data = NULL, response, subject, method, time,
                                  two = 1.96, conf_level = 0.95,
                                  include_slope = FALSE,
                                  use_ar1 = FALSE, ar1_rho = NA_real_,
                                  max_iter = 200L, tol = 1e-6,
                                  verbose = FALSE) {

  # --- resolve columns if 'data' provided and names given ---
  if (!is.null(data)) {
    if (!inherits(data, "data.frame"))
      stop("`data` must be a data.frame or data.table.")
    pull <- function(x) {
      if (is.character(x) && length(x) == 1L) {
        if (!x %in% names(data)) stop("Column '", x, "' not found in `data`.")
        data[[x]]
      } else x
    }
    response <- pull(response)
    subject  <- pull(subject)
    method   <- pull(method)
    time     <- pull(time)
    mapping <- list(
      response =
        if (is.character(substitute(response)) && length(substitute(response)) == 1L &&
                     is.character(match.call()$response)) as.character(match.call()$response) else ".response",
      subject  = if (is.character(match.call()$subject)) as.character(match.call()$subject) else ".subject",
      method   = if (is.character(match.call()$method))  as.character(match.call()$method)  else ".method",
      time     = if (is.character(match.call()$time))    as.character(match.call()$time)    else ".time"
    )

    # always use canonical internal names in the stored table
    data_long <- setNames(
      data.frame(response, subject, method, time, check.names = FALSE),
      c(".response", ".subject", ".method", ".time")
    )
    mapping <- list(
      response = ".response",
      subject  = ".subject",
      method   = ".method",
      time     = ".time"
    )
  } else {
    # vectors supplied; build canonical internal columns
    mapping <- list(
      response = ".response",
      subject  = ".subject",
      method   = ".method",
      time     = ".time"
    )
    data_long <- data.frame(
      ".response" = response,
      ".subject"  = subject,
      ".method"   = method,
      ".time"     = time,
      check.names = FALSE
    )
  }

  # ---- validate / normalise types (work with local copies) ----
  y <- data_long[[mapping$response]]
  s <- data_long[[mapping$subject ]]
  m <- data_long[[mapping$method  ]]
  t <- data_long[[mapping$time    ]]

  if (!is.numeric(y) || length(y) < 2L) stop("`response` must be numeric with length > 1.")
  if (!(is.integer(s) || is.factor(s) || is.numeric(s)))
    stop("`subject` must be integer/factor/numeric.")
  s <- as.integer(as.factor(s))
  if (!is.factor(m)) m <- as.factor(m)
  m <- droplevels(m)
  mlev <- levels(m)
  if (length(mlev) < 2L) stop("Need at least 2 distinct methods in `method`.")
  if (!(is.integer(t) || is.numeric(t))) stop("`time` must be integer/numeric.")
  t <- as.integer(t)

  if (!is.numeric(two) || length(two) != 1L || two <= 0) stop("`two` must be a positive scalar.")
  if (!(is.numeric(conf_level) && length(conf_level) == 1L && conf_level > 0 && conf_level < 1))
    stop("`conf_level` must be in (0,1).")
  if (isTRUE(use_ar1)) {
    if (!is.na(ar1_rho)) {
      if (!is.finite(ar1_rho)) stop("`ar1_rho` must be finite or NA when use_ar1=TRUE.")
      if (abs(ar1_rho) >= 0.999) stop("`ar1_rho` must be in (-0.999, 0.999).")
    }
  } else {
    ar1_rho <- NA_real_
  }

  # ---- two-method path -------------------------------------------------------
  if (length(mlev) == 2L) {
    idx <- m %in% mlev & is.finite(y) & !is.na(s) & !is.na(t)
    res1 <- .ba_rep_two_methods(
      response = y[idx],
      subject  = s[idx],
      method12 = as.integer(m[idx] == mlev[2L]) + 1L,
      time     = t[idx],
      two = two, conf_level = conf_level, include_slope = include_slope,
      use_ar1 = use_ar1, ar1_rho = ar1_rho, max_iter = max_iter, tol = tol
    )
    # attach mapping/data_long for downstream plotting
    res1$data_long <- data_long
    res1$mapping   <- mapping
    attr(res1, "conf.level") <- conf_level
    return(res1)
  }

  # ---- N-method pairwise path ------------------------------------------------
  if (isTRUE(verbose)) {
    cat("Repeated BA (pairwise):", length(mlev), "methods ->",
        choose(length(mlev), 2L), "pairs\n")
  }

  methods <- mlev
  mm <- length(methods)

  bias         <- .make_named_matrix_ba(methods)
  sd_loa       <- .make_named_matrix_ba(methods)
  loa_lower    <- .make_named_matrix_ba(methods)
  loa_upper    <- .make_named_matrix_ba(methods)
  width        <- .make_named_matrix_ba(methods)
  n_mat        <- .make_named_matrix_ba(methods, NA_integer_, "integer")
  mean_ci_low  <- .make_named_matrix_ba(methods)
  mean_ci_high <- .make_named_matrix_ba(methods)
  lo_ci_low    <- .make_named_matrix_ba(methods)
  lo_ci_high   <- .make_named_matrix_ba(methods)
  hi_ci_low    <- .make_named_matrix_ba(methods)
  hi_ci_high   <- .make_named_matrix_ba(methods)
  slope_mat    <- if (isTRUE(include_slope)) .make_named_matrix_ba(methods) else NULL
  vc_subject   <- .make_named_matrix_ba(methods)
  vc_resid     <- .make_named_matrix_ba(methods)
  ar1_rho_mat   <- if (isTRUE(use_ar1)) .make_named_matrix_ba(methods) else NULL
  ar1_estimated <- if (isTRUE(use_ar1)) {
    z <- matrix(NA, mm, mm, dimnames = list(methods, methods))
    storage.mode(z) <- "logical"
    z
  } else NULL

  .recompose_pair <- function(fit) {
    list(
      md    = as.numeric(fit$bias_mu0),
      sd    = as.numeric(fit$sd_loa),
      lo    = as.numeric(fit$loa_lower),
      hi    = as.numeric(fit$loa_upper),
      bias_l= as.numeric(fit$bias_lwr),
      bias_u= as.numeric(fit$bias_upr),
      lo_l  = as.numeric(fit$loa_lower_lwr),
      lo_u  = as.numeric(fit$loa_lower_upr),
      hi_l  = as.numeric(fit$loa_upper_lwr),
      hi_u  = as.numeric(fit$loa_upper_upr)
    )
  }

  for (j in 1:(mm - 1L)) for (k in (j + 1L):mm) {
    lev_j <- methods[j]; lev_k <- methods[k]
    sel <- m %in% c(lev_j, lev_k) & is.finite(y) & !is.na(s) & !is.na(t)
    if (!any(sel)) next
    m12 <- ifelse(m[sel] == lev_j, 1L, 2L)

    fit <- bland_altman_repeated_em_ext_cpp(
      y = y[sel], subject = s[sel], method = m12, time = t[sel],
      include_slope = include_slope,
      use_ar1 = use_ar1, ar1_rho = ar1_rho,
      max_iter = max_iter, tol = tol, conf_level = conf_level,
      two_arg = two
    )
    comp <- .recompose_pair(fit)

    bias[j,k]      <- comp$md;  bias[k,j]      <- -comp$md
    sd_loa[j,k]    <- comp$sd;  sd_loa[k,j]    <-  comp$sd
    loa_lower[j,k] <- comp$lo;  loa_lower[k,j] <- -comp$hi
    loa_upper[j,k] <- comp$hi;  loa_upper[k,j] <- -comp$lo
    width[j,k]     <- comp$hi - comp$lo; width[k,j] <- width[j,k]
    n_mat[j,k]     <- as.integer(fit$n_pairs); n_mat[k,j] <- n_mat[j,k]

    mean_ci_low[j,k]  <- comp$bias_l; mean_ci_low[k,j]  <- -comp$bias_u
    mean_ci_high[j,k] <- comp$bias_u; mean_ci_high[k,j] <- -comp$bias_l
    lo_ci_low[j,k]    <- comp$lo_l;   lo_ci_low[k,j]    <- -comp$hi_u
    lo_ci_high[j,k]   <- comp$lo_u;   lo_ci_high[k,j]   <- -comp$hi_l
    hi_ci_low[j,k]    <- comp$hi_l;   hi_ci_low[k,j]    <- -comp$lo_u
    hi_ci_high[j,k]   <- comp$hi_u;   hi_ci_high[k,j]   <- -comp$lo_l

    vc_subject[j,k] <- vc_subject[k,j] <- as.numeric(fit$sigma2_subject)
    vc_resid[j,k]   <- vc_resid[k,j]   <- as.numeric(fit$sigma2_resid)

    if (isTRUE(use_ar1)) {
      ar1_rho_used <- as.numeric(fit$ar1_rho)
      ar1_rho_mat[j,k] <- ar1_rho_mat[k,j] <- ar1_rho_used
      ar1_estimated[j,k] <- ar1_estimated[k,j] <- isTRUE(fit$ar1_estimated)
    }

    if (!is.null(slope_mat)) {
      slope <- as.numeric(fit$beta_slope)
      slope_mat[j,k] <-  slope
      slope_mat[k,j] <- -slope
    }

    if (isTRUE(verbose)) {
      cat(sprintf(" pair %s - %s: n=%d, bias=%.4f, sd=%.4f\n",
                  lev_j, lev_k, as.integer(fit$n_pairs), comp$md, comp$sd))
    }
  }

  ba_repeated <- list(
    bias = bias,
    sd_loa = sd_loa,
    loa_lower = loa_lower,
    loa_upper = loa_upper,
    width = width,
    n = n_mat,
    mean_ci_low = mean_ci_low,
    mean_ci_high = mean_ci_high,
    loa_lower_ci_low = lo_ci_low,
    loa_lower_ci_high = lo_ci_high,
    loa_upper_ci_low = hi_ci_low,
    loa_upper_ci_high = hi_ci_high,
    slope = slope_mat,
    methods = methods,
    two = two,
    conf_level = conf_level,
    use_ar1 = use_ar1,
    ar1_rho = if (use_ar1) ar1_rho else NA_real_,
    sigma2_subject = vc_subject,
    sigma2_resid   = vc_resid,
    ar1_rho_pair   = if (use_ar1) ar1_rho_mat else NULL,
    ar1_estimated  = if (use_ar1) ar1_estimated else NULL,
    data_long      = data_long,
    mapping        = mapping
  )
  class(ba_repeated) <- c("ba_repeated_matrix","list")
  attr(ba_repeated, "conf.level") <- conf_level
  ba_repeated
}




#' two-method helper
#' @keywords internal
.ba_rep_two_methods <- function(response, subject, method12, time,
                                two, conf_level, include_slope,
                                use_ar1, ar1_rho, max_iter, tol) {
  fit <- bland_altman_repeated_em_ext_cpp(
    y = response, subject = subject, method = method12, time = time,
    include_slope = include_slope,
    use_ar1 = use_ar1, ar1_rho = ar1_rho,
    max_iter = max_iter, tol = tol, conf_level = conf_level,
    two_arg = two
  )

  md  <- as.numeric(fit$bias_mu0)
  sdL <- as.numeric(fit$sd_loa)

  loa_lower <- as.numeric(fit$loa_lower)
  loa_upper <- as.numeric(fit$loa_upper)

  CI.lines <- c(
    "mean.diff.ci.lower"   = as.numeric(fit$bias_lwr),
    "mean.diff.ci.upper"   = as.numeric(fit$bias_upr),
    "lower.limit.ci.lower" = as.numeric(fit$loa_lower_lwr),
    "lower.limit.ci.upper" = as.numeric(fit$loa_lower_upr),
    "upper.limit.ci.lower" = as.numeric(fit$loa_upper_lwr),
    "upper.limit.ci.upper" = as.numeric(fit$loa_upper_upr)
  )

  means <- as.numeric(fit$pairs_mean)
  diffs <- as.numeric(fit$pairs_diff)

  ba_repeated <- list(
    means         = means,
    diffs         = diffs,
    based.on      = as.integer(fit$n_pairs),
    lower.limit   = loa_lower,
    mean.diffs    = md,
    upper.limit   = loa_upper,
    lines         = c(lower = loa_lower, mean = md, upper = loa_upper),
    CI.lines      = CI.lines,
    two           = two,
    critical.diff = two * sdL,
    include_slope = include_slope,
    beta_slope    = if (include_slope) as.numeric(fit$beta_slope) else NA_real_,
    sigma2_subject= as.numeric(fit$sigma2_subject),
    sigma2_resid  = as.numeric(fit$sigma2_resid),
    use_ar1       = use_ar1,
    ar1_rho       = if (use_ar1) as.numeric(fit$ar1_rho) else NA_real_,
    ar1_estimated = if (use_ar1) isTRUE(fit$ar1_estimated) else NA
  )
  class(ba_repeated) <- c("ba_repeated","list")
  attr(ba_repeated, "conf.level") <- conf_level
  ba_repeated
}




# ---------- printers ----------
#' @rdname bland_altman_repeated
#' @method print ba_repeated
#' @param x A \code{"ba_repeated"} object.
#' @param digits Number of digits for estimates (default 3).
#' @param ci_digits Number of digits for CI bounds (default 3).
#' @param ... Unused.
#' @export
print.ba_repeated <- function(x, digits = 3, ci_digits = 3, ...) {
  stopifnot(inherits(x, "ba_repeated"))
  n   <- as.integer(x$based.on)
  two <- as.numeric(x$two)
  cl  <- suppressWarnings(as.numeric(attr(x, "conf.level")))
  if (!is.finite(cl)) cl <- NA_real_

  md    <- as.numeric(x$mean.diffs)
  loaL  <- as.numeric(x$lower.limit)
  loaU  <- as.numeric(x$upper.limit)
  sd_d  <- as.numeric(x$critical.diff) / two
  width <- loaU - loaL

  cil <- function(nm) as.numeric(x$CI.lines[[nm]])
  bias_l <- cil("mean.diff.ci.lower"); bias_u <- cil("mean.diff.ci.upper")
  lo_l   <- cil("lower.limit.ci.lower"); lo_u <- cil("lower.limit.ci.upper")
  hi_l   <- cil("upper.limit.ci.lower"); hi_u <- cil("upper.limit.ci.upper")

  head <- sprintf("Repeated-measures Bland-Altman (pairs = %d) - LoA = mean \u00B1 %.3g | SD%s\n\n",
                  n, two, if (is.finite(cl)) sprintf(", %g%% CI", 100*cl) else "")
  cat(head)

  df <- data.frame(
    quantity = c("Mean difference", "Lower LoA", "Upper LoA"),
    estimate = c(md, loaL, loaU),
    lwr      = c(bias_l, lo_l, hi_l),
    upr      = c(bias_u, lo_u, hi_u),
    check.names = FALSE
  )
  df$estimate <- formatC(df$estimate, format = "f", digits = digits)
  df$lwr      <- formatC(df$lwr,      format = "f", digits = ci_digits)
  df$upr      <- formatC(df$upr,      format = "f", digits = ci_digits)
  print(df, row.names = FALSE, right = FALSE)

  cat(sprintf("\nSD(single-pair differences): %s   LoA width: %s",
              formatC(sd_d, format = "f", digits = digits),
              formatC(width, format = "f", digits = digits)))
  if (isTRUE(x$include_slope) && is.finite(x$beta_slope)) {
    cat(sprintf("\nProportional bias slope (vs pair mean): %s\n",
                formatC(x$beta_slope, format = "f", digits = digits)))
  } else {
    cat("\n")
  }
  invisible(x)
}

#' @rdname bland_altman_repeated
#' @method print ba_repeated_matrix
#' @param x A \code{"ba_repeated_matrix"} object.
#' @param digits Number of digits for estimates (default 3).
#' @param ci_digits Number of digits for CI bounds (default 3).
#' @param style Show as pairs or matrix format?
#' @param ... Unused.
#' @export
print.ba_repeated_matrix <- function(x,
                                     digits = 3,
                                     ci_digits = 3,
                                     style = c("pairs","matrices"),
                                     ...) {
  stopifnot(inherits(x, "ba_repeated_matrix"))
  style <- match.arg(style)
  cl <- suppressWarnings(as.numeric(attr(x, "conf.level")))
  has_ci <- all(c("mean_ci_low","mean_ci_high",
                  "loa_lower_ci_low","loa_lower_ci_high",
                  "loa_upper_ci_low","loa_upper_ci_high") %in% names(x))

  if (style == "matrices") {
    if (is.finite(cl)) cat(sprintf("Bland-Altman (pairwise matrices), %g%% CI\n\n", 100*cl))
    else               cat("Bland-Altman (pairwise matrices)\n\n")
    mats <- list(
      bias    = x$bias,
      sd_loa  = x$sd_loa,
      loa_low = x$loa_lower,
      loa_up  = x$loa_upper,
      width   = x$width,
      n       = x$n
    )
    for (nm in names(mats)) {
      cat("\n", nm, ":\n", sep = "")
      print(round(mats[[nm]], if (nm == "n") 0 else digits))
    }
    return(invisible(x))
  }

  methods <- x$methods
  m <- length(methods)
  rows <- vector("list", m * (m-1) / 2L)
  k <- 0L
  for (i in 1:(m-1)) for (j in (i+1):m) {
    k <- k + 1L
    row <- list(
      method1 = methods[i],
      method2 = methods[j],
      bias    = round(x$bias[i,j],     digits),
      sd_loa  = round(x$sd_loa[i,j],   digits),
      loa_low = round(x$loa_lower[i,j],digits),
      loa_up  = round(x$loa_upper[i,j],digits),
      width   = round(x$width[i,j],    digits),
      n       = suppressWarnings(as.integer(x$n[i,j]))
    )
    if (has_ci && is.finite(cl)) {
      row$bias_lwr <- round(x$mean_ci_low[i,j],       ci_digits)
      row$bias_upr <- round(x$mean_ci_high[i,j],      ci_digits)
      row$lo_lwr   <- round(x$loa_lower_ci_low[i,j],  ci_digits)
      row$lo_upr   <- round(x$loa_lower_ci_high[i,j], ci_digits)
      row$up_lwr   <- round(x$loa_upper_ci_low[i,j],  ci_digits)
      row$up_upr   <- round(x$loa_upper_ci_high[i,j], ci_digits)
    }
    row$sigma2_subject <- round(x$sigma2_subject[i,j], digits)
    row$sigma2_resid   <- round(x$sigma2_resid[i,j],   digits)
    if (isTRUE(x$use_ar1)) {
      rho_ij <- if (!is.null(x$ar1_rho_pair)) x$ar1_rho_pair[i,j] else NA_real_
      est_ij <- if (!is.null(x$ar1_estimated)) isTRUE(x$ar1_estimated[i,j]) else NA
      row$ar1_rho       <- round(rho_ij, digits)
      row$ar1_estimated <- est_ij
    }
    rows[[k]] <- row
  }
  df <- do.call(rbind.data.frame, rows)
  if (is.finite(cl)) cat(sprintf("Bland-Altman (row \u2212 column), %g%% CI\n\n", 100*cl))
  else               cat("Bland-Altman (row \u2212 column)\n\n")
  print(df, row.names = FALSE, right = FALSE)
  invisible(x)
}

#' @method summary ba_repeated
#' @export
summary.ba_repeated <- function(object,
                                digits = 3,
                                ci_digits = 3,
                                ...) {
  stopifnot(inherits(object, "ba_repeated"))
  cl <- suppressWarnings(as.numeric(attr(object, "conf.level")))
  n  <- as.integer(object$based.on)

  ba_repeated <- data.frame(
    method1   = "method 1",
    method2   = "method 2",
    bias      = round(num_or_na_ba(object$mean.diffs), digits),
    sd_loa    = round(num_or_na_ba(object$critical.diff) / num_or_na_ba(object$two), digits),
    loa_low   = round(num_or_na_ba(object$lower.limit), digits),
    loa_up    = round(num_or_na_ba(object$upper.limit), digits),
    width     = round(num_or_na_ba(object$upper.limit - object$lower.limit), digits),
    n         = n,
    stringsAsFactors = FALSE, check.names = FALSE
  )

  if (isTRUE(object$include_slope) && is.finite(num_or_na_ba(object$beta_slope))) {
    ba_repeated$slope <- round(num_or_na_ba(object$beta_slope), digits)
  }

  cil <- function(nm) num_or_na_ba(object$CI.lines[[nm]])
  if (!any(is.na(c(cil("mean.diff.ci.lower"), cil("mean.diff.ci.upper"),
                   cil("lower.limit.ci.lower"), cil("lower.limit.ci.upper"),
                   cil("upper.limit.ci.lower"), cil("upper.limit.ci.upper"))))) {
    ba_repeated$bias_lwr <- round(cil("mean.diff.ci.lower"), ci_digits)
    ba_repeated$bias_upr <- round(cil("mean.diff.ci.upper"), ci_digits)
    ba_repeated$lo_lwr   <- round(cil("lower.limit.ci.lower"), ci_digits)
    ba_repeated$lo_upr   <- round(cil("lower.limit.ci.upper"), ci_digits)
    ba_repeated$up_lwr   <- round(cil("upper.limit.ci.lower"), ci_digits)
    ba_repeated$up_upr   <- round(cil("upper.limit.ci.upper"), ci_digits)
  }

  ba_repeated$sigma2_subject <- round(num_or_na_ba(object$sigma2_subject), digits)
  ba_repeated$sigma2_resid   <- round(num_or_na_ba(object$sigma2_resid),   digits)
  ba_repeated$use_ar1        <- isTRUE(object$use_ar1)
  ba_repeated$ar1_rho        <- if (isTRUE(object$use_ar1)) round(num_or_na_ba(object$ar1_rho), digits) else NA_real_
  ba_repeated$ar1_estimated  <- if (isTRUE(object$use_ar1)) isTRUE(object$ar1_estimated) else NA

  attr(ba_repeated, "conf.level") <- cl
  class(ba_repeated) <- c("summary.ba_repeated","data.frame")
  ba_repeated
}

#' @method summary ba_repeated_matrix
#' @export
summary.ba_repeated_matrix <- function(object,
                                       digits = 3,
                                       ci_digits = 3,
                                       ...) {
  stopifnot(inherits(object, "ba_repeated_matrix"))
  cl <- suppressWarnings(as.numeric(attr(object, "conf.level")))
  methods <- object$methods
  m <- length(methods)

  has_ci <- all(c("mean_ci_low","mean_ci_high",
                  "loa_lower_ci_low","loa_lower_ci_high",
                  "loa_upper_ci_low","loa_upper_ci_high") %in% names(object))

  rows <- vector("list", m * (m - 1L) / 2L)
  k <- 0L
  for (i in 1:(m - 1L)) for (j in (i + 1L):m) {
    k <- k + 1L
    row <- list(
      method1 = methods[i],
      method2 = methods[j],
      bias    = round(object$bias[i, j], digits),
      sd_loa  = round(object$sd_loa[i, j], digits),
      loa_low = round(object$loa_lower[i, j], digits),
      loa_up  = round(object$loa_upper[i, j], digits),
      width   = round(object$width[i, j], digits),
      n       = suppressWarnings(as.integer(object$n[i, j])),
      sigma2_subject = round(object$sigma2_subject[i, j], digits),
      sigma2_resid   = round(object$sigma2_resid[i, j],   digits)
    )

    if (!is.null(object$slope)) {
      row$slope <- round(object$slope[i, j], digits)
    }

    if (has_ci && is.finite(cl)) {
      row$bias_lwr <- round(object$mean_ci_low[i, j],       ci_digits)
      row$bias_upr <- round(object$mean_ci_high[i, j],      ci_digits)
      row$lo_lwr   <- round(object$loa_lower_ci_low[i, j],  ci_digits)
      row$lo_upr   <- round(object$loa_lower_ci_high[i, j], ci_digits)
      row$up_lwr   <- round(object$loa_upper_ci_low[i, j],  ci_digits)
      row$up_upr   <- round(object$loa_upper_ci_high[i, j], ci_digits)
    }
    if (isTRUE(object$use_ar1)) {
      rho_ij <- if (!is.null(object$ar1_rho_pair)) object$ar1_rho_pair[i, j] else NA_real_
      est_ij <- if (!is.null(object$ar1_estimated)) isTRUE(object$ar1_estimated[i, j]) else NA
      row$ar1_rho       <- round(rho_ij, digits)
      row$ar1_estimated <- est_ij
    }
    rows[[k]] <- row
  }
  df <- do.call(rbind.data.frame, rows)
  attr(df, "conf.level") <- cl
  class(df) <- c("summary.ba_repeated_matrix", "data.frame")
  df
}


#' @method print summary.ba_repeated
#' @export
print.summary.ba_repeated <- function(x, ...) {
  cl <- suppressWarnings(as.numeric(attr(x, "conf.level")))
  if (is.finite(cl)) cat(sprintf("Bland-Altman (two methods), %g%% CI\n\n", 100*cl))
  else               cat("Bland-Altman (two methods)\n\n")
  print.data.frame(x, row.names = FALSE, right = FALSE, ...)
  invisible(x)
}

#' @method print summary.ba_repeated_matrix
#' @export
print.summary.ba_repeated_matrix <- function(x, ...) {
  cl <- suppressWarnings(as.numeric(attr(x, "conf.level")))
  if (is.finite(cl)) cat(sprintf("Bland-Altman (pairwise), %g%% CI\n\n", 100*cl))
  else               cat("Bland-Altman (pairwise)\n\n")
  print.data.frame(x, row.names = FALSE, right = FALSE, ...)
  invisible(x)
}

#-------- Plots ------------

#' @rdname bland_altman_repeated
#' @method plot ba_repeated
#' @param title Plot title (character scalar). Defaults to
#'   `"Bland-Altman (repeated measurements)"` for two methods and
#'   `"Bland-Altman (repeated, pairwise)"` for the faceted matrix plot.
#' @param subtitle Optional subtitle (character scalar). If `NULL`, a compact
#'   summary is shown using the fitted object.
#' @param point_alpha Numeric in `[0, 1]`. Transparency for scatter points
#'   drawn at (pair mean, pair difference) when point data are available.
#'   Passed to `ggplot2::geom_point(alpha = ...)`. Default `0.7`.
#'
#' @param point_size Positive numeric. Size of scatter points; passed to
#'   `ggplot2::geom_point(size = ...)`. Default `2.2`.
#'
#' @param line_size Positive numeric. Line width for horizontal bands
#'   (bias and both LoA) and, when requested, the proportional-bias line.
#'   Passed to `ggplot2::geom_hline(linewidth = ...)` (and `geom_abline`).
#'   Default `0.8`.
#'
#' @param shade_ci Logical. If `TRUE` and confidence intervals are available in
#'   the object (`CI.lines` for two methods; `*_ci_*` matrices for the pairwise
#'   case), semi-transparent rectangles are drawn to indicate CI bands for the
#'   bias and each LoA. If `FALSE`, dashed horizontal CI lines are drawn instead.
#'   Has no effect if CIs are not present. Default `TRUE`.
#'
#' @param shade_alpha Numeric in `[0, 1]`. Opacity of the CI shading
#'   rectangles when `shade_ci = TRUE`. Passed to `ggplot2::annotate("rect", alpha = ...)`.
#'   Default `0.08`.
#'
#' @param smoother One of `"none"`, `"loess"`, or `"lm"`. Adds an overlaid trend
#'   for differences vs means when points are drawn, to visualise proportional
#'   bias. `"lm"` fits a straight line with no SE ribbon; `"loess"` draws a
#'   locally-smoothed curve (span 0.9) with no SE ribbon; `"none"` draws no
#'   smoother. Ignored if `show_points = FALSE` or if no point data are available.
#'
#' @param symmetrize_y Logical (two-method plot only). If `TRUE`, the y-axis is
#'   centred at the estimated bias and expanded symmetrically to cover all
#'   elements used to compute the range (bands, CIs, and points if shown).
#'   Default `TRUE`.
#'
#' @param show_points Logical. If `TRUE`, per-pair points are drawn when present
#'   in the fitted object (two-method path) or when they can be reconstructed
#'   from `x$data_long` and `x$mapping` (pairwise path). If `FALSE` or if point
#'   data are unavailable, only the bands (and optional CI indicators) are drawn.
#'   Default `TRUE`.
#' @param ... Additional theme adjustments passed to `ggplot2::theme(...)`
#'   (e.g., `plot.title.position = "plot"`, `axis.title.x = element_text(size=11)`).
#' @importFrom graphics abline lines par rect plot
#' @export
plot.ba_repeated <- function(x,
                             title = "Bland-Altman (repeated measurements)",
                             subtitle = NULL,
                             point_alpha = 0.7,
                             point_size  = 2.2,
                             line_size   = 0.8,
                             shade_ci    = TRUE,
                             shade_alpha = 0.08,
                             smoother    = c("none", "loess", "lm"),
                             symmetrize_y = TRUE,
                             show_points = TRUE,
                             ...) {
  stopifnot(inherits(x, "ba_repeated"))
  smoother <- match.arg(smoother)

  # Prefer pre-computed points if present
  has_pts <- !is.null(x$means) && !is.null(x$diffs)
  means <- if (has_pts) as.numeric(x$means) else numeric()
  diffs <- if (has_pts) as.numeric(x$diffs) else numeric()
  if (!has_pts) show_points <- FALSE

  # Bands
  md    <- as.numeric(x$mean.diffs)
  loaL  <- as.numeric(x$lower.limit)
  loaU  <- as.numeric(x$upper.limit)
  two   <- as.numeric(x$two)
  n     <- as.integer(x$based.on)
  cl    <- suppressWarnings(as.numeric(attr(x, "conf.level")))
  ci_val <- function(nm) if (!is.null(x$CI.lines)) suppressWarnings(as.numeric(x$CI.lines[[nm]])) else NA_real_
  has_ci <- !is.null(x$CI.lines) &&
    all(is.finite(c(ci_val("mean.diff.ci.lower"), ci_val("mean.diff.ci.upper"),
                    ci_val("lower.limit.ci.lower"), ci_val("lower.limit.ci.upper"),
                    ci_val("upper.limit.ci.lower"), ci_val("upper.limit.ci.upper"))))

  if (is.null(subtitle)) {
    subtitle <- if (is.finite(cl)) {
      sprintf("pairs = %d |  mean diff = %.3g |  LoA = [%.3g, %.3g] |  %g%% CI",
              n, md, loaL, loaU, 100*cl)
    } else {
      sprintf("pairs = %d |  mean diff = %.3g |  LoA = [%.3g, %.3g]",
              n, md, loaL, loaU)
    }
  }

  # y-range
  y_bits <- c(loaL, loaU, md)
  if (has_ci) {
    y_bits <- c(y_bits,
                ci_val("mean.diff.ci.lower"), ci_val("mean.diff.ci.upper"),
                ci_val("lower.limit.ci.lower"), ci_val("lower.limit.ci.upper"),
                ci_val("upper.limit.ci.lower"), ci_val("upper.limit.ci.upper"))
  }
  if (show_points && length(diffs)) y_bits <- c(y_bits, diffs)
  y_rng <- range(y_bits, na.rm = TRUE)
  if (isTRUE(symmetrize_y) && all(is.finite(c(y_rng, md)))) {
    half <- max(abs(c(y_rng[1] - md, y_rng[2] - md)))
    y_rng <- c(md - half, md + half)
  }

  p <- ggplot2::ggplot()

  if (isTRUE(has_ci) && shade_ci) {
    p <- p +
      ggplot2::annotate("rect", xmin = -Inf, xmax = Inf,
                        ymin = ci_val("mean.diff.ci.lower"), ymax = ci_val("mean.diff.ci.upper"),
                        alpha = shade_alpha) +
      ggplot2::annotate("rect", xmin = -Inf, xmax = Inf,
                        ymin = ci_val("lower.limit.ci.lower"), ymax = ci_val("lower.limit.ci.upper"),
                        alpha = shade_alpha) +
      ggplot2::annotate("rect", xmin = -Inf, xmax = Inf,
                        ymin = ci_val("upper.limit.ci.lower"), ymax = ci_val("upper.limit.ci.upper"),
                        alpha = shade_alpha)
  } else if (isTRUE(has_ci)) {
    p <- p + ggplot2::geom_hline(yintercept = c(
      ci_val("mean.diff.ci.lower"),  ci_val("mean.diff.ci.upper"),
      ci_val("lower.limit.ci.lower"),ci_val("lower.limit.ci.upper"),
      ci_val("upper.limit.ci.lower"),ci_val("upper.limit.ci.upper")),
      linetype = "dashed"
    )
  }

  p <- p +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, linetype = "dotted", colour = "grey40") +
    ggplot2::geom_hline(yintercept = md,   linewidth = line_size) +
    ggplot2::geom_hline(yintercept = loaL, linewidth = line_size) +
    ggplot2::geom_hline(yintercept = loaU, linewidth = line_size)

  if (show_points && length(means) && length(diffs)) {
    df <- data.frame(means = means, diffs = diffs)
    p <- p + ggplot2::geom_point(data = df, ggplot2::aes(x = means, y = diffs),
                                 alpha = point_alpha, size = point_size)
    if (smoother == "lm") {
      p <- p + ggplot2::geom_smooth(data = df, ggplot2::aes(x = means, y = diffs),
                                    method = "lm", se = FALSE, linewidth = 0.7)
    } else if (smoother == "loess") {
      p <- p + ggplot2::geom_smooth(data = df, ggplot2::aes(x = means, y = diffs),
                                    method = "loess", se = FALSE, linewidth = 0.7, span = 0.9)
    }
    if (isTRUE(x$include_slope) && is.finite(x$beta_slope)) {
      p <- p + ggplot2::geom_abline(intercept = x$mean.diffs, slope = x$beta_slope, linewidth = line_size)
    }
  }

  p +
    ggplot2::coord_cartesian(ylim = y_rng, expand = TRUE) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), ...) +
    ggplot2::labs(title = title, subtitle = subtitle,
                  x = if (show_points) "Pair mean" else NULL,
                  y = "Difference (method 2 \u2212 method 1)")
}

#' @rdname bland_altman_repeated
#' @method plot ba_repeated_matrix
#' @param pairs (Faceted pairwise plot only.) Optional character vector of
#'   labels specifying which method contrasts to display. Labels must match the
#'   "row - column" convention used by `print()`/`summary()` (e.g., `"B - A"`).
#'   Defaults to all upper-triangle pairs.
#'
#' @param against (Faceted pairwise plot only.) Optional single method name.
#'   If supplied, facets are restricted to contrasts of the chosen method
#'   against all others. Ignored when `pairs` is provided.
#'
#' @param facet_scales (Faceted pairwise plot only.) Either `"free_y"` (default)
#'   to allow each facet its own y-axis limits, or `"fixed"` for a common scale
#'   across facets. Passed to `ggplot2::facet_wrap(scales = ...)`.
#' @importFrom ggplot2 ggplot annotate geom_hline geom_point geom_smooth
#' @importFrom ggplot2 coord_cartesian theme_minimal theme labs facet_wrap
#' @importFrom ggplot2 element_blank
#' @export
plot.ba_repeated_matrix <- function(
    x,
    pairs = NULL, against = NULL,
    facet_scales = c("free_y","fixed"),
    title = "Bland-Altman (repeated, pairwise)",
    point_alpha = 0.6, point_size = 1.8,
    line_size = 0.7, shade_ci = TRUE, shade_alpha = 0.08,
    smoother = c("none","loess","lm"),
    show_points = TRUE,
    ...
) {
  stopifnot(inherits(x, "ba_repeated_matrix"))
  facet_scales <- match.arg(facet_scales)
  smoother <- match.arg(smoother)
  methods <- x$methods
  m <- length(methods)
  lab_pair <- function(i,j) paste(methods[i], "\u2212", methods[j])

  idx_upper <- which(upper.tri(matrix(NA_real_, m, m)), arr.ind = TRUE)
  all_pairs <- data.frame(
    j   = idx_upper[,1],
    k   = idx_upper[,2],
    lab = lab_pair(idx_upper[,1], idx_upper[,2]),
    stringsAsFactors = FALSE
  )

  if (!is.null(against)) {
    if (!against %in% methods)
      stop("`against` must be one of: ", paste(methods, collapse=", "))
    js <- match(against, methods)
    all_pairs <- subset(all_pairs, j == js | k == js)
  } else if (!is.null(pairs)) {
    all_pairs <- subset(all_pairs, lab %in% pairs)
    if (!nrow(all_pairs)) stop("None of requested `pairs` matched.")
  }
  pairs_order <- all_pairs$lab

  bands <- data.frame(
    pair = lab_pair(idx_upper[,1], idx_upper[,2]),
    md   = as.vector(x$bias[idx_upper]),
    loaL = as.vector(x$loa_lower[idx_upper]),
    loaU = as.vector(x$loa_upper[idx_upper]),
    md_l = as.vector(x$mean_ci_low[idx_upper]),
    md_u = as.vector(x$mean_ci_high[idx_upper]),
    lo_l = as.vector(x$loa_lower_ci_low[idx_upper]),
    lo_u = as.vector(x$loa_lower_ci_high[idx_upper]),
    hi_l = as.vector(x$loa_upper_ci_low[idx_upper]),
    hi_u = as.vector(x$loa_upper_ci_high[idx_upper]),
    stringsAsFactors = FALSE
  )
  bands <- bands[bands$pair %in% pairs_order, , drop = FALSE]
  bands$pair <- factor(bands$pair, levels = pairs_order)

  has_ci <- with(bands, all(is.finite(c(md_l, md_u, lo_l, lo_u, hi_l, hi_u))))

  # Build point data from stored mapping (no hard-coded names)
  pts <- NULL
  if (isTRUE(show_points) && !is.null(x$data_long) && !is.null(x$mapping)) {
    df  <- as.data.frame(x$data_long, check.names = FALSE)
    map <- x$mapping
    mY <- .resolve_map_col_ba(df, map, "response")
    mS <- .resolve_map_col_ba(df, map, "subject")
    mM <- .resolve_map_col_ba(df, map, "method")
    mT <- .resolve_map_col_ba(df, map, "time")

    build_panel <- function(lev_j, lev_k, lab) {
      a <- df[df[[mM]] == lev_j, c(mS, mT, mY)]
      b <- df[df[[mM]] == lev_k, c(mS, mT, mY)]
      names(a) <- c("subject","time","yA"); names(b) <- c("subject","time","yB")
      ab <- merge(a, b, by = c("subject","time"), all = FALSE)
      if (!nrow(ab)) return(NULL)
      data.frame(pair = lab,
                 means = (ab$yA + ab$yB)/2,
                 diffs =  ab$yB - ab$yA,
                 stringsAsFactors = FALSE)
    }

    pts <- do.call(rbind, lapply(seq_len(nrow(all_pairs)), function(i) {
      build_panel(methods[all_pairs$j[i]], methods[all_pairs$k[i]], all_pairs$lab[i])
    }))
    if (!is.null(pts) && nrow(pts)) {
      pts$pair <- factor(pts$pair, levels = pairs_order)
    } else {
      pts <- NULL
    }
  }

  p <- ggplot2::ggplot() + ggplot2::facet_wrap(~ pair, scales = facet_scales)

  # Ensure y-scale trained even without points
  p <- p +
    ggplot2::geom_blank(data = bands, ggplot2::aes(x = 0, y = md))   +
    ggplot2::geom_blank(data = bands, ggplot2::aes(x = 0, y = loaL)) +
    ggplot2::geom_blank(data = bands, ggplot2::aes(x = 0, y = loaU))

  if (has_ci) {
    if (shade_ci) {
      p <- p +
        ggplot2::geom_rect(data = bands, ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = md_l, ymax = md_u),
                           inherit.aes = FALSE, alpha = shade_alpha) +
        ggplot2::geom_rect(data = bands, ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = lo_l, ymax = lo_u),
                           inherit.aes = FALSE, alpha = shade_alpha) +
        ggplot2::geom_rect(data = bands, ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = hi_l, ymax = hi_u),
                           inherit.aes = FALSE, alpha = shade_alpha)
    } else {
      p <- p +
        ggplot2::geom_hline(data = bands, ggplot2::aes(yintercept = md_l)) +
        ggplot2::geom_hline(data = bands, ggplot2::aes(yintercept = md_u)) +
        ggplot2::geom_hline(data = bands, ggplot2::aes(yintercept = lo_l)) +
        ggplot2::geom_hline(data = bands, ggplot2::aes(yintercept = lo_u)) +
        ggplot2::geom_hline(data = bands, ggplot2::aes(yintercept = hi_l)) +
        ggplot2::geom_hline(data = bands, ggplot2::aes(yintercept = hi_u))
    }
  }

  p <- p +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.35, linetype = "dotted", colour = "grey40") +
    ggplot2::geom_hline(data = bands, ggplot2::aes(yintercept = md),   linewidth = line_size) +
    ggplot2::geom_hline(data = bands, ggplot2::aes(yintercept = loaL), linewidth = line_size) +
    ggplot2::geom_hline(data = bands, ggplot2::aes(yintercept = loaU), linewidth = line_size)

  if (!is.null(pts)) {
    p <- p + ggplot2::geom_point(data = pts, ggplot2::aes(x = means, y = diffs),
                                 alpha = point_alpha, size = point_size)
    if (smoother == "lm") {
      p <- p + ggplot2::geom_smooth(data = pts, ggplot2::aes(x = means, y = diffs),
                                    method = "lm", se = FALSE, linewidth = 0.7)
    } else if (smoother == "loess") {
      p <- p + ggplot2::geom_smooth(data = pts, ggplot2::aes(x = means, y = diffs),
                                    method = "loess", se = FALSE, linewidth = 0.7, span = 0.9)
    }
  }

  p +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), ...) +
    ggplot2::labs(title = title,
                  x = if (!is.null(pts)) "Pair mean" else NULL,
                  y = "Difference (row \u2212 column)")
}

#' @keywords internal
num_or_na_ba <- function(x) {
  y <- suppressWarnings(as.numeric(x))
  y[!is.finite(y)] <- NA_real_
  y
}

#' @keywords internal
.resolve_map_col_ba <- function(df, mapping, key) {
  candidate <- mapping[[key]] %||% switch(key,
                                          response = ".response",
                                          subject  = ".subject",
                                          method   = ".method",
                                          time     = ".time"
  )
  if (candidate %in% names(df)) return(candidate)

  canonical <- switch(key,
                      response = ".response",
                      subject  = ".subject",
                      method   = ".method",
                      time     = ".time"
  )
  if (canonical %in% names(df)) return(canonical)

  stop("Column for '", key, "' not found in stored data_long.")
}

#' @keywords internal
.make_named_matrix_ba <- function(methods, fill = NA_real_, storage = "double") {
  m <- length(methods)
  out <- matrix(fill, m, m, dimnames = list(methods, methods))
  storage.mode(out) <- storage
  out
}
