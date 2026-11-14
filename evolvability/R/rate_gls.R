#' Generalized least squares rate model
#'
#' \code{rate_gls} fits a generalized least squares model to estimate parameters
#' of an evolutionary model of two traits x and y, where the evolutionary rate
#' of y depends on the value of x. Three models are implemented. In the two
#' first, 'predictor_BM' and 'predictor_gBM', the evolution of y follows a
#' Brownian motion with variance linear in x, while the evolution of x either
#' follows a Brownian motion or a geometric Brownian motion, respectively. In
#' the third model, 'recent_evol', the residuals of the macroevolutionary
#' predictions of y have variance linear in x. It is highly recommended  to read
#' Hansen et al. (in review) and \code{vignette("Analyzing_rates_of_evolution")}
#' before fitting these models.
#'
#' @param x The explanatory variable, which must be equal to the length of
#'   \code{y} and tips on the tree.
#' @param y The trait values of response variable. Note that the algorithm mean
#'   centers y.
#' @param species A vector with the names of the species, must be equal in
#'   length and in the same order as \code{x} and \code{y}.
#' @param tree An object of class \code{\link[ape]{phylo}}, needs to be ultrametric
#'   and with total length of unit, tips must have the same names as in
#'   \code{species}.
#' @param model The acronym of the evolutionary model to be fitted. There are
#'   three options: 'predictor_BM', 'predictor_gBM' or 'recent_evol' (see
#'   details).
#' @param startv A vector of optional starting values for the a and b
#'   parameters.
#' @param maxiter The maximum number of iterations for updating the GLS.
#' @param silent logical: if the function should not print the generalized sum
#'   of squares for each iteration.
#' @param useLFO logical: whether the focal species should be left out when
#'   calculating the corresponding species' means. Note that this is only
#'   relevant for the 'recent_evol' model. The most correct is to use
#'   \code{TRUE}, but in practice it has little effect and \code{FALSE} will
#'   speed up the model fit (particularly useful when bootstrapping). LFO is an
#'   acronym for 'Leave the Focal species Out'.
#' @param tol tolerance for convergence. If the change in 'a' and 'b' is below
#'   this limit between the two last iteration, convergence is reached. The
#'   change is measured in proportion to the standard deviation of the response
#'   for 'a' and the ratio of the standard deviation of the response to the
#'   standard deviation of the predictor for 'b'.
#' @details \code{rate_gls} is an iterative generalized least squares (GLS)
#'   model fitting a regression where the response variable is a vector of
#'   squared mean-centered \code{y}-values for the 'predictor_BM' and
#'   'predictor_gBM' models and squared deviation from the evolutionary
#'   predictions (see \code{\link{macro_pred}}) for the 'recent_evol' model.
#'   Note that the algorithm mean centers \code{x} in the 'predictor_BM' and
#'   'recent_evol' analyses, while it mean standardized \code{x} (i.e. divided
#'   \code{x} by its mean) in the 'predictor_gBM'. The evolutionary parameters a
#'   and b are inferred from the intercept and the slope of the GLS fit. Again,
#'   it is highly recommended to read Hansen et al. (in review) and
#'   \code{vignette("Analyzing_rates_of_evolution")} before fitting these
#'   models. In Hansen et al. (2021) the three models 'predictor_BM',
#'   'predictor_gBM' and 'recent_evol' are referred to as 'Model 1', 'Model 2'
#'   and 'Model 3', respectively.
#' @return An object of \code{class} \code{'rate_gls'}, which is a list with the
#'   following components: \tabular{llllll}{
#'   \code{model} \tab\tab\tab\tab The name of the model ('predictor_BM',
#'   'predictor_gBM' or 'recent_evolution').
#'   \cr \code{param} \tab\tab\tab\tab The focal parameter estimates and their
#'   standard errors, where 'a' and 'b' are parameters of the evolutionary
#'   models, and 'sigma(x)^2' is the BM-rate parameter of x for the
#'   'predictor_BM' model, the BM-rate parameter for log x for the
#'   'predictor_gBM' model, and the variance of x for the 'recent_evolution'
#'   model.
#'   \cr \code{Rsquared} \tab\tab\tab\tab The generalized R squared of the GLS
#'   model fit.
#'   \cr \code{a_all_iterations} \tab\tab\tab\tab The values for the parameter a
#'   in all iterations.
#'   \cr \code{b_all_iterations} \tab\tab\tab\tab The values for the parameter b
#'   in all iterations.
#'   \cr \code{R} \tab\tab\tab\tab The residual variance matrix.
#'   \cr \code{Beta} \tab\tab\tab\tab The intercept and slope of GLS regression
#'   (response is y2 and explanatory variable is x).
#'   \cr \code{Beta_vcov} \tab\tab\tab\tab The error variance matrix of
#'   \code{Beta}.
#'   \cr \code{tree} \tab\tab\tab\tab The phylogenetic tree.
#'   \cr \code{data} \tab\tab\tab\tab The data used in the GLS regression.
#'   \cr \code{convergence} \tab\tab\tab\tab Whether the algorithm converged or
#'   not.
#'   \cr \code{additional_param} \tab\tab\tab\tab Some additional parameter
#'   estimates.
#'   \cr \code{call} \tab\tab\tab\tab The function call.
#'   }
#'
#'
#' @references Hansen TF, Bolstad GH, Tsuboi M. 2021. Analyzing disparity and rates 
#' of morphological evolution with model-based phylogenetic comparative methods. 
#' *Systematic Biology*. syab079. doi:10.1093/sysbio/syab079
#' @author Geir H. Bolstad
#' @examples
#' # Also see vignette("Analyzing_rates_of_evolution").
#' \dontrun{
#' # Generating a tree with 500 species
#' set.seed(102)
#' tree <- ape::rtree(n = 500)
#' tree <- ape::chronopl(tree, lambda = 1, age.min = 1)
#'
#' ### model = 'predictor_BM' ###
#' sim_data <- simulate_rate(tree,
#'   startv_x = 0, sigma_x = 0.25, a = 1, b = 1, model =
#'     "predictor_BM"
#' )
#' head(sim_data$tips)
#' gls_mod <- rate_gls(
#'   x = sim_data$tips$x, y = sim_data$tips$y,
#'   species = sim_data$tips$species, tree, model = "predictor_BM"
#' )
#' gls_mod$param
#' par(mfrow = c(1, 2))
#' # Response shown on the standard deviation scale (default):
#' plot(gls_mod, scale = "SD", cex.legend = 0.8)
#' # Response shown on the variance scale, where the regression is linear:
#' plot(gls_mod, scale = "VAR", cex.legend = 0.8)
#' par(mfrow = c(1, 1))
#' # Parametric bootstrapping to get the uncertainty of the parameter estimates
#' # taking the complete process into account.
#' # (this takes some minutes)
#' gls_mod_boot <- rate_gls_boot(gls_mod, n = 1000)
#' gls_mod_boot$summary
#'
#' ### model = 'predictor_gBM' ###
#' sim_data <- simulate_rate(tree,
#'   startv_x = 1, sigma_x = 1, a = 1, b = 1,
#'   model = "predictor_gBM"
#' )
#' head(sim_data$tips)
#' gls_mod <- rate_gls(
#'   x = sim_data$tips$x, y = sim_data$tips$y, species = sim_data$tips$species,
#'   tree, model = "predictor_gBM"
#' )
#' gls_mod$param
#' plot(gls_mod)
#' par(mfrow = c(1, 2))
#' # Response shown on the standard deviation scale (default):
#' plot(gls_mod, scale = "SD", cex.legend = 0.8)
#' # Response shown on the variance scale, where the regression is linear:
#' plot(gls_mod, scale = "VAR", cex.legend = 0.8)
#' # is linear.
#' par(mfrow = c(1, 1))
#'
#' # Parametric bootstrapping to get the uncertainty of the parameter estimates
#' # taking the complete process into account. (This takes some minutes.)
#' gls_mod_boot <- rate_gls_boot(gls_mod, n = 1000)
#' gls_mod_boot$summary
#'
#' ### model = 'recent_evol' ###
#' sim_data <- simulate_rate(tree,
#'   startv_x = 0, sigma_x = 1, a = 1, b = 1, sigma_y = 1,
#'   model = "recent_evol"
#' )
#' head(sim_data$tips)
#' gls_mod <- rate_gls(
#'   x = sim_data$tips$x, y = sim_data$tips$y, species = sim_data$tips$species,
#'   tree, model = "recent_evol", useLFO = FALSE
#' )
#' # useLFO = TRUE is somewhat slower, and although more correct it should give
#' # very similar estimates in most situations.
#' gls_mod$param
#' par(mfrow = c(1, 2))
#' # Response shown on the standard deviation scale (default):
#' plot(gls_mod, scale = "SD", cex.legend = 0.8)
#' # Response shown on the variance scale, where the regression is linear:
#' plot(gls_mod, scale = "VAR", cex.legend = 0.8)
#' # linear.
#' par(mfrow = c(1, 1))
#'
#' # Parametric bootstrapping to get the uncertainty of the parameter estimates
#' # taking the complete process into account. Note that x is considered as
#' # fixed effect. (This takes a long time.)
#' gls_mod_boot <- rate_gls_boot(gls_mod, n = 1000, useLFO = FALSE)
#' gls_mod_boot$summary
#' }
#' @importFrom ape vcv
#' @importFrom lme4 VarCorr
#' @importFrom Matrix Matrix solve Diagonal rowSums
#' @importFrom stats coef lm lm.fit runif
#' @export
rate_gls <-
  function(x,
           y,
           species,
           tree,
           model = "predictor_BM",
           startv = list(a = NULL,
                         b = NULL
                         ),
           maxiter = 100,
           silent = FALSE,
           useLFO = TRUE,
           tol = 0.001
          ) {

    #### Phylogenetic relatedness matrix ####
    if (!ape::is.ultrametric(tree)) {
      stop("The tree is not ultrametric")
    }
    A <- ape::vcv(tree)
    if (round(A[1, 1], 5) != 1) {
      stop("The tree is not standardized to unit length")
    }
    A <- A[match(species, colnames(A)), match(species, colnames(A))]
    I <- diag(nrow(A))

    #### storing original y and x values ####
    y_original <- y
    x_original <- x

    #### y-variable ####
    X <- matrix(rep(1, length(y)), ncol = 1) # design matrix
    mod <- GLS(y, X, R = A)$coef
    mean_y <- mod[1]
    Vy <- mod[2]^2

    # mean centering
    y <- y - c(mean_y)

    #### x-variable ####
    if (model == "predictor_BM") {
      mod <- GLS(x, X, A)
      mean_x <- mod$coef[1]
      Vx <- mod$coef[2]^2
      s2 <- mod$sigma2
      x <- x - c(mean_x)
      AoA <- A * A
      x <- c(x - (1 / 2) * AoA %*% solve(A, x))
      # solve(A, x) equals solve(A)%*%x, but it is numerically more stable (and
      # faster).
    }
    if (model == "predictor_gBM") {
      mod <- GLS(log(x), X, A)
      Vx <- mod$coef[2]^2
      mean_x <- exp(mod$coef[1])
      s2 <- mod$sigma2
      Vs2 <- 2 * (s2^2) / (length(x) + 2) # var(s2)
      x <- x / c(mean_x) - exp((s2 - Vx) / 2) # The centering is done on the
      # theoretical mean

      inv_s2 <- 1 / s2
      inv_s4 <- 1 / s2^2
      e_32s2A <- exp(3 / 2 * s2 * A) - 1
      e_s2A <- exp(s2 * A) - 1
      x <- c(2 * inv_s2 *
        (x - (2 / 3) * exp(-s2 / 2) * e_32s2A %*% solve(e_s2A, x)))

      # # Hack to correct for measurement error in s2, need to comment out the
      # # calculation of x above
      # e_32s2A <- exp(3/2 * s2 * A)
      # e_s2A <-   exp(s2 * A)
      # solve_e_s2A <- solve(e_s2A-1)
      # H <- (2/s2^3)*I - (2/3)*exp(-s2/2)*inv_s2*
      #   ((9/4) * A * A * e_32s2A - 3 * (0.5 + inv_s2) * A * e_32s2A +
      #      (0.25 + inv_s2 + 2 * inv_s4) * (e_32s2A - 1) -
      #      (e_32s2A - 1) %*% solve_e_s2A %*% (A * A * e_s2A) +
      #      2 * (0.5 + inv_s2) * (e_32s2A - 1) %*% solve_e_s2A %*%
      #      (A * e_s2A) - 3 * (A * e_32s2A) %*% solve_e_s2A %*% (A * e_s2A) +
      #      2 * (e_32s2A - 1) %*% solve_e_s2A %*% (A * e_s2A) %*%
      #      solve_e_s2A %*% (A * e_s2A)) %*% solve_e_s2A
      # # x <- c(2/s2 * x - (4/3)*exp(-s2/2)/s2*(e_32s2A-1) %*%
      # # solve(e_s2A-1, x) + Vs2*H%*%x)
      # x <- c((2/s2 * I - (4/3) * exp(-s2/2)/s2 * (e_32s2A - 1) %*%
      #         solve_e_s2A + Vs2 * H) %*% x)
      # e_32s2A <- e_32s2A - 1
      # e_s2A <-  e_s2A - 1
    }
    if (model == "recent_evol") {
      mean_x <- mean(x)
      s2 <- var(x)
      Vx <- s2 / length(x)
      x <- x - c(mean_x)
    }
    s2 <- c(s2)
    s2_SE <- sqrt(2 * (s2^2) / (length(x) + 2)) # from Lynch and Walsh 1998 eq.
    # A1.10c

    #### Internal functions ####
    if (model == "predictor_BM") {
      a_func <- function(Beta) Beta[1, 1] + Vy
      b_func <- function(Beta) Beta[2, 1]
      Q <- (A * A * A) - (1 / 4) * AoA %*% solve(A, AoA) # outside function
      # to avoid repeating this in the loop
      R_func <- function(a, b) {
        4 * a * Vy * A +
          2 * (a^2 + b^2 * Vx) * AoA +
          b^2 * s2 * Q
      }
      a_SE_func <- function(Beta_vcov) sqrt(Beta_vcov[1, 1])
      b_SE_func <- function(Beta_vcov) sqrt(Beta_vcov[2, 2])
    }
    if (model == "predictor_gBM") {
      b_func <- function(Beta) Beta[2, 1]
      # Hack to ensure a positive a-parameter:
      # b_func <- function(Beta){
      #   if((Beta[1, 1] + Vy - c(2*Beta[2, 1]*exp(Vx/2)*
      #     (exp(s2/2)-1)*inv_s2))<0){
      #     return((Beta[1, 1] + Vy)/c(2*exp(Vx/2)*(exp(s2/2)-1)*inv_s2))
      #   } else{
      #     return(Beta[2, 1])
      #   }
      # }

      a_func <- function(Beta) {
        a <- Beta[1, 1] + Vy - 
          c(2 * b_func(Beta) * exp(Vx / 2) * (exp(s2 / 2) - 1) * inv_s2)
        return(a)
      }
      # Hack to correct for error in s2:
      # a_func <- function(Beta){
      #  a <- Beta[1, 1] + Vy - c(2*Beta[2, 1]*exp(Vx/2)*((exp(s2/2)-1)*inv_s2 +
      #               Vs2*((s2^2 - 4*s2 + 8)*exp(s2/2)-8)/(8*s2^3)))
      #  return(a)
      # }

      # Some matrix calculations to avoid repeating them in the loop:
      AoA <- A * A
      e_hlfVx <- exp(Vx / 2)
      e_2Vx <- exp(2 * Vx)
      e_hlfs2A <- exp(s2 / 2 * A) - 1
      e_2s2A <- exp(2 * s2 * A) - 1
      Q1 <- 8 * e_hlfVx * inv_s2 * A * e_hlfs2A
      Q2 <- 2 * e_2Vx * inv_s4 *
        (8 / 3 * (e_2s2A - e_hlfs2A) - e_2s2A - 8 / 9 * e_32s2A %*%
          solve(e_s2A, e_32s2A))

      R_func <- function(a, b) {
        if (a < 0) a <- 0 # forces a to be 0 or positive
        R <- 4 * a * Vy * A +
          8 * b * Vy * e_hlfVx * inv_s2 * e_hlfs2A +
          2 * a^2 * AoA +
          a * b * Q1 +
          b^2 * Q2
        e <- eigen(R)
        if (any(e$values < 1e-8)) {
          e$values[e$values < 1e-8] <- 1e-8 # ensures a positive definite R
          R <- e$vectors %*% diag(e$values) %*% t(e$vectors)
        }
        return(R)
      }
      a_SE_func <- function(Beta_vcov) {
        bb <- c(2 * exp(Vx / 2) * (exp(s2 / 2) - 1) * inv_s2)
        sqrt(Beta_vcov[1, 1] + bb^2 * Beta_vcov[2, 2] - 2 * bb * Beta_vcov[1, 2])
      }
      b_SE_func <- function(Beta_vcov) sqrt(Beta_vcov[2, 2])
    }
    if (model == "recent_evol") {
      a_func <- function(Beta) Beta[1, 1]
      b_func <- function(Beta) Beta[2, 1]
      R_func <- function(a, b) {
        R <- 2 * (Q * Q)
        e <- eigen(R)
        if (any(e$values < 1e-8)) {
          e$values[e$values < 1e-8] <- 1e-8 # ensures a positive definite R
          R <- e$vectors %*% diag(e$values) %*% t(e$vectors)
        }
        return(R)
      }
      a_SE_func <- function(Beta_vcov) sqrt(Beta_vcov[1, 1])
      b_SE_func <- function(Beta_vcov) sqrt(Beta_vcov[2, 2])
    }


    # ~~~~~~~~~~~~~~~~~~~~~# The GLS model #### ~~~~~~~~~~~~~~~~~~~~~#

    # GLS design matrix
    X <- as.matrix(cbind(rep(1, length(x)), x))

    #### Response variable and initial values ####
    a <- startv$a
    b <- startv$b

    if (model == "predictor_BM" | model == "predictor_gBM") {
      y2 <- y^2 # The response variable
      # finding starting values when not specified
      if (is.null(a) | is.null(b)) {
        coef_lm <- coef(lm(y2 ~ 1))
        if (is.null(a)) {
          a <- coef_lm[1]
        }
        if (is.null(b)) {
          b <- 0
        }
      }
      R <- matrix(nrow = nrow(A), ncol = ncol(A))
    } else {
      # model == 'recent_evol'
      mod_Almer <- 
        Almer(y ~ 1 + (1 | species),
          A = list(species = Matrix::Matrix(A, sparse = TRUE)),
          control = lme4::lmerControl(
            check.nobs.vs.nlev = "ignore",
            check.nobs.vs.rankZ = "ignore", check.nobs.vs.nRE = "ignore",
            optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)
          )
        )
      coef_Almer <- coef(summary(mod_Almer))
      y <- y - coef_Almer[1, 1] # centering on mean of y
      Vy <- coef_Almer[1, 2]^2 # error variance in mean of y
      sigma2_y <- lme4::VarCorr(mod_Almer)$species[1] # species variance
      if (is.null(a)) {
        residvar <- attr(lme4::VarCorr(mod_Almer), "sc")^2 # residual variance
        a <- residvar
      }
      if (is.null(b)) {
        b <- 0
      }
    }
    a_start <- a
    b_start <- b

    #### Iterative GLS ####
    starting_values_used <- c(1, rep(0, maxiter - 1))
    convergence_score <- c()
    for (i in 1:maxiter) {
      if (model == "recent_evol") {
        diag_V_micro <- a_start + b_start * (x_original - c(mean_x))
        diag_V_micro[diag_V_micro < 1e-8] <- 1e-8 # Effectively zero or
        # negative variances are replaced by a small value.
        V_micro <- diag(c(diag_V_micro))
        V <- sigma2_y * A + V_micro
        e <- eigen(V)
        if (any(e$values < 1e-8)) {
          e$values[e$values < 1e-8] <- 1e-8 # Ensures a positive definite V.
          V <- e$vectors %*% diag(e$values) %*% t(e$vectors)
        }
        Vinv <- chol2inv(chol(V))
        dVinv <- diag(x = diag(Vinv))
        inv_dVinv <- diag(x = 1 / diag(Vinv))
        U1 <- inv_dVinv %*% Vinv
        U2 <- Vinv %*% inv_dVinv
        Q <- U1 %*% inv_dVinv
        UU <- U1 * U1
        x <- c(UU %*% (x_original - c(mean_x)))
        y_predicted <- macro_pred(y = y, V = V, useLFO = useLFO)
        y2 <- (y - y_predicted)^2 - sigma2_y * diag(U1 %*% A %*% U2)
        X <- as.matrix(cbind(diag(U1 %*% U2), x))
      }

      # Residual variance matrix
      R <- R_func(a = a_start, b = b_start)

      # GLS estimates
      mod <- GLS(y = y2, X, R, coef_only = TRUE)
      Beta <- cbind(mod$coef)
      a[i + 1] <- a_func(Beta)
      b[i + 1] <- b_func(Beta)

      # Convergence & verbose
      a_diff <- abs(a[i + 1] - a[i]) / sd(y2)
      b_diff <- abs(b[i + 1] - b[i]) / (sd(y2) / sd(x))
      convergence_score[i] <- max(a_diff, b_diff)

      if (!silent) {
        print(paste0(
          "i=", i, "; a=", a[i + 1], "; b=", b[i + 1],
          "; converg score=", convergence_score[i]
        ))
      }

      if (convergence_score[i] < tol &
        !is.na(convergence_score[i]) &
        starting_values_used[i] == 0
      ) {
        break()
      }

      # Starting value for next iteration
      a_start <- a[i + 1]
      b_start <- b[i + 1]

      # Provides new starting values for a and b if b fluctuates between states
      if (i %in% seq.int(maxiter * 0.2, maxiter * 0.7, by = 10)) {
        diff_1 <- abs(b[i + 1] - b[i])
        diff_2 <- abs(b[i + 1] - b[i - 1])
        diff_3 <- abs(b[i + 1] - b[i - 2])
        diff_4 <- abs(b[i + 1] - b[i - 3])
        if (min(c(diff_2, diff_3, diff_4)) < diff_1 * 0.8) {
          a_start <- runif(1, min(a[(i - 2):(i + 1)]), max(a[(i - 2):(i + 1)]))
          b_start <- runif(1, min(b[i:((i - 2) + 1)]), max(b[(i - 2):(i + 1)]))
          starting_values_used[i + 1] <- 1
        }
      }
    }
    mod <- GLS(y2, X, R)
    Beta_vcov <- mod$coef_vcov
    param <- 
      cbind(rbind(a[i + 1], b[i + 1], s2),
            rbind(a_SE_func(Beta_vcov), b_SE_func(Beta_vcov), s2_SE)
            )
    colnames(param) <- c("Estimate", "SE")
    rownames(param) <- c("a", "b", "sigma(x)^2")
    intercept_plot <- mod$coef[1, 1]
    Rsquared <- mod$R2
    if (model == "recent_evol") {
      y2 <- (y - y_predicted)^2
      intercept_plot <- 
        a[i + 1] * mean(diag(U1 %*% U2)) + sigma2_y * mean(diag(U1 %*% A %*% U2))
      param <- rbind(param, cbind(sigma2_y, NA))
      rownames(param)[4] <- "sigma(y)^2"
    }
    if (i == maxiter) {
      warning("Model reached maximum number of iterations without convergence")
      convergence <- "No convergence"
    } else {
      convergence <- "Convergence"
    }
    report <- 
      list(model = model, 
           param = param, 
           Rsquared = Rsquared,
           a_all_iterations = a, 
           b_all_iterations = b, 
           R = R, 
           Beta = Beta,
           Beta_vcov = Beta_vcov, 
           tree = tree,
           data = 
             list(y2 = y2, 
                  x = x, 
                  y = y, 
                  x_original = x_original,
                  y_original = y_original
                  ),
           convergence = convergence,
           additional_param = 
             c(mean_y = mean_y, 
               Vy = Vy, 
               mean_x = mean_x,
               Vx = Vx, 
               intercept_plot = intercept_plot
               )
           )
    class(report) <- "rate_gls"
    report$call <- match.call()
    report
  }


#' Plot of rate_gls object
#'
#' \code{plot} method for class \code{'rate_gls'}.
#'
#' @param x An object of class \code{'rate_gls'}.
#' @param scale The scale of the y-axis, either the variance scale ('VAR'), that
#'   is y^2, or the standard deviation scale ('SD'), that is abs(y).
#' @param print_param logical: if parameter estimates should be printed in the
#'   plot or not.
#' @param digits_param The number of significant digits displayed for the
#'   parameters in the plots.
#' @param digits_rsquared The number of decimal places displayed for the
#'   r-squared.
#' @param main as in \code{\link{plot}}.
#' @param xlab as in \code{\link{plot}}.
#' @param ylab as in \code{\link{plot}}.
#' @param col as in \code{\link{plot}}.
#' @param cex.legend A character expansion factor relative to current par("cex")
#'   for the printed parameters.
#' @param ... Additional arguments passed to \code{\link{plot}}.
#' @details Plots the gls rate regression fitted by the \code{\link{rate_gls}}
#'   function. The regression line gives the expected variance or standard
#'   deviation (depending on scale). The regression is linear on the variance
#'   scale.
#' @return \code{plot} returns a plot of the gls rate regression
#' @examples
#' # See the vignette 'Analyzing rates of evolution'.
#' @author Geir H. Bolstad
#' @importFrom graphics plot lines legend
#' @export
plot.rate_gls <- 
  function(x, 
           scale = "SD", 
           print_param = TRUE, 
           digits_param = 2,
           digits_rsquared = 1, 
           main = "GLS regression",
           xlab = "x", 
           ylab = "Response", 
           col = "grey",
           cex.legend = 1, 
           ...
   ) {
  mod <- x
  x <- seq(min(mod$data$x), max(mod$data$x), length.out = 100)
  y <- mod$additional_param["intercept_plot"] + mod$Beta[2] * x
  if (scale == "SD") {
    y <- try(sqrt(y))
  }
  if (scale == "VAR") {
    plot(mod$data$x, mod$data$y2,
      main = main, xlab = xlab, ylab = ylab,
      col = col, ...)
  }
  if (scale == "SD") {
    plot(mod$data$x, sqrt(mod$data$y2),
      main = main, xlab = xlab, ylab = ylab,
      col = col, ...)
  }
  lines(x, y)
  if (print_param) {
    n_decimals <- function(x){
      n <- nchar(
        strsplit(
          x = round_and_format(x, sign_digits = digits_param), 
          split = ".", 
          fixed = TRUE
        )[[1]][2]
      )
      if (is.na(n)) n <- 0
      return(n)
    }
    n1 <- min(n_decimals(mod$param["a", 1]), n_decimals(mod$param["a", 2]))
    n2 <- min(n_decimals(mod$param["b", 1]), n_decimals(mod$param["b", 2]))
    legend(
      "topleft",
      legend = 
        c(as.expression(bquote(
          italic(a) == .(round_and_format(mod$param["a", 1], digits = n1)) 
            ~ "\u00B1" ~ .(round_and_format(mod$param["a", 2], digits = n1))
            ~ ~ ~ 
          italic(b) == .(round_and_format(mod$param["b", 1], digits = n2))
            ~ "\u00B1" ~ .(round_and_format(mod$param["b", 2], digits = n2)))), 
          as.expression(bquote(italic(R)^2 == .(round_and_format(100 * 
            mod$Rsquared, digits_rsquared)) ~ "%"))
          ), 
      box.lty = 0, 
      bg = "transparent", 
      xjust = 0, 
      cex = cex.legend
      )
  }
}

#' Simulate responses from \code{\link{rate_gls}} fit
#'
#' \code{rate_gls_sim} responses from the models defined by an object of class
#' \code{'rate_gls'}.
#'
#' @param object The fitted object from \code{\link{rate_gls}}
#' @param nsim The number of simulations.
#' @details \code{rate_gls_sim} simply passes the estimates in an object of
#'   class \code{'rate_gls'} to the function \code{\link{simulate_rate}} for
#'   simulating responses of the evolutionary process. It is mainly intended for
#'   internal use in \code{\link{rate_gls_boot}}.
#' @return An object of \code{class} \code{'simulate_rate'}, which is a list
#'   with the following components: \tabular{llllll}{ \code{tips}
#'   \tab\tab\tab\tab A data frame of x and y values for the tips. \cr
#'   \code{percent_negative_roots} \tab\tab\tab\tab The percent of iterations
#'   with negative roots in the rates of y (not given for model =
#'   'recent_evol'). \cr \code{compl_dynamics}  \tab\tab\tab\tab A list with the
#'   output of the complete dynamics (not given for model = 'recent_evol'). }
#' @author Geir H. Bolstad
#' @examples
#' # See the vignette 'Analyzing rates of evolution'.
#' @export
rate_gls_sim <- function(object, nsim = 10) {
  sim_out <- list()
  if (object$model == "recent_evol") {
    if (object$param["sigma(y)^2", 1] == 0) {
      object$param["sigma(y)^2", 1] <- 1e-16
    }
  }
  for (i in 1:nsim) {
    sim_out[[i]] <- 
      try(simulate_rate(tree = object$tree, 
                        startv_x = ifelse(object$model == "predictor_gBM", 1, 0),
                        sigma_x = sqrt(object$param["sigma(x)^2", 1]),
                        a = object$param["a", 1], 
                        b = object$param["b", 1], 
                        x = ifelse(rep(object$model == "recent_evol", 
                          length(object$data$x)), c(object$data$x), NULL), 
                        sigma_y = ifelse(object$model == "recent_evol", 
                           sqrt(object$param["sigma(y)^2", 1]), NULL), 
                        model = object$model
            ),
          silent = TRUE
        )
  }
  return(sim_out)
}

#' Bootstrap of the \code{\link{rate_gls}} model fit
#'
#' \code{rate_gls_boot} performs  a parametric bootstrap of a
#' \code{\link{rate_gls}} model fit.
#'
#' @param object The output from \code{\link{rate_gls}}.
#' @param n The number of bootstrap samples
#' @param useLFO logical: when calculating the mean vector of the traits in the
#'   'recent_evol' analysis, should the focal species be left out when
#'   calculating the corresponding species' mean. The correct way is to use
#'   TRUE, but in practice it has little effect and FALSE will speed up the
#'   model fit (particularly useful when bootstrapping).
#' @param silent logical: whether or not the bootstrap iterations should be
#'   printed.
#' @param maxiter The maximum number of iterations for updating the GLS.
#' @param tol tolerance for convergence. If the change in 'a' and 'b' is below
#'   this limit between the two last iteration, convergence is reached. The
#'   change is measured in proportion to the standard deviation of the response
#'   for 'a' and the ratio of the standard deviation of the response to the
#'   standard deviation of the predictor for 'b'.
#' @return A list where the first slot is a table with the original estimates
#'   and SE from the GLS fit in the two first columns followed by the bootstrap
#'   estimate of the SE and the 2.5\%, 50\% and 97.5\% quantiles of the
#'   bootstrap distribution. The second slot contains the complete distribution.
#' @author Geir H. Bolstad
#' @examples 
#' # See the vignette 'Analyzing rates of evolution' and in the help 
#' # page of rate_gls.
#' @importFrom stats var quantile
#' @export
rate_gls_boot <- 
  function(object, 
           n = 10, 
           useLFO = TRUE, 
           silent = FALSE, 
           maxiter = 100, 
           tol = 0.001
  ) {
  if (object$model == "recent_evol") {
    boot_distribution <- matrix(NA, ncol = 5, nrow = n)
    colnames(boot_distribution) <- 
      c("a", "b", "sigma2_x", "sigma2_y", "Rsquared")
  } else {
    boot_distribution <- matrix(NA, ncol = 4, nrow = n)
    colnames(boot_distribution) <- c("a", "b", "s2", "Rsquared")
  }
  perc_neg <- c()
  for (i in 1:n) {
    sim_out <- rate_gls_sim(object, nsim = 1)
    mod <- 
      rate_gls(x = sim_out[[1]][[1]][, "x"], 
               y = sim_out[[1]][[1]][, "y"], 
               species = sim_out[[1]][[1]][, "species"], 
               tree = object$tree, 
               model = object$model, 
               maxiter = maxiter, 
               silent = TRUE, 
               useLFO = useLFO,
               tol = tol
      )
    boot_distribution[i, ] <- c(mod$param[, 1], mod$Rsquared)
    perc_neg[i] <- sim_out[[1]][[2]]
    if (mod$convergence != "Convergence") {
      boot_distribution[i, ] <- NA
    }
    if (!silent) {
      print(paste("Bootstrap iteration", i))
    }
  }
  return(
    list(
      summary = 
        cbind(
          rbind(object$param, 
                Rsquared = c(object$Rsquared, NA)),
          boot_mean = apply(boot_distribution, 2, mean, na.rm = TRUE),
          boot_median = apply(boot_distribution, 2, median, na.rm = TRUE),
          boot_SD = apply(boot_distribution, 2, sd, na.rm = TRUE),
          t(apply(boot_distribution, 2, function(x) quantile(x, 
             probs = c(0.025, 0.975), na.rm = TRUE)))
          ),
      boot_distribution = 
        cbind(boot_distribution, 
              percent_negative_roots = perc_neg)
      )
    )
}
