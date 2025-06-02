#' Evaluate informative hypotheses using the GORICA
#'
#' GORICA is an acronym for "generalized order-restricted information criterion
#' approximation". It can be utilized to evaluate informative hypotheses, which
#' specify directional relationships between model parameters in terms of
#' (in)equality constraints.
#'
#' @param x An R object containing the outcome of a statistical analysis.
#' Currently, the following objects can be processed:
#' \itemize{
#' \item \code{lm()} objects (anova, ancova, multiple regression).
#' \item \code{t_test()} objects.
#' \item \code{lavaan} objects.
#' \item \code{lmerMod} objects.
#' \item A named vector containing the estimates resulting from a statistical
#' analysis, when the argument \code{Sigma} is also specified.
#' Note that, named means that each estimate has to be labeled such that it can
#' be referred to in \code{hypotheses}.
#' }
#' @param hypothesis A character string containing the informative hypotheses to
#' evaluate (see Details).
#' @param comparison A character string indicating what the \code{hypothesis}
#' should be compared to. Defaults to \code{comparison = "unconstrained"};
#' options include \code{c("unconstrained", "complement", "none")}.
#' @param iterations Integer. Number of samples to draw from the parameter space
#' when computing the \code{gorica} penalty.
#' @param ... Additional arguments passed to the internal function
#' \code{compare_hypotheses}.
#'
#' @details
#' The GORICA is applicable to not only normal linear models, but also applicable to generalized linear models (GLMs) (McCullagh & Nelder, 1989), generalized linear
#' mixed models (GLMMs) (McCullogh & Searle, 2001), and structural equation
#' models (SEMs) (Bollen, 1989). In addition, the GORICA can be utilized in the context of contingency tables for which (in)equality constrained hypotheses do not necessarily contain linear restrictions on cell probabilities, but instead often contain non-linear restrictions on cell probabilities.
#'
#' \code{hypotheses} is a character string that specifies which informative
#' hypotheses have to be evaluated. A simple example is \code{hypotheses <- "a >
#' b > c; a = b = c;"} which specifies two hypotheses using three estimates with
#' names "a", "b", and "c", respectively.
#'
#' The hypotheses specified have to adhere to the following rules:
#' \enumerate{
#' \item Parameters are referred to using the names specified in \code{names()}.
#' \item Linear combinations of parameters must be specified adhering to the
#' following rules:
#'         \enumerate{ \item Each parameter name is used at most once.
#'                     \item Each parameter name may or may not be
#'                     pre-multiplied with a number.
#'                     \item A constant may be added or subtracted from each
#'                     parameter name.
#'                     \item A linear combination can also be a single number.}
#' Examples are: \code{3 * a + 5}; \code{a + 2 * b + 3 * c - 2}; \code{a - b};
#' and \code{5}.
#' \item (Linear combinations of) parameters can be constrained using <, >, and
#' =. For example, \code{a > 0} or
#' \code{a > b = 0} or \code{2 * a < b + c > 5}.
#' \item The ampersand & can be used to combine different parts of a hypothesis.
#' For example, \code{a > b & b > c} which is equivalent to \code{a > b > c} or
#' \code{a > 0 & b > 0 & c > 0}.
#' \item Sets of (linear combinations of) parameters subjected to the same
#' constraints can be specified using (). For
#' example, \code{a > (b,c)} which is equivalent to \code{a > b & a > c}.
#' \item The specification of a hypothesis is completed by typing ; For example,
#' \code{hypotheses <- "a > b > c; a = b = c;"}, specifies two hypotheses.
#' \item Hypotheses have to be compatible, non-redundant and possible. What
#' these terms mean will be elaborated below.
#' }
#'
#' \emph{The set of hypotheses has to be compatible}. For the statistical
#' background of this requirement see Gu, Mulder, Hoijtink (2018). Usually the
#' sets of hypotheses specified by researchers are compatible, and if not,
#' \code{gorica} will return an error message. The following steps can be used to
#' determine if a set of hypotheses is compatible:
#' \enumerate{
#' \item	Replace a range constraint, e.g., \code{1 < a1 < 3}, by an equality
#' constraint in which the parameter involved is equated to the midpoint of the
#' range, that is, \code{a1 = 2}.
#' \item Replace in each hypothesis the < and > by =. For example, \code{a1 = a2
#' > a3 > a4} becomes \code{a1 = a2 = a3 = a4}.
#' \item The hypotheses are compatible if there is at least one solution to the
#' resulting set of equations. For the two hypotheses considered under 1. and
#' 2., the solution is a1 = a2 = a3 = a4 = 2. An example of two non-compatible
#' hypotheses is \code{hypotheses <- "a = 0; a > 2;"} because there is no
#' solution to the equations \code{a=0} and \code{a=2}.
#' }
#'
#' \emph{Each hypothesis in a set of hypotheses has to be non-redundant.} A
#' hypothesis is redundant if it can also be specified with fewer constraints.
#' For example, \code{a = b & a > 0 & b > 0} is redundant because it can also be
#' specified as \code{a = b & a > 0}. \code{gorica} will work correctly if
#' hypotheses specified using only < and > are redundant. \code{gorica} will
#' return an error message if hypotheses specified using at least one = are
#' redundant.
#'
#' \emph{Each hypothesis in a set of hypotheses has to be possible.} An
#' hypothesis is impossible if estimates in agreement with the hypothesis do not
#' exist. For example: values for \code{a} in agreement with \code{a = 0 &
#' a > 2} do not exist. It is the responsibility of the user to ensure that the
#' hypotheses specified are possible. If not, \code{gorica} will either return an
#' error message or render an output table containing \code{Inf}'s.
#'
#' @return An object of class \code{gorica}, containing the following elements:
#' \itemize{
#' \item \code{fit}  A \code{data.frame} containing the loglikelihood, penalty
#' (for complexity), the GORICA value, and the GORICA weights. The GORICA
#' weights are calculated by taking into account the misfits and complexities of
#' the hypotheses under evaluation. These weights are used to quantify the
#' support in the data for each hypothesis under evaluation. By looking at the
#' pairwise ratios between the GORICA weights, one can determine the relative
#' importance of one hypothesis over another hypothesis.
#' \item \code{call}  The original function call.
#' \item \code{model}  The original model object (\code{x}).
#' \item \code{estimates}  The parameters extracted from the \code{model}.
#' \item \code{Sigma}  The asymptotic covariance matrix of the
#' \code{estimates}.
#' \item \code{comparison}  Which alternative hypothesis was used.
#' \item \code{hypotheses}  The hypotheses evaluated in \code{fit}.
#' \item \code{relative_weights}  The relative weights of each hypothesis (rows)
#' versus each other hypothesis in the set (cols). The diagonal is equal to one,
#' as each hypothesis is equally likely as itself. A value of, e.g., 6, means
#' that the hypothesis in the row is 6 times more likely than the hypothesis in
#' the column.
#' }
#' @author Caspar van Lissa, Yasin Altinisik, Rebecca Kuiper
#' @references Altinisik, Y., Van Lissa, C. J., Hoijtink, H., Oldehinkel, A. J.,
#' & Kuiper, R. M. (2021). Evaluation of inequality constrained hypotheses using
#' a generalization of the AIC. Psychological Methods, 26(5), 599–621.
#' \doi{10.31234/osf.io/t3c8g}.
#'
#' Bollen, K. (1989). Structural equations with latent variables. New York, NY:
#' John Wiley and Sons.
#'
#' Kuiper, R. M., Hoijtink, H., & Silvapulle, M. J. (2011).
#' An Akaike-type information criterion for model selection under inequality
#' constraints. Biometrika, 98, 495-501.
#' \doi{10.31219/osf.io/ekxsn}
#'
#' Kuiper, R. M., Hoijtink, H., & Silvapulle, M. J. (2012).
#' Generalization of the order-restricted information criterion for multivariate
#' normal linear models. Journal of statistical planning and inference, 142(8),
#' 2454-2463. \doi{10.1016/j.jspi.2012.03.007}
#'
#' Vanbrabant, L., Van Loey, N., and Kuiper, R.M. (2019).
#' Evaluating a theory-based hypothesis against its complement using an AIC-type
#' information criterion with an application to facial burn injury.
#' Psychological Methods. \doi{10.31234/osf.io/n6ydv}
#'
#' McCullagh, P. & Nelder, J. (1989). Generalized linear models (2nd ed.). Boca
#' Raton, FL: Chapman & Hall / CRC.
#'
#' McCulloch, C. E., & Searle, S. R. (2001). Generalized linear and mixed
#' models. New York, NY: Wiley.
#' @examples
#' \dontshow{
#' # EXAMPLE 1. One-sample t test
#' ttest1 <- t_test(iris$Sepal.Length,mu=5)
#' gorica(ttest1,"x<5.8", iterations = 5)
#'
#' # EXAMPLE 2. ANOVA
#' aov1 <- aov(yield ~ block-1 + N * P + K, npk)
#' gorica(aov1,hypothesis="block1=block5;
#'    K1<0", iterations = 5)
#'
#' # EXAMPLE 3. glm
#' counts <- c(18,17,15,20,10,20,25,13,12)
#' outcome <- gl(3,1,9)
#' treatment <- gl(3,3)
#' fit <- glm(counts ~ outcome-1 + treatment, family = poisson())
#' gorica(fit, "outcome1 > (outcome2, outcome3)", iterations = 5)
#'
#' # EXAMPLE 4. ANOVA
#' res <- lm(Sepal.Length ~ Species-1, iris)
#' est <- get_estimates(res)
#' est
#' gor <- gorica(res, "Speciessetosa < (Speciesversicolor, Speciesvirginica)",
#' comparison = "complement", iterations = 5)
#' gor
#' }
#' \donttest{
#' # EXAMPLE 1. One-sample t test
#' ttest1 <- t_test(iris$Sepal.Length,mu=5)
#' gorica(ttest1,"x<5.8")
#'
#' # EXAMPLE 2. ANOVA
#' aov1 <- aov(yield ~ block-1 + N * P + K, npk)
#' gorica(aov1,hypothesis="block1=block5;
#'    K1<0")
#'
#' # EXAMPLE 3. glm
#' counts <- c(18,17,15,20,10,20,25,13,12)
#' outcome <- gl(3,1,9)
#' treatment <- gl(3,3)
#' fit <- glm(counts ~ outcome-1 + treatment, family = poisson())
#' gorica(fit, "outcome1 > (outcome2, outcome3)")
#'
#' # EXAMPLE 4. ANOVA
#' res <- lm(Sepal.Length ~ Species-1, iris)
#' est <- get_estimates(res)
#' est
#' gor <- gorica(res, "Speciessetosa < (Speciesversicolor, Speciesvirginica)",
#' comparison = "complement")
#' gor
#' }
#' @rdname gorica
#' @export
#' @importFrom stats as.formula coef complete.cases cov lm model.frame
#' model.matrix pt qt sd setNames summary.lm var vcov
#'
gorica <- function(x, hypothesis, comparison = "unconstrained", iterations = 100000, ...) {
  UseMethod("gorica", x)
}

#' @method gorica default
#' @export
gorica.default <- function(x,
                           hypothesis,
                           comparison = "unconstrained",
                           iterations = 100000,
                           Sigma,
                           ...
)
{
  # Check for analysisType == "contingency_tables"hier, en ga naar gorica.table indien dat het geval is
  cl <- match.call()
  Goricares <- list(
    fit = NULL,
    call = cl,
    model = x,
    estimates = x,
    Sigma = Sigma,
    comparison = comparison
  )

  if(is.list(Sigma) & length(Sigma) == 1) Sigma <- Sigma[[1]]
  names(x) <- rename_function(names(x))
  colnames(Sigma) <- rownames(Sigma) <- names(x)
  # Perform housekeeping --------------------------------------------------------
  with_env(gorica_housekeeping)

  # Evaluate each hypothesis ------------------------------------------------

  hypotheses <- mapply(function(this_hyp, nec_num){
    ormle(x,
          Sigma,
          constr = this_hyp[, -ncol(this_hyp), drop = FALSE],
          nec = nec_num,
          this_hyp[, ncol(this_hyp)]
    )
  }, this_hyp = hypothesis$hyp_mat, nec_num = hypothesis$n_ec, SIMPLIFY = FALSE)

  hyp <- reverse_rename_function(hypothesis$original_hypothesis)

  # Add unconstrained hypothesis --------------------------------------------

  if(comparison == "unconstrained"){
    hypotheses <- c(hypotheses,
                    list(ormle(est = x,
                               covmtrx = Sigma,
                               constr = matrix(c(rep(0, length(x))), nrow = 1),
                               nec = 0,
                               rhs = 0)
                    ))
    hyp <- c(hyp, "Hu")
  }


  # Add complement ----------------------------------------------------------

  Args_res <- c(
    list(
      object = hypotheses,
      iterations = force(iterations)
    ),
    list(...)
  )
  res <- do.call(compare_hypotheses, Args_res)
  fit <- res$comparisons

  if(comparison == "complement"){
    if(length(res[["gorica_penalties"]]) > 1){
      warning("Cannot compute complement for more than one hypothesis.", call. = FALSE)
    } else {
      use_wtbar <- res[["gorica_penalties"]][[1]][["wt_bar"]]
      use_wtbar <- use_wtbar[length(use_wtbar) - hypothesis$n_ec[1]]
      complement <- do.call(comp, c(hypotheses[[1]], wt_bar = use_wtbar))
      fit <- rbind(fit, complement)
      hyp <- c(hyp, "Hc")
    }
  }
  # Prepare output ----------------------------------------------------------

  if(any(fit$penalty < 0)) warning("Some gorica penalties were below 0. This is not theoretically possible. Please send a bug report to c.j.vanlissa@uu.nl", call. = FALSE)
  fit$gorica_weights <- compute_weights(fit$gorica)

  Goricares[c("fit", "hypotheses")] <- list(fit, hyp)

  Goricares$relative_weights <- Goricares$fit$gorica_weights %*% t(1/Goricares$fit$gorica_weights)
  mat_nams <- hyp
  mat_nams[!nchar(mat_nams) == 2] <- paste0("H", 1:sum(!nchar(mat_nams) == 2))
  colnames(Goricares$relative_weights) <- rownames(Goricares$relative_weights) <- mat_nams
  restest <- t(sapply(hypotheses, `[[`, "restrictedest"))
  rownames(restest) <- mat_nams[1:nrow(restest)]
  Goricares$restricted_estimates <- restest
  class(Goricares) <- "gorica"
  Goricares
}



#' @method gorica htest
#' @export
gorica.htest <-
  function(x,
           hypothesis,
           comparison = "unconstrained",
           iterations = 100000,
           ...) {
    stop("To be able to run gorica on the results of an object returned by t.test(), you must first load the 'gorica' package, and then conduct your t.test. The standard t.test does not return group-specific variances and sample sizes, which are required by gorica. When you load the gorica package, the standard t.test is replaced by a version that does return this necessary information.")
  }

#' @method gorica t_test
#' @export
gorica.t_test <-
  function(x,
           hypothesis,
           comparison = "unconstrained",
           iterations = 100000,
           ...) {
    cl <- match.call()
    Args <- as.list(cl[-1])
    ests <- get_estimates(x)
    Args$x <- ests$estimate
    Args$hypothesis <- force(hypothesis)
    #Args$n <- x$n

    if(length(x$estimate) == 1){
      Args$Sigma <- list(matrix(x$v/x$n))
    } else {
      if (!x$method == " Two Sample t-test") {
        Args$Sigma <- list(diag(x$v/x$n)) #lapply(x$v/x$n, as.matrix)
      } else {
        df <- sum(x$n) - 2
        v <- 0
        if (x$n[1] > 1)
          v <- v + (x$n[1] - 1) * x$v[1]
        if (x$n[2] > 1)
          v <- v + (x$n[2] - 1) * x$v[2]
        v <- v/df
        Args$Sigma <- list(diag(v / x$n)) #lapply(v / x$n, as.matrix)
      }
    }

    Gorica_res <- do.call(gorica, Args)
    Gorica_res$call <- cl
    Gorica_res$model <- x
    class(Gorica_res) <- c("t_test", class(Gorica_res))
    Gorica_res
  }



#' @method gorica lm
#' @export
gorica.lm <-
  function(x,
           hypothesis,
           comparison = "unconstrained",
           iterations = 100000,
           ...) {

    cl <- match.call()
    Args <- as.list(cl[-1])
    if(!is.null(Args[["standardize"]])){
      if(Args[["standardize"]]){
        warning("Cannot standardize an object of class 'lm'. Using unstandardized coefficients.")
      }
    }
    Args$x <- coef(x)
    Args$Sigma <- vcov(x)
    Args$hypothesis <- force(hypothesis)
    Gorica_res <- do.call(gorica, Args)
    Gorica_res$call <- cl
    Gorica_res$model <- x

    #if(!is.null(Warnings)){
    #  Gorica_res$Warnings <- Warnings
    #}
    class(Gorica_res) <- c("gorica_lm", class(Gorica_res))
    Gorica_res
  }

#' @method gorica mplus.model
#' @export
#' @keywords internal
gorica.mplus.model <-
  function(x,
           hypothesis,
           comparison = "unconstrained",
           iterations = 100000,
           ...) {

    cl <- match.call()
    Args <- as.list(cl[-1])
    mplus_est <- get_estimates(x)
    Args$x <- mplus_est$estimate
    Args$Sigma <- mplus_est$Sigma
    Args$hypothesis <- force(hypothesis)
    Gorica_res <- do.call(gorica, Args)
    Gorica_res$call <- cl
    Gorica_res$model <- x

    #if(!is.null(Warnings)){
    #  Gorica_res$Warnings <- Warnings
    #}
    class(Gorica_res) <- c("gorica_mplus", class(Gorica_res))
    Gorica_res
  }

#' @method gorica lavaan
#' @export
#' @rdname gorica
#' @param standardize Logical. For \code{lavaan} objects, whether or not to
#' extract the standardized model coefficients. Defaults to \code{FALSE}.
gorica.lavaan <-
  function(x,
           hypothesis,
           comparison = "unconstrained",
           iterations = 100000,
           ...,
           standardize = FALSE) {
    cl <- match.call()
    Args <- as.list(cl[-1])
    mplus_est <- get_estimates(x, standardize)
    Args$x <- mplus_est$estimate
    Args$Sigma <- mplus_est$Sigma
    Args$hypothesis <- force(hypothesis)
    Gorica_res <- do.call(gorica, Args)
    Gorica_res$call <- cl
    Gorica_res$model <- x

    #if(!is.null(Warnings)){
    #  Gorica_res$Warnings <- Warnings
    #}
    class(Gorica_res) <- c("gorica_lavaan", class(Gorica_res))
    Gorica_res
  }

#' @method gorica lmerMod
#' @export
gorica.lmerMod <-
  function(x,
           hypothesis,
           comparison = "unconstrained",
           iterations = 100000,
           ...) {

    cl <- match.call()
    Args <- as.list(cl[-1])

    Args$x <- fixef(x)
    Args$Sigma <- vcov(x)
    Args$hypothesis <- force(hypothesis)
    Gorica_res <- do.call(gorica, Args)
    Gorica_res$call <- cl
    Gorica_res$model <- x

    #if(!is.null(Warnings)){
    #  Gorica_res$Warnings <- Warnings
    #}
    class(Gorica_res) <- c("gorica_lmerMod", class(Gorica_res))
    Gorica_res
  }

#' @method gorica model_estimates
#' @export
gorica.model_estimates <-
  function(x,
           hypothesis,
           comparison = "unconstrained",
           iterations = 100000,
           ...) {

    cl <- match.call()
    Args <- as.list(cl[-1])

    Args$x <- x$estimate
    Args$Sigma <- x$Sigma
    Args$hypothesis <- force(hypothesis)
    Gorica_res <- do.call(gorica, Args)
    Gorica_res$call <- cl
    Gorica_res$model <- x

    #if(!is.null(Warnings)){
    #  Gorica_res$Warnings <- Warnings
    #}
    class(Gorica_res) <- c("gorica_model_estimates", class(Gorica_res))
    Gorica_res
  }


#' @section Contingency tables:
#' When specifying hypotheses about contingency tables, the asymptotic
#' covariance matrix of the model estimates is derived by means of
#' bootstrapping. This makes it possible for users to define derived parameters:
#' For example, a ratio between cell probabilities. For this purpose, the
#' \code{\link[bain]{bain}} syntax has been enhanced with the command \code{:=}.
#' Thus, the syntax \code{"a := x[1,1]/(x[1,1]+x[1,2])"} defines a new parameter
#' \code{a} by reference to specific cells of the table \code{x}. This new
#' parameter can now be named in hypotheses.
#' @rdname gorica
#' @method gorica table
#' @export
#' @importFrom limSolve ldei
#' @importFrom methods formalArgs
gorica.table <- function(x,
                         hypothesis,
                         comparison = "unconstrained",
                         ...){
  cl <- match.call()
  Args <- as.list(cl[-1])
  original_estimate <- x

  # Get constraints from hypothesis
  n_pars <- length(original_estimate)
  const <- paste0(get_const(hypothesis), collapse = ";")
  hasconstraints <- isFALSE(const == "")
  hypothesis <- rename_table_est(paste0(get_hyp(hypothesis), collapse = ";"))
  coefs_in_hyp <- params_in_hyp(hypothesis)
  cl_est <- cl
  cl_est <- cl_est[c(1, which(names(cl_est) %in% formalArgs(get_estimates.table)))]
  cl_est[["constraints"]] <- const
  cl_est[[1L]] <- quote(get_estimates)
  est <- eval.parent(cl_est)
  x <- est$estimate
  Sigma <- est$Sigma

  names(x) <- rename_table_est(rename_function(names(x)))
  colnames(Sigma) <- rownames(Sigma) <- names(x)
  # Put x and Sigma in Args already, just to have an unaltered backup
  Args$x <- x
  Args$Sigma <- Sigma
  # Perform housekeeping --------------------------------------------------------
  with_env(gorica_housekeeping)
  null_hyps <- sapply(hypothesis$hyp_mat, is.null)
  if(any(null_hyps)){
    which_null <- which(null_hyps)
    remove_these <- as.vector(sapply(which_null, seq_tuples, tuples = 2))
    hypothesis$hyp_mat[which_null] <- NULL
    hypothesis$n_constraints <- hypothesis$n_constraints[-remove_these]
    hypothesis$n_ec <- hypothesis$n_ec[-which_null]
    hypothesis$original_hypothesis <- hypothesis$original_hypothesis[-which_null]
  }

  if(hasconstraints & (any(original_estimate == 0) & any(x == 1 | x == Inf)) | any(is.nan(Sigma))) {
    stop("Some of the defined parameters are invalid due to empty cell(s) in the table. Please rewrite the hypotheses.")
  }
  if(all(Sigma == 0)){
    stop("All parameter covariances are equal to zero.")
  }

  # Establish whether the constraints are specified on raw probabilities
  sum_to_one <- sum(x) == 1
  if(sum_to_one & hasconstraints){
    stop("The defined parameters contain linear dependencies. Consequently, their covariance matrix is not positive definite. Please rewrite the hypotheses.")
  }
  # 1. Check if the covariance matrix is singular. If so, linear dependency and:
  linear_dependency <- !pos_definite(Sigma)
  if(linear_dependency){
    # 2. Check eta’s without variation.
    null_coefs <- rowSums(Sigma == 0) == ncol(Sigma)
    # If any:
    # 3. Check if this/these eta == 0.
    if(any(null_coefs)){
      if(any(x[null_coefs] != 0)){ # So, at least one of the eta’s without variation is non-zero
        # If not, then message about rewriting.
        stop("Please rewrite the hypothesis.")
      } else {
        # If so:
        # Delete eta(s) without variation (which are now all 0; which gives eta_adj);
        # delete corresponding rows and columns in Sigma (which gives Sigma_adj)
        # and corresponding columns in R (R_adj);
        zero_est <- which(null_coefs)

        # Discard the rows and thus columns from Sigma for which all elements are zero.
        # discard the corresponding etas which leads to etaadj
        # and the corresponding columns from the restriction matrix Rm, which leads to Rmadj
        # and do the check to see if you need to adjust the rhs (which then gives rhs_adj)
        #with_env(hypothesis_remove_nulls, which_par = zero_est)
        R <- do.call(rbind, hypothesis$hyp_mat)
        if(sum_to_one){
          remove_par <- max(which(x != 0))
          R <- sweep(R, MARGIN = 1, as.vector(R[, remove_par]))
        }
        rhs <- R[, ncol(R), drop = FALSE]
        R <- R[, -ncol(R), drop = FALSE]

        # Delete eta(s) without variation (which are now all 0; which gives eta_adj);
        x <- x[-zero_est]
        # delete corresponding rows and columns in Sigma (which gives Sigma_adj)
        Sigma <- Sigma[-zero_est, -zero_est]

        # and the corresponding columns from the restriction matrix Rm, which leads to Rmadj
        R_adj <- R[, -zero_est, drop = FALSE]

        # and do the check to see if you need to adjust the rhs (which then gives rhs_adj)
        null_rows <- which(apply(R_adj, 1, function(i){all(i == 0)}))
        if(length(null_rows) > 0){
          if(any(rhs[null_rows] > 0)){
            qadj <- which(apply(R[null_rows, , drop = FALSE], 2, function(j){any(j != 0)}))
            G <- diag(1,ncol(hypothesis$hyp_mat[[1]])-1)[null_rows,]
            H <- rep(0,length(null_rows))
            q <- ldei(E=R, F=rhs, G = G, H = H)$X
            q[qadj] <- 0
            rhs <- R%*%q

          }
        }
        new_hypmat <- cbind(R_adj, rhs)
        lengths <- c(0, sapply(hypothesis$hyp_mat, nrow))
        hypothesis$hyp_mat <- lapply(1:length(hypothesis$hyp_mat), function(mat_num){
          new_hypmat[(sum(lengths[1:mat_num])+1):sum(lengths[1:(mat_num+1)]), ]
        })
      }
    }
  }

  # Then, check whether there is still linear dependency; i.e., check det(Sigma_adj) == 0.
  # If not, then proceed with eta_adj, Sigma_adj, R_adj, and rhs or rhs_adj.
  # If so, check whether the sum of the (remaining) eta’s (i.e., sum(eta_adj) is 1
  if(!pos_definite(Sigma)){
    if(!hasconstraints){
      # If the hypothesis is specified on raw probabilities, it is possible to
      # rewrite the hypothesis by setting last column of remaining eta’s to
      # 1 – rest; etc; but now using eta_adj, Sigma_adj, R_adj, and rhs or rhs_adj.
      message("The hypotheses involve all table cells, which introduces a linear dependency. The final cell probability was defined as one minus the other cell probabilities, and hypotheses were respecified to reflect this.")
      remove_par <- max(which(x != 0))
      # If hyp sum to one, remove last non-zero par
      hypothesis$hyp_mat <- lapply(hypothesis$hyp_mat, function(R){
        sweep(R[, -remove_par, drop = FALSE], MARGIN = 1, as.vector(R[, remove_par]))
      })
      # Drop parameters not in hypothesis
      x <- x[-remove_par]
      Sigma <- Sigma[-remove_par, -remove_par, drop = FALSE]
      if(!pos_definite(Sigma)){
        stop("The covariance matrix is still not positive definite. Please rewrite the hypotheses.")
      }
    } else {
      stop("The defined parameters contain linear dependencies. Consequently, their covariance matrix is not positive definite. Please rewrite the hypotheses.")
    }
  }

  #
  # I am not sure whether you need to check for 1 (and further again) after 3 or 4.


  # if(all(x[coefs_in_hyp] == 0)){
  #   print(out)
  #   stop("All x referenced in the hypothesis are equal to zero.")
  # }
  Args$x <- x
  Args$Sigma <- Sigma
  Args$hypothesis <- hypothesis
  Args$comparison <- force(comparison)
  Gorica_res <- do.call(gorica, Args)
  Gorica_res$estimates <- est$estimate
  Gorica_res$Sigma <- est$Sigma
  Gorica_res$call <- cl
  Gorica_res$model <- original_estimate
  Gorica_res$hypotheses <- sapply(Gorica_res$hypotheses, reverse_rename_table_est)

  class(Gorica_res) <- c("gorica_table", class(Gorica_res))
  return(Gorica_res)
}


