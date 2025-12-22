#' Setup bma Design Object
#'
#' Creates an object of class \code{bma}.
#'
#' @template k
#' @template p0
#' @template shape_beta
#'
#' @return An S3 object of class \code{bma}
#' @export
#'
#' @details The class \code{bma} implements the Bayesian Model Averaging
#' design by Pisoda et al. (2021). Functions for this class are mostly
#' wrappers for functions of the \code{bmabasket} package.
#'
#' @references Psioda, M. A., Xu, J., Jiang, Q. I., Ke, C., Yang, Z., &
#' Ibrahim, J. G. (2021). Bayesian adaptive basket trial design using model
#' averaging. Biostatistics, 22(1), 19-34.
#'
#' @examples
#' design_bma <- setup_bma(k = 3, p0 = 0.2)
setup_bma <- function(k, p0, shape1 = 1, shape2 = 1) {
  mu0 <- shape1 / (shape1 + shape2)
  phi0 <- shape1 + shape2
  structure(
    list(k = k, p0 = p0, mu0 = mu0, phi0 = phi0),
    class = "bma"
  )
}

#' Setup mml Design Object
#'
#' Creates an object of class \code{mml}.
#'
#' @template k
#' @template p0
#' @template shape_beta
#'
#' @details The class \code{mml} implements a modified version of the
#' empirical Bayes method by Gravestock & Held (2017) which was proposed for
#' borrowing strength from an external study. In their approach, the sharing
#' weight is found as the maximum of the marginal likelihood of the
#' weight, given the external data set. This leads, however, to
#' non-symmetric weights when applied to sharing in basket trials, i.e.
#' Basket i would not share the information from Basket j as the other way
#' round. Therefore, a symmetrised version is used, where the mean of the
#' two weights resulting from sharing in both directions is used.
#'
#' @references Gravestock, I., & Held, L. (2017). Adaptive power priors with
#' empirical Bayes for clinical trials. Pharmaceutical statistics, 16(5),
#' 349-360.
#'
#' @return An S3 object of class \code{mml}
#' @export
#'
#' @examples
#' design_mml <- setup_mml(k = 3, p0 = 0.2)
setup_mml <- function(k, p0, shape1 = 1, shape2 = 1) {
  validate_betabin(structure(
    list(k = k, p0 = p0, shape1 = shape1, shape2 = shape2),
    class = "mml"
  ))
}

#' Setup mmlglobal Design Object
#'
#' Creates an object of class \code{mmlglobal}.
#'
#' @template k
#' @template p0
#' @template shape_beta
#'
#' @details The class \code{mmlglobal} implements an empirical Bayes method
#' by Gravestock & Held (2019) which was proposed for borrowing strength
#' from multiple external studies.
#'
#' @references Gravestock, I., & Held, L. (2019). Power priors based on
#' multiple historical studies for binary outcomes. Biometrical Journal, 61(5),
#' 1201-1218.
#'
#' Baumann, L., Sauer, L., & Kieser, M. (2024). A basket trial design based on
#' power priors. arXiv:2309.06988.
#'
#' @return An S3 object of class \code{mmlglobal}
#' @export
#'
#' @examples
#' design_mmlglobal <- setup_mmlglobal(k = 3, p0 = 0.2)
setup_mmlglobal <- function(k, p0, shape1 = 1, shape2 = 1) {
  validate_betabin(structure(
    list(k = k, p0 = p0, shape1 = shape1, shape2 = shape2),
    class = "mmlglobal"
  ))
}

#' Setup BHM Design Object
#'
#' @template k
#' @template p0
#' @template p_target
#' @template mu_bhm
#'
#' @details The class \code{bhm} implements the Bayesian Hierarchical Model
#' proposed by Berry et al. (2013). Methods for this class are
#' mostly wrappers for functions from the package \code{bhmbasket}.
#'
#' In the BHM the thetas of all baskets are modeled, where theta_i =
#' logit(p_i) - logit(p_target). These thetas are assumed to come from
#' a normal distribution with mean mu_mean and standard deviation mu_sd.
#' If \code{mu_mean = NULL} then mu_mean is determined as logit(p0) -
#' logit(p_target), hence the mean of the normal distribution corresponds
#' to the null hypothesis.
#'
#' @references Berry, S. M., Broglio, K. R., Groshen, S., & Berry, D. A. (2013).
#' Bayesian hierarchical modeling of patient subpopulations: efficient designs
#' of phase II oncology clinical trials. Clinical Trials, 10(5), 720-734.
#'
#' @return An S3 object of class \code{bhm}
#' @export
#'
#' @examples
#' design_bhm <- setup_bhm(k = 3, p0 = 0.2, p_target = 0.5)
setup_bhm <- function(k, p0, p_target, mu_mean = NULL, mu_sd = 100) {
  if (is.null(mu_mean)) mu_mean <- bhmbasket::logit(p0) -
      bhmbasket::logit(p_target)
  structure(
    list(k = k, p0 = p0, p_target = p_target, mu_mean = mu_mean, mu_sd = mu_sd),
    class = "bhm"
  )
}

#' Setup EXNEX Design Object
#'
#' @template k
#' @template p0
#' @template params_exnex
#'
#' @details The class \code{exnex} implements the EXNEX model proposed by
#' Neuenschwander et al. (2016). Methods for this class are mostly wrappers
#' for functions from the package \code{bhmbasket}.
#'
#' In the EXNEX model the thetas of all baskets are modeled as a mixture
#' of individual models and a Bayesian Hierarchical Model with a fixed
#' mixture weight w. If \code{mu_mean} and \code{basket_mean} are \code{NULL}
#' then they are set to logit(p0).
#' Note that Neuenschwander et al. (2016) use different prior means and
#' standard deviations. The default values here are used for better comparison
#' with the BHM model (see \code{\link{setup_bhm}}).
#'
#' @references Neuenschwander, B., Wandel, S., Roychoudhury, S., & Bailey, S.
#' (2016). Robust exchangeability designs for early phase clinical trials with
#' multiple strata. Pharmaceutical statistics, 15(2), 123-134.
#'
#' @return An S3 object of class \code{exnex}
#' @export
#'
#' @examples
#' design_exnex <- setup_exnex(k = 3, p0 = 0.2)
setup_exnex <- function(k, p0, basket_mean = NULL, basket_sd = 100,
                        mu_mean = NULL, mu_sd = 100) {
  if (is.null(basket_mean)) basket_mean <- bhmbasket::logit(p0)
  if (is.null(mu_mean)) mu_mean <- bhmbasket::logit(p0)
  structure(
    list(k = k, p0 = p0, basket_mean = basket_mean, basket_sd = basket_sd,
      mu_mean = mu_mean, mu_sd = mu_sd),
    class = "exnex"
  )
}

#' Setup Fujikawa Design Object
#'
#' @template k
#' @template p0
#' @template shape_beta
#'
#' @details The class \code{fujikawa} implements a design by Fujikawa et al.
#' (2020) in which information is shared based on the pairwise similarity
#' between baskets which is quantified using the Jensen-Shannon divergence
#' between the individual posterior distributions between baskets.
#'
#' @references Fujikawa, K., Teramukai, S., Yokota, I., & Daimon, T. (2020).
#' A Bayesian basket trial design that borrows information across strata based
#' on the similarity between the posterior distributions of the response
#' probability. Biometrical Journal, 62(2), 330-338.
#'
#' @return An S3 object of class \code{fujikawa}
#' @export
#'
#' @examples
#' design_fujikawa <- setup_fujikawa(k = 3, p0 = 0.2)
setup_fujikawa <- function(k, p0, shape1 = 1, shape2 = 1) {
  validate_betabin(structure(
    list(k = k, p0 = p0, shape1 = shape1, shape2 = shape2),
    class = "fujikawa"
  ))
}

#' Setup Global JSD Design Object
#'
#' @template k
#' @template p0
#' @template shape_beta
#'
#' @details The class \code{jsdglobal} implements a version of the power prior
#' design, in which information is shared based on pairwise similarity
#' and overall heterogeneity between baskets. Both pairwise similarity and
#' overall heterogeneity are assessed using the Jensen-Shannon divergence.
#'
#' @references Baumann, L., Sauer, L., & Kieser, M. (2024). A basket trial
#' design based on power priors. arXiv:2309.06988.
#'
#' @return An S3 object of class \code{jsdglobal}
#' @export
#'
#' @examples
#' design_jsdglobal <- setup_jsdglobal(k = 3, p0 = 0.2)
setup_jsdglobal <- function(k, p0, shape1 = 1, shape2 = 1) {
  validate_betabin(structure(
    list(k = k, p0 = p0, shape1 = shape1, shape2 = shape2),
    class = "jsdglobal"
  ))
}

#' Setup Calibrated Power Prior Design Object
#'
#' @template k
#' @template p0
#' @template shape_beta
#'
#' @details The class \code{cpp} implements a version of the power prior design,
#' in which the amount of information that is shared between baskets is
#' determined by the Kolmogorov-Smirnov test statistic between baskets (which
#' is equivalent to the absolut difference in response rates).
#'
#' @references Baumann, L., Sauer, L., & Kieser, M. (2024). A basket trial
#' design based on power priors. arXiv:2309.06988.
#'
#' @return An S3 object of class \code{cpp}
#' @export
#'
#' @examples
#' design_cpp <- setup_cpp(k = 3, p0 = 0.2)
setup_cpp <- function(k, p0, shape1 = 1, shape2 = 1) {
  validate_betabin(structure(
    list(k = k, p0 = p0, shape1 = shape1, shape2 = shape2),
    class = "cpp"
  ))
}

#' Setup Global Calibrated Power Prior Design Object
#'
#' @template k
#' @template p0
#' @template shape_beta
#'
#' @details The class \code{cppglobal} implements a version of the power prior
#' design, in which the amount of information that is shared between baskets is
#' determined by the Kolmogorov-Smirnov test statistic between basekts and
#' a function based on response rate differences that quantifies the
#' overall heterogeneity.
#'
#' @references Baumann, L., Sauer, L., & Kieser, M. (2024). A basket trial
#' design based on power priors. arXiv:2309.06988.
#'
#' @return An S3 object of class \code{cppglobal}
#' @export
#'
#' @examples
#' design_cppglobal <- setup_cppglobal(k = 3, p0 = 0.2)
setup_cppglobal <- function(k, p0, shape1 = 1, shape2 = 1) {
  validate_betabin(structure(
    list(k = k, p0 = p0, shape1 = shape1, shape2 = shape2),
    class = "cppglobal"
  ))
}
