# S4 class, constructors and methods for simulating treatment variables and covariates
# Call function from utils.R
# Last update on 082320

setClassUnion("numericORNULL", c("numeric", "NULL"))
setClassUnion("charORNULL", c("character", "NULL"))
setClassUnion("logicalORNULL", c("logical", "NULL"))
setClassUnion("listORNULL", c("list", "NULL"))
setClassUnion("matrixORNULL", c("matrix", "NULL"))
`%notin%` <- Negate(`%in%`)

#' Simulate external trial indicator and treatment arm indicator
#'
#' This function conducts validity check and generates a matrix with two binary variables indicating
#' 1) if the observation belongs to the external trial
#' 2) if the observation belongs to the treatment arm.
#'
#' @param ssC Number of observations in the internal control arm. Default is 100
#' @param ssE Number of observations in the internal experiment arm. Default is the same number
#' of observations as `ssC`
#'
#' @param ssExt Number of observations in the external control arm. Default is the same number
#' of observations as `ssC`
#'
#' @return A \code{matrix} containing external trial indicator and treatment indicator
#'
#' @export
#' @keywords constructor
set_n <- function(ssC, ssE, ssExt) {
  if (missing(ssC) || !is.numeric(ssC)) {
    ps_message("ssC is not correctly specified. Default value 100 is used")
    ssC <- 100
  }
  if (missing(ssE) || !is.numeric(ssE)) {
    ps_message("ssE is not recognized. ssC is used")
    ssE <- ssC
  }
  if (missing(ssExt) || !is.numeric(ssExt)) {
    ps_message("ssExt is not recognized. ssC is used")
    ssExt <- ssC
  }
  s_trt(ssC, ssE, ssExt) # call function from utils.R
}

#' S4 Class for setting up covariates
#'
#' @keywords classes
.covClass <- setClass(
  ".covClass",
  slots = list(
    n_cat = "numeric",
    n_cont = "numeric",
    mu_int = "numericORNULL",
    mu_ext = "numericORNULL",
    var = "numericORNULL",
    cov = "numericORNULL",
    prob_int = "numericORNULL",
    prob_ext = "numericORNULL"
  )
)

#' Set up covariates
#'
#' This function saves the mean, variance and covariance among covariates. For technical
#' details, see the vignette.
#'
#' @param n_cat Number of binary variable. See details
#'
#' @param n_cont Number of continuous variable
#'
#' @param mu_int Mean of covariates in the internal trial. All the covariates are simulated from a
#' multivariate normal distribution. If left \code{NULL}, it uses default value 0 for all
#' covariates. If provided one value, this value is used for all covariates
#'
#' @param mu_ext Mean of covariates in the external trial. If left \code{NULL}, it uses the same
#' mean as `mu_int`
#'
#' @param var Variance of covariates. If left \code{NULL}, it uses default value 0 for all
#' covariates. If provided one value, it uses this value for all covariates
#'
#' @param cov Covariance between each pair of covariates. Covariance needs to be provided in
#' a certain order and users are encouraged to read the example provided in the vignette. If
#' left \code{NULL}, it uses default value 0 for all covariates. If provided one value, it
#' uses this value for every pair of covariates
#'
#' @param prob_int Probability of binary covariate equalling 1 in the internal trial. If left
#' \code{NULL}, it uses default value 0.5 for all covariates. If provided one value, it uses
#' this value for all covariates
#'
#' @param prob_ext Probability of binary covariate equalling 1 in the external trial. If
#' left \code{NULL}, it uses the same probability as `prob_int`
#'
#' @details
#'
#' Categorical variables are created by sampling a continuous variable from the multivariate
#' normal
#' distribution (thus respecting the correlation to other covariates specified by `cov`)
#' and then applying a cut point derived from the `prob_int` or `prob_ext` quantile
#' of said distribution i.e. for a univariate variable it would be derived as:
#' ```
#' binvar <- as.numeric(rnorm(n, mu, sqrt(var)) < qnorm(prob, mu, sqrt(var)))
#' ```
#' Please note that this means that the value of `mu_int` & `mu_ext` has no impact on categorical
#' covariates and thus can be set to any value.
#'
#' As an example of how this process works assume `n_cat=3` and `n_cont=2`. First 5 variables are
#' sampled from the multivariate normal distribution as specified by `mu_int`/`mu_ext`, `var` &
#' `cov`. Then, the first 3 of these variables are converted to binary based on the probabilities
#' specified by `prob_int` and `prob_ext`. This means that that the 2 continuous variables will
#' take their mean and sd from the last 2 entries in the vectors `mu_int`/`mu_ext` and `var`.
#'
#' @return A \code{.covClass} class containing covariate information
#'
#' @export
#' @keywords constructor
set_cov <- function(n_cat, n_cont, mu_int, mu_ext, var, cov, prob_int, prob_ext) {

  if (missing(n_cat) & missing(n_cont)) {
    stop(
      paste(
        "User provided no information on the number of covariates (n_cat, n_cont).",
        "There's no need to use this function or add_cov."
      )
    )
  } else if (missing(n_cat))  {

    ps_message("Number of binary covariates (n_cat) is not detected. Default value 0 is used.")
    n_cat <- 0
  } else if (missing(n_cont)) {
    n_cont <- 0

    if (missing(mu_int) & missing(mu_ext) & missing(var) & missing(cov)) {
      ps_message(
        paste(
          "User wants to simulate independent binary covariates (n_cont) only.",
          "Independent bernoulli distribution is directly used."
        )
      )
      mu_int <- rep(0, n_cat)
      mu_ext <- rep(0, n_cat)
      var <- rep(0, n_cat)
      if (n_cat == 1) {
        cov <- 0
      } else {
        cov <- rep(0, sum(seq(1, n_cat - 1, by = 1)))
      }
    } else {
      ps_message("Number of continuous covariates (n_cont) is not detected. Default value 0 is used.")
    }
  }

  n_cov <- n_cont + n_cat

  if (missing(mu_int) || length(mu_int) %notin% c(1, n_cov)) {
    ps_message(
      paste(
        "Mean of covariate in internal trials is not recognized or correctly specified.",
        "Default value 0 is used for all covariates"
      )
    )
    mu_int <- rep(0, n_cov)
  } else if (n_cov != 1 & length(mu_int) == 1) {
    ps_message(
      paste(
        "User provides one mean of covariate in internal trials.",
        "This value is used for all covariates"
      )
    )
    mu_int <- rep(mu_int, n_cov)
  }

  if (missing(mu_ext) || length(mu_ext) %notin% c(1, n_cov)) {
    ps_message(
      paste(
        "Mean of covariate in external trials is not recognized or correctly specified.",
        "mu_int is used for all covariates"
      )
    )
    mu_ext <- mu_int
  } else if (n_cov != 1 & length(mu_ext) == 1) {
    ps_message(
      paste(
        "User provides one mean of covariate in external trials.",
        "This value is used for all covariates"
      )
    )
    mu_ext <- rep(mu_ext, n_cov)
  }

  if (missing(var) || length(var) %notin% c(1, n_cov)) {
    ps_message(
      paste(
        "Variance of covariate in external trials is not recognized or correctly specified.",
        "Default value 1 is used for all covariates"
      )
    )
    var <- rep(1, n_cov)
  } else if (n_cov != 1 & length(var) == 1) {
    ps_message("User provides one number for variance. This variance is used for all covariates")
    var <- rep(var, n_cov)
  }

  if (missing(cov)) {
    cov <- NULL
  }
  if (n_cov > 1) {
    len_cov <- sum(seq(1, n_cov - 1, by = 1))
    if (length(cov) %notin% c(1, len_cov)) {
      ps_message(
        paste(
          "Covariance for covariate (cov) is not recognized.",
          "Default value 0 is used for all covariates"
        )
      )
      cov <- rep(0, len_cov)
    } else if (length(cov) == 1) {
      ps_message(
        paste(
          "User provides one number:",
          cov,
          "for covariance. This covariance is used for all covariates"
        )
      )
      cov <- rep(cov, len_cov)
    }
  }

  if (missing(prob_int)) {
    prob_int <- NULL
  }
  if (missing(prob_ext)) {
    prob_ext <- NULL
  }
  if (n_cat > 0) {
    if (length(prob_int) %notin% c(1, n_cat)) {
      ps_message(
        paste(
          "Probability of binary covariate in the internal trial is not recognized",
          "or correctly specified. Default value 0.5 is used for all binary covariates"
        )
      )
      prob_int <- rep(0.5, n_cat)
    } else if (length(prob_int) == 1 & !n_cat == 1) {
      ps_message(
        paste(
          "User provides one number for probability for the binary covariate in the",
          "internal trial. This probability is used for all binary covariates"
        )
      )
      prob_int <- rep(prob_int, n_cat)
    }

    if (length(prob_ext) %notin% c(1, n_cat)) {
      ps_message(
        paste(
          "Probability of binary covariate in the external trial is not recognized",
          "or correctly specified. mu_int is used."
        )
      )
      prob_ext <- prob_int
    } else if (length(prob_ext) == 1 & !n_cat == 1) {
      ps_message(
        paste(
          "User provides one number for probability for the binary covariate in",
          "the external trial. This probability is used for all binary covariates"
        )
      )
      prob_ext <- rep(prob_ext, n_cat)
    }
  }
  new(
    ".covClass",
    n_cat = n_cat,
    n_cont = n_cont,
    mu_int = mu_int,
    mu_ext = mu_ext,
    var = var,
    cov = cov,
    prob_int = prob_int,
    prob_ext = prob_ext
  )
}

#' Concatenate multiple \code{.covClasss} classes
#'
#' @param x A \code{.covClasss} class with covariate information generated in \code{\link{set_cov}}
#'
#' @param ... Other \code{.covClasss} classes with covariate information generated in
#' \code{\link{set_cov}}
#' @return A vector of \code{.covClasss} classes
#'
#'
#' @examples
#' # combine two sets of covariates
#' covset1 = set_cov(n_cat = 2, n_cont = 0, mu_int = 0, mu_ext = 0, var = 1)
#' covset2 = set_cov(n_cat = 0, n_cont = 1, mu_int = 62, mu_ext = 65, var = 11)
#' cov_list = c(covset1, covset2)
#'
#'
#'
#' @export
#' @keywords helper method
setMethod("c", signature(x = ".covClass"), function(x, ...) {
  elements <- list(x, ...)
  covClassList <- list()
  for (i in seq_len(length(elements))) {
    covClassList[[i]] <- new(
      ".covClass",
      n_cat = slot(elements[[i]], "n_cat"),
      n_cont = slot(elements[[i]], "n_cont"),
      mu_int = slot(elements[[i]], "mu_int"),
      mu_ext = slot(elements[[i]], "mu_ext"),
      var = slot(elements[[i]], "var"),
      cov = slot(elements[[i]], "cov"),
      prob_int = slot(elements[[i]], "prob_int"),
      prob_ext = slot(elements[[i]], "prob_ext")
    )
  }
  class(covClassList) <- ".covClass"
  covClassList
})


#' Simulate covariates with a multivariate normal distribution
#' @param dt a \code{matrix} generated in \code{\link{set_n}}
#' @param covObj an object of class \code{.covClass} generated in \code{\link{set_cov}}
#' @param seed the seed of R‘s random number generator. Default is the first element of .Random.seed
#'
#' @keywords internal method
#' @return a \code{matrix} containing simulated covariates information
add_cov <- function(dt, covObj, seed) {

  if (missing(seed)) {
    ps_message("Setting up covariance... Set seed to ", .Random.seed[1])
    seed <- .Random.seed[1]
  } else set.seed(seed)

  if (length(covObj) == 1) {
    covObj <- c(covObj)
  }

  for (i in seq_len(length(covObj))) {
    # call function from utils.R
    cov_int <- s_cov(
      ext = 0,
      dt = dt,
      n_cat = covObj[[i]]@n_cat,
      n_cont = covObj[[i]]@n_cont,
      mu = covObj[[i]]@mu_int,
      var = covObj[[i]]@var,
      cov = covObj[[i]]@cov,
      prob = covObj[[i]]@prob_int
    )

    cov_ext <- s_cov(
      ext = 1,
      dt = dt,
      n_cat = covObj[[i]]@n_cat,
      n_cont = covObj[[i]]@n_cont,
      mu = covObj[[i]]@mu_ext,
      var = covObj[[i]]@var,
      cov = covObj[[i]]@cov,
      prob = covObj[[i]]@prob_ext
    )

    dt <- cbind(dt, rbind(cov_ext, cov_int))
  }
  dt
}
