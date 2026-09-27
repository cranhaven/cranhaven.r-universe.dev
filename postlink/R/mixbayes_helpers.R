#' Bayesian Mixture Helpers (Stan Generation + Priors)
#'
#' Internal helper functions for Bayesian mixture workers.
#' These are used by glmMixBayes / survregMixBayes engines and corresponding
#' fit*.adjMixBayes dispatch methods.
#'
#' @noRd

fill_defaults <- function(priors = list(), p_family, model_type = 'glm') {
  # Null prior means all defaults need to be filled. Create empty list
  if (is.null(priors)) {
    priors <- list()
  }

  # very simple validation of prior format
  if (!is.list(priors)) {
    stop("Invalid argument: 'priors' must be a named list.")
  }

  # Create list of defaults based on family and model
  # Intercept priors (intercept1/intercept2) are decoupled from slope priors
  # (beta1/beta2) to allow separate regularization. If the user does not specify
  # intercept priors, they inherit the corresponding slope prior defaults so that
  # the behavior is backward-compatible.
  if (model_type == 'glm') {
    defaults <- switch(
      p_family,
      "gaussian" = list(
        intercept1 = "normal(0,10)",
        intercept2 = "normal(0,10)",
        beta1 = "normal(0,5)",
        sigma1 = "cauchy(0,2.5)",
        beta2  = "normal(0,5)",
        sigma2 = "cauchy(0,2.5)",
        theta = "beta(1,1)"
      ),
      "poisson" = list(
        intercept1 = "normal(0,10)",
        intercept2 = "normal(0,10)",
        beta1 = "normal(0,5)",
        beta2 = "normal(0,5)",
        theta = "beta(1,1)"
      ),
      "binomial" = list(
        intercept1 = "normal(0,10)",
        intercept2 = "normal(0,10)",
        beta1 = "normal(0,2.5)",
        beta2 = "normal(0,5)",
        theta = "beta(1,1)"
      ),
      "gamma" = list(
        intercept1 = "normal(0,10)",
        intercept2 = "normal(0,10)",
        beta1 = "normal(0,5)",
        beta2 = "normal(0,5)",
        phi1 = "gamma(2,0.1)",
        phi2 = "gamma(2,0.1)",
        theta = "beta(1,1)"
      ),
      stop("`p_family` must be one of 'gaussian','poisson','binomial','gamma'",
           " for model_type == 'glm'")
    )
  } else if (model_type == 'survival') {
    defaults <- switch(
      p_family,
      "gamma" = list(
        # priors for intercept and regression coefficients
        intercept1 = "normal(0,10)",
        intercept2 = "normal(0,10)",
        beta1 = "normal(0,5)",
        beta2 = "normal(0,5)",
        theta = "beta(1,1)",
        # priors for shape parameters
        phi1 = "exponential(1)",
        phi2 = "exponential(1)"
      ),
      "weibull" = list(
        intercept1 = "normal(0,10)",
        intercept2 = "normal(0,10)",
        beta1 = "normal(0,2)",
        beta2 = "normal(0,2)",
        shape1 = "gamma(2,1)",
        shape2 = "gamma(2,1)",
        scale1 = "gamma(2,1)",
        scale2 = "gamma(2,1)",
        theta = "beta(1,1)"
      ),
      stop("`p_family` must be 'gamma' or 'weibull' for model_type 'survival'.")
    )
  } else {
    stop("Unknown model_type in fill_defaults")
  }

  # merge user priors and appropriate defaults
  utils::modifyList(defaults, priors)
}


stan_func <- function(stan_function_str) {
  if (!is.character(stan_function_str)) {
    stop("`stan_function_str` passed to stan_func() must be a single string.", call. = FALSE)
  } else if (!is_valid_stan_func(stan_function_str)) {
    stop("stan_func() called on invalid stan function")
  }
  structure(stan_function_str, class = c("stan_function_string", "character"))
}


is_valid_stan_func <- function(str) {
  txt <- gsub("[\r\n]+", " ", str)
  grepl("\\([^)]*\\).*\\{.*return.*\\}", txt, perl = TRUE) &&
    !grepl("<-", txt, fixed = TRUE)
}


validate_args <- function(priors, p_family) {
  # p_family must be one of the supported families
  supported_families <- c("gaussian","poisson","binomial","gamma", "weibull")
  if (!(p_family %in% supported_families)) {
    stop("`p_family` must be one of: ", paste(supported_families, collapse = ", "))
  }

  # If priors == NULL, no need to check its validity beyond that
  if (is.null(priors)) {
    return(invisible(TRUE))
  }

  # priors list must be a named list or null (in which case all defaults are used)
  if (!is.list(priors) || is.null(names(priors))) {
    stop("`priors` must be a named list of prior strings. Yours is:\n",
         priors)
  }

  for (elt in priors) {
    if (is.character(elt) && !is_func(elt)) { # only looking at prior strings
      trimmed_elt <- trimws(elt) # remove whitespace

      # verify basic structure <dist>(<args>)
      pattern <- "^([^(\\s]+)\\s*\\(([^)]*)\\)\\s*$"
      match <- regexec(pattern, trimmed_elt, perl = TRUE)
      parts <- regmatches(trimmed_elt, match)[[1]]
      dist = parts[2]
      hyperparam = parts[3]

      if (length(parts) == 0
          || (hyperparam == "" && all(!vapply(priors, is_func, logical(1))))
          || dist == "") {
        stop("Prior defining string '",
             elt,
             "' must be of form '<dist>(<comma-separated args>)'")
      }

      # send warning about untested distributions
      if (!(dist %in% c("normal", "cauchy", "beta", "gamma", "exponential", "multi_normal"))) {
        warning("An untested distribution was defined as prior. Tested ones",
                " include but are not limited to normal, cauchy, beta, gamma,",
                " exponential, and multi_normal. Correct output can be expected",
                " regardless if the distribution is a valid stan distribution,",
                " or defined by a stan function injection with stan_func()---",
                " see documentation for details.")
      }

      # validate args
      if (nzchar(trimws(hyperparam))) { # if args has non-whitespace
        # get individual args
        raw_hyperparam_list <- strsplit(hyperparam, ",", fixed = TRUE)[[1]]
        hyperparam_list <- trimws(raw_hyperparam_list) # trim white space

        if (any(hyperparam_list == "") || grepl(",\\s*$", hyperparam)) {
          stop("Missing values in list of hyperparameters of prior string")
        }

        for (hyperparam in hyperparam_list) {
          # if hyperparameter is not a number and not a key defined in the priors
          if (is.na(suppressWarnings(as.numeric(hyperparam)))
              && !(hyperparam %in% names(priors))) {
            stop("A variable used in a prior string is not defined.",
                 " If a string defining a prior in priors list is normal(x,y),",
                 " x and y must be their own keys in list 'priors' to be used.")
          }
        }
      }
    }
  }
  invisible(TRUE)
  }


is_func <- function(stan_function) {
  inherits(stan_function, "stan_function_string")
}


get_stan_definitions <- function(priors) {
  # generate from list of priors the necessary variable definition Stan strings
  # to concatenate before the prior definition
  # ex. 'vector[2] mu;' and 'mu = [1, 2];' from list item mu = c(1,2)
  variable_declarations <- "" # ex. cov_matrix[3] beta1_sigma;
  variable_definitions <- "" # ex. beta1_sigma = ...; or vector[3] vec = [1,2,3]'
  function_definitions <- "" # for injecting stan into function blocks
  for (item_key in names(priors)) {
    # concatenate stan code for variables in dynamic stan generation
    # generated from processing key-value pair in priors list

    processed_vars <- process_variable(priors[[item_key]], item_key)
    variable_declarations <- paste0(
      variable_declarations, processed_vars[["declaration"]])
    variable_definitions <- paste0(
      variable_definitions, processed_vars[["definition"]])
    function_definitions <- paste0(
      function_definitions, processed_vars[["stan_func"]]
    )
    }
  # combine separate declarations with definitions to create a single variable
  # holding a string of complete definitions of prior hyperparameters
  # These were generated separately because declarations must come before definitions
  variable_definitions <- paste0(variable_declarations, variable_definitions)

  list(
    variable_defs = variable_definitions,
    function_defs = function_definitions
  )
}


process_variable <- function(value, key) {
  if (is.character(value)) { # is string
    if (is_func(value)) { # the string is a function i.e. stan_func("...stan function...")
      warning("Detected stan function definition. Note that stan function
                injection does not fully validate stan syntax.")
      return(list(declaration="", definition="", stan_func=as.character(value)))
    } else { # the string is the prior definition itself ex. normal(x,y)
      return(list(declaration="", definition="", stan_func=""))
  }
  } else if (is.matrix(value)) {
    if (nrow(value) == ncol(value) && isSymmetric(value)) {
      dim = nrow(value)
    } else {
      stop("Non-square or unsymmetric matrix found in list 'priors'")
    }
    dec = paste0("cov_matrix[", dim, "] ", key, ";") # declaration
    component_defs = ""
    for (i in 1:dim) {
      for (j in 1:dim) {
        component <- paste0(key, "[", i, ", ", j, "] = ", value[i, j], ";" )
        component_defs <- paste0(component_defs, component)
      }
    }
    return(list(declaration=dec, definition=component_defs, stan_func=""))
  } else if (is.numeric(value)) { # non-matrix numeric
    if (length(value) == 1L) { # not a vector; scalar
      def = paste0("real ", key, " = ", value, ";")
      return(list(declaration="", definition=def, stan_func=""))
    } else { # value is a vector
      len = length(value)
      # convert vector to stan list as a string:
      stan_vector = paste0("[", paste(value, collapse=", "), "]'")
      def = paste0("vector[", len, "] ", key, " = ", stan_vector, ";")
      return(list(declaration="", definition=def, stan_func=""))
    }
  } else {
    stop("Element of invalid type or form found in list 'priors'")
  }
}


generate_stan <- function(components, priors = list()) {

  # helper defs from priors_helpers.R
  defs <- get_stan_definitions(priors)
  function_definitions <- defs$function_defs
  variable_definitions <- defs$variable_defs

  # checks that inputs are as expected
  if (identical(components, c("gaussian","gaussian"))) {
    # Fixed Stan code for linear-linear mixture
    stan_code <- paste(
      "functions {",
      function_definitions,
      "}",
      "data {",
      "  int<lower=1> N;             // Number of data points",
      "  int<lower=1> K;             // Number of predictors",
      "  matrix[N, K] X;             // Predictor matrix",
      "  vector[N] y;                // Response vector",
      "}",
      "transformed data {",
      variable_definitions, # defines any hyperparameter variables used in prior string
      "}",
      "parameters {",
      "  real<lower=0, upper=1> theta; // Mixture weight for the first component",
      "  real<lower=0> sigma1;         // Standard deviation of the first component",
      "  real<lower=0> sigma2;         // Standard deviation of the second component",
      "  vector[K] beta1;              // Regression coefficients for the first component",
      "  vector[K] beta2;              // Regression coefficients for the second component",
      "}",
      "model {",
      "  // priors: separate intercept and slope priors",
      "  sigma1 ~ ", priors$sigma1, ";",
      "  sigma2 ~ ", priors$sigma2, ";",
      "  beta1[1] ~ ", priors$intercept1, ";",
      "  beta2[1] ~ ", priors$intercept2, ";",
      "  if (K > 1) {",
      "    beta1[2:K] ~ ", priors$beta1, ";",
      "    beta2[2:K] ~ ", priors$beta2, ";",
      "  }",
      "  theta ~ ", priors$theta, ";",
      "  // Mixture model likelihood",
      "  for (n in 1:N) {",
      "    target += log_sum_exp(",
      "      log(theta) + normal_lpdf(y[n] | dot_product(X[n], beta1), sigma1),",
      "      log1m(theta) + normal_lpdf(y[n] | dot_product(X[n], beta2), sigma2)",
      "    );",
      "  }",
      "}",
      "generated quantities {",
      "  int<lower=1, upper=2> z[N];    // Mixture membership",
      "  for (n in 1:N) {",
      "    // Calculate unnormalized log probabilities for each component",
      "    vector[2] lw;",
      "    lw[1] = log(theta) + normal_lpdf(y[n] | dot_product(X[n], beta1), sigma1);",
      "    lw[2] = log1m(theta) + normal_lpdf(y[n] | dot_product(X[n], beta2), sigma2);",
      "    ",
      "    // Normalize probabilities using softmax",
      "    ",
      "    // Sample z[n] based on the posterior probabilities",
      "    z[n] = categorical_rng(softmax(lw));",
      "  }",
      "}",
      sep = "\n"
    )

    return(stan_code)

 } else if (identical(components, c("poisson", "poisson"))) {
  # Poisson-Poisson mixture code
    stan_code <- paste(
      "functions {",
      function_definitions,
      "}",
      "data {",
      "  int<lower=1> N;             // Number of observations",
      "  int<lower=0> y[N];          // Poisson response variable (counts)",
      "  int<lower=1> K;             // Number of predictors",
      "  matrix[N, K] X;             // Predictor matrix",
      "}",
      "",
      "transformed data {",
      variable_definitions,
      "}",
      "parameters {",
      "  real<lower=0, upper=1> theta;           // Mixing proportions (constrained to sum to 1)",
      "  vector[K] beta1;            // Regression coefficients for component 1",
      "  vector[K] beta2;            // Regression coefficients for component 2",
      "}",
      "",
      "model {",
      "  vector[N] log_lik1;         // Log-likelihood for component 1",
      "  vector[N] log_lik2;         // Log-likelihood for component 2",
      "  ",
      "  // Linear predictors for each component",
      "  vector[N] eta1 = X * beta1; // Linear predictor for component 1",
      "  vector[N] eta2 = X * beta2; // Linear predictor for component 2",
      "",
      "  // Calculate log-likelihoods for each component",
      "  for (n in 1:N) {",
      "    log_lik1[n] = poisson_log_lpmf(y[n] | eta1[n]); // Component 1",
      "    log_lik2[n] = poisson_log_lpmf(y[n] | eta2[n]); // Component 2",
      "    target += log_mix(theta, log_lik1[n], log_lik2[n]);",
      "  }",
      "",
      "  // Priors: separate intercept and slope priors",
      "  beta1[1] ~ ", priors$intercept1, ";",
      "  beta2[1] ~ ", priors$intercept2, ";",
      "  if (K > 1) {",
      "    beta1[2:K] ~ ", priors$beta1, ";",
      "    beta2[2:K] ~ ", priors$beta2, ";",
      "  }",
      "  theta ~ ", priors$theta, ";",
      "}",
      "",
      "generated quantities {",
      "  int<lower=1, upper=2> z[N];    // Mixture membership",
      "  for (n in 1:N) {",
      "    // Calculate unnormalized log probabilities for each component",
      "    vector[2] lw;",
      "    lw[1] = log(theta) + poisson_log_lpmf(y[n] | dot_product(X[n], beta1));",
      "    lw[2] = log1m(theta) + poisson_log_lpmf(y[n] | dot_product(X[n], beta2));",
      "    // Normalize probabilities using softmax",
      "    // Sample z[n] based on the posterior probabilities",
      "    z[n] = categorical_rng(softmax(lw));",
      "  }",
      "}",
      sep = "\n"
    )
    return(stan_code)

  } else if (identical(components, c("gamma","gamma"))) {
    stan_code <- paste(
      "functions {",
      function_definitions,
      "}",
      "data {",
      "  int<lower=1> N;               // Number of observations",
      "  int<lower=1> K;               // Number of predictors",
      "  vector<lower=0>[N] y;         // Response variable (positive values)",
      "  matrix[N, K] X;               // Predictor matrix",
      "}",
      "",
      "transformed data {",
      variable_definitions,
      "}",
      "parameters {",
      "  real<lower=0, upper=1> theta; // Mixing proportions (must sum to 1)",
      "  vector[K] beta1;              // Regression coefficients for component 1",
      "  vector[K] beta2;              // Regression coefficients for component 2",
      "  real<lower=0> phi1;           // Shape parameter for component 1",
      "  real<lower=0> phi2;           // Shape parameter for component 2",
      "}",
      "",
      "model {",
      "  vector[N] eta1 = X * beta1;  // eta for component 1",
      "  vector[N] eta2 = X * beta2;  // eta for component 2",
      "  vector[N] log_lik1;",
      "  vector[N] log_lik2;",
      "",
      "  // Calculate log-likelihoods for each component",
      "  // likelihood: shape=phi, rate=phi/exp(eta)",
      "  for (n in 1:N) {",
      "    log_lik1[n] = gamma_lpdf(y[n] | phi1, phi1 * exp(-eta1[n]));",
      "    log_lik2[n] = gamma_lpdf(y[n] | phi2, phi2 * exp(-eta2[n]));",
      "    target += log_mix(theta, log_lik1[n], log_lik2[n]);",
      "  }",
      "",
      "  // Priors: separate intercept and slope priors",
      "  beta1[1] ~ ", priors$intercept1, ";",
      "  beta2[1] ~ ", priors$intercept2, ";",
      "  if (K > 1) {",
      "    beta1[2:K] ~ ", priors$beta1, ";",
      "    beta2[2:K] ~ ", priors$beta2, ";",
      "  }",
      "  // Priors for shape parameters",
      "  phi1 ~ ", priors$phi1, ";",
      "  phi2 ~ ", priors$phi2, ";",
      "  theta ~ ", priors$theta, ";",
      "}",
      "generated quantities {",
      "  int<lower=1, upper=2> z[N];    // Mixture membership",
      "  vector[N] eta1 = X * beta1;  // Recompute eta1 for generated quantities",
      "  vector[N] eta2 = X * beta2;  // Recompute eta2 for generated quantities",
      "  for (n in 1:N) {",
      "    // Calculate unnormalized log probabilities for each component",
      "    vector[2] lw;",
      "    lw[1] = log(theta) + gamma_lpdf(y[n] | phi1, phi1 * exp(-eta1[n]));",
      "    lw[2] = log1m(theta) + gamma_lpdf(y[n] | phi2, phi2 * exp(-eta2[n]));",
      "    // Normalize probabilities using softmax",
      "    // Sample z[n] based on the posterior probabilities",
      "    z[n] = categorical_rng(softmax(lw));",
      "  }",
      "}",
      sep = "\n"
    )
    return(stan_code)

    } else if (identical(components, c("binomial","binomial"))) {
      # Logistic-logistic mixture Stan code
    stan_code <- paste(
      "functions {",
      function_definitions,
      "}",
      "data {",
      "  int<lower=1> N;           // Number of observations",
      "  int<lower=1> K;           // Number of predictors",
      "  matrix[N, K] X;           // Predictor matrix",
      "  int<lower=0, upper=1> y[N]; // Binary outcome",
      "}",
      "",
      "transformed data {",
      variable_definitions,
      "}",
      "parameters {",
      "  real<lower=0, upper=1> theta;     // Mixing proportion (for component 1)",
      "  vector[K] beta1;                 // Regression coefficients for component 1",
      "  vector[K] beta2;                 // Regression coefficients for component 2",
      "}",
      "",
      "model {",
      "  vector[N] eta1 = X * beta1;  // eta1 from component 1",
      "  vector[N] eta2 = X * beta2;  // eta2 from component 2",
      "  vector[N] log_lik1;  // Log-likelihood contributions from component 1",
      "  vector[N] log_lik2;  // Log-likelihood contributions from component 2",
      "  // priors: separate intercept and slope priors",
      "  beta1[1] ~ ", priors$intercept1, ";",
      "  beta2[1] ~ ", priors$intercept2, ";",
      "  if (K > 1) {",
      "    beta1[2:K] ~ ", priors$beta1, ";",
      "    beta2[2:K] ~ ", priors$beta2, ";",
      "  }",
      "  theta ~ ", priors$theta, ";",
      "",
      "  // Mixture model likelihood",
      "  for (n in 1:N) {",
      "    log_lik1[n] = bernoulli_logit_lpmf(y[n] | eta1[n]);",
      "    log_lik2[n] = bernoulli_logit_lpmf(y[n] | eta2[n]);",
      "    target += log_mix(theta, log_lik1[n], log_lik2[n]);",
      "  }",
      "}",
      "generated quantities {",
      "  int<lower=1, upper=2> z[N];      // Mixture membership for each observation",
      "  for (n in 1:N) {",
      "    vector[2] lw;",
      "    lw[1] = log(theta) + bernoulli_logit_lpmf(y[n] | dot_product(X[n], beta1));",
      "    lw[2] = log1m(theta) + bernoulli_logit_lpmf(y[n] | dot_product(X[n], beta2));",
      "    z[n] = categorical_rng(softmax(lw)); // Sample membership",
      "  }",
      "}",
      sep = "\n"
    )
    return(stan_code)

  } else {
    stop("Invalid mixture inputs. Must be: gaussian, poisson, gamma, or binomial")
  }
}

################################################################################
# New Functions for Pre-Compiled STAN Pipeline
################################################################################

# Regex parser to safely extract numbers from strings (e.g., "normal(0, 5)")
parse_prior_string <- function(prior_str) {
 prior_str <- trimws(prior_str)

 pattern <- "^([a-zA-Z0-9_]+)\\s*\\(([^)]*)\\)$"
 if (!grepl(pattern, prior_str)) {
  stop(sprintf("Prior string '%s' must be in format 'dist(arg1, arg2)'.", prior_str), call. = FALSE)
 }

 match <- regexec(pattern, prior_str)
 parts <- regmatches(prior_str, match)[[1]]

 args_str <- parts[3]
 args_num <- suppressWarnings(as.numeric(strsplit(args_str, ",")[[1]]))

 if (anyNA(args_num)) {
  stop(sprintf("Could not parse numeric arguments from '%s'.", prior_str), call. = FALSE)
 }

 list(dist = parts[2], args = args_num)
}

# Extracts arguments from priors list and builds a flat list for Stan data block
prepare_stan_priors <- function(priors, family, model_type) {
 priors <- fill_defaults(priors, family, model_type)

 get_args <- function(key, expected_len = 2) {
  if (is.null(priors[[key]])) stop(sprintf("Missing prior for '%s'", key), call. = FALSE)
  parsed <- parse_prior_string(priors[[key]])

  # Handle single-parameter exponential (converts to gamma shape=1, rate=param)
  if (parsed$dist == "exponential" && expected_len == 2 && length(parsed$args) == 1) {
   return(c(1.0, parsed$args[1]))
  }

  if (length(parsed$args) != expected_len) {
   stop(sprintf("Prior for '%s' expected %d arguments.", key, expected_len), call. = FALSE)
  }
  return(parsed$args)
 }

 # Core parameters for all models
 # Intercept priors are decoupled from slope priors for GLM models
 if (model_type == "glm") {
  out <- list(
   prior_intercept1_mu = get_args("intercept1")[1], prior_intercept1_sd = get_args("intercept1")[2],
   prior_intercept2_mu = get_args("intercept2")[1], prior_intercept2_sd = get_args("intercept2")[2],
   prior_beta1_mu = get_args("beta1")[1], prior_beta1_sd = get_args("beta1")[2],
   prior_beta2_mu = get_args("beta2")[1], prior_beta2_sd = get_args("beta2")[2],
   prior_theta_alpha = get_args("theta")[1], prior_theta_beta = get_args("theta")[2]
  )
 } else {
  out <- list(
   prior_intercept1_mu = get_args("intercept1")[1], prior_intercept1_sd = get_args("intercept1")[2],
   prior_intercept2_mu = get_args("intercept2")[1], prior_intercept2_sd = get_args("intercept2")[2],
   prior_beta1_mu = get_args("beta1")[1], prior_beta1_sd = get_args("beta1")[2],
   prior_beta2_mu = get_args("beta2")[1], prior_beta2_sd = get_args("beta2")[2],
   prior_theta_alpha = get_args("theta")[1], prior_theta_beta = get_args("theta")[2]
  )
 }

 if (model_type == "glm") {
  if (family == "gaussian") {
   out$prior_sigma1_loc <- get_args("sigma1")[1]; out$prior_sigma1_scale <- get_args("sigma1")[2]
   out$prior_sigma2_loc <- get_args("sigma2")[1]; out$prior_sigma2_scale <- get_args("sigma2")[2]
  } else if (family == "gamma") {
   out$prior_phi1_alpha <- get_args("phi1")[1]; out$prior_phi1_beta <- get_args("phi1")[2]
   out$prior_phi2_alpha <- get_args("phi2")[1]; out$prior_phi2_beta <- get_args("phi2")[2]
  }
 } else if (model_type == "survival") {
  if (family == "gamma") {
   out$prior_phi1_alpha <- get_args("phi1")[1]; out$prior_phi1_beta <- get_args("phi1")[2]
   out$prior_phi2_alpha <- get_args("phi2")[1]; out$prior_phi2_beta <- get_args("phi2")[2]
  } else if (family == "weibull") {
   out$prior_shape1_alpha <- get_args("shape1")[1]; out$prior_shape1_beta <- get_args("shape1")[2]
   out$prior_shape2_alpha <- get_args("shape2")[1]; out$prior_shape2_beta <- get_args("shape2")[2]
   out$prior_scale1_alpha <- get_args("scale1")[1]; out$prior_scale1_beta <- get_args("scale1")[2]
   out$prior_scale2_alpha <- get_args("scale2")[1]; out$prior_scale2_beta <- get_args("scale2")[2]
  }
 }

 return(out)
}
