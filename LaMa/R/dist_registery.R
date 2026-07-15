#' 
#' # Internal — lives at package level
#' .arg_str <- function(pname, sname, tvary) {
#'   if (tvary[[pname]]) {
#'     paste0(pname, "_", sname, "[ind, j]")
#'   } else {
#'     paste0(pname, "_", sname, "[j]")
#'   }
#' }
#' 
#' .make_call <- function(dfun, arg_spec)
#'   function(sname, tvary) {
#'     args <- vapply(arg_spec, function(a)
#'       if (is.list(a)) {
#'         as.character(a$fixed) 
#'       } else {
#'         .arg_str(a, sname, tvary)
#'       },
#'       character(1))
#'     paste0(dfun, "(", sname, "[ind], ", paste(args, collapse = ", "), ")")
#'   }
#' 
#' 
#' ## Distribution registery, holding parameter names, link functions and inverse link functions
#' # each entry is list with
#' # - params: parameter names
#' # - link_pfx: the prefix given to parameters
#' # - inv_link: the inverse link function
#' .dist_registry <- list(
#'   
#'   # ---- Continuous, real-valued ----
#'   norm = list(
#'     params   = c("mean", "sd"),
#'     prefix   = c(mean = "",     sd = "log_"),
#'     link     = c(mean = "",     sd = "log"),
#'     inv_link = c(mean = "",     sd = "exp")
#'   ),
#'   
#'   logis = list(
#'     params   = c("location", "scale"),
#'     prefix   = c(location = "", scale = "log_"),
#'     link     = c(location = "", scale = "log"),
#'     inv_link = c(location = "", scale = "exp")
#'   ),
#'   
#'   cauchy = list(
#'     params   = c("location", "scale"),
#'     prefix   = c(location = "", scale = "log_"),
#'     link     = c(location = "", scale = "log"),
#'     inv_link = c(location = "", scale = "exp")
#'   ),
#'   
#'   t = list(                                        # standard t, no ncp in AD version
#'     params   = "df",
#'     prefix   = c(df = "log_"),
#'     link     = c(df = "log"),
#'     inv_link = c(df = "exp")
#'   ),
#'   
#'   sn = list(                                       # standardised skew-normal (mean=0, sd=1)
#'     params   = "alpha",
#'     prefix   = c(alpha = ""),
#'     link     = c(alpha = ""),
#'     inv_link = c(alpha = "")
#'   ),
#'   
#'   SHASHo = list(
#'     params   = c("mu", "sigma", "nu", "tau"),
#'     prefix   = c(mu = "", sigma = "log_", nu = "", tau = "log_"),
#'     link     = c(mu = "", sigma = "log",  nu = "", tau = "log"),
#'     inv_link = c(mu = "", sigma = "exp",  nu = "", tau = "exp")
#'   ),
#'   
#'   # ---- Continuous, positive ----
#'   lnorm = list(
#'     params   = c("meanlog", "sdlog"),
#'     prefix   = c(meanlog = "", sdlog = "log_"),
#'     link     = c(meanlog = "", sdlog = "log"),
#'     inv_link = c(meanlog = "", sdlog = "exp")
#'   ),
#'   
#'   gamma = list(                                    # named args needed: dgamma has rate/scale ambiguity
#'     params   = c("shape", "scale"),
#'     prefix   = c(shape = "log_", scale = "log_"),
#'     link     = c(shape = "log",  scale = "log"),
#'     inv_link = c(shape = "exp",  scale = "exp"),
#'     call_fn  = function(sname, tvary)
#'       paste0("dgamma(", sname, "[ind], shape = ", .arg_str("shape", sname, tvary),
#'              ", scale = ", .arg_str("scale", sname, tvary), ")")
#'   ),
#'   
#'   exp = list(
#'     params   = "rate",
#'     prefix   = c(rate = "log_"),
#'     link     = c(rate = "log"),
#'     inv_link = c(rate = "exp")
#'   ),
#'   
#'   weibull = list(
#'     params   = c("shape", "scale"),
#'     prefix   = c(shape = "log_", scale = "log_"),
#'     link     = c(shape = "log",  scale = "log"),
#'     inv_link = c(shape = "exp",  scale = "exp")
#'   ),
#'   
#'   lgamma = list(
#'     params   = c("shape", "scale"),
#'     prefix   = c(shape = "log_", scale = "log_"),
#'     link     = c(shape = "log",  scale = "log"),
#'     inv_link = c(shape = "exp",  scale = "exp")
#'   ),
#'   
#'   f = list(
#'     params   = c("df1", "df2"),
#'     prefix   = c(df1 = "log_", df2 = "log_"),
#'     link     = c(df1 = "log",  df2 = "log"),
#'     inv_link = c(df1 = "exp",  df2 = "exp")
#'   ),
#'   
#'   chisq = list(                                    # ncp not supported in AD version
#'     params   = "df",
#'     prefix   = c(df = "log_"),
#'     link     = c(df = "log"),
#'     inv_link = c(df = "exp")
#'   ),
#'   
#'   # ---- Continuous, (0, 1) ----
#'   beta = list(
#'     params   = c("shape1", "shape2"),
#'     prefix   = c(shape1 = "log_", shape2 = "log_"),
#'     link     = c(shape1 = "log",  shape2 = "log"),
#'     inv_link = c(shape1 = "exp",  shape2 = "exp")
#'   ),
#'   
#'   # ---- Count ----
#'   pois = list(
#'     params   = "lambda",
#'     prefix   = c(lambda = "log_"),
#'     link     = c(lambda = "log"),
#'     inv_link = c(lambda = "exp")
#'   ),
#'   
#'   nbinom = list(                                   # size/prob parameterisation (RTMB native)
#'     params   = c("size", "prob"),
#'     prefix   = c(size = "log_", prob = "logit_"),
#'     link     = c(size = "log",  prob = "logit"),
#'     inv_link = c(size = "exp",  prob = "plogis")
#'   ),
#'   
#'   nbinom2 = list(                                  # mean/variance parameterisation
#'     params   = c("mu", "var"),
#'     prefix   = c(mu = "log_", var = "log_"),
#'     link     = c(mu = "log",  var = "log"),
#'     inv_link = c(mu = "exp",  var = "exp")
#'   ),
#'   
#'   nbinom_robust = list(                            # params already on log scale
#'     params   = c("log_mu", "log_var_minus_mu"),
#'     prefix   = c(log_mu = "", log_var_minus_mu = ""),
#'     link     = c(log_mu = "", log_var_minus_mu = ""),
#'     inv_link = c(log_mu = "", log_var_minus_mu = "")
#'   ),
#'   
#'   binom = list(                                    # size fixed in dat
#'     params   = "prob",
#'     prefix   = c(prob = "logit_"),
#'     link     = c(prob = "logit"),
#'     inv_link = c(prob = "plogis"),
#'     call_fn  = function(sname, tvary)
#'       paste0("dbinom(", sname, "[ind], size[ind], ", .arg_str("prob", sname, tvary), ")")
#'   ),
#'   
#'   binom_robust = list(                             # size fixed in dat; logit_p already on logit scale
#'     params   = "logit_p",
#'     prefix   = c(logit_p = ""),
#'     link     = c(logit_p = ""),
#'     inv_link = c(logit_p = ""),
#'     call_fn  = function(sname, tvary)
#'       paste0("dbinom_robust(", sname, "[ind], size[ind], ", .arg_str("logit_p", sname, tvary), ")")
#'   ),
#'   
#'   compois = list(                                  # Conway-Maxwell-Poisson, mode parameterisation
#'     params   = c("mode", "nu"),
#'     prefix   = c(mode = "log_", nu = "log_"),
#'     link     = c(mode = "log",  nu = "log"),
#'     inv_link = c(mode = "exp",  nu = "exp")
#'   ),
#'   
#'   compois2 = list(                                 # Conway-Maxwell-Poisson, mean parameterisation
#'     params   = c("mean", "nu"),
#'     prefix   = c(mean = "log_", nu = "log_"),
#'     link     = c(mean = "log",  nu = "log"),
#'     inv_link = c(mean = "exp",  nu = "exp")
#'   ),
#'   
#'   tweedie = list(                                  # p (power) fixed in dat
#'     params   = c("mu", "phi"),
#'     prefix   = c(mu = "log_", phi = "log_"),
#'     link     = c(mu = "log",  phi = "log"),
#'     inv_link = c(mu = "exp",  phi = "exp"),
#'     call_fn  = function(sname, tvary)
#'       paste0("dtweedie(", sname, "[ind], ",
#'              .arg_str("mu", sname, tvary), ", ",
#'              .arg_str("phi", sname, tvary), ", p)")  # p from dat
#'   )
#' )
#' 
#' #' @export
#' dist_info <- function(dist_name) {
#'   spec <- .dist_registry[[dist_name]]
#'   if (is.null(spec))
#'     stop("Distribution '", dist_name, "' is not supported by hmm_template(). ",
#'          "Supported: ", paste(sort(names(.dist_registry)), collapse = ", "), ".")
#'   spec
#' }