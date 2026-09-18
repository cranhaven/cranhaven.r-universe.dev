getCritVal <- function(n, filter, family = c("jules", "jsmurf", "jsmurfPS", "jsmurfLR",
                                             "hjsmurf", "hjsmurfSPS", "hjsmurfLR", "LR", "2Param"), 
                       alpha = 0.05, r = NULL, nq = n, options = NULL,
                       stat = NULL, messages = NULL, ...) {
  if (!is.numeric(n) || length(n) != 1 || !is.finite(n)) {
    stop("number of observations 'n' must be a single positive integer")
  }
  
  if (!is.integer(n)) {
    n <- as.integer(n + 1e-6)
  }
  
  if (n < 1L) {
    stop("number of observations 'n' must be a single positive integer")
  }
  
  if (!is.numeric(nq) || length(nq) != 1 || !is.finite(nq)) {
    stop("nq must be a single integer greather or equal than the number of observations 'n'")
  }
  
  if (!is.integer(nq)) {
    nq <- as.integer(nq + 1e-6)
  }
  
  if (nq < n) {
    stop("nq must be a single integer greather or equal than the number of observations 'n'")
  }
  
  if (!is.numeric(alpha) || length(alpha) != 1 || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a probability, i.e. a single numeric between 0 and 1")
  }
  
  if (!is(filter, "lowpassFilter")) {
    stop("filter must be an object of class 'lowpassFilter'")
  }
  
  family <- match.arg(family)
  if (family == "jules") {
    family <- "mDependentPS"
    output <- "value"
  } else {
    if (family %in% c("jsmurf", "jsmurfPS", "jsmurfLR")) {
      output <- "value"
    } else {
      output <- "vector"
    }
  }
  
  if (is.null(r)) {
    if(family == "LR" || family == "2Param") {
      r <- 1e3L
    } else {
      r <- 1e4L
    }
  }
  
  if (family == "LR" || family == "2Param") {
    ret <- stepR::critVal(output = output, alpha = alpha, n = n, nq = nq, family = family,
                          stat = stat, filter = filter, r = r, options = options, messages = messages, ...)
  } else {
    if (!identical(list(...), list())) {
      warning("... contains arguments, they will be ignored as ... is only usable for families 'LR' and '2Param'")
    }
    ret <- stepR::critVal(output = output, alpha = alpha, n = n, nq = nq, family = family,
                          stat = stat, filter = filter, r = r, options = options, messages = messages)
  }
  ret
}
