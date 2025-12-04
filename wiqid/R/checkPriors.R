
# Combine the list of priors and the default list, check the result and
#   expand scalar values to vector when necessary.

# Not exported.

# Current version can only deal with vector-valued priors, not varcovar matrices.
# TODO ### check for NAs and sigma <= 0

# priors0 : list of priors specified by the user
# defaultPriors : list of default priors, used when no user specified value is missing;
#   each element of defaultPriors must be the correct length.

checkPriors <- function(priors0, defaultPriors) {
  priorErrorFlag <- FALSE
  priors <- replace (defaultPriors, names(priors0), priors0)
  parNames <- names(defaultPriors)
  cruft <- !(names(priors) %in% parNames)
  if(any(cruft)) {
    message("The following invalid elements in 'priors' will be ignored:")
    message(paste(names(priors)[cruft], collapse=" "))
  }
  priors <- priors[parNames]  # removes cruft in priors

  for(i in seq_along(parNames)) {
    this <- parNames[i]  # just to improve readability
    nPars <- length(defaultPriors[[this]])
    if(length(priors[[this]]) == 1)
      priors[[this]] <- rep(priors[[this]], nPars)
    if(length(priors[[this]]) != nPars) {
      message("Wrong length for priors for ", this, " which should have ", nPars, " value(s).")
      priorErrorFlag <- TRUE
    }
  }
  if(priorErrorFlag)
    stop("Invalid prior specification.")
  return(priors)
}
