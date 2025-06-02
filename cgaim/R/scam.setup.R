scam.setup <- function(mf, control)
{
  # Prepare the formula
  mt <- attr(mf, "terms")
  ptot <- length(attr(mt, "term.labels"))
  gind <- attr(mt, "specials")$g
  sind <- attr(mt, "specials")$s
  # Default parameters for control
  fcons <- sapply(mf[c(gind, sind)], "attr", "fcons")
  defcontrol <- list()
  if (length(unlist(fcons)) > 0) defcontrol$sp <- rep(0, length(c(gind, sind)))
  # Match control
  mc <- names(defcontrol) %in% names(control)
  control <- c(control, defcontrol[!mc])
  # Create call to "s" for each term
  term_s <- sapply(mf[c(gind, sind)], function(x){
    cpars <- list(str2lang("s"), str2lang(attr(x, "label")))
    fcons <- attr(x, "fcons")
    if (!is.null(fcons)) cpars$bs <- unname(scam_lookup[fcons])
    cpars <- c(cpars, attr(x, "s_opts"))
    cl <- as.call(cpars)
    deparse(cl)
  })
  # Add linear terms
  term_lin <- attr(mt, "term.labels")[-(c(gind, sind) - 1)]
  # Put together
  rhsform <- paste(c(term_s, term_lin), collapse = " + ")
  control$formula <- stats::reformulate(c(term_s, term_lin), all.vars(mt)[1],
    attr(mt, "intercept"))
  return(control)
}