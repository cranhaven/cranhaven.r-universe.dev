scar.setup <- function(mf, control)
{
  # Vairable types
  mt <- attr(mf, "terms")
  ptot <- length(attr(mt, "term.labels"))
  gind <- attr(mt, "specials")$g
  sind <- attr(mt, "specials")$s
  ptot <- length(attr(mt, "term.labels"))
  # Create shape vector
  gsh <- rep("l", length(gind))
  fcons <- lapply(mf[gind], attr, "fcons")
  nonull <- !sapply(fcons, is.null)
  gsh[nonull] <- scar_lookup[unlist(fcons[nonull])]
  # Add covariates
  covind <- (1:ptot)[-(gind - 1)]
  csh <- rep("l", length(covind))
  fcons <- lapply(mf[covind + 1], attr, "fcons")
  nonull <- !sapply(fcons, is.null)
  csh[nonull] <- scar_lookup[unlist(fcons[nonull])]
  # Put together
  shp <- c(gsh, csh)
  if (sum(shp == "l") > 1) stop(paste0("'scar' only allows for one ",
    "unconstrained term"))
  control$shape <- shp
  return(control)
}
