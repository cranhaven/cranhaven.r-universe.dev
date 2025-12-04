
# A wrapper for secr::secr.fit to fix the damage done by the 'upgrade' to v. 4.

# fastproximity is new in v.4, and reconfigures some models (but not others) so that
#  they run faster. The default is fastproximity=TRUE. This means that you can have
#  a set of models where some have been reconfigured, others not. Unfortunately,
#  AICs are not comparable across these models. Even more unfortunately, secr::AIC
#  compares them.

# This function just facilitates the consistent use of secr.fit with fastproximity=FALSE.

# In versions prior to v.4.0.0, fastproximity is silently ignored.

secrFit <- function(capthist, model = list(D~1, g0~1, sigma~1), mask = NULL, buffer = NULL,
    CL = FALSE, detectfn = NULL, ...) {

  secr::secr.fit(capthist=capthist, model=model, mask=mask, buffer=buffer,
  CL=CL, detectfn=detectfn, ..., details=list(fastproximity=FALSE))

}



