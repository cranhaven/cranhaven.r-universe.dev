################################################################################
#
# Default parameters for coneproj
#
################################################################################

def.coneproj <- function(qp_pars){
  defpars <- list(msg = TRUE)
  utils::modifyList(defpars, qp_pars)
}