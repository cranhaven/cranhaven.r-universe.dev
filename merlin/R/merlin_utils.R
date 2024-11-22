#
# Utility functions for merlin
#

#' merlin_util_depvar - returns the response variable(s)
#'
#' Utility function to extract the dependent variable(s) for the current model. If the model
#' is a survival/time-to-event model, then it will contain the event times in the first column
#' and the event indicator in the second.
#'
#' @author Emma C. Martin, Alessandro Gasparini and Michael J. Crowther
#'
#' @param gml merlin object - should not be edited
#'
#' @references Crowther MJ. Extended multivariate generalised linear and non-linear mixed effects models. \url{https://arxiv.org/abs/1710.02223}
#' @references Crowther MJ. merlin - a unified framework for data analysis and methods development in Stata. \url{https://arxiv.org/abs/1806.01615}
#' @references Martin EC, Gasparini A, Crowther MJ. merlin - an R package for mixed effects regression of linear and non-linear models.
#'

#' @export
merlin_util_depvar <- function(gml) {
  return(gml$data[gml$datause[[gml$modelind]],gml$y[[gml$modelind]]])
}


#' merlin_util_xzb - returns the observation-level complex predictor
#'
#' Utility function to extract the complex predictor evaluated at the current parameter estimates
#' for a particular model.
#'
#' @author Emma C. Martin, Alessandro Gasparini and Michael J. Crowther
#'
#' @param gml merlin object - should not be edited
#' @param t specifies the variable which represents time
#'
#' @references Crowther MJ. Extended multivariate generalised linear and non-linear mixed effects models. \url{https://arxiv.org/abs/1710.02223}
#' @references Crowther MJ. merlin - a unified framework for data analysis and methods development in Stata. \url{https://arxiv.org/abs/1806.01615}
#' @references Martin EC, Gasparini A, Crowther MJ. merlin - an R package for mixed effects regression of linear and non-linear models.
#'
#' @examples
#' library(merlin)
#' data(pbc.merlin, package = "merlin")
#'
#' # Linear fixed-effects model
#' merlin(model = list(logb ~ year),
#'        family = "gaussian",
#'        data = pbc.merlin)

#' @export
merlin_util_xzb <- function(gml,t=NULL)
{
    hast 	    = length(t)
    m         = gml$modelind
    modtouse	= gml$modtouse

    Ncmps 	  = gml$Ncmps[m]			# of components
    Nels 	    = gml$Nels[[m]]			# els per component

    b         = as.matrix(gml$par)
    cmpbindex = gml$bindex[[m]]
    xb        = matrix(0,nrow=gml$Nobs[modtouse],ncol=1)     # overall linear predictor
    hasb      = gml$cmphasb[[m]]

    # loop through the components
    cind = 1
    if (Ncmps) {

        for (c in 1:Ncmps) {

            eltype	 = gml$eltype[[m]][[c]]
            cmp		   = matrix(1,nrow=gml$Nobs[modtouse],ncol=1)

            # fixed elements
            for (e in 1:Nels[c]) {

                rebuild = 0
                if      (eltype[e] == 1) {                                          # variable
                    elvars = as.matrix(merlin_xz_var(gml,t,m,c,e))
                    rebuild = 1
                }
                else if (eltype[e] == 3) {                                          # rcs
                    elvars = merlin_xz_rcs(gml,m,c,e,t)
                    rebuild = 1
                }
                else if (eltype[e] == 4) {                                          # fp
                    elvars = merlin_xz_fp(gml,m,c,e,t)
                    rebuild = 1
                }

                #rebuild
                if (rebuild) {
                    Nold = ncol(cmp)
                    Nnew = ncol(elvars)
                    copyold = cmp
                    cmp = vector()
                    for (j in 1:Nnew) {
                        cmp = cbind(cmp,sweep(copyold,1,elvars[,j],"*"))
                    }
                }

            }

            #apply coefficients here
            if (hasb[cind]) {
                cmp 	= cmp %*% b[cmpbindex[1,cind]:cmpbindex[2,cind]]
                cind = cind + 1
            }

        		#now add in elements which can contain random effects -> dimensions effects above
        		nocontrib = 1
        		if (gml$Nlevels & gml$fixedonly==FALSE) {
        		      recmp = matrix(1,nrow=gml$Nobs[modtouse],ncol=gml$Ndim[gml$Nlevels])
        		}
            else  recmp = matrix(1,nrow=gml$Nobs[modtouse],ncol=1)

            # random elements
            for (e in 1:Nels[c]) {

                if 		  (eltype[e]==2) {										#random effect
                    if (gml$fixedonly==FALSE) recmp = recmp * merlin_xz_b(gml,m,c,e)
                    else 					            recmp = recmp * 0
                    nocontrib = 0
                }
                else if (eltype[e]==5) {										#EV[]
                  elmod = gml$elinfo[[m]][[c]][[e]]
                  if (hast) 	recmp  = recmp * merlin_util_ev_mod(gml,elmod,t)
                  else 		    recmp  = recmp * merlin_util_ev_mod(gml,elmod)
                  nocontrib = 0
                }
                else if (eltype[e]==6) {										#iEV[]
                  elmod = gml$elinfo[[m]][[c]][[e]]
                  if (hast) 	recmp  = recmp * merlin_util_ev_integ_mod(gml,elmod,t)
                  else 		    recmp  = recmp * merlin_util_ev_integ_mod(gml,elmod)
                  nocontrib = 0
                }
                else if (eltype[e]==7) {										#dEV[]
                  elmod = gml$elinfo[[m]][[c]][[e]]
                  if (hast) 	recmp  = recmp * merlin_util_ev_deriv_mod(gml,elmod,t)
                  else 		    recmp  = recmp * merlin_util_ev_deriv_mod(gml,elmod)
                  nocontrib = 0
                }
                else if (eltype[e]==8) {										#d2EV[]
                  elmod = gml$elinfo[[m]][[c]][[e]]
                  if (hast) 	recmp  = recmp * merlin_util_ev_deriv2_mod(gml,elmod,t)
                  else 		    recmp  = recmp * merlin_util_ev_deriv2_mod(gml,elmod)
                  nocontrib = 0
                }
                else if (eltype[e]==10) {										#XB[]
                    elmod = gml$elinfo[[m]][[c]][[e]]
                    if (hast) 	recmp  = recmp * merlin_util_xzb_mod(gml,elmod,t)
                    else 		    recmp  = recmp * merlin_util_xzb_mod(gml,elmod)
                    nocontrib = 0
                }
                else if (eltype[e]==11) {										#iXB[]
                    elmod = gml$elinfo[[m]][[c]][[e]]
                    if (hast) 	recmp  = recmp * merlin_util_xzb_integ_mod(gml,elmod,t)
                    else 		    recmp  = recmp * merlin_util_xzb_integ_mod(gml,elmod)
                    nocontrib = 0
                }
                else if (eltype[e]==12) {										#dXB[]
                    elmod = gml$elinfo[[m]][[c]][[e]]
                    if (hast) 	recmp  = recmp * merlin_util_xzb_deriv_mod(gml,elmod,t)
                    else 		    recmp  = recmp * merlin_util_xzb_deriv_mod(gml,elmod)
                    nocontrib = 0
                }
                else if (eltype[e]==13) {										#d2XB[]
                  elmod = gml$elinfo[[m]][[c]][[e]]
                  if (hast) 	recmp  = recmp * merlin_util_xzb_deriv2_mod(gml,elmod,t)
                  else 		    recmp  = recmp * merlin_util_xzb_deriv2_mod(gml,elmod)
                  nocontrib = 0
                }

            }

        		if (nocontrib) 	{
        		    xb = xb + cmp
        		}
        		else {
        		    if (ncol(cmp) < ncol(recmp)) xb2 = sweep(recmp,1,cmp,"*")
        		    else                         xb2 = cmp * recmp
        		    if (ncol(xb) < ncol(xb2))    xb  = sweep(xb2,1,xb,"+")
        		    else                         xb  = xb + xb2
        		}

        }
    }

    #intercept
    if (gml$hascons[m]) xb = xb + b[cmpbindex[1,cind]]

    return(xb)
}


#' merlin_util_xzb_mod - returns the observation-level complex predictor of the specified model
#'
#' Utility function to extract the complex predictor evaluated at the current parameter estimates
#' for a particular model.
#'
#' @author Michael J. Crowther
#'
#' @param gml merlin object - should not be edited
#' @param m specifies the merlin submodel
#' @param t specifies the variable which represents time
#'
#' @references Crowther MJ. Extended multivariate generalised linear and non-linear mixed effects models. \url{https://arxiv.org/abs/1710.02223}
#' @references Crowther MJ. merlin - a unified framework for data analysis and methods development in Stata. \url{https://arxiv.org/abs/1806.01615}
#' @references Martin EC, Gasparini A, Crowther MJ. merlin - an R package for mixed effects regression of linear and non-linear models.

#' @export
merlin_util_xzb_mod <- function(gml,m,t=NULL)
{
    gml$modelind = m
    return(merlin_util_xzb(gml,t))
}

#' merlin_util_xzb_deriv - returns the first derivative with respect to time of the
#' observation-level complex predictor
#'
#' Utility function to extract d/dt of the complex predictor evaluated at the current
#' parameter estimates for a particular model.
#'
#' @author Michael J. Crowther
#'
#' @param gml merlin object - should not be edited
#' @param t specifies the variable which represents time
#'
#' @references Crowther MJ. Extended multivariate generalised linear and non-linear mixed effects models. \url{https://arxiv.org/abs/1710.02223}
#' @references Crowther MJ. merlin - a unified framework for data analysis and methods development in Stata. \url{https://arxiv.org/abs/1806.01615}
#' @references Martin EC, Gasparini A, Crowther MJ. merlin - an R package for mixed effects regression of linear and non-linear models.
#'
#' @examples
#' library(merlin)
#' data(pbc.merlin, package = "merlin")
#'
#' # Linear fixed-effects model
#' merlin(model = list(logb ~ year),
#'        family = "gaussian",
#'        data = pbc.merlin)

#' @export
merlin_util_xzb_deriv <- function(gml,t=NULL)
{
    if (length(t)) tvar = t
    else 		       tvar = gml$data[gml$datause[[gml$modtouse]],gml$timevar[gml$modtouse]]

    smp  			   = .Machine$double.eps
    hstep 			 = rep_len(1,length(tvar))
    index        = abs(tvar) <= 1
    hstep[index] = abs(tvar)[index]
    hstep 			 = hstep * smp ^(1/3)

    lh 				   = merlin_util_xzb(gml,tvar + hstep)
    rh 				   = merlin_util_xzb(gml,tvar - hstep)
    return((lh - rh)/(2*hstep))
}

#' merlin_util_xzb_deriv_mod - returns the first derivative with respect to time of the
#' observation-level complex predictor of a specified model
#'
#' @author Michael J. Crowther
#'
#' @param gml merlin object - should not be edited
#' @param m specifies the merlin submodel
#' @param t specifies the variable which represents time
#'
#' @references Crowther MJ. Extended multivariate generalised linear and non-linear mixed effects models. \url{https://arxiv.org/abs/1710.02223}
#' @references Crowther MJ. merlin - a unified framework for data analysis and methods development in Stata. \url{https://arxiv.org/abs/1806.01615}
#' @references Martin EC, Gasparini A, Crowther MJ. merlin - an R package for mixed effects regression of linear and non-linear models.

#' @export
merlin_util_xzb_deriv_mod <- function(gml,m,t=NULL)
{
    gml$modelind = m
    return(merlin_util_xzb_deriv(gml,t))
}

#' merlin_util_xzb_deriv2 - returns the second derivative with respect to time of the
#' observation-level complex predictor
#'
#' @author Michael J. Crowther
#'
#' @param gml merlin object - should not be edited
#' @param t specifies the variable which represents time
#'
#' @references Crowther MJ. Extended multivariate generalised linear and non-linear mixed effects models. \url{https://arxiv.org/abs/1710.02223}
#' @references Crowther MJ. merlin - a unified framework for data analysis and methods development in Stata. \url{https://arxiv.org/abs/1806.01615}
#' @references Martin EC, Gasparini A, Crowther MJ. merlin - an R package for mixed effects regression of linear and non-linear models.

#' @export
merlin_util_xzb_deriv2 <- function(gml,t=NULL)
{
    if (length(t)) tvar = t
    else 		       tvar = gml$data[gml$datause[[gml$modtouse]],gml$timevar[gml$modtouse]]

    smp  			   = .Machine$double.eps
    hstep 			 = rep_len(1,length(tvar))
    index        = abs(tvar) <= 1
    hstep[index] = abs(tvar)[index]
    hstep 			 = hstep * smp ^(1/3)

    lh 				   = merlin_util_xzb(gml,tvar + hstep)
    rh 				   = merlin_util_xzb(gml,tvar - hstep)
    mid				   = merlin_util_xzb(gml,tvar)
    return((lh - 2 * mid + rh)/(hstep^2))
}

#' merlin_util_xzb_deriv2_mod - returns the second derivative with respect to time of the
#' observation-level complex predictor of the specified model
#'
#' @author Michael J. Crowther
#'
#' @param gml merlin object - should not be edited
#' @param m specifies the merlin submodel
#' @param t specifies the variable which represents time
#'
#' @references Crowther MJ. Extended multivariate generalised linear and non-linear mixed effects models. \url{https://arxiv.org/abs/1710.02223}
#' @references Crowther MJ. merlin - a unified framework for data analysis and methods development in Stata. \url{https://arxiv.org/abs/1806.01615}
#' @references Martin EC, Gasparini A, Crowther MJ. merlin - an R package for mixed effects regression of linear and non-linear models.

#' @export
merlin_util_xzb_deriv2_mod <- function(gml,m,t=NULL)
{
    gml$modelind = m
    return(merlin_util_xzb_deriv2(gml,t))
}

#' merlin_util_xzb_integ - returns the integral with respect to time of the
#' observation-level complex predictor
#'
#' @author Michael J. Crowther
#'
#' @param gml merlin object - should not be edited
#' @param t specifies the variable which represents time
#'
#' @references Crowther MJ. Extended multivariate generalised linear and non-linear mixed effects models. \url{https://arxiv.org/abs/1710.02223}
#' @references Crowther MJ. merlin - a unified framework for data analysis and methods development in Stata. \url{https://arxiv.org/abs/1806.01615}
#' @references Martin EC, Gasparini A, Crowther MJ. merlin - an R package for mixed effects regression of linear and non-linear models.

#' @export
merlin_util_xzb_integ <- function(gml,t=NULL)
{
    if (length(t)) tvar = t
    else 		       tvar = gml$data[gml$datause[[gml$modtouse]],gml$timevar[gml$modtouse]]

    gq      = statmod::gauss.quad(30,kind="legendre")
    nodes   = sweep((tvar %*% (gq[[1]]/2)),1,(tvar/2),"+")
    weights = (tvar %*% (gq[[2]]/2))

    res = matrix(0,nrow = length(tvar), ncol = gml$Ndim[gml$Nlevels])
    for (q in 1:30) {
       res = res + sweep(merlin_util_xzb(gml,nodes[,q]),1,weights[,q],"*")
    }
    return(res)
}

#' merlin_util_xzb_integ_mod - returns the integral with respect to time of the
#' observation-level complex predictor of the specified model
#'
#' @author Michael J. Crowther
#'
#' @param gml merlin object - should not be edited
#' @param m specifies the merlin submodel
#' @param t specifies the variable which represents time
#'
#' @references Crowther MJ. Extended multivariate generalised linear and non-linear mixed effects models. \url{https://arxiv.org/abs/1710.02223}
#' @references Crowther MJ. merlin - a unified framework for data analysis and methods development in Stata. \url{https://arxiv.org/abs/1806.01615}
#' @references Martin EC, Gasparini A, Crowther MJ. merlin - an R package for mixed effects regression of linear and non-linear models.

#' @export
merlin_util_xzb_integ_mod <- function(gml,m,t=NULL)
{
    gml$modelind = m
    return(merlin_util_xzb_integ(gml,t))
}

#' merlin_util_ev - returns the observation-level expected value of the outcome
#'
#' Utility function to extract the expected value of the outcome at the current parameter estimates
#' for a particular model.
#'
#' @author Michael J. Crowther
#'
#' @param gml merlin object - should not be edited
#' @param t specifies the variable which represents time
#'
#' @references Crowther MJ. Extended multivariate generalised linear and non-linear mixed effects models. \url{https://arxiv.org/abs/1710.02223}
#' @references Crowther MJ. merlin - a unified framework for data analysis and methods development in Stata. \url{https://arxiv.org/abs/1806.01615}
#' @references Martin EC, Gasparini A, Crowther MJ. merlin - an R package for mixed effects regression of linear and non-linear models.

merlin_util_ev <- function(gml,t=NULL)
{
  cmd = paste0(gml$invlink[gml$modelind],"(","merlin_util_xzb(gml,t)",")")
  return(eval(parse(text=cmd)))
}

#' merlin_util_ev_mod - returns the observation-level expected value of the outcome for a specified model
#'
#' Utility function to extract the expected value of the outcome at the current parameter estimates
#' for a specified model.
#'
#' @author Michael J. Crowther
#'
#' @param gml merlin object - should not be edited
#' @param m specifies the model
#' @param t specifies the variable which represents time
#'
#' @references Crowther MJ. Extended multivariate generalised linear and non-linear mixed effects models. \url{https://arxiv.org/abs/1710.02223}
#' @references Crowther MJ. merlin - a unified framework for data analysis and methods development in Stata. \url{https://arxiv.org/abs/1806.01615}
#' @references Martin EC, Gasparini A, Crowther MJ. merlin - an R package for mixed effects regression of linear and non-linear models.

merlin_util_ev_mod <- function(gml,m,t=NULL)
{
  gml$modelind = m
  cmd = paste0(gml$invlink[gml$modelind],"(","merlin_util_xzb(gml,t)",")")
  return(eval(parse(text=cmd)))
}

#' merlin_util_timevar - returns the time variable
#'
#' Utility function to extract the time variable(s) for the current model.
#'
#' @author Michael J. Crowther
#'
#' @param gml merlin object - should not be edited
#'
#' @references Crowther MJ. Extended multivariate generalised linear and non-linear mixed effects models. \url{https://arxiv.org/abs/1710.02223}
#' @references Crowther MJ. merlin - a unified framework for data analysis and methods development in Stata. \url{https://arxiv.org/abs/1806.01615}
#' @references Martin EC, Gasparini A, Crowther MJ. merlin - an R package for mixed effects regression of linear and non-linear models.
#'

#' @export
merlin_util_timevar <- function(gml) {
    return(gml$data[gml$datause[[gml$modelind]],gml$timevar[gml$modelind]])
}

#' merlin_util_ev_deriv - returns the first derivative with respect to time of the
#' observation-level expected value of the outcome
#'
#' Utility function to extract d/dt of the expected value of the outcome at the current
#' parameter estimates for a particular model.
#'
#' @author Michael J. Crowther
#'
#' @param gml merlin object - should not be edited
#' @param t specifies the variable which represents time
#'
#' @references Crowther MJ. Extended multivariate generalised linear and non-linear mixed effects models. \url{https://arxiv.org/abs/1710.02223}
#' @references Crowther MJ. merlin - a unified framework for data analysis and methods development in Stata. \url{https://arxiv.org/abs/1806.01615}
#' @references Martin EC, Gasparini A, Crowther MJ. merlin - an R package for mixed effects regression of linear and non-linear models.
#'
#' @examples
#' library(merlin)
#' data(pbc.merlin, package = "merlin")
#'
#' # Linear fixed-effects model
#' merlin(model = list(logb ~ year),
#'        family = "gaussian",
#'        data = pbc.merlin)

#' @export
merlin_util_ev_deriv <- function(gml,t=NULL)
{
  if (length(t)) tvar = t
  else 		       tvar = gml$data[gml$datause[[gml$modtouse]],gml$timevar[gml$modtouse]]

  smp  			   = .Machine$double.eps
  hstep 			 = rep_len(1,length(tvar))
  index        = abs(tvar) <= 1
  hstep[index] = abs(tvar)[index]
  hstep 			 = hstep * smp ^(1/3)

  lh 				   = merlin_util_ev(gml,tvar + hstep)
  rh 				   = merlin_util_ev(gml,tvar - hstep)
  return((lh - rh)/(2*hstep))
}

#' merlin_util_ev_deriv - returns the first derivative with respect to time of the
#' observation-level expected value of the specified outcome
#'
#' Utility function to extract d/dt of the expected value of the outcome at the current
#' parameter estimates for a specified model.
#'
#' @author Michael J. Crowther
#'
#' @param gml merlin object - should not be edited
#' @param m specifies the merlin submodel
#' @param t specifies the variable which represents time
#'
#' @references Crowther MJ. Extended multivariate generalised linear and non-linear mixed effects models. \url{https://arxiv.org/abs/1710.02223}
#' @references Crowther MJ. merlin - a unified framework for data analysis and methods development in Stata. \url{https://arxiv.org/abs/1806.01615}
#' @references Martin EC, Gasparini A, Crowther MJ. merlin - an R package for mixed effects regression of linear and non-linear models.

#' @export
merlin_util_ev_deriv_mod <- function(gml,m,t=NULL)
{
  gml$modelind = m
  return(merlin_util_ev_deriv(gml,t))
}

#' merlin_util_ev_deriv2 - returns the second derivative with respect to time of the
#' observation-level expected value of the outcome
#'
#' @author Michael J. Crowther
#'
#' @param gml merlin object - should not be edited
#' @param t specifies the variable which represents time
#'
#' @references Crowther MJ. Extended multivariate generalised linear and non-linear mixed effects models. \url{https://arxiv.org/abs/1710.02223}
#' @references Crowther MJ. merlin - a unified framework for data analysis and methods development in Stata. \url{https://arxiv.org/abs/1806.01615}
#' @references Martin EC, Gasparini A, Crowther MJ. merlin - an R package for mixed effects regression of linear and non-linear models.

#' @export
merlin_util_ev_deriv2 <- function(gml,t=NULL)
{
  if (length(t)) tvar = t
  else 		       tvar = gml$data[gml$datause[[gml$modtouse]],gml$timevar[gml$modtouse]]

  smp  			   = .Machine$double.eps
  hstep 			 = rep_len(1,length(tvar))
  index        = abs(tvar) <= 1
  hstep[index] = abs(tvar)[index]
  hstep 			 = hstep * smp ^(1/3)

  lh 				   = merlin_util_ev(gml,tvar + hstep)
  rh 				   = merlin_util_ev(gml,tvar - hstep)
  mid				   = merlin_util_ev(gml,tvar)
  return((lh - 2 * mid + rh)/(hstep^2))
}

#' merlin_util_ev_deriv2_mod - returns the second derivative with respect to time of the
#' observation-level expected value of the specified outcome
#'
#' @author Michael J. Crowther
#'
#' @param gml merlin object - should not be edited
#' @param m specifies the merlin submodel
#' @param t specifies the variable which represents time
#'
#' @references Crowther MJ. Extended multivariate generalised linear and non-linear mixed effects models. \url{https://arxiv.org/abs/1710.02223}
#' @references Crowther MJ. merlin - a unified framework for data analysis and methods development in Stata. \url{https://arxiv.org/abs/1806.01615}
#' @references Martin EC, Gasparini A, Crowther MJ. merlin - an R package for mixed effects regression of linear and non-linear models.

#' @export
merlin_util_ev_deriv2_mod <- function(gml,m,t=NULL)
{
  gml$modelind = m
  return(merlin_util_ev_deriv2(gml,t))
}

#' merlin_util_ev_integ - returns the integral with respect to time of the
#' observation-level expected value of the outcome
#'
#' @author Michael J. Crowther
#'
#' @param gml merlin object - should not be edited
#' @param t specifies the variable which represents time
#'
#' @references Crowther MJ. Extended multivariate generalised linear and non-linear mixed effects models. \url{https://arxiv.org/abs/1710.02223}
#' @references Crowther MJ. merlin - a unified framework for data analysis and methods development in Stata. \url{https://arxiv.org/abs/1806.01615}
#' @references Martin EC, Gasparini A, Crowther MJ. merlin - an R package for mixed effects regression of linear and non-linear models.

#' @export
merlin_util_ev_integ <- function(gml,t=NULL)
{
  if (length(t)) tvar = t
  else 		       tvar = gml$data[gml$datause[[gml$modtouse]],gml$timevar[gml$modtouse]]

  gq      = statmod::gauss.quad(30,kind="legendre")
  nodes   = sweep((tvar %*% (gq[[1]]/2)),1,(tvar/2),"+")
  weights = (tvar %*% (gq[[2]]/2))

  res = matrix(0,nrow = length(tvar), ncol = gml$Ndim[gml$Nlevels])
  for (q in 1:30) {
    res = res + sweep(merlin_util_ev(gml,nodes[,q]),1,weights[,q],"*")
  }
  return(res)
}

#' merlin_util_ev_integ_mod - returns the integral with respect to time of the
#' observation-level expected value of the specified outcome
#'
#' @author Michael J. Crowther
#'
#' @param gml merlin object - should not be edited
#' @param m specifies the merlin submodel
#' @param t specifies the variable which represents time
#'
#' @references Crowther MJ. Extended multivariate generalised linear and non-linear mixed effects models. \url{https://arxiv.org/abs/1710.02223}
#' @references Crowther MJ. merlin - a unified framework for data analysis and methods development in Stata. \url{https://arxiv.org/abs/1806.01615}
#' @references Martin EC, Gasparini A, Crowther MJ. merlin - an R package for mixed effects regression of linear and non-linear models.

#' @export
merlin_util_ev_integ_mod <- function(gml,m,t=NULL)
{
  gml$modelind = m
  return(merlin_util_ev_integ(gml,t))
}

#' merlin_util_ap - returns the current estimate of the ith ancillary parameter for the associated model
#'
#' @author Michael J. Crowther
#'
#' @param gml merlin object - should not be edited
#' @param i index for which ancillary parameter to extract
#'
#' @references Crowther MJ. Extended multivariate generalised linear and non-linear mixed effects models. \url{https://arxiv.org/abs/1710.02223}
#' @references Crowther MJ. merlin - a unified framework for data analysis and methods development in Stata. \url{https://arxiv.org/abs/1806.01615}
#' @references Martin EC, Gasparini A, Crowther MJ. merlin - an R package for mixed effects regression of linear and non-linear models.

#' @export
merlin_util_ap <- function(gml,i)
{
    return(gml$par[gml$apbindex[[gml$modelind]][i]])
}

#' merlin_util_ap_mod - returns the current estimate of the ith ancillary parameter for the specified model
#'
#' @author Michael J. Crowther
#'
#' @param gml merlin object - should not be edited
#' @param m specifies the merlin submodel
#' @param i index for which ancillary parameter to extract
#'
#' @references Crowther MJ. Extended multivariate generalised linear and non-linear mixed effects models. \url{https://arxiv.org/abs/1710.02223}
#' @references Crowther MJ. merlin - a unified framework for data analysis and methods development in Stata. \url{https://arxiv.org/abs/1806.01615}
#' @references Martin EC, Gasparini A, Crowther MJ. merlin - an R package for mixed effects regression of linear and non-linear models.

#' @export
merlin_util_ap_mod <- function(gml,m,i)
{
    return(gml$par[gml$apbindex[[m]][i]])
}
