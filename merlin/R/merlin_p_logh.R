#predict fucntions for family(weibull)

merlin_p_logh_h <- function(gml,t=NULL)
{
  if (length(t)==0) t = merlin_util_timevar(gml)
  return(exp(merlin_util_xzb(gml,t)))
}

merlin_p_logh_ch <- function(gml,t=NULL)
{
    if (length(t)==0) t = merlin_util_timevar(gml)

    N   = gml$Nobs[gml$modtouse]
    gq 	= merlin_get_GK()
    qp	= sweep((t %*% (gq[[1]]/2)),1,(t/2),"+")
    if (gml$Nlevels & !gml$fixedonly) {
      result = matrix(0,nrow=N,ncol=gml$Ndim[gml$Nlevels])
    }
    else  result = matrix(0,nrow=N,ncol=1)
    for (q in 1:15) {
      result = result + sweep(merlin_p_logh_h(gml,qp[,q]),1,gq[[2]][q] * t/2,"*")
    }
    return(result)
}

merlin_p_logh_logch <- function(gml,t=NULL)
{
  if (length(t)==0) t = merlin_util_timevar(gml)
  return(log(merlin_p_logh_ch(gml,t)))
}

merlin_p_logh_s <- function(gml,t=NULL)
{
  if (length(t)==0) t = merlin_util_timevar(gml)
  return(exp(-merlin_p_logh_ch(gml,t)))
}
