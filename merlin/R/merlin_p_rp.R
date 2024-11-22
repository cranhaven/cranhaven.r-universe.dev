#predict fucntions for family(rp)

merlin_p_rp_h <- function(gml,t=NULL)
{
  if (length(t)==0) t = merlin_util_timevar(gml)
  return(exp(merlin_util_xzb(gml,t) + log(merlin_util_xzb_deriv(gml,t))))
}

merlin_p_rp_ch <- function(gml,t=NULL)
{
  if (length(t)==0) t = merlin_util_timevar(gml)
  return(exp(merlin_util_xzb(gml,t)))
}

merlin_p_rp_logch <- function(gml,t=NULL)
{
  if (length(t)==0) t = merlin_util_timevar(gml)
  return(merlin_util_xzb(gml,t))
}

merlin_p_rp_s <- function(gml,t=NULL)
{
  if (length(t)==0) t = merlin_util_timevar(gml)
  return(exp(-exp(merlin_util_xzb(gml,t))))
}
