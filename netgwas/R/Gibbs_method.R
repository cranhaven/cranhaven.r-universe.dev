#-------------------------------------------------------------------------------#
# Package: Network-Based Genome-Wide Association Studies                        #
# Author: Pariya Behrouzi                                                       #
# Emails: <pariya.Behrouzi@gmail.com>                                           #
# Date: Nov 21th 2017                                                           #
#-------------------------------------------------------------------------------#

Gibbs_method = function(y, rho = NULL, n_rho = NULL, rho_ratio = NULL, Theta=NULL, ncores = 4, chain = 1, max.elongation = 10, em.tol=0.001) 
{
  p <- ncol(y)
  n <- nrow(y)
  lower.upper = lower.upper(y)

  if(is.null(rho))
  {
	if(is.null(n_rho)) n_rho = 10
	cr = cor(y, method="spearman") - diag(p)
	cr[is.na(cr)] <- 0
	rho_max = max(max(cr),-min(cr))
	if(rho_max == 0) 
	{
		ty <- npn(y, npn.func= "shrinkage")
		cr = cor(ty, method="spearman") - diag(p)
		rho_max = max(max(cr),-min(cr))
	}
	if(rho_max >= .7) rho_max = .7
	rho_min = rho_ratio * rho_max
	rho = exp(seq(log(rho_max), log(rho_min), length = n_rho))
	rm(cr, rho_max, rho_min, rho_ratio)
  }

  Gibbs.method <- calculate_EM_Gibbs(chain, y, rho = rho[chain], Theta=Theta, lower.upper = lower.upper, em.tol = em.tol, em.iter = max.elongation, gibbs.iter = 500, mc.iter = 1000, ncores = ncores)
  invisible(return(Gibbs.method))
}  