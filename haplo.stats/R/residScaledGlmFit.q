#$Author: sinnwell $
#$Date: 2011/11/10 15:29:40 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/residScaledGlmFit.q,v 1.2 2011/11/10 15:29:40 sinnwell Exp $
#$Locker:  $
#$Log: residScaledGlmFit.q,v $
#Revision 1.2  2011/11/10 15:29:40  sinnwell
#major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
#
#Revision 1.1  2004/02/26 22:35:38  sinnwell
#Initial revision
#
#Revision 1.3  2003/12/24 17:38:51  sinnwell
# fix the check for "logit" and "log" links to work in S and R
#
#Revision 1.2  2003/11/17 23:27:43  schaid
#made compatible with R
#
#Revision 1.1  2003/09/16 16:03:20  schaid
#Initial revision
#
residScaledGlmFit <- function(fit){

  # Given a glm model fit, compute scaled residual, {y - fit}/a(phi), and a(phi)
  #
  # For normal, a.phi = mse
  #     binom,  a.phi = 1
  #     pois,   a.phi = 1
  #
 
  resid <- NULL
  a.phi <- 1
  ## JPS 9/26/11 -- replace weights.expanded by prior.weights
  wt <- fit$prior.weights * fit$haplo.post.info$post

  switch(as.character(fit$family[1]),
        "Binomial"= , "binomial" =
            { if(grep("logit",as.character(casefold(fit$family[2][[1]]))) < 0)
                 stop("Only logit link for binomial")
              resid <- (fit$y - fit$fitted.values)
            },
        "Gaussian"=, "gaussian" =
            { y <- fit$y
              mu <- fit$fitted.values

              # Should df be reduced for estimation of haplotype freqs?             
              df.residual <- sum(wt) - length(fit$coef) 

              mse <- sum(wt*(y-mu)^2)/df.residual    
              resid <- (y - mu) / mse 
              a.phi <- mse
            },

        "Poisson"=, "poisson" =
            { if(grep("log", casefold(fit$family[2][[1]])) <0) stop("Only log link for poisson")
               resid <- (fit$y - fit$fitted.values)
            },

         stop(paste("Residual glm fit  for",fit$family[1],"not defined"))
      )

  return(list(resid=resid, a.phi=a.phi))

}

