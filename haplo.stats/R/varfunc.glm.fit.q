#$Author: sinnwell $
#$Date: 2003/12/24 17:35:55 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/varfunc.glm.fit.q,v 1.3 2003/12/24 17:35:55 sinnwell Exp $
#$Locker:  $
#$Log: varfunc.glm.fit.q,v $
#Revision 1.3  2003/12/24 17:35:55  sinnwell
#fix the check for "logit" and "log" links to work in S and R
#
#Revision 1.2  2003/11/17 23:27:53  schaid
#made compatible with R
#
#Revision 1.1  2003/09/16 16:03:28  schaid
#Initial revision
#
varfunc.glm.fit <- function(fit){

  # Given a glm model fit, compute the variance function, b''(theta),
  # where theta = X'B

  switch(as.character(fit$family[1]),
        "Binomial"= , "binomial" =
         { if(grep("logit",casefold(fit$family[2][[1]])) < 0) stop("Only logit link for binomial")
           fit$fitted.values * (1 - fit$fitted.values)
         },
         "Gaussian"=, "gaussian" =
         {
           rep(1, length(fit$fitted.values))
         },
        "Poisson"=, "poisson" =
         { if(grep("log",casefold(fit$family[2][[1]])) < 0)
             stop("Only log link for poisson")
           fit$fitted.values
         },
         stop(paste("Variance function for",fit$family[1],"not defined"))
         )
}

