#$Author: sinnwell $
#$Date: 2011/11/10 15:29:40 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/dglm.fit.q,v 1.4 2011/11/10 15:29:40 sinnwell Exp $
#$Locker:  $
#$Log: dglm.fit.q,v $
#Revision 1.4  2011/11/10 15:29:40  sinnwell
#major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
#
#Revision 1.3  2003/12/24 17:36:50  sinnwell
# fix the check for "logit" and "log" links to work in S and R
#
#Revision 1.2  2003/11/17 23:27:35  schaid
#made compatible with R
#
#Revision 1.1  2003/09/16 16:00:28  schaid
#Initial revision
#
dglm.fit <- function(fit, mse=NULL){

# Given a glm model fit, compute the probability density P(y|Xb).
# Note that mse has denom N if no prior.weights, or 
# sum(prior.weights) if they exist, and not N-p, where
# p is the number of coefficients in the model. This is
# because division by N gives the mle of the residual
# variance, and this mle is needed for computing the
# likelihood ratio test (LRT). Use of N-p leads to 
# biased LRT.

switch(as.character(fit$family[1]),
        "Binomial"= , "binomial" =
           {
             # for binomial, fitted.values = P(y=1|Xb)
             ifelse(fit$y==1, fit$fitted.values, (1-fit$fitted.values))
           },
         "Gaussian"=, "gaussian" =
            { y <- fit$y
              mu <- fit$fitted.values
              if(is.null(mse)) {
                if(is.null(fit$prior.weights)){
                  mse <- sum((y-mu)^2)/length(fit$y)
                }
                if(!is.null(fit$prior.weights)){
                  mse <- sum(fit$prior.weights*(y-mu)^2)/sum(fit$prior.weights)
                }
              }
##              cat("mse=", mse, "\n")
              dnorm(y, mean=mu, sd=sqrt(mse) )
            },
         "Poisson"=, "poisson" =
            { if(grep("log",casefold(fit$family[2][[1]])) < 0)
                stop("Only log link for poisson")
              lamda <- exp(fit$linear.predictors)
              dpois(fit$y,lamda)
            },

          stop(paste("Methods for",fit$family[1],"not defined"))
     )

}
