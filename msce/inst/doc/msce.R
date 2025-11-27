## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=FALSE,fig.align='center', fig.cap='Schematic depiction of the MSCE model.'----
knitr::include_graphics('MSCE.jpg',dpi=96)

## -----------------------------------------------------------------------------
library(msce)
data("lungCancerSmoking")

## -----------------------------------------------------------------------------
lungCancerSmoking$laggedAge <- lungCancerSmoking$age-5

## -----------------------------------------------------------------------------
#consider smoking for less than one year as non-smoker
lungCancerSmoking$nonSmoke <- (lungCancerSmoking$ageStart==lungCancerSmoking$ageQuit) |
                       (lungCancerSmoking$ageStart >= lungCancerSmoking$laggedAge)
lungCancerSmoking$exSmoke <- (lungCancerSmoking$ageQuit < lungCancerSmoking$laggedAge) &
                      !lungCancerSmoking$nonSmoke
lungCancerSmoking$smoke <- !lungCancerSmoking$nonSmoke & !lungCancerSmoking$exSmoke

## -----------------------------------------------------------------------------
# t: nonSmoke: 0        0        laggedAge
#    exSmoke:  ageStart ageQuit  laggedAge
#    smoke:    0        ageStart laggedAge
t<-matrix(0,nrow=NROW(lungCancerSmoking),ncol=3)
t[,3] <- lungCancerSmoking$laggedAge
t[lungCancerSmoking$smoke,2] <-lungCancerSmoking$ageStart[lungCancerSmoking$smoke]
t[lungCancerSmoking$exSmoke,2] <- lungCancerSmoking$ageQuit[lungCancerSmoking$exSmoke]
t[lungCancerSmoking$exSmoke,1] <- lungCancerSmoking$ageStart[lungCancerSmoking$exSmoke]

# smInd: nonSmoke: 0 0 0
#        exSmoke:  0 1 0
#        smoke:    0 0 1
smInd <- matrix(0,nrow=NROW(lungCancerSmoking),ncol=3)
smInd[lungCancerSmoking$smoke,3] <- 1
smInd[lungCancerSmoking$exSmoke,2] <- 1

## -----------------------------------------------------------------------------
wrap <-function(pars)
{
  # assume alpha to be small and constant
  alpha <- matrix(1,nrow=1,ncol=3)
  
  Nnu0 <- matrix(exp(pars[1]),nrow=1,ncol=3)
  nu1 <- matrix(exp(pars[2]),nrow=1,ncol=3)
  
  # assume only gamma to depend on smoking
  gamma <- matrix(pars[3],nrow=NROW(lungCancerSmoking),ncol=3) + 
           pars[4]*smInd +
           pars[5]*smInd * (lungCancerSmoking$cigsPerDay>5)
  
  parList <- list(Nnu0=Nnu0, alpha=alpha,gamma=gamma,nu1=nu1)
  result <- tsce(t,parList)

  return (result$hazard)
}

## -----------------------------------------------------------------------------
loglik <- function(pars)
{
    return (-sum(dpois(lungCancerSmoking$cases, lungCancerSmoking$pyr*wrap(pars), log = TRUE)))
}

## -----------------------------------------------------------------------------
# use upper bounds to ensure gamma < alpha
minResult <- nlminb(start = c(-3,-14,0.1,0.05,0.0), objective = loglik, upper=c(0,0,0.3,0.3,0.3))
bestEstimates <- minResult$par
bestEstimates

## ----fig.dim=c(5,4),fig.align='center',echo=FALSE-----------------------------
alpha <- matrix(1,nrow=1,ncol=3)
Nnu0 <- matrix(exp(bestEstimates[1]),nrow=1,ncol=3)
nu1 <- matrix(exp(bestEstimates[2]),nrow=1,ncol=3)
# non-smoker
t<-matrix(0,nrow=95,ncol=3)
t[,3]<-1:95
gamma <- matrix(bestEstimates[3],nrow=95,ncol=3)
parList <- list(Nnu0=Nnu0, alpha=alpha,gamma=gamma,nu1=nu1)
resultN <- tsce(t,parList)
plot(6:100,resultN$hazard,type="l",log="y",
     xlab="age",ylab="hazard [per year]",ylim=c(1e-6,1e-2))
# smoker
# starts smoking at age 20
t[20:95,2] <-20
gamma[20:95,3] <- bestEstimates[3] + bestEstimates[4] + bestEstimates[5]
parList <- list(Nnu0=Nnu0, alpha=alpha,gamma=gamma,nu1=nu1)
resultS <- tsce(t,parList)
lines(6:100,resultS$hazard,type="l",lty=2)
# ex-smoker
# stops smoking at age 50
t[50:95,1] <-20
t[50:95,2] <-50
gamma[50:95,2] <- bestEstimates[3] + bestEstimates[4] + bestEstimates[5]
gamma[50:95,3] <- bestEstimates[3] 
parList <- list(Nnu0=Nnu0, alpha=alpha,gamma=gamma,nu1=nu1)
resultE <- tsce(t,parList)
lines(6:100,resultE$hazard,type="l",lty=3)
legend("topleft", legend=c("Smoking", "Quitting", "Non-Smoking"), 
       lty=c(2,3,1),cex=0.8)

