\name{Estimate.b.k}
\alias{Estimate.b.k}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  RDS population size estimation
}
\description{
Estimate population size from respondent driven samples (RDS) using maximum likelihood, and several variation. 
The underlying idea is that the sample spreads like an epidemic in the target population as described in the reference.

}
\usage{
Estimate.b.k(rds.object, type = "mle", jack.control = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rds.object}{
    A object of class \code{rds-object} as constructed by \code{\link{initializeRdsObject}} or outputted by \code{Estimate.b.k} (depending on the \code{type} used).
}
  \item{type}{
    A character vector with the type of estimation. Possible values:
    \itemize{
      \item{\strong{mle} }{Maximum likelihood.}
      \item{\strong{integrated} }{Integrated maximum likelihood.}
      \item{\strong{observed} }{Estimate with observed degrees.}
      \item{\strong{jeffreys} }{MAP estimation with Jeffreys prior.}
      \item{\strong{parametric} }{Assume \eqn{\beta[k]:=\beta * \theta^k}.}
      \item{\strong{rescaling} }{Naive rescaling heuristic estimation. }
      \item{\strong{leave-d-out} }{Leave-d-out resampling estimator. }
    }
    
}
  \item{jack.control}{
    A object of class \code{jack.control} as constructed by \code{\link{makeJackControl}.}
}
}
\details{
As of version 0.95, this function is the main workhorse of the \code{chords} package. 
Given an \code{rds-class} object, it will return population size estimates for each degree. 
Note that for the \code{rescaling} and \code{parametric} estimators, the input \code{rds-object} is expected to contain some initial estimate in the \code{estimates} slot. 


See the reference for a description of the likelihood problem solved.
Optimization is performed by noting that likelihood is coordinate-wise convex, thus amounts to a series of line-searches.

}
\value{
An \code{rds-class} object with an updated \code{estimates} slot.
The \code{estiamtes} slot is \code{list} with the following components:
  \item{call}{The function call.}
  \item{Nk.estimates}{The estimated degree frequencies.}
  \item{log.bk.estimates}{The estimated sampling rates for each degree. In log scale.}
  \item{convergence}{0 if estimation of \eqn{N[k]}'s converged. Otherwise, 1 or -1, depending on the sign of the score function at the MLE. }
}
\references{
    [1] Berchenko, Y., Rosenblatt D.J., and S.D.W. Frost. 
    "Modeling and Analyzing Respondent Driven Sampling as a Counting Process."
    arXiv:1304.3505, 
  }

\seealso{
  \code{\link{initializeRdsObject}}, 
  \code{\link{makeRdsSample}}, 
  \code{\link{getTheta}}.
}

\examples{
# Preliminaries
data(brazil)
rds.object2<- initializeRdsObject(brazil)
see <- function(x) plot(x$estimates$Nk.estimates, type='h')

# Maximum likelihood
rds.object <- Estimate.b.k(rds.object = rds.object2 )
see(rds.object)

# View estimates:
plot(rds.object$estimates$Nk.estimates, type='h')
# Population size estimate:
sum(rds.object$estimates$Nk.estimates)
plot(rds.object$estimates$log.bk.estimates, type='h')

## Recover theta assuming b.k=b_0*k^theta
getTheta(rds.object)

# How many degrees were imputed?:
table(rds.object$estimates$convergence)


# Observed degree estimation:
rds.object.4 <- Estimate.b.k(rds.object = rds.object, type='observed')
see(rds.object.4)

# Naive rescaling
rds.object.5 <- Estimate.b.k(rds.object = rds.object, type='rescaling')
see(rds.object.5)

# Parametric rates
rds.object.6 <- Estimate.b.k(rds.object = rds.object, 
                             type='parametric')
see(rds.object.6)
jack.control <- makeJackControl(3, 1e1)
rds.object.7 <- Estimate.b.k(rds.object = rds.object, 
                             type='leave-d-out', 
                             jack.control = jack.control)
see(rds.object.7)
rds.object.8 <- Estimate.b.k(rds.object = rds.object, 
                             type='integrated', 
                             jack.control = jack.control)
see(rds.object.8)
rds.object.9 <- Estimate.b.k(rds.object = rds.object, 
                             type='jeffreys')
see(rds.object.9)



\dontrun{
## Simulated data example:
    dk <- c(2, 1e1) # unique degree classes
    true.dks <- rep(0,max(dk)); true.dks[dk] <- dk
    true.Nks <- rep(0,max(dk)); true.Nks[dk] <- 1e3
    beta <- 1 #5e-6
    theta <-  0.1
    true.log.bks <- rep(-Inf, max(dk))
    true.log.bks[dk] <- theta*log(beta*dk)
    sample.length <- 4e2
    nsims <- 1e2
    
    simlist <- list()
    for(i in 1:nsims){
      simlist[[i]] <- makeRdsSample(
        N.k =true.Nks , 
        b.k = exp(true.log.bks),
        sample.length = sample.length)
    }
    
    
    # Estimate betas and theta with chords:
    llvec <- rep(NA,nsims)
    bklist <- list()
    for(i in 1:nsims){
      # i <- 2
      simlist[[i]] <- Estimate.b.k(rds.object = simlist[[i]])
      # llvec[i] <- simlist[[i]]$estimates$likelihood
      bklist[[i]] <- simlist[[i]]$estimates$log.bk.estimates
    }
    b1vec <- bklist %>% lapply(.,"[",2) %>% unlist
    b2vec <- bklist %>% lapply(.,"[",10) %>% unlist
    
    hist(b1vec)
    abline(v=true.log.bks[2])
    hist(b2vec)
    abline(v=true.log.bks[10])
    
    beta0vec <- rep(-Inf,nsims)
    thetavec <- rep(-Inf,nsims)
    nvec <- rep(-Inf,nsims)
    converged <- rep(9999,nsims)
    
    for(i in 1:nsims){
      # i <- 2
      nvec[i] <- sum(simlist[[i]]$estimates$Nk.estimates)
      converged[i] <- sum(simlist[[i]]$estimates$convergence, na.rm=TRUE)
      # tfit <- getTheta(simlist[[i]])
      # beta0vec[i] <- tfit$log.beta_0
      # thetavec[i] <- tfit$theta
    }
    summary(beta0vec)
    summary(nvec)
    # summary(thetavec)
    # hist(thetavec)
    # abline(v=theta)
    hist(nvec)
    abline(v=sum(true.Nks), col='red')
    abline(v=median(nvec, na.rm = TRUE), lty=2)
    table(converged)
    
    # Try various re-estimatinons:
    rds.object2 <- simlist[[which(is.infinite(nvec))[1]]]
    
    rds.object <- Estimate.b.k(rds.object = rds.object2 )
    see(rds.object)
    rds.object$estimates$Nk.estimates
    
    rds.object.5 <- Estimate.b.k(rds.object = rds.object, type='rescaling')
    see(rds.object.5) # will not work. less than 2 converging estimates.
    rds.object.5$estimates$Nk.estimates
    
    rds.object.6 <- Estimate.b.k(rds.object = rds.object, type='parametric')
    see(rds.object.6) # will not work. less than 2 converging estimates.
    
    
    jack.control <- makeJackControl(3, 1e2)
    rds.object.7 <- Estimate.b.k(rds.object = rds.object, 
                             type='leave-d-out', 
                             jack.control = jack.control)
    see(rds.object.7)
    rds.object.7$estimates$Nk.estimates
    
    
    rds.object.8 <- Estimate.b.k(rds.object = rds.object, type='integrated')
    see(rds.object.8)
    rds.object.8$estimates$Nk.estimates
    
    
    rds.object.9 <- Estimate.b.k(rds.object = rds.object, type='jeffreys')
    see(rds.object.9)
    rds.object.9$estimates$Nk.estimates
}

}
