\name{F.cjs.simulate}
\alias{F.cjs.simulate}

\title{
F.cjs.simulate - Generation of capture histories that follow a CJS model.
}

\description{
This function generates capture history matrices that 
follow open Cormack-Jolly-Seber (CJS) models.  A super-population 
approach is taken wherein individuals with unique capture and survival 
probabilities are randomly 'born' into the 
realized population and captured. Any CJS model, including those with 
heterogeneous survival or capture probabilities, can be 
simulated.  Closed populations can also be simulated.   
}

\usage{
F.cjs.simulate(super.p, super.s, fit, N1 = 1000, 
  births.per.indiv = "constant.popln", R = 100)
}

\arguments{
  \item{super.p}{A matrix or vector of true capture probabilities in the super-population of individuals.
         \itemize{
            \item If \code{super.p} is a VECTOR, all individuals in the realized population will have the 
                same true capture probabilities, but capture probabilities can vary by occasion.  In this case, 
                \code{length(super.p)} capture occasions will be simulated.  
            \item If \code{super.p} is a MATRIX, the rows of \code{super.p} will be 
                 randomly selected and used for the capture probabilities of individuals 
                 when they are 'born' into the population.  
                 Number of rows in \code{super.p} must be 
                 greater than or equal to 1, and does not need to match number of rows in \code{super.s}.
                 When \code{super.p} is a matrix, \code{ncol(super.p)} capture occasions will be simulated.  
            }
         }

  \item{super.s}{A matrix or vector of true survival probabilities in the super-population of individuals.
         \itemize{
            \item If \code{super.s} is a VECTOR, all individuals in the realized population will have the 
                 same true survival probabilities after they are 'born' into the realized population.  
                 If the number of occasions to simulate is NS (see \code{super.p} above),
                 \code{super.s} must be of length \eqn{NS - 1}.
            \item If \code{super.p} is a MATRIX, the rows of \code{super.p} will be 
                 randomly selected and used as survival probabilities for individuals when they 
                 are 'born' into the population.  If the number of occasions to simulate 
                 is NS, \code{super.s} must have \eqn{NS - 1} columns. The vector
                 \code{super.s[,j]} is the set of true survival probabilities for animals alive just after 
                 occasion \code{j} until just before occasion \code{j+1}. Number of rows in \code{super.s} must be 
                 greater than or equal to 1, and does not need to match number of rows in \code{super.p}.
         }
         Number of survival probabilities in \code{super.s} is one less than NS because 
         survival probabilities apply between capture occasions. }

  \item{fit}{A previously estimated CJS object.  Instead of specifying \code{super.p} and \code{super.s},
        a fitted CJS model can be specified.  If either one of \code{super.p} or \code{super.s} is missing, 
        the (estimated) probabilities in \code{fit} will be used for their respective place.  That is, 
        if \code{super.p} is missing, \code{fit} must be present and \code{fit$p.hat} will be used for the 
        matrix of true capture probabilities. If \code{super.p} is missing, \code{fit} must be present and 
        \code{fit$s.hat} will be used for the 
        matrix of true survival probabilities. Because capture probabilities for the 
        first occasion are not usually estimable by CJS models, capture 
        probabilities for the first occasion are set equal to 1.0.  All members of the realized population 
        will be observed on the first occasion in this case. }
        
  \item{N1}{A scalar specifying the initial population size.  I.e., \code{N1} individuals will be 
        'born' into the realized population just before the first sampling occasion.}
  
  \item{births.per.indiv}{Either a vector of births per individual in the realized population, or 
        the string "constant.popln" (the default).  If 
        \code{births.per.indiv} = "constant.popln", the total number of births into the realized population between capture occasions 
        will equal the number of deaths between occasions.  In this case,  
        true realized population size will be (exactly) constant through time.  If \code{births.per.indiv} is 
        a vector of length \eqn{NS - 1}, then round( \eqn{N_j}{N(j)}*\code{births.per.indiv[,j]} ) births 
        will occur between occasions \code{j} and \code{j+1}, where \eqn{N_j}{N(j)} is the true number of 
        individuals in the realized population at occasion \code{j}.  Values in \code{birth.rate} must  
        be 0 or greater.  As an example, all animals in the realized population have one offspring  
        between occasions if \code{births.per.indiv = rep(1,NS)}.  Assuming a sex ratio of 50\%, all females 
        alive in the population between occasions have one offspring if \code{births.per.indiv} = 0.5. All females 
        in the population have two offspring if \code{births.per.indiv} = 1.}
  
  \item{R}{A scalar specifying the number of replications for the simulation. A total of \code{R} 
        independent capture history matrices will be generated. }
}

\details{Some examples: A two-group heterogeneous population contains one group of individuals with one common
        set of capture probabilities, and another group of individuals with another set of common 
        capture probabilities.  A population with one group of individuals having capture probability equal 
        to 0.25, and another group with capture probability equal to 0.75 can be simulated using 
        \itemize{ \item F.cjs.simulate( rbind( rep(0.25,10),rep(0.75,10) ), rep(s,9) ). },
        where \code{s} is some survival probability between 0 and 1. If \code{s} = 1, a 
        closed (no births or deaths) two-group heterogeneous model is simulated.  In this example, the 
        realized population is sampled for 10 occasions. 
        
        Non-equal sized heterogeneous groups can be simulated using        
        \itemize{ \item F.cjs.simulate( rbind( matrix(0.25,1,10),matrix(0.75,9,10) ), rep(1,9) ). }
        Using this call, approximately 10\% of individuals in the realized population will have capture probabilities 
        equal to 0.25, while 90\% will have capture probabilities equal to 0.75. Additional 
        groups can be included by including more rows with distinct probabilities in \code{super.p}.
        
        A population with heterogeneous capture probabilities proportional to a vector \code{w} can be simulated using 
        \itemize{ \item F.cjs.simulate( matrix( w/sum(x), length(w), 10), rep(s,9) ) }.  
    
        A stochastic population that varies around a specified size of \code{N1} = 1000
        can be simulated with a statement like 
        \itemize{ \item F.cjs.simulate( rep(0.25,10), rep(s,9), N1=1000, births.per.indiv=rep((1-s)/s,9) ). }
        In this simulation, N(j)*(1-s) individuals die between each occasion, but are replaced because the N(j)*s
        surviving individuals each have (1-s)/s offspring. 

        Because of the super-population approach taken here, it is not possible to specify which individuals 
        have which survival or capture probabilities, nor to guarantee that a certain number of individuals 
        in the realized population have capture probabilities equal to any particular value.  

}


\value{A list of length \code{R}.  Each component of this list is a list of length 2. 
        Each of these \code{R} sublists contains the following components:  

    \item{hists}{The simulated capture histories for a particular iteration. This is 
        a matrix with a random number of rows (due to the stochastic nature of captures) 
        and NS columns.}
    \item{popln.n}{A vector of length NS containing the true population sizes at each 
        sampling occasion.}}

%\note{Lists are returned to make it easier to use \code{lapply} and \code{sapply} for 
%    analysis (see Examples).  If \code{lapply} and \code{sapply} are used to apply an analysis 
%    to each iteration, it is relatively easy to parallelize code using the \code{snow} package, or 
%    similar.  Using package \code{snow}, it is possible to have separate processors analyze separate iterations 
%    of the simulation by simply replacing calls to \code{lapply} with calls to \code{parLapply}.  The 
%    author has had success with this approach and has realized significant reductions in computation speed when 
%    running in parallel mode. }


\author{Trent McDonald, WEST Inc. (tmcdonald@west-inc.com)}



\seealso{
\code{\link{F.cjs.estim}}
}

\examples{

\dontrun{

## Don't run specified because these examples can take > 10 seconds. 

## Simulate constant model, and analyze


ns <- 10
N <- 100
sim.list <- F.cjs.simulate( rep(0.3,ns), rep(0.9,ns-1), N1=N, R=100 )

f.analyze <- function(x){
    fit <- F.cjs.estim( ~1, ~1, x$hists, control=mra.control(maxfn=200, cov.meth=2) )
    if( fit$exit.code == 1 ){
        return( fit$n.hat )
    } else {
        return( rep(NA,ncol(x$hists)) )
    }
}
results <- t(sapply(sim.list, f.analyze))
plot( 1:10, colMeans(results, na.rm=TRUE), xlab="Occasion", 
  ylab="Mean population estimate", col="red", type="b")
abline( h=N )

## Plot RMSE by occasion
std <- apply(results, 2, sd, na.rm=TRUE)
bias <- apply(results - N, 2, mean, na.rm=TRUE)
plot( std, bias, type="n" )
text( std, bias, 2:10 )
abline(h=0)
title(main="RMSE by Sample Occasion")

## Show bias when heterogeniety is present
sim.list <- F.cjs.simulate( matrix(c(0.3,.7,.7,.7),4,ns), rep(0.9,ns-1), N1=N, R=100 ) 
results <- t(sapply(sim.list, f.analyze))
mean.N <- colMeans(results, na.rm=TRUE)
plot( 1:length(mean.N), mean.N, ylim=range(c(mean.N,N),na.rm=TRUE), 
  xlab="Occasion", ylab="Mean population estimate", col="red", type="b")
abline( h=N )
abline( h=mean(mean.N), col="red", lty=2)
title(main="Heterogeniety causes negative bias")


## Simulate CJS model, first estimate one
data(dipper.histories)
ct <- as.factor( paste("T",1:ncol(dipper.histories), sep=""))
attr(ct,"nan")<-nrow(dipper.histories)
dipper.cjs <- F.cjs.estim( ~tvar(ct,drop=c(1,2)), ~tvar(ct,drop=c(1,6,7)), dipper.histories )

## Now generate histories from it.
sim.list <- F.cjs.simulate( fit=dipper.cjs, N1=100, birth.rate=rep(1,6), R=100 )

## Now analyze generated histories using lapply or sapply.  Can fit any model. 
## Here we fit the correct model.
f.analyze <- function(x){
    #   write a counter to console, this is not necessary
    i <- get("i", env=.GlobalEnv) + 1
    cat(paste("Iteration", i, "\n"))
    assign("i",i,env=.GlobalEnv)
    
    ct <- as.factor( 1:ncol(x$hists) )
    fit <- F.cjs.estim( ~tvar(ct,nan=nrow(x$hists),drop=c(1,2)), 
        ~tvar(ct,nan=nrow(x$hists),drop=c(1,6,7)), 
        x$hists, control=mra.control(maxfn=200, cov.meth=2) )
    if( fit$exit.code == 1 ){
        return( fit$n.hat )
    } else {
        return( rep(NA,ncol(x$hists)) )
    }
}
i <- 0
results <- t(sapply(sim.list, f.analyze))
mean.N <- colMeans(results, na.rm=TRUE)
plot( 1:length(mean.N), mean.N, ylim=range(c(mean.N,N),na.rm=TRUE), 
  xlab="Occasion", ylab="Mean population estimate", col="red", type="b")
abline( h=N )
title(main="Time varying CJS model")
}

}

\keyword{ survival }
\keyword{ models }% __ONLY ONE__ keyword per line
