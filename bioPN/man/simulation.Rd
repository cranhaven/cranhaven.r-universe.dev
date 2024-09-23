\name{b) Simulation Functions}
\alias{GillespieOptimDirect}
\alias{GillespieDirectGB}
\alias{GibsonBruck}
\alias{GillespieDirectCR}
\alias{HaseltineRawlings}
\alias{RungeKuttaDormandPrince45}
\alias{PartitionedLeaping}
\title{Simulation of a biochemical system}
\description{These functions simulate a biochemical reacton system
  parameterized as a Petri Net. 
  \code{GillespieOptimDirect}, \code{GillespieDirectGB},
  \code{GibsonBruck}, and \code{GillespieDirectCR} performs pure
  stochastic simulations, \code{RungeKuttaDormandPrince45} a pure
  deterministic integration, \code{HaseltineRawlings} a hybrid of the
  above. \code{PartitionedLeaping} a dynamic-repartitioning
  simulation. Multiple runs can be performed at once.

  See \code{\link{init}} for a way of defining the model that is close
  to the way reactions are written.
}
\usage{
## Exact stochastic simulation: 
GillespieOptimDirect(model, timep, delta=1, runs=1)
GillespieDirectGB(model, timep, delta=1, runs=1)
GibsonBruck(model, timep, delta=1, runs=1)
GillespieDirectCR(model, timep, delta=1, runs=1)

## Pure deterministic:
RungeKuttaDormandPrince45(model, timep, delta=1, ect = 1e-09)

## Hybrid stochastic/deterministic:
HaseltineRawlings(model, timep, delta=1, runs=1, ect = 1e-09)

## Dynamic re-partitioning:
PartitionedLeaping(model, timep, delta=1, runs=1, ect = 1e-09)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{list containing named elements:}
  \item{timep}{It can be either a numeric, indicating for how long (in
    the same time units as the propensity constants) the process will
    run, or a functions (R or C), in which case can be used to change
    the protocol at time intervals. See details.}
  \item{delta}{Interval time at which the state will be saved.}
  \item{runs}{How many runs will be performed.}
  \item{ect}{Precision for the fast reactions.}
}
\details{
  model is a list containing the following elements:
  \itemize{
    \item{model$pre}{: pre matrix, with as many rows as transitions
      (reactions), and columns as places (reactants). It has
      the stoichiometrics of the left sides of the reactions.}
    \item{model$post}{: post matrix, with as many rows as transitions,
      and columns as places (products). It has the stoichiometrics of
      the right sides of the reactions.}
    \item{model$h}{: list of propensity constants or functions returning
      the propensity (with as many elements as transitions).}
    \item{model$slow}{: vector of zeros for slow transitions and ones
      for fast transitions. Only needed for
      \code{HaseltineRawlings}. Ignored otherwise.}
    \item{model$M}{: initial marking (state) of the system.}
    \item{model$place}{: vector with names of the places.}
    \item{model$transition}{: vector with names of the transitions.}
  }
}
\value{
  The functions return a list with the following elements:  
  \item{place}{vector with the names of the places if supplied. If not, the function creates names as follows: P1, P2, ...}
  \item{transition}{vector with the names of the transitions if supplied. If not, the function creates names as follows: T1, T2, ...}
  \item{dt}{vector containing the discretized times at which the state is saved (according to delta)}
  \item{run}{list with as many elements as runs. We will describe the first element, run[[1]], as the rest have exactly the same structure. It is also a list, with the following elements:}
  \item{run[[1]]$M}{list with as many elements as places, each of them containing the state of the system sampled according to delta.}
  \item{run[[1]]$transitions}{vector with as many elements as transitions, with the total of time each slow reaction fired.}
  \item{run[[1]]$tot.transitions}{numeric with the summ of run[[1]]$transitions.}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{init}}, \code{\link{atr}}
}
\examples{
## bioPN has been tested only on 64 bits machines.
## It may fail in 32 bits architecture.
if (.Machine$sizeof.pointer == 8) {

####### Reaction constants
H <- 10
K <- 6
r <- 0.25
c <- 3
b <- 2
#######

Gi <- 1
Ga <- 2
mRNA <- 3
Protein <- 4

model <- list(
    pre=matrix(c(1,0,0,0, 0,1,0,0, 0,1,0,0,
        0,0,1,0, 0,0,1,0, 0,0,0,1),
        ncol=4, byrow=TRUE),
    post=matrix(c(0,1,0,0, 1,0,0,0, 0,1,1,0,
        0,0,0,0, 0,0,1,1, 0,0,0,0),
        ncol=4, byrow=TRUE),
    h=list(c, b, H, 1, K, r),
    M=c(1,0,0,0))

timep <- 200
delta <- 1

##############################
## Completely Deterministic ##
##############################
Sim <- RungeKuttaDormandPrince45(model, timep, delta)

## Note, it could also be done as follows
## slow <- rep(0, transitions)
## Sim <- HaseltineRawlings(model, timep, delta, runs = 1)

mRNA.run <- Sim$run[[1]]$M[[mRNA]]
protein.run <- Sim$run[[1]]$M[[Protein]]

## Theoretical results (red lines in following plots)
Mean.mRNA <- c/(c+b)*H
Mean.protein <- Mean.mRNA * K/r

par(mfrow=c(1,2))
par(mar=c(2, 4, 2, 1) + 0.1)
plot(Sim$dt, mRNA.run,type="l", ylab="Mean",main="mRNA")
legend(x="bottom", paste("Deterministic run"))
abline(h=Mean.mRNA,col="red", lwd=1)
plot(Sim$dt, protein.run,type="l", ylab="Mean",main="Protein")
legend(x="bottom", paste("Deterministic run"))
abline(h=Mean.protein,col="red", lwd=1)


runs <- 100	## Increase to 10000 for better fit
###########################
## Completely Stochastic ##
###########################
set.seed(19761111)  ## Set a seed (for reproducible results)
Sim <- GillespieOptimDirect(model, timep, delta, runs)

## Note, it could also be done as follows
## slow <- rep(1, transitions)
## Sim <- HaseltineRawlings(model, timep, delta, runs)

mRNA.run <- sapply(Sim$run, function(run) {run$M[[mRNA]]})
protein.run <- sapply(Sim$run, function(run) {run$M[[Protein]]})

## Histograms of protein at different time points.
par(mfrow=c(2,2))
par(mar=c(2, 4, 2.5, 1) + 0.1)
hist(protein.run[Sim$dt == 1,], main="Protein Distribution at t=1sec")
hist(protein.run[Sim$dt == 2,], main="Protein Distribution at t=2sec")
hist(protein.run[Sim$dt == 10,], main="Protein Distribution at t=10sec")
hist(protein.run[Sim$dt == 200,], main="Protein Distribution at t=200sec")

## Theoretical results (red lines in following plots)
Mean.mRNA <- c/(c+b)*H
Var.mRNA <- b/(c*(1+c+b))*Mean.mRNA^2 + Mean.mRNA
Mean.protein <- Mean.mRNA * K/r
Var.protein <- r*b*(1+c+b+r)/(c*(1+r)*(1+c+b)*(r+c+b))*Mean.protein^2 +
  r/(1+r)*Mean.protein^2/Mean.mRNA + Mean.protein

if (runs > 1 ) {
  par(mfrow=c(2,2))
} else {
  par(mfrow=c(1,2))
}
par(mar=c(2, 4, 2, 1) + 0.1)
plot(Sim$dt, apply(mRNA.run,1,function(tpt) {mean(tpt)}),type="l", ylab="Mean",main="mRNA")
legend(x="bottom", paste("Gene, mRNA and Protein Stochastic\nRuns :", runs))
abline(h=Mean.mRNA,col="red", lwd=1)
plot(Sim$dt, apply(protein.run,1,function(tpt) {mean(tpt)}),type="l", ylab="Mean",main="Protein")
legend(x="bottom", paste("Gene, mRNA and Protein Stochastic\nRuns :", runs))
abline(h=Mean.protein,col="red", lwd=1)
if (runs > 1 ) {
  par(mar=c(2, 4, 0, 1) + 0.1)
  plot(Sim$dt, apply(mRNA.run,1,function(tpt) {var(tpt)}),type="l", ylab="Var")
  abline(h=Var.mRNA,col="red", lwd=1)
  plot(Sim$dt, apply(protein.run,1,function(tpt) {var(tpt)}),type="l", ylab="Var")
  abline(h=Var.protein,col="red", lwd=1)
}


######################################################################
## Hybrid: mRNA and protein fast, gene activation/inactivation slow ##
######################################################################
model$slow <- c(1,1,0,0,0,0)

Sim <- HaseltineRawlings(model, timep, delta, runs)

mRNA.run <- sapply(Sim$run, function(run) {run$M[[mRNA]]})
protein.run <- sapply(Sim$run, function(run) {run$M[[Protein]]})
    
Mean.mRNA <- c/(c+b)*H
Var.mRNA <- b/(c*(1+c+b))*Mean.mRNA^2
Mean.protein <- Mean.mRNA * K/r
Var.protein <- r*b*(1+c+b+r)/(c*(1+r)*(1+c+b)*(r+c+b))*Mean.protein^2

if (runs > 1 ) {
  par(mfrow=c(2,2))
} else {
  par(mfrow=c(1,2))
}
par(mar=c(2, 4, 2, 1) + 0.1)
plot(Sim$dt, apply(mRNA.run,1,function(tpt) {mean(tpt)}),type="l", ylab="Mean",main="mRNA")
legend(x="bottom", paste("Only Gene Stochastic\nRuns :", runs))
abline(h=Mean.mRNA,col="red", lwd=1)
plot(Sim$dt, apply(protein.run,1,function(tpt) {mean(tpt)}),type="l", ylab="Mean",main="Protein")
legend(x="bottom", paste("Only Gene Stochastic\nRuns :", runs))
abline(h=Mean.protein,col="red", lwd=1)
if (runs > 1 ) {
  par(mar=c(2, 4, 0, 1) + 0.1)
  plot(Sim$dt, apply(mRNA.run,1,function(tpt) {var(tpt)}),type="l", ylab="Var")
  abline(h=Var.mRNA,col="red", lwd=1)
  plot(Sim$dt, apply(protein.run,1,function(tpt) {var(tpt)}),type="l", ylab="Var")
  abline(h=Var.protein,col="red", lwd=1)
}
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
