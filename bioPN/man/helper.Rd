\name{c) Model Definition}
\alias{init}
\alias{atr}
\alias{load.cfn}
\alias{unload.cfns}
\title{Helper functions for model definition}
\description{
  These functions are used to define models. They become more useful as
  the model has more places and transitions, as pre and post are sparse
  matrices so their direct manipulation may be error prone. See example
  of use below.
}
\usage{
init(place)
atr(trans.name=NULL)
load.cfn(place, code)
unload.cfns()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{place}{Places}
  \item{trans.name}{Name of the transition (reaction)}
  \item{code}{C code that returns the propensity}
}
\details{
  Function \code{init} accesses the frame of the calling function,
  creating variables with the names "model", "L", "R", and "h", that are
  considered reserved to bioPN. It also creates a variable for each
  element in the place vector submitted to the function
  \code{init}. Function \code{atr} creates a variable for each
  transition name sent.
  \code{load.cfn} and \code{unload.cfns} are used on cases where the
  transitions are of a special form, and a C function wants to be used
  to compute it for increase performance.
}
\value{
  The functions do not return values.
}

\seealso{
 \code{\link{GillespieOptimDirect}}, \code{\link{HaseltineRawlings}}
}
\examples{
## bioPN has been tested only on 64 bits machines.
## It may fail in 32 bits architecture.
if (.Machine$sizeof.pointer == 8) {

####### Constants definition (convenient but not required)
H <- 10
K <- 6
r <- 0.25
c <- 3
b <- 2
#######

place <- c( "Gi", "Ga", "mRNA", "Protein")

## WARNING: function init() accesses the frame
##          of the calling function, creating variables
##          with the names "model", "L", "R", and "h",
##          that are considered reserved to bioPN.
##          It also creates a variable for each element
##          in the place vector submitted to the function
##          init(). Function atr() creates a variable
##          for each transition name sent.

####### Initialization
init(place)

####### Start of model definition

## Gi -> Ga
h <- c
L[Gi] <- 1
R[Ga] <- 1
atr("gene_activation") ## Add this reaction

## Ga -> Gi
h <- b
L[Ga] <- 1
R[Gi] <- 1
atr("gene_inactivation")

## Ga -> Ga + mRNA
h <- H
L[Ga] <- 1
R[Ga] <- 1; R[mRNA] <- 1
atr("transcription")
  
## mRNA -> mRNA + Protein
h <- K
L[mRNA] <- 1
R[mRNA] <- 1; R[Protein] <- 1
atr("mRNA_degradation")

## mRNA -> 0
h <- 1
L[mRNA] <- 1
atr("translation")

## Protein -> 0
h <- r
L[Protein] <- 1
atr("protein_degradation")

####### End of model definition

model$M=rep(0, model$places)
model$M[Gi] <- 1

timep <- 200
delta <- 1

## Completely Deterministic
Sim <- RungeKuttaDormandPrince45(model, timep, delta)

runs <- 100

## Completely Stochastic
set.seed(19761111)  ## Set a seed (for reproducible results)
Sim <- GillespieOptimDirect(model, timep, delta, runs)

## Hybrid run
model$slow <- rep(0, model$transitions)
model$slow[c(gene_activation, gene_inactivation)] <- 1

set.seed(19761111)  ## Set a seed (for reproducible results)
Sim <- HaseltineRawlings(model, timep, delta, runs)
}
}
