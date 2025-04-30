\name{mogavs}
\alias{mogavs}
\alias{mogavs.formula}
\alias{mogavs.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Multiobjective Genetic Algorithm for Variable Selection}
\description{
The main function for the mogavs genetic algorithm, returning a list containing the full archive set of regression models tried and the nondominated set.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{mogavs}{default}(x, y, maxGenerations = 10*ncol(x), popSize = ncol(x), noOfOffspring = ncol(x),
crossoverProbability = 0.9, mutationProbability = 1/ncol(x), kBest = 1, 
plots = F, additionalPlots = F, ...)
\method{mogavs}{formula}(formula, data, maxGenerations= 10*ncol(x), popSize = ncol(x), 
noOfOffspring = ncol(x), crossoverProbability = 0.9, mutationProbability = 1/ncol(x), 
kBest = 1, plots = F, additionalPlots = F, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
  %%     ~~Describe \code{formula} here~~
  Formula interface with y~x1+x2 or y~. for predicting y with x1 and x2 or all predictors, respectively.
}
  \item{data}{
  A data frame containing the variables mentioned in the formula.
%%     ~~Describe \code{data} here~~
}
  \item{x}{
  An n x p matrix containing the n observations of p values used in the regression.
%%     ~~Describe \code{x} here~~
}
  \item{y}{
  An n x 1 vector of values to fit the regression to.
%%     ~~Describe \code{y} here~~
}
  \item{maxGenerations}{
  Number of maximum generations to be run in the evolutionary algorithm. Default is 10*ncol(x)
%%     ~~Describe \code{maxGenerations} here~~
}
  \item{popSize}{
  Population size, ie. how many regression models the population holds. Default is ncol(x).
%%     ~~Describe \code{popSize} here~~
}
  \item{noOfOffspring}{
  Indicates how many offspring models are generated for each generation. Default is ncol(x).
%%     ~~Describe \code{noOfOffspring} here~~
}
  \item{crossoverProbability}{
  Indicates the probability of crossover for each offpring. Default is 0.9.
%%     ~~Describe \code{crossoverProbability} here~~
}
  \item{mutationProbability}{
  Indicates the probability of mutation for each offspring. Default is 1/ncol(x).
%%     ~~Describe \code{mutationProbability} here~~
}
  \item{kBest}{
  Indicates how many best models for each number of variables are highlighted in printing at the end of the run (default=1).
%%     ~~Describe \code{kBest} here~~
}
  \item{plots}{
  Binary variable for turning plotting for each generation on/off.
%%     ~~Describe \code{plots} here~~
}
  \item{additionalPlots}{
  Binary variable for turning additional plotting at the end of the run on/off. Plot can also be generated after the run with given \code{createAdditionalPlots} functions.
%%     ~~Describe \code{additionalPlots} here~~
}
  \item{\dots}{
  Any additional arguments.
  }
}
\details{
Runs genetic algorithm for the linear regression model space, with predicting variables x and predicted values y. Alternatively, can be given a data frame and formula. Setting \code{plots=TRUE} creates for each generation a plot, showing the current efficient boundary of the models. Setting \code{additionalPlots=TRUE} gives out an additional plot at the end of the algorithm, showing the full set of tried models and the \code{kBest} best models for each number of variables. All plotting is turned off by default to make processing faster.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns model of class \code{mogavs} with items
\item{nonDominatedSet}{Matrix of the nondominated models.}
\item{numOfVariables}{Vector of the number of variables for each model in the nonDominatedSet.}
\item{MSE}{Vector of mean square errors for each model in the nonDominatedSet.}
\item{archiveSet}{The full archive set of models tried}
\item{kBest}{The value of kBest used}
\item{maxGenerations}{Number of generations used.}
\item{crossoverProbability}{The crossover probability used.}
\item{noOfOffspring}{Number of generated offspring for each generation.}
\item{popSize}{The population size.}
}
\references{
%% ~put references to the literature/web site here ~
Sinha, A., Malo, P. & Kuosmanen, T. (2015) A Multi-objective Exploratory Procedure for Regression Model Selection. \emph{Journal of Computational and Grahical Statistics, 24(1). pp. 154-182.}
}
\author{
%%  ~~who you are~~
Tommi Pajala <tommi.pajala@aalto.fi>
}

\seealso{
\code{\link{createAdditionalPlots}}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(sampleData)
#just a few generations to keep test fast
mogavs(y~.,data=sampleData,maxGenerations=5)

#with a more sensible number of generations, with all plotting on
\dontrun{mogavs(y~.,data=sampleData,maxGenerations=100,plots=TRUE,additionalPlots=TRUE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{regression}
