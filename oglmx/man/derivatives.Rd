\name{scoreMean}
\alias{scoreMean}
\alias{scoreVar}
\alias{scoreThresh}
\alias{hessMean_Mean}
\alias{hessMean_Thresh}
\alias{hessMean_Var}
\alias{hessVar_Thresh}
\alias{hessVar_Var}
\alias{hessThresh_Thresh}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate derivatives of loglikelihood
%%  ~~function to do ... ~~
}
\description{
Functions used to calculate the first and second derivatives of the log-likelihood with respect to the estimated parameters.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
scoreMean(eta_1,eta_0,std.dev,prob,link)

scoreVar(eta_1,eta_0,std.dev,gstd.dev,prob,link)

scoreThresh(estThresh,outcomematrix,eta_1,eta_0,std.dev,prob,link)

hessMean_Mean(eta_1,eta_0,std.dev,prob,link)

hessMean_Var(eta_1,eta_0,std.dev,gstd.dev,prob,link)

hessVar_Var(eta_1,eta_0,std.dev,gstd.dev,hstd.dev,prob,link)

hessMean_Thresh(estThresh,outcomematrix,eta_1,eta_0,std.dev,prob,link)

hessVar_Thresh(estThresh,outcomematrix,eta_1,eta_0,std.dev,gstd.dev,prob,link)

hessThresh_Thresh(estThresh,outcomematrix,eta_1,eta_0,std.dev,prob,link)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eta_1}{numeric vector or matrix. Refers to the input to the link function to calculate the probability at the right threshold of the outcome.}
  \item{eta_0}{numeric vector or matrix. Refers to the input to the link function to calculate the probability at the left threshold of the outcome.}
  \item{std.dev}{numeric vector or matrix. The standard deviation of the error term for the observations given the data and parameters.}
  \item{prob}{numeric vector or matrix. Probability of the outcome given the parameters and data.}
  \item{link}{character, indicates link function for the estimated model.}
  \item{estThresh}{numeric vector indicating which of the threshold values are estimated.}
  \item{outcomematrix}{numeric matrix indicating the outcome for each observation.}
  \item{gstd.dev}{numeric vector or matrix. The first derivative of standard deviation of the error term for the observations given the data and parameters.}
  \item{hstd.dev}{numeric vector or matrix. The second derivative of standard deviation of the error term for the observations given the data and parameters.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
    numeric vector or matrix, depending on the structure of the inputs. Derivatives of the log-likelihood with respect to constants in the mean and variance equations and the threshold values.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Nathan Carroll, \email{nathan.carroll@ur.de}
%%  ~~who you are~~
}


\seealso{
\code{\link{oglmx}}%% ~~objects to See Also as \code{\link{help}}, ~~~
}
