\name{gen4PMData}
\alias{gen4PMData}
\title{Generate item response data for 1, 2, 3, or 4-parameter IRT models}
\description{
Generate item response data for or 1, 2, 3 or 4-parameter IRT Models.}
\usage{ gen4PMData(NSubj, abcdParams, D = 1.702,  seed = NULL, 
                   theta = NULL, thetaMN = 0, thetaVar = 1)
}
\arguments{
 \item{NSubj}{the desired number of subject response vectors.}
 \item{abcdParams}{a p(items)-by-4 matrix of IRT item parameters: a = discrimination, b = difficulty, c = lower asymptote, and d = upper asymptote.}
 \item{D}{Scaling constant to place the IRF on the normal ogive or logistic metric. Default = 1.702 (normal ogive metric)}       
 \item{seed}{Optional seed for the random number generator.}
 \item{theta}{Optional vector of latent trait scores. If theta = NULL (the default value) then gen4PMData will simulate
 theta from a normal distribution.}
 \item{thetaMN}{Mean of simulated theta distribution. Default = 0.}
 \item{thetaVar}{Variance of simulated theta distribution. Default = 1}
}
\value{
 \item{data}{N(subject)-by-p(items) matrix of item response data.}
 \item{theta}{Latent trait scores.}
 \item{seed}{Value of the random number seed.}
}
\author{Niels Waller}
\examples{

## Generate simulated 4PM data for 2,000 subjects
# 4PM Item parameters from MMPI-A CYN scale

Params<-matrix(c(1.41, -0.79, .01, .98, #1  
                 1.19, -0.81, .02, .96, #2 
                 0.79, -1.11, .05, .94, #3
                 0.94, -0.53, .02, .93, #4
                 0.90, -1.02, .04, .95, #5
                 1.00, -0.21, .02, .84, #6
                 1.05, -0.27, .02, .97, #7
                 0.90, -0.75, .04, .73, #8  
                 0.80, -1.42, .06, .98, #9
                 0.71,  0.13, .05, .94, #10
                 1.01, -0.14, .02, .81, #11
                 0.63,  0.18, .18, .97, #12
                 0.68,  0.18, .02, .87, #13
                 0.60, -0.14, .09, .96, #14
                 0.85, -0.71, .04, .99, #15
                 0.83, -0.07, .05, .97, #16
                 0.86, -0.36, .03, .95, #17
                 0.66, -0.64, .04, .77, #18
                 0.60,  0.52, .04, .94, #19
                 0.90, -0.06, .02, .96, #20
                 0.62, -0.47, .05, .86, #21
                 0.57,  0.13, .06, .93, #22
                 0.77, -0.43, .04, .97),23,4, byrow=TRUE) 

 data <- gen4PMData(NSubj=2000, abcdParams = Params, D = 1.702,  
                    seed = 123, thetaMN = 0, thetaVar = 1)$data
 
 cat("\nClassical item difficulties for simulated data")                   
 print( round( apply(data,2,mean),2) )
}

\keyword{statistics}% at least one, from doc/KEYWORDS
