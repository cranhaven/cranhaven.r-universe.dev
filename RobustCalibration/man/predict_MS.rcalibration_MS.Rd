\name{predict_MS}
\alias{predict_MS}
\alias{predict_MS.rcalibration_MS}
\alias{predict_MS,rcalibration_MS-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Prediction for the robust calibration model for multiple sources
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Function to make prediction on Robust Calibration models after the rcalibration class has been constructed for multiple sources.
}
\usage{
\S4method{predict_MS}{rcalibration_MS}(object, testing_input,
                                       X_testing=as.list(rep(0,object@num_sources)),
                                       testing_output_weights=NULL, 
                                       n_thinning=10,
                                       interval_est=NULL,
                                       interval_data=rep(F,length(testing_input)),
                                       math_model=NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of  class \code{rcalibration_MS}.}
  \item{testing_input}{a list of matrices containing the inputs where the \code{predict_MS} is to perform prediction. Each element of the list is a matrix of testing inputs for the corresponding source of data. The number of rows of the matrix is equal to the number of predictive outputs for the corresponding source. }
  \item{X_testing}{a list of matrices of mean/trend for prediction if specified.  The number of rows of the matrix is equal to the number of predictive outputs for the corresponding source.}
  \item{testing_output_weights}{a list of vecots for the weight of testing outputs for multiple sources.}
  \item{n_thinning}{number of points further thinning the MCMC posterior samples.}
    \item{interval_est}{a list of vectors for the posterior predctive credible interval for multiple sources. If interval_est is NULL, we do  not compute the posterior credible interval.  It can be specified as a vector of values ranging from zero to one. E.g.  }
  \item{interval_data}{a vector of bool values to decide whether the experimental noise is included for computing the posterior credible interval.  }
  \item{math_model}{
  a list of functions  for the math model to be calibrated for multiple sources.
  }
  \item{...}{extra arguments to be passed to the function (not implemented yet).}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%Provide here some details.
%}


\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
The returned value is a S4 CLass \code{predictobj.rcalibration}. 
}
\references{
A. O'Hagan and M. C. Kennedy (2001), \emph{Bayesian calibration of computer models}, \emph{Journal of the Royal Statistical Society: Series B (Statistical Methodology}, \bold{63}, 425-464.

K. R. Anderson and M. P. Poland (2016), \emph{Bayesian estimation of magma supply, storage, and eroption rates using a multiphysical volcano model: Kilauea volcano, 2000-2012.}. \emph{Eath and Planetary Science Letters}, \bold{447}, 161-171.

K. R. Anderson and M. P. Poland (2017), \emph{Abundant carbon in the mantle beneath Hawaii}. \emph{Nature Geoscience}, \bold{10}, 704-708.

Bayarri, Maria J and Berger, James O and Paulo, Rui and Sacks, Jerry and Cafeo, John A and Cavendish, James and Lin, Chin-Hsu and Tu, Jian (2007) \emph{A framework for validation of computer models}. \emph{Technometrics}. \bold{49}, 138--154.

M. Gu (2016), \emph{Robust Uncertainty Quantification and Scalable Computation for Computer Models with Massive Output}, Ph.D. thesis., Duke University.

M. Gu and L. Wang (2017) \emph{Scaled Gaussian Stochastic Process for Computer Model Calibration and Prediction}. arXiv preprint arXiv:1707.08215.

M. Gu (2018) \emph{Jointly Robust Prior for Gaussian Stochastic Process in Emulation, Calibration and Variable Selection
}. arXiv preprint arXiv:1804.09329.
}

\author{
%%  ~~who you are~~
\packageAuthor{RobustCalibration}

Maintainer: \packageMaintainer{RobustCalibration}
}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{
#---------------------------------------------------------------------------------------------
# An example for calibrating and predicting mathematical models for data from multiple sources
#---------------------------------------------------------------------------------------------
    


library(RobustCalibration)


##reality
test_funct<-function(x){
  sin(pi*x/2)+2*cos(pi*x/2)
}


##math model from two sources
math_model_source_1<-function(x,theta){
  sin(theta*x) 
}

math_model_source_2<-function(x,theta){
  cos(theta*x) 
}

input1=seq(0,2,2/(10-1))
input2=seq(0,3,3/(10-1))
##
output1=test_funct(input1)+rnorm(length(input1), sd=0.01)
output2=test_funct(input2)+rnorm(length(input2), sd=0.02)

plot(input1, output1)
plot(input2, output2)



design=list()
design[[1]]=as.matrix(input1)
design[[2]]=as.matrix(input2)

observations=list()
observations[[1]]=output1
observations[[2]]=output2


p_theta=1


theta_range=matrix(0,p_theta,2)
theta_range[1,]=c(0, 8)  
simul_type=c(1,1)

math_model=list()

math_model[[1]]=math_model_source_1
math_model[[2]]=math_model_source_2


## calibrating two mathematical models for these two sources
model_sgasp=rcalibration_MS(design=design, observations=observations, p_theta=1,
                            simul_type=simul_type,math_model=math_model,
                            theta_range=theta_range, 
                            S=10000,S_0=2000,
                            discrepancy_type=rep('S-GaSP',length(design)))

#plot(model_sgasp@post_theta[,1],type='l')
mean(model_sgasp@post_theta[,1])

testing_input1=seq(0,2,2/(25-1))

testing_input2=seq(0,3,3/(25-1))

testing_input=list()
testing_input[[1]]=as.matrix(testing_input1)
testing_input[[2]]=as.matrix(testing_input2)


predict_sgasp=predict_MS(model_sgasp, testing_input, math_model=math_model)
  

testing_output1=test_funct(testing_input1)
testing_output2=test_funct(testing_input2)


plot(predict_sgasp@mean[[1]])
lines(testing_output1)

plot(predict_sgasp@mean[[2]])
lines(testing_output2)



}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
