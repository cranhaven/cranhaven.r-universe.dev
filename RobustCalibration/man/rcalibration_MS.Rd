\name{rcalibration_MS}
\alias{rcalibration_MS}
%\alias{show.rgasp}
\alias{rcalibration_MS-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Setting up the robust Calibration model for multiple sources data
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Setting up the Calibration model for estimating the parameters via MCMC for multiple sources. 
%The range and noise-variance ratio parameters are given and/or have been estimated.
}
\usage{
  rcalibration_MS(design, observations, p_theta=NULL, index_theta=NULL,
                  X=as.list(rep(0,length(design))), 
                  have_trend=rep(FALSE,length(design)),
                  simul_type=rep(1, length(design)),
                  input_simul=NULL, output_simul=NULL,
                  simul_nug=rep(FALSE,length(design)),loc_index_emulator=NULL,
                  math_model=NULL, 
                  theta_range=NULL, 
                  sd_proposal_theta=NULL, 
                  sd_proposal_cov_par=NULL,
                  S=10000,S_0=2000, thinning=1,measurement_bias=FALSE, 
                  shared_design=NULL,have_measurement_bias_recorded=F,
                            shared_X=0,have_shared_trend=FALSE,
                  discrepancy_type=rep('S-GaSP',length(design)+measurement_bias),
                  kernel_type=rep('matern_5_2',length(design)+measurement_bias),
                  lambda_z=as.list(rep(NA,length(design)+measurement_bias)),
                  a=NULL,b=NULL,alpha=NULL,
                  output_weights=NULL,...)
 % \S4method{show}{rgasp}(object)    
      
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{design}{a list of observed inputs from multiple sources. Each element of the list is a matrix of observable inputs, where each row is a row vector of observable inputs corresponding to one observation and the number of field or experimental data is the total number of rows. 
%%     ~~Describe \code{design} here~~
}
  \item{observations}{a list of experimental data from multiple sources. Each element is a vector of observations. 
%%     ~~Describe \code{response} here~~
}
  \item{index_theta}{a list of vectors for the index of calibration parameter contained in each source.
%%     ~~Describe \code{response} here~~
}
  \item{p_theta}{an integer about the number of parameters, which should be specified by the user. 
%%     ~~Describe \code{response} here~~
}

  \item{X}{a list of matrices of the mean/trend discrepancy between the reality and math model for multiple sources. 
%%     ~~Describe \code{trend} here~~
}
  \item{have_trend}{a vector of bool value meaning whether we assume a mean/trend discrepancy  function.
%%     ~~Describe \code{trend} here~~
}

  \item{simul_type}{a vector of integer about the math model/simulator for multiple sources. If the simul_type is 0, it means we use RobustGaSP R package to build an emulator for emulation. If the simul_type is 1, it means the function of the math model is given by the user. When simul_type is 2 or 3, the mathematical model is the geophyiscal model for Kilauea Volcano.  If the simul_type is 2, it means it is for the ascending mode InSAR data; if the simul_type is 3, it means it is for the descending mode InSAR data.}

  \item{input_simul}{a list of matices, each having dimension D x (p_x+p_theta) being the design for emulating the math model. It is only useful if the ith value of simul_type is 0 for the ith source, meaning that we emulate the output of the math model.}

  \item{output_simul}{a list of vectors, each having dimension D x 1 being the math model outputs on the design (input_simul). It is only useful if the ith value of simul_type is 0 for the ith source, meaning that we emulate the output of the math model.}
  \item{simul_nug}{a vectors of bool values meaning whether we have a nugget for emulating the math model/simulator for this source. If the math model is stochastic, we often need a nugget. If simul_Nug is TRUE, it means we have a nugget for the emulator. If simul_Nug is FALSE, it means we do not have a nugget for the emulator.}

  \item{loc_index_emulator}{a list for location index to output in the ppgasp emulator  for computer models with vectorized output.}




  \item{math_model}{a list of functions of the math models provided by the user for multiple sources. It is only useful if simul_type is 1, meaning that we know the math model and it can be computed fast. If  the math model is computationally slow, one should set simul_type to be 0 to emulate the math model. If defined, each element of the list is a function of math models, where the first input of the function is a vector of observable inputs and the second input is a vector of  calibration parameters. The output of each function is a scalar. Each function corresponds to one source of data. }

  \item{theta_range}{a p_theta x 2 matrix of the range of the calibration parameters. The first column is the lower bound and the second column  is the upper bound. It should be specified by the user if the simul_type is 0.}


  \item{sd_proposal_theta}{a vector of the standard deviation of the proposal distribution for the calibration parameters in MCMC. The default value of sd of the calibration parameter is 0.05 times \code{theta_range}. }
    \item{sd_proposal_cov_par}{a list of vectors of the standard deviation of the proposal distribution for range and nugget parameters in MCMC for each source.}



  \item{S}{an integer about about how many posterior samples to run.}
  \item{S_0}{an integer about about the number of burn-in samples.}
  \item{thinning}{the ratio between the number of posterior samples and the number of recorded samples.}

  \item{measurement_bias}{containing measurement bias or not.}

  \item{shared_design}{A matrix for shared design across different sources of data used 
   when measurement bias exists.}
  \item{have_measurement_bias_recorded}{A bool value whether we record measurement bias or not.}
    \item{shared_X}{A matrix of shared trend when measurement bias exists.}

    \item{have_shared_trend}{A bool value whether we have shared trend when measurement bias exist.}

  \item{discrepancy_type}{a vector of characters about the type of the discrepancy for each source.  If it is 'no-discrepancy', it means no discrepancy function. If it is 'GaSP', it means the GaSP model for the discrepancy function. If it is 'S-GaSP', it means the S-GaSP model for the discrepancy function.}

  \item{kernel_type}{a vector of characters about the type of kernel for each data source. \code{matern_3_2} and \code{matern_5_2} are \code{Matern kernel} with roughness parameter 3/2 and 5/2 respectively. \code{pow_exp} is power exponential kernel with roughness parameter alpha. If \code{pow_exp} is to be used, one needs to specify its roughness parameter alpha.}
 \item{lambda_z}{a \code{vector} numeric values about how close the math model to the reality in squared distance when the S-GaSP model is used for modeling the discrepancy for each source.}
 \item{a}{a vector of the prior parameter for multiple sources.}
  \item{b}{a vector of the prior parameter for multiple sources.}
  \item{alpha}{a list of vectors of roughness parameters in the kernel for multiple sources.}
  \item{output_weights}{a list of vectors of the weights of the outputs for multiple sources.}
  \item{...}{Extra arguments to be passed to the function (not implemented yet).}

%  \item{post_sample}{a matrix of the posterior samples after burn-in.}
%  \item{post_value}{a vector of the posterior values after burn-in.}
%  \item{accept_S}{a vector of  the number of proposed samples of the calibation parameters are accepted in MCMC. The first value is the number of proposed calibration parameters  are accepted in MCMC. The second value is  the number of proposed range and nugget parameters  are accepted, if  \code{discrepancy_type}  is specified as 'GaSP' or 'S-GaSP'.}
%  \item{count_boundary}{a vector of  the number of proposed samples of the calibation parameters are outside the range and they are rejected directly.}


}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%expand here the details.
%}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\code{rcalibration_MS} returns an S4 object of class \code{rcalibration_MS} (see \code{rcalibration_MS-class}).

%If there is an emulator (i.e. simul_type is 0), \code{rcalibration} returns a list with 
%\item{rcalibration}{an S4 object of class \code{rcalibration} (see \code{rcalibration-class}.}
%\item{emulator}{an S4 object of class \code{rgasp} produced by RobustGaSP R Package .}


}

\references{
%% ~put references to the literature/web site here ~

A. O'Hagan and M. C. Kennedy (2001), \emph{Bayesian calibration of computer models}, \emph{Journal of the Royal Statistical Society: Series B (Statistical Methodology}, \bold{63}, 425-464.

K. R. Anderson and M. P. Poland (2016), \emph{Bayesian estimation of magma supply, storage, and eroption rates using a multiphysical volcano model: Kilauea volcano, 2000-2012.}. \emph{Eath and Planetary Science Letters}, \bold{447}, 161-171.

K. R. Anderson and M. P. Poland (2017), \emph{Abundant carbon in the mantle beneath Hawaii}. \emph{Nature Geoscience}, \bold{10}, 704-708.

M. Gu (2016), \emph{Robust Uncertainty Quantification and Scalable Computation for Computer Models with Massive Output}, Ph.D. thesis., Duke University.

M. Gu and L. Wang (2017) \emph{Scaled Gaussian Stochastic Process for Computer Model Calibration and Prediction}. arXiv preprint arXiv:1707.08215.

M. Gu (2018) \emph{Jointly Robust Prior for Gaussian Stochastic Process in Emulation, Calibration and Variable Selection
}. arXiv preprint arXiv:1804.09329.}

\author{
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
#------------------------------------------------------------------------------
# An example for calibrating mathematical models for data from multiple sources
#------------------------------------------------------------------------------

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
input2=seq(0,3,3/(15-1))
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

plot(model_sgasp@post_theta[,1],type='l')
mean(model_sgasp@post_theta[,1])


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
