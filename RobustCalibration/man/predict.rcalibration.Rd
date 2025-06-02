\name{predict}
\alias{predict}
\alias{predict.rcalibration}
\alias{predict,rcalibration-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Prediction for the robust calibration model
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Function to make prediction on Robust Calibration models after the rcalibration class has been constructed.
}
\usage{
\S4method{predict}{rcalibration}(object, testing_input,X_testing=NULL,
                                 n_thinning=10,
                                 testing_output_weights=NULL,
                                 interval_est=NULL,interval_data=F,
                                 math_model=NULL,test_loc_index_emulator=NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of  class \code{rcalibration}.}
  \item{testing_input}{a matrix containing the inputs where the \code{predict} is to perform prediction. To predict one observable input with multiple dimension, user should supply a row vector. }
  \item{X_testing}{a matrix of mean/trend for prediction.}
  \item{n_thinning}{number of points further thinning the MCMC posterior samples.}
  \item{testing_output_weights}{the weight of testing outputs.}
    \item{interval_est}{a vector for the the posterior credible interval. If interval_est is NULL, we do  not compute the posterior credible interval.  It can be specified as a vector of values ranging from zero to one. E.g. if \code{interval_est=c(0.025, 0.975)}, the 95 posterior credible interval will be computed. }
  \item{interval_data}{a bool value to decide whether the experimental noise is included for computing the posterior credible interval.  }
  \item{math_model}{
  a function for the math model to be calibrated.
  }
  
  \item{test_loc_index_emulator}{
   a vector of the location index from the ppgasp emulator to output. Only useful for vectorized output computer model emulated by the ppgasp emulator.
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
#------------------------------------------------------------------------------
# Example: an example used in Susie Bayarri et. al. 2007 Technometrics paper
#------------------------------------------------------------------------------
    

##reality
test_funct_eg1<-function(x){
  3.5*exp(-1.7*x)+1.5
}


##math model
math_model_eg1<-function(x,theta){
  5*exp(-x*theta) 
}


## noise observations (sampled from reality + independent Gaussian noises)
## each has 3 replicates
input=c(rep(.110,3),rep(.432,3),rep(.754,3),rep(1.077,3),rep(1.399,3),rep(1.721,3),
        rep(2.043,3),rep(2.366,3),rep(2.688,3),rep(3.010,3))
output=c(4.730,4.720,4.234,3.177,2.966,3.653,1.970,2.267,2.084,2.079,2.409,2.371,1.908,1.665,1.685,
         1.773,1.603,1.922,1.370,1.661,1.757,1.868,1.505,1.638,1.390,1.275,1.679,1.461,1.157,1.530)


## calculating the average or the stack data
n_stack=length(output)/3
output_stack=rep(0,n_stack)
input_stack=rep(0,n_stack)
for(j in 1:n_stack){
  output_stack[j]=mean(output[ ((j-1)*3+1):(3*j)])
  input_stack[j]=mean(input[ ((j-1)*3+1):(3*j)])
}
output_stack=as.matrix(output_stack)
input_stack=as.matrix(input_stack)
## plot the output and stack output
#plot(input,output,pch=16,col='red')
#lines(input_stack,output_stack,pch=16,col='blue',type='p')



## fit model using S-GaSP for the discrepancy
## one can change S and S_0 for the number of posterior and burn-in samples
## Normallly you may need a larger number of posterior sample
## you can set S=50000 and S_0=5000
## one may also change the sd of the proposal distribution using sd_proposal
model_sgasp=rcalibration(design=input_stack, observations=output_stack, p_theta=1,simul_type=1,
                         math_model=math_model_eg1,theta_range=matrix(c(0,10),1,2),
                         S=10000,S_0=2000,discrepancy_type='S-GaSP')

# one can  fit the GaSP model for discrepancy function by discrepancy_type='GaSP'
# one can  fit a model without the discrepancy function by discrepancy_type='no-discrepancy'

## posterior of the calibration parameter
#plot(model_sgasp@post_sample[,1],type='l',xlab='num',ylab=expression(theta))   
show(model_sgasp)

##

## test data set
testing_input=as.matrix(seq(0,6,0.02))

##perform prediction
prediction_sgasp=predict(model_sgasp,testing_input,math_model=math_model_eg1,
                         interval_est=c(0.025,0.975),interval_data=TRUE,
                         n_thinning =20 )

##real test output
testing_output=test_funct_eg1(testing_input)

##the prediction by S-GaSP
min_val=min(prediction_sgasp@mean,prediction_sgasp@interval,output,testing_output)
max_val=max(prediction_sgasp@mean,prediction_sgasp@interval,output,testing_output)

plot(testing_input,prediction_sgasp@mean,type='l',col='blue',xlab='x',ylab='y',
     ylim=c(min_val,max_val) )
lines(testing_input,prediction_sgasp@interval[,1],col='blue',lty=2)
lines(testing_input,prediction_sgasp@interval[,2],col='blue',lty=2)

lines(input,output,type='p')
lines(testing_input,prediction_sgasp@math_model_mean,col='blue',lty=3)

lines(testing_input,testing_output,type='l')

legend("topright", legend=c("reality", "predictive mean","95 percent posterior credible interval",
                            "predictive mean of the math model"),
                            col=c("black", "blue","blue","blue"), lty=c(1,1,2,3),cex=.6)


## MSE if the math model and discrepancy are used for prediction
mean((testing_output-prediction_sgasp@mean)^2)

## MSE if the math model is used for prediction 
mean((testing_output-prediction_sgasp@math_model_mean)^2)



##################################
#the example with a mean structure
##################################

##now let's fit  model with mean
model_sgasp_with_mean=rcalibration(design=input_stack, observations=output_stack,
                                   p_theta=1,X=matrix(1,dim(input_stack)[1],1),
                                   have_trend=TRUE,simul_type=1,
                                   math_model=math_model_eg1,
                                   theta_range=matrix(c(0,10),1,2),
                                   S=10000,S_0=2000,
                                   discrepancy_type='S-GaSP')

#posterior
#plot(model_sgasp_with_mean@post_sample[,1],type='l',xlab='num',ylab=expression(theta))   
show(model_sgasp_with_mean)

## test data set
testing_input=as.matrix(seq(0,6,0.02))


prediction_sgasp_with_mean=predict(model_sgasp_with_mean,testing_input, X_testing=matrix(1,dim
(testing_input)[1],1),
math_model=math_model_eg1,n_thinning = 50,
interval_est=c(0.025,0.975),interval_data=TRUE)


##plot for the S-GaSP 
##for this example, with a mean structure, it fits much better
min_val=min(prediction_sgasp_with_mean@mean,output,testing_output,
prediction_sgasp_with_mean@interval[,1])
max_val=max(prediction_sgasp_with_mean@mean,output,testing_output,
prediction_sgasp_with_mean@interval[,2])

plot(testing_input,prediction_sgasp_with_mean@mean,type='l',col='blue',xlab='x',
     ylab='y',ylim=c(min_val,max_val) )
#lines(testing_input,prediction_sgasp_with_mean@interval[,1],col='blue',lty=2)
#lines(testing_input,prediction_sgasp_with_mean@interval[,2],col='blue',lty=2)

lines(input,output,type='p')
lines(testing_input,prediction_sgasp_with_mean@math_model_mean,col='blue',lty=3)
lines(testing_input,prediction_sgasp_with_mean@interval[,1],col='blue',lty=2)
lines(testing_input,prediction_sgasp_with_mean@interval[,2],col='blue',lty=2)

lines(testing_input,testing_output,type='l')

legend("topright", legend=c("reality", "predictive mean", "predictive mean of the math model"),
       col=c("black", "blue","blue"), lty=c(1,1,3),cex=.6)


## MSE if the math model and discrepancy are used for prediction
mean((testing_output-prediction_sgasp_with_mean@mean)^2)

## MSE if the math model is used for prediction 
mean((testing_output-prediction_sgasp_with_mean@math_model_mean)^2)


\dontrun{
  #-------------------------------------------------------------
  #the example with the emulator
  #-------------------------------------------------------------
  
  n_design=80
  
  design_simul=matrix(runif(n_design*2),n_design,2)
  #library(lhs)
  #design_simul=maximinLHS(n=n_design,k=2)
  
  design_simul[,1]=6*design_simul[,1]   ##the first one is the observed input x
  design_simul[,2]=10*design_simul[,2]   ##the second one is the calibration parameter \theta
  
  output_simul=math_model_eg1(design_simul[,1],design_simul[,2])
  
  
  ##this is a little slow compared with the previous model
  
  model_sgasp_with_mean_emulator=rcalibration(design=input_stack, observations=output_stack,
                                              p_theta=1,simul_type=0, 
                                              have_trend=T,X=matrix(1,dim(input_stack)[1],1),
                                              input_simul=design_simul, output_simul=output_simul,
                                              theta_range=matrix(c(0,10),1,2),
                                              S=10000,S_0=2000,discrepancy_type='S-GaSP')
  
  ##now the output is a list
  show(model_sgasp_with_mean_emulator)
  
  ##here is the plot
  plot(model_sgasp_with_mean_emulator@post_sample[,4],type='l',xlab='num',ylab=expression(theta))   
  plot(model_sgasp_with_mean_emulator@post_value,type='l',xlab='num',ylab='posterior value')   
  
  
  prediction_sgasp_with_mean_emulator=predict(model_sgasp_with_mean_emulator,testing_input,
                                              X_testing=matrix(1,dim(testing_input)[1],1),
                                              interval_est=c(0.025,0.975),
                                              interval_data=TRUE)
  
  ##for this example, with a mean structure, it fits much better
  min_val=min(prediction_sgasp_with_mean_emulator@mean,output,testing_output,
              prediction_sgasp_with_mean_emulator@math_model_mean)
  max_val=max(prediction_sgasp_with_mean_emulator@mean,output,testing_output,
              prediction_sgasp_with_mean_emulator@math_model_mean)
  
  plot(testing_input,prediction_sgasp_with_mean_emulator@mean,type='l',col='blue',xlab='x',
       ylab='y',ylim=c(min_val,max_val) )
  #lines(testing_input,prediction_sgasp_with_mean@interval[,1],col='blue',lty=2)
  #lines(testing_input,prediction_sgasp_with_mean@interval[,2],col='blue',lty=2)
  
  lines(input,output,type='p')
  lines(testing_input,prediction_sgasp_with_mean_emulator@math_model_mean,col='blue',lty=3)
  
  lines(testing_input,testing_output,type='l')
  
  legend("topright", legend=c("reality", "predictive mean", "predictive mean of the math model"),
         col=c("black", "blue","blue"), lty=c(1,1,3),cex=.6)
  
  
  ## MSE if the math model and discrepancy are used for prediction
  mean((testing_output-prediction_sgasp_with_mean_emulator@mean)^2)
  
  ## MSE if the math model is used for prediction 
  mean((testing_output-prediction_sgasp_with_mean_emulator@math_model_mean)^2)

}


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
