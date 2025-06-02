##########################################################################
## example.R
## 
## Robust Calibration Package
##
## This software is distributed under the terms of the GNU GENERAL
## PUBLIC LICENSE Version 3, April 2013.
##
## Copyright (C) 2018-present Mengyang Gu
##							  
##    
##########################################################################

###################
# Simulated example
####################

library(RobustCalibration)

#------------------------------------------------------------------------------
# Example 1: # an example used in Susie Bayarri et. al. 2007 Technometrics paper
#------------------------------------------------------------------------------

###reality
test_funct_eg1<-function(x){
  # 4/3*x^2
  3.5*exp(-1.7*x)+1.5
}


##math model
math_model_eg1<-function(x,theta){
  5*exp(-x*theta) 
}

## noise observations (sampled from reality + independent Gaussian noises)
##each has 3 replicates
input=c(rep(.110,3),rep(.432,3),rep(.754,3),rep(1.077,3),rep(1.399,3),rep(1.721,3),
        rep(2.043,3),rep(2.366,3),rep(2.688,3),rep(3.010,3))
output=c(4.730,4.720,4.234,3.177,2.966,3.653,1.970,2.267,2.084,2.079,2.409,2.371,1.908,1.665,1.685,
         1.773,1.603,1.922,1.370,1.661,1.757,1.868,1.505,1.638,1.390,1.275,1.679,1.461,1.157,1.530)  


n_stack=length(output)/3
output_stack=rep(0,n_stack)
input_stack=rep(0,n_stack)
for(j in 1:n_stack){
  output_stack[j]=mean(output[ ((j-1)*3+1):(3*j)])
  input_stack[j]=mean(input[ ((j-1)*3+1):(3*j)])
  
}
output_stack=as.matrix(output_stack)
input_stack=as.matrix(input_stack)
## plot the output and stack
#plot(input,output,pch=16,col='red')
#lines(input_stack,output_stack,pch=16,col='blue',type='p')



## fit three models: no-discrepancy, using GaSP model for the discrepancy and S-GaSP for the discrepancy
model_no_discrepancy=rcalibration(design=input_stack, observations=output_stack, p_theta=1,simul_type=1,
                                  math_model=math_model_eg1,theta_range=matrix(c(0,10),1,2),S=50000,S_0=5000,
                                  discrepancy_type='no-discrepancy')

model_gasp=rcalibration(design=input_stack, observations=output_stack, p_theta=1,simul_type=1,
                        math_model=math_model_eg1,theta_range=matrix(c(0,10),1,2),S=50000,S_0=5000,
                        discrepancy_type='GaSP')
model_sgasp=rcalibration(design=input_stack, observations=output_stack, p_theta=1,simul_type=1,
                         math_model=math_model_eg1,theta_range=matrix(c(0,10),1,2),S=50000,S_0=5000,
                         discrepancy_type='S-GaSP')

#posterior
plot(model_no_discrepancy@post_sample[,1],type='l',xlab='num',ylab=expression(theta))   
plot(model_gasp@post_sample[,1],type='l',xlab='num',ylab=expression(theta))   
plot(model_sgasp@post_sample[,1],type='l',xlab='num',ylab=expression(theta))   

show(model_no_discrepancy)
show(model_gasp)
show(model_sgasp)


## test data set
testing_input=as.matrix(seq(0,6,0.01))


prediction_no_discrepancy=predict(model_no_discrepancy,testing_input,
                                               math_model=math_model_eg1,
                                               interval_est=c(0.025,0.975),interval_data=TRUE)

prediction_gasp=predict(model_gasp,testing_input,math_model=math_model_eg1,
                                     interval_est=c(0.025,0.975),interval_data=TRUE)

prediction_sgasp=predict(model_sgasp,testing_input,math_model=math_model_eg1,
                                      interval_est=c(0.025,0.975),interval_data=TRUE)


testing_output=test_funct_eg1(testing_input)
##the prediction by GaSP

min_val=min(prediction_gasp@mean,prediction_gasp@interval,output,testing_output)
max_val=max(prediction_gasp@mean,prediction_gasp@interval,output,testing_output)

plot(testing_input,prediction_gasp@mean,type='l',col='red',xlab='x',ylab='y',ylim=c(min_val,max_val) )
lines(testing_input,prediction_gasp@interval[,1],col='red',lty=2)
lines(testing_input,prediction_gasp@interval[,2],col='red',lty=2)

lines(testing_input,prediction_gasp@math_model_mean,col='red',lty=3)

lines(input,output,type='p')

legend("topright", legend=c("reality", "predictive mean","95  percent  posterior credible interval", "predictive mean of the math model"),
       col=c("black", "red","red","red","black"), lty=c(1,1,2,3),cex=.6)

lines(testing_input,testing_output,type='l')


##the prediction by S-GaSP
min_val=min(prediction_sgasp@mean,prediction_sgasp@interval,output,testing_output)
max_val=max(prediction_sgasp@mean,prediction_sgasp@interval,output,testing_output)

plot(testing_input,prediction_sgasp@mean,type='l',col='blue',xlab='x',ylab='y',ylim=c(min_val,max_val) )
lines(testing_input,prediction_sgasp@interval[,1],col='blue',lty=2)
lines(testing_input,prediction_sgasp@interval[,2],col='blue',lty=2)

lines(input,output,type='p')
lines(testing_input,prediction_sgasp@math_model_mean,col='blue',lty=3)

lines(testing_input,testing_output,type='l')

legend("topright", legend=c("reality", "predictive mean","95 percent posterior credible interval", "predictive mean of the math model"),
       col=c("black", "blue","blue","blue"), lty=c(1,1,2,3),cex=.6)


## MSE if the math model and discrepancy are used for prediction
mean((testing_output-prediction_gasp@mean)^2)
mean((testing_output-prediction_sgasp@mean)^2)

mean((testing_output[301:601]-prediction_gasp@mean[301:601])^2)
mean((testing_output[301:601]-prediction_sgasp@mean[301:601])^2)

## MSE if the math model is used for prediction 
mean((testing_output-prediction_gasp@math_model_mean)^2)
mean((testing_output-prediction_sgasp@math_model_mean)^2)

mean((testing_output[301:601]-prediction_gasp@math_model_mean[301:601])^2)
mean((testing_output[301:601]-prediction_sgasp@math_model_mean[301:601])^2)

## Prediction if no discrepancy is assumed

min_val=min(prediction_no_discrepancy@mean,prediction_no_discrepancy@interval,output,testing_output)
max_val=max(prediction_no_discrepancy@mean,prediction_no_discrepancy@interval,output,testing_output)

plot(testing_input,prediction_no_discrepancy@math_model_mean,type='l',col='green',xlab='x',ylab='y',ylim=c(min_val,max_val) )
lines(testing_input,prediction_no_discrepancy@interval[,1],col='green',lty=2)
lines(testing_input,prediction_no_discrepancy@interval[,2],col='green',lty=2)

lines(input,output,type='p')

lines(testing_input,testing_output,type='l')

legend("topleft", legend=c("reality", "predictive mean of the math model","95 percent posterior credible interval"),
       col=c("black", "green","green"), lty=c(1,1,2),cex=.6)

## MSE if no discrepancy is assumed
mean((testing_output-prediction_no_discrepancy@math_model_mean)^2)


## in this example, adding a constant discrepancy parameter will make prediction better
## It is also more reasonale as the the output of the reality deviate from zero.
#------------------------------------------------------------------------------
# Example 2: assuming now we add a constant discreancy parameter to the model 
#------------------------------------------------------------------------------

###reality
test_funct_eg1<-function(x){
  # 4/3*x^2
  3.5*exp(-1.7*x)+1.5
}


##math model
math_model_eg1<-function(x,theta){
  5*exp(-x*theta) 
}

## noise observations (sampled from reality + independent Gaussian noises)
##each has 3 replicates
input=c(rep(.110,3),rep(.432,3),rep(.754,3),rep(1.077,3),rep(1.399,3),rep(1.721,3),
        rep(2.043,3),rep(2.366,3),rep(2.688,3),rep(3.010,3))
output=c(4.730,4.720,4.234,3.177,2.966,3.653,1.970,2.267,2.084,2.079,2.409,2.371,1.908,1.665,1.685,
         1.773,1.603,1.922,1.370,1.661,1.757,1.868,1.505,1.638,1.390,1.275,1.679,1.461,1.157,1.530)  


n_stack=length(output)/3
output_stack=rep(0,n_stack)
input_stack=rep(0,n_stack)
for(j in 1:n_stack){
  output_stack[j]=mean(output[ ((j-1)*3+1):(3*j)])
  input_stack[j]=mean(input[ ((j-1)*3+1):(3*j)])
  
}
output_stack=as.matrix(output_stack)
input_stack=as.matrix(input_stack)

## fit three models: no-discrepancy, using GaSP model for the discrepancy and S-GaSP for the discrepancy
## all with a mean structure by setting X=matrix(1,dim(input_stack)[1],1) and have_trend=T
model_no_discrepancy_with_mean=rcalibration(design=input_stack, observations=output_stack, p_theta=1,X=matrix(1,dim(input_stack)[1],1),have_trend=T,simul_type=1,
                                  math_model=math_model_eg1,theta_range=matrix(c(0,10),1,2),S=50000,S_0=5000,
                                  discrepancy_type='no-discrepancy')

model_gasp_with_mean=rcalibration(design=input_stack, observations=output_stack, p_theta=1,X=matrix(1,dim(input_stack)[1],1),have_trend=T,simul_type=1,
                        math_model=math_model_eg1,theta_range=matrix(c(0,10),1,2),S=50000,S_0=5000,
                        discrepancy_type='GaSP')
model_sgasp_with_mean=rcalibration(design=input_stack, observations=output_stack, p_theta=1,X=matrix(1,dim(input_stack)[1],1),have_trend=T,simul_type=1,
                         math_model=math_model_eg1,theta_range=matrix(c(0,10),1,2),S=50000,S_0=5000,
                         discrepancy_type='S-GaSP')

model_sgasp_with_mean@X
#posterior
plot(model_no_discrepancy_with_mean@post_sample[,1],type='l',xlab='num',ylab=expression(theta))   
plot(model_gasp_with_mean@post_sample[,1],type='l',xlab='num',ylab=expression(theta))   
plot(model_sgasp_with_mean@post_sample[,1],type='l',xlab='num',ylab=expression(theta))   

show(model_no_discrepancy_with_mean)
show(model_gasp_with_mean)
show(model_sgasp_with_mean)


## test data set
testing_input=as.matrix(seq(0,6,0.01))


prediction_no_discrepancy_with_mean=predict(model_no_discrepancy_with_mean,testing_input,X_testing=matrix(1,dim(testing_input)[1],1),
                                               math_model=math_model_eg1,interval_est=c(0.025,0.975),interval_data=TRUE)

prediction_gasp_with_mean=predict(model_gasp_with_mean,testing_input,X_testing=matrix(1,dim(testing_input)[1],1),
                                               math_model=math_model_eg1,interval_est=c(0.025,0.975),interval_data=TRUE)

prediction_sgasp_with_mean=predict(model_sgasp_with_mean,testing_input,X_testing=matrix(1,dim(testing_input)[1],1),
                                                math_model=math_model_eg1, interval_est=c(0.025,0.975),interval_data=TRUE)


testing_output=test_funct_eg1(testing_input)


##the prediction by GaSP

min_val=min(prediction_gasp_with_mean@mean,prediction_gasp_with_mean@interval,output,testing_output)
max_val=max(prediction_gasp_with_mean@mean,prediction_gasp_with_mean@interval,output,testing_output)

plot(testing_input,prediction_gasp_with_mean@mean,type='l',col='red',xlab='x',ylab='y',ylim=c(min_val,max_val) )
lines(testing_input,prediction_gasp_with_mean@interval[,1],col='red',lty=2)
lines(testing_input,prediction_gasp_with_mean@interval[,2],col='red',lty=2)

lines(testing_input,prediction_gasp_with_mean@math_model_mean,col='red',lty=3)

lines(input,output,type='p')

legend("topright", legend=c("reality", "predictive mean","95  percent  posterior credible interval", "predictive mean of the math model"),
       col=c("black", "red","red","red","black"), lty=c(1,1,2,3),cex=.6)

lines(testing_input,testing_output,type='l')


##the prediction by S-GaSP
min_val=min(prediction_sgasp_with_mean@mean,prediction_sgasp_with_mean@interval,output,testing_output)
max_val=max(prediction_sgasp_with_mean@mean,prediction_sgasp_with_mean@interval,output,testing_output)

plot(testing_input,prediction_sgasp_with_mean@mean,type='l',col='blue',xlab='x',ylab='y',ylim=c(min_val,max_val) )
lines(testing_input,prediction_sgasp_with_mean@interval[,1],col='blue',lty=2)
lines(testing_input,prediction_sgasp_with_mean@interval[,2],col='blue',lty=2)

lines(input,output,type='p')
lines(testing_input,prediction_sgasp_with_mean@math_model_mean,col='blue',lty=3)

lines(testing_input,testing_output,type='l')

legend("topright", legend=c("reality", "predictive mean","95 percent posterior credible interval", "predictive mean of the math model"),
       col=c("black", "blue","blue","blue"), lty=c(1,1,2,3),cex=.6)


## MSE if the math model and discrepancy are used for prediction
mean((testing_output-prediction_gasp_with_mean@mean)^2)
mean((testing_output-prediction_sgasp_with_mean@mean)^2)

mean((testing_output[301:601]-prediction_gasp_with_mean@mean[301:601])^2)
mean((testing_output[301:601]-prediction_sgasp_with_mean@mean[301:601])^2)

## MSE if the math model is used for prediction 
mean((testing_output-prediction_gasp_with_mean@math_model_mean)^2)
mean((testing_output-prediction_sgasp_with_mean@math_model_mean)^2)

mean((testing_output[301:601]-prediction_gasp_with_mean@math_model_mean[301:601])^2)
mean((testing_output[301:601]-prediction_sgasp_with_mean@math_model_mean[301:601])^2)

## Prediction if no discrepancy is assumed

min_val=min(prediction_no_discrepancy_with_mean@math_model_mean,prediction_no_discrepancy_with_mean@interval,output,testing_output)
max_val=max(prediction_no_discrepancy_with_mean@math_model_mean,prediction_no_discrepancy_with_mean@interval,output,testing_output)

plot(testing_input,prediction_no_discrepancy_with_mean@math_model_mean,type='l',col='green',xlab='x',ylab='y',ylim=c(min_val,max_val) )
lines(testing_input,prediction_no_discrepancy_with_mean@interval[,1],col='green',lty=2)
lines(testing_input,prediction_no_discrepancy_with_mean@interval[,2],col='green',lty=2)

lines(input,output,type='p')

lines(testing_input,testing_output,type='l')

legend("topleft", legend=c("reality", "predictive mean of the math model","95 percent posterior credible interval"),
       col=c("black", "green","green"), lty=c(1,1,2),cex=.6)

## MSE if no discrepancy is assumed
mean((testing_output-prediction_no_discrepancy_with_mean@math_model_mean)^2)


#-------------------------------------------------------------
# Example 3: assuming now we don't know the math model but use an emulator
#-------------------------------------------------------------
# first we need to run the computer model some times


###reality
test_funct_eg1<-function(x){
  # 4/3*x^2
  3.5*exp(-1.7*x)+1.5
}


##math model
math_model_eg1<-function(x,theta){
  5*exp(-x*theta) 
}

## noise observations (sampled from reality + independent Gaussian noises)
##each has 3 replicates
input=c(rep(.110,3),rep(.432,3),rep(.754,3),rep(1.077,3),rep(1.399,3),rep(1.721,3),
        rep(2.043,3),rep(2.366,3),rep(2.688,3),rep(3.010,3))
output=c(4.730,4.720,4.234,3.177,2.966,3.653,1.970,2.267,2.084,2.079,2.409,2.371,1.908,1.665,1.685,
         1.773,1.603,1.922,1.370,1.661,1.757,1.868,1.505,1.638,1.390,1.275,1.679,1.461,1.157,1.530)  


n_stack=length(output)/3
output_stack=rep(0,n_stack)
input_stack=rep(0,n_stack)
for(j in 1:n_stack){
  output_stack[j]=mean(output[ ((j-1)*3+1):(3*j)])
  input_stack[j]=mean(input[ ((j-1)*3+1):(3*j)])
  
}
output_stack=as.matrix(output_stack)
input_stack=as.matrix(input_stack)

n_design=80
  
design_simul=matrix(runif(n_design*2),n_design,2)
#library(lhs)
#design_simul=maximinLHS(n=n_design,k=2)

design_simul[,1]=6*design_simul[,1]   ##the first one is the observed input x
design_simul[,2]=10*design_simul[,2]   ##the second one is the calibration parameter \theta

output_simul=math_model_eg1(design_simul[,1],design_simul[,2])



## fit three models: no-discrepancy, using GaSP model for the discrepancy and S-GaSP for the discrepancy
## all with a mean structure by setting X=matrix(1,dim(input_stack)[1],1) and have_trend=T
model_no_discrepancy_with_mean_emulator=rcalibration(design=input_stack, observations=output_stack, p_theta=1,X=matrix(1,dim(input_stack)[1],1),have_trend=T,
                                            simul_type=0, input_simul=design_simul, output_simul=output_simul,theta_range=matrix(c(0,10),1,2),S=10000,S_0=2000,
                                            discrepancy_type='no-discrepancy')

model_gasp_with_mean_emulator=rcalibration(design=input_stack, observations=output_stack, p_theta=1,X=matrix(1,dim(input_stack)[1],1),have_trend=T,
                                  simul_type=0, input_simul=design_simul, output_simul=output_simul,theta_range=matrix(c(0,10),1,2),S=10000,S_0=2000,
                                  discrepancy_type='GaSP')
model_sgasp_with_mean_emulator=rcalibration(design=input_stack, observations=output_stack, p_theta=1,X=matrix(1,dim(input_stack)[1],1),have_trend=T,
                                   simul_type=0, input_simul=design_simul, output_simul=output_simul,theta_range=matrix(c(0,10),1,2),S=10000,S_0=2000,
                                   discrepancy_type='S-GaSP')


plot(model_no_discrepancy_with_mean_emulator@post_sample[,1],type='l',xlab='num',ylab=expression(theta))   
plot(model_gasp_with_mean_emulator@post_sample[,1],type='l',xlab='num',ylab=expression(theta))   
plot(model_sgasp_with_mean_emulator@post_sample[,1],type='l',xlab='num',ylab=expression(theta))   

show(model_no_discrepancy_with_mean_emulator)
show(model_gasp_with_mean_emulator)
show(model_sgasp_with_mean_emulator)




##testing 
testing_input=as.matrix(seq(0,6,0.03))


prediction_no_discrepancy_with_mean_emulator=predict(model_no_discrepancy_with_mean_emulator,testing_input,X_testing=matrix(1,dim(testing_input)[1],1))

prediction_gasp_with_mean_emulator=predict(model_gasp_with_mean_emulator,testing_input,X_testing=matrix(1,dim(testing_input)[1],1),
                                                interval_est=c(0.025,0.975),interval_data=TRUE)

prediction_sgasp_with_mean_emulator=predict(model_sgasp_with_mean_emulator,testing_input,X_testing=matrix(1,dim(testing_input)[1],1),
                                                interval_est=c(0.025,0.975),interval_data=TRUE)


testing_output=test_funct_eg1(testing_input)



##the prediction by GaSP

min_val=min(prediction_gasp_with_mean_emulator@mean,prediction_gasp_with_mean_emulator@interval,output,testing_output)
max_val=max(prediction_gasp_with_mean_emulator@mean,prediction_gasp_with_mean_emulator@interval,output,testing_output)

plot(testing_input,prediction_gasp_with_mean_emulator@mean,type='l',col='red',xlab='x',ylab='y',ylim=c(min_val,max_val) )
lines(testing_input,prediction_gasp_with_mean_emulator@interval[,1],col='red',lty=2)
lines(testing_input,prediction_gasp_with_mean_emulator@interval[,2],col='red',lty=2)

lines(testing_input,prediction_gasp_with_mean_emulator@math_model_mean,col='red',lty=3)

lines(input,output,type='p')

legend("topright", legend=c("reality", "predictive mean","95  percent  posterior credible interval", "predictive mean of the math model"),
       col=c("black", "red","red","red","black"), lty=c(1,1,2,3),cex=.6)

lines(testing_input,testing_output,type='l')


##the prediction by S-GaSP
min_val=min(prediction_sgasp_with_mean_emulator@mean,prediction_sgasp_with_mean_emulator@interval,output,testing_output)
max_val=max(prediction_sgasp_with_mean_emulator@mean,prediction_sgasp_with_mean_emulator@interval,output,testing_output)

plot(testing_input,prediction_sgasp_with_mean_emulator@mean,type='l',col='blue',xlab='x',ylab='y',ylim=c(min_val,max_val) )
lines(testing_input,prediction_sgasp_with_mean_emulator@interval[,1],col='blue',lty=2)
lines(testing_input,prediction_sgasp_with_mean_emulator@interval[,2],col='blue',lty=2)

lines(input,output,type='p')
lines(testing_input,prediction_sgasp_with_mean_emulator@math_model_mean,col='blue',lty=3)

lines(testing_input,testing_output,type='l')

legend("topright", legend=c("reality", "predictive mean","95 percent posterior credible interval", "predictive mean of the math model"),
       col=c("black", "blue","blue","blue"), lty=c(1,1,2,3),cex=.6)


## MSE if the math model and discrepancy are used for prediction
mean((testing_output-prediction_gasp_with_mean_emulator@mean)^2)
mean((testing_output-prediction_sgasp_with_mean_emulator@mean)^2)

mean((testing_output[101:201]-prediction_gasp_with_mean_emulator@mean[101:201])^2)
mean((testing_output[101:201]-prediction_sgasp_with_mean_emulator@mean[101:201])^2)

## MSE if the math model is used for prediction 
mean((testing_output-prediction_gasp_with_mean_emulator@math_model_mean)^2)
mean((testing_output-prediction_sgasp_with_mean_emulator@math_model_mean)^2)

mean((testing_output[101:201]-prediction_gasp_with_mean_emulator@math_model_mean[101:201])^2)
mean((testing_output[101:201]-prediction_sgasp_with_mean_emulator@math_model_mean[101:201])^2)

#-------------------------------------------------------------
# Example 4: an example with multiple local maximum of minimum in L2 loss
#-------------------------------------------------------------

# the reality 
test_funct_eg2<-function(x){
  x*cos(3/2*x)+x
}



## obtain 20 data from the reality plus a noise
set.seed(1)
n=20
input=seq(0,5,5/(n-1))

input=as.matrix(input)

output=test_funct_eg2(input)+rnorm(length(input),mean=0,sd=0.05)

num_obs=n=length(output)


## the math model 
math_model_eg2<-function(x,theta){
  sin(theta*x)+x  
}

## fit three models: no-discrepancy, using GaSP model for the discrepancy and S-GaSP for the discrepancy
model_no_discrepancy=rcalibration(design=input, observations=output, p_theta=1,simul_type=1,
                                  math_model=math_model_eg2,theta_range=matrix(c(0,3),1,2),
                                  discrepancy_type='no-discrepancy')

model_gasp=rcalibration(design=input, observations=output, p_theta=1,simul_type=1,
                        math_model=math_model_eg2,theta_range=matrix(c(0,3),1,2),
                        discrepancy_type='GaSP')
model_sgasp=rcalibration(design=input, observations=output, p_theta=1,simul_type=1,
                         math_model=math_model_eg2,theta_range=matrix(c(0,3),1,2),
                         discrepancy_type='S-GaSP')


## posterior samples 
plot(model_no_discrepancy@post_sample[,1],type='l',xlab='num',ylab=expression(theta))   
plot(model_gasp@post_sample[,1],type='l',xlab='num',ylab=expression(theta))   
plot(model_sgasp@post_sample[,1],type='l',xlab='num',ylab=expression(theta))   

show(model_no_discrepancy)
show(model_gasp)
show(model_sgasp)

## test data set
testing_input=as.matrix(seq(0,5,0.01))

prediction_no_discrepancy=predict(model_no_discrepancy,testing_input,math_model=math_model_eg2,interval_est=c(0.025,0.975),interval_data=TRUE)

prediction_gasp=predict(model_gasp,testing_input,math_model=math_model_eg2,
                                     interval_est=c(0.025,0.975),interval_data=TRUE)

prediction_sgasp=predict(model_sgasp,testing_input,math_model=math_model_eg2,
                                      interval_est=c(0.025,0.975),interval_data=TRUE)

testing_output=test_funct_eg2(testing_input)
##the prediction by GaSP

min_val=min(prediction_gasp@mean,prediction_gasp@interval,output,testing_output)
max_val=max(prediction_gasp@mean,prediction_gasp@interval,output,testing_output)

plot(testing_input,prediction_gasp@mean,type='l',col='red',xlab='x',ylab='y',ylim=c(min_val,max_val) )
lines(testing_input,prediction_gasp@interval[,1],col='red',lty=2)
lines(testing_input,prediction_gasp@interval[,2],col='red',lty=2)

lines(testing_input,prediction_gasp@math_model_mean,col='red',lty=3)

lines(input,output,type='p')

legend("topleft", legend=c("reality", "predictive mean","95  percent  posterior credible interval", "predictive mean of the math model"),
       col=c("black", "red","red","red","black"), lty=c(1,1,2,3),cex=.6)

lines(testing_input,testing_output,type='l')

##the prediction by S-GaSP
testing_output=test_funct_eg2(testing_input)

min_val=min(prediction_sgasp@mean,prediction_sgasp@interval,output,testing_output)
max_val=max(prediction_sgasp@mean,prediction_sgasp@interval,output,testing_output)

plot(testing_input,prediction_sgasp@mean,type='l',col='blue',xlab='x',ylab='y',ylim=c(min_val,max_val) )
lines(testing_input,prediction_sgasp@interval[,1],col='blue',lty=2)
lines(testing_input,prediction_sgasp@interval[,2],col='blue',lty=2)

lines(input,output,type='p')
lines(testing_input,prediction_sgasp@math_model_mean,col='blue',lty=3)

lines(testing_input,testing_output,type='l')

legend("topleft", legend=c("reality", "predictive mean","95 percent posterior credible interval", "predictive mean of the math model"),
       col=c("black", "blue","blue","blue"), lty=c(1,1,2,3),cex=.6)


## MSE if the math model and discrepancy are used for prediction
mean((testing_output-prediction_gasp@mean)^2)
mean((testing_output-prediction_sgasp@mean)^2)

## MSE if the math model is used for prediction 
mean((testing_output-prediction_gasp@math_model_mean)^2)
mean((testing_output-prediction_sgasp@math_model_mean)^2)


## Prediction if no discrepancy is assumed

min_val=min(prediction_no_discrepancy@mean,prediction_no_discrepancy@interval,output,testing_output)
max_val=max(prediction_no_discrepancy@mean,prediction_no_discrepancy@interval,output,testing_output)

plot(testing_input,prediction_no_discrepancy@math_model_mean,type='l',col='green',xlab='x',ylab='y',ylim=c(min_val,max_val) )
lines(testing_input,prediction_no_discrepancy@interval[,1],col='green',lty=2)
lines(testing_input,prediction_no_discrepancy@interval[,2],col='green',lty=2)

lines(input,output,type='p')

lines(testing_input,testing_output,type='l')

legend("topleft", legend=c("reality", "predictive mean of the math model","95 percent posterior credible interval"),
       col=c("black", "green","green"), lty=c(1,1,2),cex=.6)

## MSE if no discrepancy is assumed
mean((testing_output-prediction_no_discrepancy@math_model_mean)^2)

