\name{run_simulation}
\alias{run_simulation}
\title{Run Clinical Trial Simulations Based on User Defined Trial Settings}
\usage{
run_simulation(samplesize, rand_ratio=c(1,1), blocksize, factors=NULL,trtHR=trtHR, 
trt_timeinterval=NULL, accrual_interval=NULL, accrual_rate=NULL, rampuptime=NULL,
acceleration=NULL, lambda, gamma, timeinterval=NULL, dropoutrate=0,gammac=1,
censordist='exponential', eventtarget=NULL,maxlpfollowup=NULL, N_simulation=1,
alpha=0.05)
  

}

\arguments{
  \item{samplesize}{Total number of patients in the simulated clinical trial}
  \item{rand_ratio}{Randomization ratio between control and treatment}
  \item{blocksize}{The value of this parameter is used to define the size of the randomizaiton blocks. The actual blocksize is number of treatment levels mutipled by this parameter. Please refer to "blockrand" package for detailed usage.}
  \item{factors}{stratification factors. Default is NULL}
  \item{trtHR}{Hazard ratio between treatment groups (treatment vs control)}
  \item{trt_timeinterval}{Time windows for trtHR when trtHR is piecewise. Always start with time 0. Example: c(0,10,30)}
  \item{accrual_interval}{Time windows for accrual}
  \item{accrual_rate}{accrual rate for each accrual time window}
  \item{rampuptime}{rampup time for linear increased accrual}
  \item{acceleration}{acceleration rate for linear increased accrual}
  \item{lambda}{lambda for event hazard function}
  \item{gamma}{gamma for event hazard function}
  \item{timeinterval}{time intervals for piecewise baseline hazard function}
  \item{dropoutrate}{Patient dropout rate with range [0,1).  If dropoutrate contains only one number. The program will control the dropout rate at population level(treatment + control).  If dropoutrate contains two numbers (ie. c(0.2,0.1)), the program will control the dropout rate of control and treatment arm seperately, with the first dropout rate number for control and the second number for treatment. Default value is "0" (no dropout)}

  
  \item{gammac}{gamma for censor hazard function. Default is 1 (exponential)}
  \item{censordist}{censor hazard distribution. Can be "weibull", "exponential" or "uniform". Default is exponential}
  \item{eventtarget}{Number of target events}
  \item{maxlpfollowup}{maximum follow up time for the last enrolled patient}
  \item{N_simulation}{number of simulations to run}
  \item{alpha}{Two sided alpha for testing power calculation}
}

\value{
 \item{TrilInfo}{Summary of the simulated trial data}
 \item{ModelResult}{Cox model results comparing treatment vs control}
 \item{StraModelResult}{Stratified Cox model results comparing treatment vs control}
 \item{Data}{simulated dataset only if "N_simulateion" is set to 1}
}


\description{
Runs single or mutiple clinical trial (Time to event endpoint) simulations based on the clinical trial settings. Trial data summary will be provied for each simulation. Cox model will be fitted afther trial simulation. If stratification factors were provided, stratified cox model results will also be provided. If "N_simulation" is set to 1, one simulation dataset will be generated.
}
\examples{


f1<-list(name='Region', N_level=3, prevalence=c(0.1,0.2,0.7), HR=c(1,0.7,0.9), strata=TRUE)
f2<-list(name='Gender', N_level=2, prevalence=c(0.5,0.5), HR=c(1,0.9), strata=TRUE)
f3<-list(name='Stage', N_level=4, prevalence=c(0.2,0.25,0.3,0.25), HR=c(1,1.05,1.3,1.5),
strata=TRUE)

factors<-list(f1,f2,f3)

samplesize<-400
blocksize<-2
accrual_interval<-c(0,5,10)
accrual_rate<-c(5,10,20)
trtHR<-0.7
lambda<-0.03
gamma<-1.2
dropoutrate<-0.2
eventtarget<-240
N_simulation<-10


out<-run_simulation(samplesize=samplesize,blocksize=blocksize,factors=factors,
accrual_interval=accrual_interval,accrual_rate=accrual_rate, trtHR=trtHR, lambda=lambda,
gamma=gamma,dropoutrate=dropoutrate,eventtarget=eventtarget,N_simulation=N_simulation)


}
