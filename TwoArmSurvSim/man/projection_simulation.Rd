\name{projection_simulation}
\alias{projection_simulation}
\title{Project Final Event Numbers or Trial Stop Time Based on Interim Analysis Data}
\usage{

projection_simulation(snapshot_data,  rand_ratio=c(1,1), enroll_continue=FALSE, 
samplesize=0, blocksize=1,accrual_interval=NULL, accrual_rate=NULL, lambda=NULL,
trtHR=NULL, dropoutrate=NULL,eventtarget=NULL,maxlpfollowup=NULL,N_simulation=1)


}

\arguments{
    \item{snapshot_data}{Snapshot dataset or interim analysis dataset}
  \item{rand_ratio}{Randomization ratio between control and treatment}
  \item{blocksize}{The value of this parameter is used to define the size of the randomization blocks. The actual blocksize is number of treatment levels mutiplied by this parameter. Please refer to "blockrand" package for detailed usage.}
  \item{enroll_continue}{True if trial is still in the enrollment period}
  \item{samplesize}{if enroll_continue=TRUE, please provide the total sample size of the trial.}
  \item{trtHR}{Hazard ratio between treatment groups (treatment vs control)}

  \item{accrual_interval}{Time windows for accrual}
  \item{accrual_rate}{accrual rate for each accrual time window}

  \item{lambda}{lambda for event hazard function (exponential)}

  \item{dropoutrate}{Patient dropout rate with range [0,1).  If dropoutrate contains only one number. The program will control the dropout rate at population level(treatment + control).  If dropoutrate contains two numbers (ie. c(0.2,0.1)), the program will control the dropout rate of control and treatment arm seperately, with the first dropout rate number for control and the second number for treatment. Default value is "0" (no dropout)}

  \item{eventtarget}{Number of target events}
  \item{maxlpfollowup}{maximum follow up time for the last enrolled patient}
  \item{N_simulation}{number of simulations to run}

}


\description{
Project final event numbers or trial stop time based on interim analysis data
}

