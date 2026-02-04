\name{projection}
\alias{projection}
\title{Event and Trial Projection}
\usage{
projection(snapshot_data, enroll_continue=FALSE, samplesize=0, rand_ratio=c(1,1), 
blocksize=1,accrual_interval=NULL, accrual_rate=NULL, lambda=NULL, 
trtHR=NULL, dropoutrate=NULL,eventtarget=0,maxlpfollowup=NULL)
  

}

\arguments{
  \item{snapshot_data}{Snapshot dataset in data frame. Data frame must include column "time","status" and "accrual". Optional column "onstudy" is indicator of patients are still on study.}
  \item{rand_ratio}{randomization ratio: control vs treatment}
  \item{enroll_continue}{Indicate whether trial is still enrolling new patients. Default is FALSE.  if TRUE, user needs provide "samplesize", "rand_ratio","blocksize", "accrual_interval","accrual_rate".}
  \item{samplesize}{Total sample size of the trial. only needed if "enroll_continue=TRUE"}
  \item{blocksize}{Randomization blocksize, only needed if "enroll_continue=TRUE"}
  \item{accrual_interval}{accrual time windows. only needed if "enroll_continue=TRUE"}
  \item{accrual_rate}{accrual rate for each time window. only needed if "enroll_continue=TRUE"}
  \item{lambda}{Baseline hazard function parameter for exponential distribution}
  \item{trtHR}{hazard ratio between treatment and control}
  \item{dropoutrate}{Dropout rate}
  \item{eventtarget}{Total target event number}
  \item{maxlpfollowup}{Maximum followup time for last enrolled patient}
  
}


\description{
Trial projection given current snapshot dataset. 
}

