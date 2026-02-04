\name{trial_data_simulation}
\alias{trial_data_simulation}
\title{Simulate Clinical Trial with Accrual Time and Trial Stop Rules}
\usage{

trial_data_simulation(simdata,accrual,eventtarget=NULL,maxlpfollowup=NULL)


}

\arguments{
  \item{simdata}{A data frame contains simulated survival time and censor status.}


  \item{accrual}{Simulated accrual time.}
  \item{eventtarget}{Target number of events of the trial}
  \item{maxlpfollowup}{The maximum follow up time for the last enrolled patient.}

}


\description{
Simulate clinical trial by adding accrual time to the simulated survival data. And cut the trial at the target event time or at the maximum follow up time
}

