\name{step_accrual}
\alias{step_accrual}
\title{Simulate Accrual Time}
\usage{
step_accrual(np,tt,arate)


}

\arguments{
  \item{np}{Total number of patients }
  \item{tt}{Time windows}
  \item{arate}{accrual rate for each time windows}
}


\description{
Simulate accrual time
}
\examples{

timeinterval<-c(0,5,10)
accrualrate<-c(10,15,20)
N<-200
accrual<-step_accrual(N,timeinterval,accrualrate)


}
