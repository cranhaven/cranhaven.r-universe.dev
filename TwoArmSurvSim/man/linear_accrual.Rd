\name{linear_accrual}
\alias{linear_accrual}
\title{Simulate Accrual Time Where the Accrual Rate is Linearly Increased}
\usage{

linear_accrual(np,rampupt,acceleration)


}

\arguments{
  \item{np}{Total number of patients }


  \item{rampupt}{The length of the ramp up period.}
  \item{acceleration}{The acceleration of the accrual rate (increase of each time unit).}

}


\description{
simulate accrual time where the accrual rate is linearly increased
}
\examples{

linear_accrual(np=200,rampupt=10,acceleration=5)

}
