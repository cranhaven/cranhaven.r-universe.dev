\name{initializeRdsObject}
\alias{initializeRdsObject}

\title{
  Construct a \code{rds-object} from a data.frame.
}
\description{
Given a data frame with the appropriate variables, initializes a \code{rds-object} with the components required by the \code{\link{Estimate.b.k}} function for estimation.
}
\usage{
  initializeRdsObject(rds.sample, bin=1L, seeds=1L)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rds.sample}{A data frame with required columns. See Details. }
  \item{bin}{The number of degrees fo bin together. See details.}
  \item{seeds}{The number of seed recruiters. See details.}
}

\details{
The essence of the function is in recovering the sampling snowball required by \code{\link{Estimate.b.k}}. 
The function allows for recruiters to enter and exit the sampling snowball.
The number of seed recruiters is typically not specified in an RDS file.
The \code{seeds} argument is a workaround that allows to specify directly this number.

The \code{rds.sample} object is assumed to be a data frame with the following column names:
\enumerate{
\item{MyUniID}{an identifier of the sampling unit.[not required]}
\item{NS1}{The reported degree.[required]}
\item{refCoupNum}{ The number of the referring coupon.}
\item{coup1}{The number of the 1st supplied coupon. NA if none. [required].}
\item{coupX}{The number of the Xth supplied coupon. NA if none.[not required]}
\item{interviewDt}{The time of the interview. In numeric representation from some origin. Ties are not allowed.}
}

See \code{\link{brazil}} for a sample data.frame.


If the sample is short, stabilization of degree estimates can be achieved by binning degrees together. This can be done with the \code{bin} argument. Note however that the interpretation of the estimated degree counts is now different as the k'th degree is actually the k'th bin, which is only proportional to \eqn{k}. An exception is the function \code{\link{getTheta}} which also accepts a \code{bin} argument for proper estimation of \eqn{theta}.

}

\value{
A list with the following components.
\itemize{
\item{rds.sample}{The input data frame. After ordering along time of arrival.}
\item{I.t}{The sampling snowball. A list including the following items:
\item{I.t}{An integer of the count of the sampling individuals at the moments of recruitment.}
\item{degree.in}{An integer with the degree of an added recruiter at the moments of recruitment.}
\item{degree.out}{An integer with the degree of a removed recruiter at the moment of recruitment.}}
\item{original.ordering}{The order of the arrivals as was inputed in \code{rds.sample$interviewDt}}
\item{estimates}{A placeholder for the future output of \code{\link{Estimate.b.k}}}
}
}

\references{
[1] Berchenko, Y., Rosenblatt J.D., and S.D.W. Frost. 
  "Modeling and Analyzing Respondent Driven Sampling as a Counting Process." 
  arXiv:1304.3505
  }

\seealso{
  \code{\link{Estimate.b.k}}, 
  \code{\link{makeRdsSample}}, 
  \code{\link{brazil}}
}
