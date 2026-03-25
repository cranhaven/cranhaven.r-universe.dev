
% --- Source file: flat.pattern.Rd ---

\name{flat.pattern}
\Rdversion{1.0}
\alias{flat.pattern}
\docType{package}
\title{flat.pattern}
\description{ 
Returns the log-maximum likelihood and the estimator of the mean under the equality constraint that all means are equal.
}

\usage{
flat.pattern(data,x,n.rep)

}
\arguments{

\item{data}{
 A vector containing the expressions of one gene.
}
\item{x}{A vector consisting of the average expression at time points (1, 2,\ldots ,T), where T is the total number of time points.
}
\item{n.rep}{A vector consisting of the number of replicate arrays at time points (1, 2,\ldots ,T), where T is the total number of time points.
}


}

\value{
\item{logelr}{ Log-maximum likelihood 
}
\item{mu}{A vector containing the estimator of the mean
}


}
\author{
Tianqing Liu, Nan Lin, Ningzhong Shi and Baoxue Zhang 

Maintainer:  Tianqing Liu <tianqingliu@gmail.com>
}
\references{
Liu, T., Lin, N., Shi, N. and Zhang, B. (2009), \emph{Information criterion-based clustering with order-restricted candidate profiles in short time-course microarray experiments.} BMC Bioinformatics, \bold{10}: 146.

Robertson, T., Wright, F. T. and Dykstra, R. L. (1988). \emph{Order restricted statistical inference.} New York: Wiley.


Shi, N., Gao, W. and Zhang, B. (2001). \emph{One sided estimation and testing
problems for location models from grouped samples.}
Commun Statist-Simula, \bold{30}: 885-898.

}

\keyword{package}























































