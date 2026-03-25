
% --- Source file: complete.profile.Rd ---

\name{complete.profile}
\Rdversion{1.0}
\alias{complete.profile}
\docType{package}
\title{complete.profile}
\description{ 
Returns the log-maximum likelihood and the estimator of the mean when there is no inequality constraint.
}

\usage{

complete.profile(data,x,n.rep)

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

Maintainer:Tianqing Liu <tianqingliu@gmail.com> 
}

\keyword{package}























































