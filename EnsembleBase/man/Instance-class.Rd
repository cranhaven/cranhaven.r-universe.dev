\name{Instance-class}
\Rdversion{1.1}
\docType{class}
\alias{Instance-class}
\alias{Instance.List-class}

\title{Classes \code{"Instance"} and \code{"Instance.List"}}
\description{
A base learner \code{Instance} is a combination of a base learner configuration and data partition. Instances constitute the major input into the cross-validation-based functions such as \code{\link{Regression.CV.Batch.Fit}}. An \code{Instance.List} is a collection of instances, along with the underlying definition of data partitions referenced in the instance objects. The function \code{\link{make.instances}} is a convenient function for generating an instance list from all permutations of a given list of base learner configurations and data partitions.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Instance", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
  \code{Instance} has the following slots:
    \item{\code{config}:}{Object of class \code{"BaseLearner.Config"} ~~ }
    \item{\code{partid}:}{Object of class \code{"character"} ~~ }
  \code{Instance.List} has the following slots:  
    \item{\code{instances}:}{Object of class \code{"list"}, with each element being an object of class \code{Instance}.}
    \item{\code{partitions}:}{Object of class \code{"matrix"}, defining data partitions referenced in each instance. This object is typically the output of \code{\link{generate.partitions}}. }
  }
}
\section{Methods}{
No methods defined with class "Instance" in the signature.
}

\author{
Alireza S. Mahani, Mansour T.A. Sharabiani
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{make.instances}}, \code{\link{generate.partitions}}, \code{\link{Regression.CV.Batch.Fit}}
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("Instance")
}
\keyword{classes}
