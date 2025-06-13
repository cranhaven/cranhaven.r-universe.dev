\name{apc.get.index}
\alias{apc.get.index}
\title{Get indices for mapping data into trapezoid formation}
\description{This function does the internal book keeping between the original data format and the trapezoid format.
It creates index matrices to transform data between original format, trapezoid format and a vector,
as well as values to keep track of the labels for the time scales.

The generalized trapezoids are introduced in Kuang, Nielsen and Nielsen (2008), see also Nielsen (2014).}
\usage{apc.get.index(apc.data.list)}
\arguments{
  \item{apc.data.list}{See \code{\link{apc.data.list}} for a description of the format}
}
\value{
 A list containing the following values.
	\item{response}{Matrix.  An argument}
	\item{dose}{Matrix or NULL.  An argument}
	\item{data.format}{Character.  An argument}
	\item{unit}{Numeric. An argument.}
	\item{data.xmax}{Numeric.  Number of rows of response matrix.}
	\item{data.ymax}{Numeric.  Number of columns of response matrix.}
	\item{data.xlab}{Character. Label for row index of response  matrix. Derived from \code{data.format}.}
	\item{data.ylab}{Character. Label for column index of response  matrix. Derived from \code{data.format}.}
	\item{data.xlab1}{Numeric.  Year for smallest row index of response  matrix.    }
	\item{data.ylab1}{Numeric.  Year for smallest column index of response  matrix. }
	\item{n.data}{Numeric.  Number of observations. }
	\item{index.data}{Matrix of dimension \code{n.data}x2.  Index pairs for observations in the original coordinate system as given by \code{data.format}.  Same order as in \code{index.trap}.}
	\item{index.trap}{Matrix of dimension \code{n.data}x2.  Index pairs for observations in an age/cohort system.  Hence the coordinates of a trapezoid matrix.  Same order as in \code{index.data}.}
	\item{age.max}{Numeric.  Number of age groups.	 }
	\item{per.max}{Numeric.  Number of period groups.}
	\item{coh.max}{Numeric.  Number of cohort groups.}
	\item{per.zero}{Numeric.  Anchor for period index, so that period starts from \code{per.zero}+1.}
	\item{per.odd}{Logic.  TRUE if per.zero is odd.}
	\item{U}{Numeric.  Integer value of (per.zero+3)/2.}
	\item{age1}{Numeric.  Year for smallest age index. Derived for data.format="CP", "PC", otherwise an argument.}
	\item{per1}{Numeric.  Year for smallest period index. Derived for data.format="AC","CA","CL","CL.vector.by.row","trapezoid", otherwise an argument.}
	\item{coh1}{Numeric.  Year for smallest cohort index. Derived for data.format="AP", "PA", otherwise an argument.}
}
\references{
Kuang, D., Nielsen, B. and Nielsen, J.P. (2008a) Identification of the age-period-cohort model and the extended chain ladder model. Biometrika 95, 979-986. \emph{Download}: \href{https://doi.org/10.1093/biomet/asn026}{Article}; Earlier version \href{http://www.nuffield.ox.ac.uk/economics/papers/2007/w5/KuangNielsenNielsen07.pdf}{Nuffield DP}.

Nielsen, B. (2014) Deviance analysis of age-period-cohort models.  \href{http://www.nuffield.ox.ac.uk/economics/papers/2014/apc_deviance.pdf}{Nuffield DP}.
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 31 Mar 2015}
\examples{
################
#	Artificial data

###############
#	Artificial data
#	Generate a 3x5 matrix and make arbitrary decisions for rest

response <- matrix(data=seq(1:15),nrow=3,ncol=5)
data.list	<- list(response=response,dose=NULL,data.format="AP",
					age1=25,per1=1955,coh1=NULL,
					unit=5,per.zero=NULL,per.max=NULL,time.adjust=0)
apc.get.index(data.list)

}