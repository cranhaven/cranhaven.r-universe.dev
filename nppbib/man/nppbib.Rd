\name{nppbib}
\alias{nppbib}
\title{Nonparametric Partially-Balanced Incomplete Block Design Analysis}
\description{
 Implements a nonparametric statistical test for rank or score data from partially-balanced
 incomplete block-design experiments.
}
\usage{
 nppbib(datafilename)
}
\arguments{
 \item{datafilename}{A character string giving the name of the data file to analyse.}
}
\details{
 This package implements the nonparametric statistical test for rank or score data from
 partially-balanced imcomplete block-design experiments described in the paper by
 DJ Best, JCW Rayner and David Allingham, (Journal of Sensory Studies, 2011).  It reads
 a dataset from a plain-text file and returns a p-value, as well as other values of
 interest.  If the file contains scores, these are converted to ranks for the analysis.  Ties
 are allowed.

 \bold{Data format:}

 Each line should list the rank or score for each product tested by that judge, with
 entries of NA for products that that judge did not test.  The number of lines is, therefore,
 equal to the number of judges, and the number of columns is equal to the number of products.

 Comments (starting with \#) are allowed (both on their own lines and at the end of lines of data).

 Currently there are no error checks on the format.  Users should examine the values of
 Njudges, Nproducts and Nperjudgement in the output to ensure that they are as expected.
}
\value{
 \item{$Njudges}{the number of judges in the data file (number of data lines).}
 \item{$Nproducts}{the number of products tested (number of data columns).}
 \item{$Nperjudgement}{the number of products that each judge considered.}
 \item{$rawdata}{a matrix containing the data that was read from the input file (ranks or scores).}
 \item{$rankdata}{a matrix containing the ranked data.}
 \item{$varCovarMatrix}{the variance-covariance matrix of the ranked data.}
 \item{$adjustedSum}{the adjusted sum of ranks of the products, also called the product effects.}
 \item{$SMstatistic}{the Skilling-Mack test statistic computed from the ranked data.}
 \item{$pvalue}{the p-value for the null hypothesis that there exist no pairwise differences between products.}
}
\references{
 ``A Statistical Test for Ranking Data from Partially-Balanced Incomplete
 Block Designs'', Best, D.J., Rayner, J.C.W. and Allingham, David. Journal of Sensory Studies,
 26(1): 81--84, 2011.
}
\author{
 Allingham, David \email{David.Allingham@newcastle.edu.au}

 Best, D.J.
}
\examples{
 # Analyse the sample data set provided with this package:
 nppbib(system.file('extdata', 'sm6.txt', package='nppbib'))
}
\keyword{htest}
\keyword{design}
\keyword{nonparametric}
