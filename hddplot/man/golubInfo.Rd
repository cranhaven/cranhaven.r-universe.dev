\name{golubInfo}
\alias{golubInfo}
\docType{data}
\title{Classifying factors for the 72 columns of the Golub data set}
\description{Details are given of the classifying factors for the 72
  columns of the Golub data set.
}
\usage{data(golubInfo)}
\format{
  A data frame with 72 observations on the following 6 variables,
that identifies the samples (observations) in the data set \code{Golub}
  \describe{
    \item{\code{Samples}}{a numeric vector: sample number}
    \item{\code{BM.PB}}{a factor with levels \code{BM} (from bone marrow) 
\code{PB} (from peripheral blood)}
    \item{\code{Gender}}{a factor with levels \code{F} \code{M}}
    \item{\code{Source}}{a factor with levels \code{CALGB} \code{CCG} \code{DFCI} \code{St-Jude}. These are the hospitals from which the sample came}
    \item{\code{tissue.mf}}{a factor with levels \code{BM:NA} \code{BM:f} \code{BM:m} \code{PB:NA} \code{PB:f} \code{PB:m}.  This factor identifies the
several combinations of \code{source} and \code{Gender}}
    \item{\code{cancer}}{a factor with levels \code{allB} \code{allT} \code{aml}
There are two types of Acute Lymphoblastic Leukemia (\code{allB} and 
\code{allT}), plus Acute Myoblastic Leukemia (\code{aml})
}
  }
}
\source{See the help page for the dataset \code{golubMerge}, in the
\code{golubEsets} package, for details of the source of the original
data.
}
\references{
Molecular Classification of Cancer: Class Discovery and Class Prediction by 
Gene Expression Monitoring, Science, 531-537, 1999, T. R. Golub and 
D. K. Slonim and P. Tamayo and C. Huard and M. Gaasenbeek and J. P. Mesirov 
and H. Coller and M.L. Loh and J. R. Downing and M. A. Caligiuri and 
C. D. Bloomfield and E. S. Lander
}
\examples{
data(golubInfo)
str(golubInfo)
}
\keyword{datasets}
