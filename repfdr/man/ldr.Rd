\name{ldr}
\alias{ldr}
\title{
Estimation of posterior probabilities for the vectors of association status
}
\description{
The function finds the posterior probabilities ofeach vector of association status for each feature, given the feature's vector of binned z-scores.   
}
\usage{
ldr(pdf.binned.z, binned.z.mat, Pi, h.vecs = NULL)
}
\arguments{
  \item{pdf.binned.z}{Same input as in \code{\link{repfdr}}.
A 3-dimensional array which contains for each study (first dimension), the probability of a z-score to fall in the bin (second dimension), under each hypothesis status (third dimension). The third dimension can be of size 2 or 3, depending on the number of association states: if the association can be either null or only in one direction, the dimension is 2; if the association can be either null, or positive, or negative, the dimension is 3.  
Element \code{[[1]]} in the output of \code{\link{ztobins}}.
}
  \item{binned.z.mat}{
  Same input as in \code{\link{repfdr}}.
A matrix of the bin numbers for each of the z-scores (rows) in each study (columns).
Element \code{[[2]]} in the output of \code{\link{ztobins}}.
}
  \item{Pi}{
  The estimated prior probabilities for each association status vector. Can be extracted from the output of   \code{\link{repfdr}} or \code{\link{piem}}, see Example section. 
}
  \item{h.vecs}{
The row indices in \code{H} (see \code{\link{hconfigs}}), corresponding to the association status vectors. By default the posterior probabilities of all possible vectors of association status are computed.
}
}
\details{
A subset of features (e.g most significant) can be specified as the rows in \code{binned.z.mat}, so the posterior probabilities of the vectors of association status are computed for this subset of features. See Example section.  
}
\value{
Matrix with rows that contain for each of the vectors of association status  the posterior probabilities. The columns are the different feature.
}
\seealso{
\code{\link{repfdr}}, \code{\link{piem}}, \code{\link{hconfigs}}
}
\examples{

\dontrun{
download.file('http://www.math.tau.ac.il/~ruheller/repfdr_RData/binned_zmat.RData',
  destfile = "binned_zmat.RData")
load(file = "binned_zmat.RData")
  
data(Pi)

# Fdr calculation:
output3 <- repfdr(pbz, bz, "replication",Pi.previous.result = Pi)

BayesFdr <- output3$mat[,"Fdr"]
sum(BayesFdr <= 0.05)

# The posterior probabilities for the the first five features with Bayes FDR at most 0.05:
post <- ldr(pbz,bz[which(BayesFdr <= 0.05)[1:5],],Pi)
round(post,4)

# posteriors for a subset of the association status vectors can also be reported,
# here the subset is the four first association status vectors:
post <- ldr(pbz,bz[which(BayesFdr <= 0.05)[1:5],],Pi,h.vecs= 1:4)
round(post,4)
}
}