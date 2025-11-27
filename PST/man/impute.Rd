\name{impute}
\docType{methods}
\alias{impute}
\alias{impute,PSTf,stslist-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Impute missing values using a probabilistic suffix tree
}
\description{
	Missing states in a set of sequences are imputed by using the probability distributions stored in a probabilistic suffix tree.
}
\usage{
\S4method{impute}{PSTf,stslist}(object, data, method="pmax")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	a probabilistic suffix tree, i.e., an object of class \code{"\linkS4class{PSTf}"} as returned by the \code{\link{pstree}}, \code{\link{prune}} or \code{\link{tune}} function.
}
  \item{data}{
	a sequence object, i.e., an object of class \code{'stslist'} as created by TraMineR \code{\link[TraMineR]{seqdef}} function, containing the sequences to impute. See details.
}
  \item{method}{
	character. If \code{method='pmax'} the state having the highest probability according to the probability distribution associated with the context preceding the missing status is imputed. If \code{method='prob'} the imputation is done randomly by using this probability distribution.

}
}
\details{
A probabilistic suffix tree (PST) can be used to impute missing states in sequences built on the same alphabet. When a missing state occurs in a sequence the procedure searches in the PST for the context preceding the missing state and impute the state according to the conditional distribution associated with the context. The imputation can be done either randomly (method="prob") or with the state having the highest probability. However, more sophisticated modelling taking account of the non response mechanism could be required for imputing missing states. For more details, see \cite{Gabadinho 2016}.
}
\value{
	A sequence object (of class \code{stslist}) containing original sequences in \code{data} with missing states imputed.  
}
\references{
Gabadinho, A. & Ritschard, G. Analyzing State Sequences with Probabilistic Suffix Trees: The PST R Package. \emph{Journal of Statistical Software}, 2016, \bold{72}(3), 1-39.
}
\author{
 Alexis Gabadinho
}
\examples{
## Loading the SRH.seq sequence object
data(SRH)

## working with a sub-sample of 500 sequences
## to reduce computing time
subs <- sample(nrow(SRH.seq), size=500)
SRH.sub.seq <- SRH.seq[subs,]

## Learning the model (missing state is not included)
SRH.pst.L10 <- pstree(SRH.sub.seq, nmin=2, ymin=0.001)

## Pruning
C99 <- qchisq(0.99,5-1)/2
SRH.pst.L10.C99 <- prune(SRH.pst.L10, gain="G2", C=C99)

## Imputing missing values in the SRH sequences
SRH.sub.iseq <- impute(SRH.pst.L10, SRH.sub.seq, method="prob")

## locating sequences having missing values
## in sequence object missing states are identified by '*'
have.miss <- which(rowSums(SRH.sub.seq=='*')>0)

## plotting non imputed vs imputed sequence
## (first 10 sequences in the set) 
par(mfrow=c(1,2))
seqiplot(SRH.sub.seq[have.miss,], withlegend=FALSE)
seqiplot(SRH.sub.iseq[have.miss,], withlegend=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
\keyword{ datagen }
