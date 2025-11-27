\name{cprob}
\docType{methods}
\alias{cprob}
%% \alias{cprob-methods}
\alias{cprob,stslist-method}

\title{
	Empirical conditional probability distributions of order \code{L}
}
\description{
	Compute the empirical conditional probability distributions of order L from a set of sequences
}
\usage{
\S4method{cprob}{stslist}(object, L, cdata=NULL, context, stationary=TRUE, nmin=1, prob=TRUE, 
weighted=TRUE, with.missing=FALSE, to.list=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	a sequence object, that is an object of class stslist as created by TraMineR \code{\link[=TraMineR]{seqdef}} function.
}
\item{L}{ 
integer. Context length.
}
\item{cdata}{ 
under development 
}
\item{context}{
	character. An optional subsequence (a character string where symbols are separated by '-') for which the conditional probability distribution is to be computed.
}
\item{stationary}{
   logical. If \code{FALSE} probability distributions are computed for each sequence position L+1 ... l where l is the maximum sequence length. If \code{TRUE} the probability distributions are  stationary that is time homogenous. 
}
\item{nmin}{
	integer. Minimal frequency of a context. See details.
}
\item{prob}{
	logical. If \code{TRUE} the probability distributions are returned. If \code{FALSE} the function returns the empirical counts on which the probability distributions are computed.
}
\item{weighted}{
	logical. If \code{TRUE} case weights attached to the sequence object are used in the computation of the probabilities. 
}
\item{with.missing}{
	logical. If \code{FALSE} only contexts contining no missing status are considered. 
}
\item{to.list}{
	logical. If \code{TRUE} and \code{stationary=TRUE}, a list instead of a matrix is returned. See \code{value}.
}
}
\details{
The empirical conditional probability \eqn{\hat{P}(\sigma | c)} of observing a symbol \eqn{\sigma \in A} after the subsequence \eqn{c=c_{1}, \ldots, c_{k}} of length \eqn{k=L} is computed as 
\deqn{
\hat{P}(\sigma | c) = \frac{N(c\sigma)}{\sum_{\alpha \in A} N(c\alpha)}
}
where
\deqn{
N(c)=\sum_{i=1}^{\ell} 1 \left[x_{i}, \ldots, x_{i+|c|-1}=c \right], \; x=x_{1}, \ldots, x_{\ell}, \; c=c_{1}, \ldots, c_{k}
}
is the number of occurrences of the subsequence \eqn{c} in the sequence \eqn{x} and \eqn{c\sigma} is the concatenation of the subsequence \eqn{c} and the symbol \eqn{\sigma}.  

Considering a - possibly weighted - sample of \eqn{m} sequences having weights \eqn{w^{j}, \; j=1 \ldots m}, the function \eqn{N(c)} is replaced by
\deqn{
N(c)=\sum_{j=1}^{m} w^{j} \sum_{i=1}^{\ell} 1 \left[x_{i}^{j}, \ldots, x_{i+|c|-1}^{j}=c \right], \; c=c_{1}, \ldots, c_{k}
}
where \eqn{x^{j}=x_{1}^{j}, \ldots, x_{\ell}^{j}} is the \eqn{j}th sequence in the sample. For more details, see \cite{Gabadinho 2016}.
}
\value{
	If \code{stationary=TRUE} a matrix  with one row for each subsequence of length \eqn{L} and minimal frequency \eqn{nmin} appearing in \code{object}.  If \code{stationary=FALSE} a list where each element corresponds to one subsequence and contains a matrix whith the probability distribution at each position \eqn{p} where a state is preceded by the subsequence.  
}
\references{
Gabadinho, A. & Ritschard, G. (2016). Analyzing State Sequences with Probabilistic Suffix Trees: The PST R Package. \emph{Journal of Statistical Software}, \bold{72}(3), pp. 1-39.
}
\author{
Alexis Gabadinho
}

\examples{
## Example with the single sequence s1
data(s1)
s1 <- seqdef(s1)
cprob(s1, L=0, prob=FALSE)
cprob(s1, L=1, prob=TRUE)

## Preparing a sequence object with the SRH data set
data(SRH)
state.list <- levels(SRH$p99c01)
## sequential color palette
mycol5 <- rev(brewer.pal(5, "RdYlGn"))
SRH.seq <- seqdef(SRH, 5:15, alphabet=state.list, states=c("G1", "G2", "M", "B2", "B1"), 
	labels=state.list, weights=SRH$wp09lp1s, right=NA, cpal=mycol5)
names(SRH.seq) <- 1999:2009

## Example 1: 0th order: weighted and unweigthed counts
cprob(SRH.seq, L=0, prob=FALSE, weighted=FALSE)
cprob(SRH.seq, L=0, prob=FALSE, weighted=TRUE)

## Example 2: 2th order: weighted and unweigthed probability distrib.
cprob(SRH.seq, L=2, prob=TRUE, weighted=FALSE)
cprob(SRH.seq, L=2, prob=TRUE, weighted=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ distribution }

