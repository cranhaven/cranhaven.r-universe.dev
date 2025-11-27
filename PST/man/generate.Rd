\name{generate}
\alias{generate}
\alias{generate,PSTf-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Generate sequences using a probabilistic suffix tree
}
\description{
Generate sequences using a probabilistic suffix tree
}
\usage{
\S4method{generate}{PSTf}(object, l, n, s1, p1, method, L, cnames)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	a probabilistic suffix tree, i.e., an object of class \code{"\linkS4class{PSTf}"} as returned by the \code{\link{pstree}}, \code{\link{prune}} or \code{\link{tune}} function.
}
  \item{l}{
	integer. Length of the sequence(s) to generate.
}
  \item{n}{
	integer. Number of the sequence(s) to generate.
}
  \item{s1}{
	character. The first state in the sequences. The length of the vector should equal \code{n}. If specified, the first state in the sequence(s) is not randomly generated but taken from \code{s1}. 
}
  \item{p1}{
	numeric. An optional probability vector for generating the first position state in the sequence(s). If specified, the first state in the sequence(s) is randomly generated using the probability distribution in \code{p1} instead of the probability distribution taken fron the root node of \code{object}. 
}
  \item{method}{
	character. If \code{method=pmax}, at each position the state having the highest probability is chosen. If \code{method=prob}, at each position the state is generated using the corresponding probability distribution taken from \code{object}.
}
  \item{L}{
	integer: Maximal depth used to extract the probability distributions from the PST object.  
}
  \item{cnames}{
	character: Optional column (position) names for the returned state sequence object. By default, the names of the sequence object to which the model was fitted are used (slot "data" of the PST).
}
}
\details{
As a probabilistic suffix tree (PST) represents a generating model, it can be used to generate artificial sequence data sets. Sequences are built by generating the states at each successive position. The process is similar to sequence prediction (see \code{\link{predict}}), except that the retrieved conditional probability distributions provided by the PST are used to generate a symbol instead of computing the probability of an existing state. For more details, see \cite{Gabadinho 2016}.
}
\value{
A state sequence object (an object of class \code{stslist}) containing \code{n} sequences. This object can be passed as argument to all the functions for visualization and analysis provided by the \code{\link{TraMineR}} package. 
}
\references{
Gabadinho, A. & Ritschard, G. (2016). Analyzing State Sequences with Probabilistic Suffix Trees: The PST R Package. \emph{Journal of Statistical Software}, \bold{72}(3), pp. 1-39.
}
\author{
Alexis Gabadinho
}

\examples{
data(s1)
s1.seq <- seqdef(s1)
S1 <- pstree(s1.seq, L=3)

## Generating 10 sequences
generate(S1, n=10, l=10, method="prob")

## First state is generated with p(a)=0.9 and p(b)=0.1
generate(S1, n=10, l=10, method="prob", p1=c(0.9, 0.1))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ datagen }
\keyword{ methods }

