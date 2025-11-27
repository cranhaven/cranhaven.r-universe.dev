\name{predict}
\docType{methods}
\alias{predict}
\alias{predict,PSTf-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Compute the probability of categorical sequences using a probabilistic suffix tree
}
\description{
Compute the probability (likelihood) of categorical sequences using a Probabilistic Suffix Tree
}
\usage{
\S4method{predict}{PSTf}(object, data, cdata, group, L=NULL, p1=NULL, output="prob", decomp=FALSE, base=2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	a probabilistic suffix tree, i.e., an object of class \code{"\linkS4class{PSTf}"} as returned by the \code{\link{pstree}}, \code{\link{prune}} or \code{\link{tune}} function.
}
  \item{data}{
	a sequence object, i.e., an object of class \code{'stslist'} as created by TraMineR \code{\link[TraMineR]{seqdef}} function, containing the sequences to predict.
}
  \item{cdata}{
	not implemented yet.
}
  \item{group}{if \code{object} is a segmented PST, providing a vector of group membership so that each sequence probability will be predicted with the conditional probability distributions for the group it belongs to. If \code{object} is a segmented PST and \code{group} is not provided, each sequence will be predicted by each of the submodel, and the output will be a matrix with \code{nbgroup} columns, where \code{nbgroup} is the number of segments in the PST.
}
	\item{L}{
		integer. Maximal context length for sequence prediction. This is the same as pruning the PST by removing all nodes of depth<L before prediction.
}
	\item{p1}{
		vector. A probability distribution for the first position in the sequence that will be used instead of the root node of the tree.
	}
	\item{output}{
		character. One of \code{'prob'}, \code{'logloss'},  \code{'SIMn'} or \code{'SIMo'}. See details.
	}
	\item{decomp}{
		logical. If \code{TRUE} the predicted probability for each state in the sequence(s) is returned instead of the whole sequence probability.
	}
	\item{base}{
	integer. Base for the logarithm if a logarithm is used in the used prediction measure.
    }
}
\details{
A probabilistic suffix tree (PST) allows to compute the likelihood of any sequence built on the alphabet of the learning sample. This feature is called sequence prediction. The likelihood of the sequence \code{a-b-a-a-b} given a PST \code{S1} fitted to the example sequence \code{s1} (see example) is
\deqn{
P^{S1}(abaab)= P^{S1}(a) \times P^{S1}(b|a) \times P^{S1}(a|ab) \times P^{S1}(a|aba) \times P^{S1}(b|abaa)
}

The probability of each of the state is retrieved from the PST. To get for example \code{P(a|a-b-a)}, the tree is scanned for the node labelled with the string \code{a-b-a}, and if this node does not exist, it is scanned for the node labelled with the longest suffix of this string, that is \code{b-a}, and so on. The node \code{a-b-a} is not found in the tree (it has been removed during the pruning stage), and the longest suffix of \code{a-b-a} found is \code{b-a}. The probability \code{P(a|b-a)} is then used instead of \code{P(a|a-b-a)}. \cr

The sequence likelihood is returned by the \code{predict} function. By setting \code{decomp=TRUE} the output is a matrix containing the probability of each of the symbol composing the sequence. The score \eqn{P^S(x)} of a sequence \eqn{x} represents the probability that the VLMC model stored by the PST \eqn{S} generates \eqn{x}. It can be turned into a more readable prediction quality measure such as the \emph{average log-loss}
\deqn{
logloss(S,x)=-\frac{1}{\ell} \sum_{i=1}^{\ell} \log_{2} P^{S}(x_{i}| x_{1}, \ldots, x_{i-1})=-\frac{1}{\ell} \log_{2} P^{S}(x)
}
by using \code{'output=logloss'}.
The returned value is the average log-loss of each state in the sequence, which allows to compare the prediction for sequences of unequal lengths. The average log-loss can be interpreted as a residual, that is the distance between the prediction of a sequence by a PST \eqn{S} and the perfect prediction  \eqn{P(x)=1} yielding \eqn{logloss(P^{S},x)=0}. The lower the value of \eqn{logloss(P^{S},s)} the better the sequence is predicted. For more details, see \cite{Gabadinho 2016}.
}
\value{
	Either a vector of sequence probabilities (decomp=FALSE) or a matrix (if decomp=FALSE) containing for each sequence (row) the probability of each state in columns.   

}
\references{
Gabadinho, A. & Ritschard, G. (2016) Analyzing State Sequences with Probabilistic Suffix Trees: The PST R Package. \emph{Journal of Statistical Software}, \bold{72}(3), 1-39.
}
\author{
Alexis Gabadinho
}

\examples{
data(s1)
s1 <- seqdef(s1)

S1 <- pstree(s1, L=3, nmin=2, ymin=0.001)
S1 <- prune(S1, gain="G1", C=1.20, delete=FALSE)

predict(S1, s1, decomp=TRUE)
predict(S1, s1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }

