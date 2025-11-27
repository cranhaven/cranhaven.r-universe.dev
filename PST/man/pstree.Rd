\name{pstree}
\docType{methods}
\alias{pstree}
%% \alias{pstree-methods}
\alias{pstree,stslist-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Build a probabilistic suffix tree
}
\description{
Build a probabilistic suffix tree that stores a variable length Markov chain (VLMC) model 
}
\usage{
\S4method{pstree}{stslist}(object, group, L, cdata=NULL, stationary=TRUE, 
	nmin = 1, ymin=NULL, weighted = TRUE, with.missing = FALSE, lik = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	a sequence object, i.e., an object of class \code{'stslist'} as created by TraMineR \code{\link[TraMineR]{seqdef}} function.
}
  \item{group}{
	a vector giving the group membership for each observation in x. If specified, a segmented PST is produced containing one PST for each group. 
}
  \item{cdata}{ 
	Not implemented yet.
}
  \item{stationary}{ 
	Not implemented yet.
}
 \item{L}{
	Integer. Maximal depth of the PST. Default to maximum length of the sequence(s) in object minus 1.
}
  \item{nmin}{
	Integer. Minimum number of occurences of a string to add it in the tree 
}
  \item{ymin}{
	Numeric. Smoothing parameter for conditional probabilities, assuring that no symbol, and hence no sequence, is predicted to have a null probability. The parameter $ymin$ sets a lower bound for a symbol's probability.  
}
  \item{weighted}{
	Logical. If TRUE, weights attached to the sequence object are used in the estimation of probabilities.
}
  \item{with.missing}{
	Logical. If TRUE, the missing state is added to the alphabet
}
  \item{lik}{Logical. If TRUE, the log-likelihood of the model, i.e. the likelihood of the training sequences given the model, is computed and stored in the 'logLik' slot of the PST. Setting to FALSE will spare the time required to compute the likelihood.}
}
\details{
A probabilistic suffix tree (PST) is built from a learning sample of \eqn{n, \; n \geq 1} sequences by successively adding nodes labelled with subsequences (contexts) \eqn{c} of length \eqn{L,  \; 0 \leq L \leq L_{max}} found in the data. When the value \eqn{L_{max}} is not defined by the user it is set to its theorectical maximum \eqn{\ell-1} where \eqn{\ell} is the maximum sequence length in the learning sample. The \code{nmin} argument specifies the minimum frequency of a subsequence required to add it to te tree.\cr
Each node of the tree is labelled with a context \eqn{c} and stores the next symbol empirical probability distribution \eqn{\hat{P}(\sigma|c), \; \sigma \in A}, where \eqn{A} is an alphabet of finite size. The root node labelled with the empty string \eqn{e} stores the \eqn{0th} order probability \eqn{\hat{P}(\sigma), \; \sigma \in A} of oberving each symbol of the alphabet in the whole learning sample.\cr
The building algorithm calls the \code{\link{cprob}} function which returns the empirical next symbol counts observed after each context \eqn{c} and computes the corresponding empirical probability distribution. Each node in the tree is connected to its longest suffix, where the longest suffix of a string \eqn{c=c_{1},c_{2}, \ldots, c_{k}} of length \eqn{k} is \eqn{suffix(c)=c_{2}, \ldots, c_{k}}.\cr
Once an initial PST is built it can be pruned to reduce its complexity by removing nodes that do not provide significant information (see \code{\link{prune}}). A model selection procedure based on information criteria is also available (see \code{\link{tune}}). For more details, see \cite{Gabadinho 2016}.
}
\value{
	An object of class \code{"\linkS4class{PSTf}"}.
}
\references{
Bejerano, G. & Yona, G. (2001) Variations on probabilistic suffix trees: statistical modeling and prediction of protein families. \emph{Bioinformatics} 17, 23-43.

Gabadinho, A. & Ritschard, G. (2016) Analyzing State Sequences with Probabilistic Suffix Trees: The PST R Package. \emph{Journal of Statistical Software} \bold{72}(3), 1-39.

Maechler, M. & Buehlmann, P. (2004) Variable Length Markov Chains: Methodology, Computing, and Software. \emph{Journal of Computational and Graphical Statistics} 13, pp. 435-455.

Ron, D.; Singer, Y. & Tishby, N. (1996) The power of amnesia: Learning probabilistic automata with variable memory length. \emph{Machine Learning} 25, 117-149.
}
\author{
	Alexis Gabadinho
}

\seealso{
	\code{\link{prune}}, \code{\link{tune}}
}
\examples{
## Build a PST on one single sequence
data(s1)
s1.seq <- seqdef(s1)
s1.seq
S1 <- pstree(s1.seq, L = 3)
print(S1, digits = 3)
S1
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }

