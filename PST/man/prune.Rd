\name{prune}
\docType{methods}
% \alias{prune-methods}
\alias{prune}
\alias{prune,PSTf-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prune a probabilistic suffix tree
}
\description{
Prune a PST, using either a gain function, a maximal depth or a list of nodes to keep or remove. Optionally, nodes are not removed from the tree but tagged as deleted, helping to visualize the pruning process.
}
\usage{
\S4method{prune}{PSTf}(object, nmin, L, gain, C, keep, drop, state, delete = TRUE, lik =TRUE)
}

\arguments{
  \item{object}{
	a probabilistic suffix tree, i.e., an object of class \code{"\linkS4class{PSTf}"} as returned by the \code{\link{pstree}}, \code{\link{prune}} or \code{\link{tune}} function.
}
  \item{nmin}{
	integer. All strings having counts less than nmin are removed.
}
  \item{L}{
	integer. If specified the the tree is cut at depth L., that is all nodes with depth > L are removed.
}
  \item{gain}{
	character. Function for measuring information gain. See \code{details}.
}
 \item{C}{
	numeric. Cutoff value to use with the gain function
}
  \item{keep}{
	character. A vector of character strings containing the names of the nodes to keep in the tree. All nodes that are not a suffix of contexts in keep are removed from the tree.
}

  \item{drop}{
	character. A vector of character strings containing the names of the nodes to remove from the tree. All nodes that are a suffix of contexts in drop are removed from the tree as weel.
}
  \item{state}{
	character. All nodes corresponding to contexts which include \code{state} are pruned.	
}
  \item{delete}{
	Logical. If FALSE, the pruned nodes are not removed from the tree but tagged as pruned=FALSE, so that when plotting the pruned tree these nodes wil appear surrounded with red (can be set to another color) lines.
}
  \item{lik}{Logical. If TRUE, the log-likelihood of the pruned model, i.e. the likelihood of the training sequences given the model, is computed and stored in the 'logLik' slot of the PST. Setting to FALSE will spare the time required to compute the likelihood.}
}

\details{
The initial tree returned by the \code{\link{pstree}} function may yield an overly complex model containing all contexts of maximal length \eqn{L} and frequency \eqn{N(c) \geq nmin} found in the learning sample. The pruning stage potentially reduces the number of nodes in the tree, and thus the model complexity. It compares the conditional probabilities associated to a node labelled by a subsequence \eqn{c=c_{1},c_{2}, \ldots, c_{k}} to the conditional probabilities of its parent node labelled by the longest suffix of \eqn{c}, \eqn{suf(c)=c_{2}, \ldots, c_{k}}. The general idea is to remove a node if it does not contribute additional information with respect to its parent in predicting the next symbol, that is if \eqn{\hat{P}(\sigma | c)} is not \emph{significantly} different from \eqn{\hat{P}(\sigma | suf(c))} for all \eqn{\sigma \in A}. 

The pruning procedure starts from the terminal nodes and is applied recursively until all terminal nodes remaining in the tree represent an information gain relative to their parent. % Indeed, starting the pruning from the root node and removing an internal node would lead to remove all its children as well even if one of the children at higher depth adds significant additional information.
A gain function,  whose outcome will determine the pruning decision, is used to compare the two probability distributions. The gain function is driven by a cut-off, and different values of this parameter will yield more or less complex trees. A method for selecting the pruning cut-off is described in the \code{\link{tune}} help page.

A first implemented gain function, which is used by the \emph{Learn-PSA} algorithm, is based on the ratio between \eqn{\hat{P}(\sigma|c)} and \eqn{hat{P}(\sigma|suf(c))} for each \eqn{\sigma \in A}. A node represents an information gain if for any symbol \eqn{\sigma \in A} the ratio is greater than the cut-off \eqn{C} or lower than \eqn{1/C}, that is if
\deqn{
G_{1}(c)=\sum_{\sigma \in A} 1 \left[ \frac{\hat{P}(\sigma |c)}{\hat{P}(\sigma | suf(c))} \geq C \; \cup \; 
\frac{\hat{P}(\sigma |c)}{\hat{P}(\sigma | suf(c))} \leq \frac{1}{C} \right] \geq 1
}
where \eqn{C} is a user defined cut-off value. Nodes that do not satisfy the above condition are pruned. For \eqn{C=1} no node is removed since even a node having a next probability distribution similar to the one of its parent does not satisfy the pruning condition.

The \emph{context} algorithm uses another gain function, namely
\deqn{
G_{2}(c)=\sum_{\sigma \in A} \hat{P}(\sigma|c)\log \left( \frac{\hat{P}(\sigma|c)}{\hat{P}(\sigma|suf(c))} \right) N(c) > C
}
where \eqn{c} is the context labelling the terminal node, \eqn{N(c)} is the number of occurrences of \eqn{c} in the data. The cutoff \eqn{C} is specified on the scale of \eqn{\chi^{2}}-quantiles \cite{Maechler-2004}
\deqn{
C=C(\alpha)=\frac{1}{2}qchisq(1-\alpha,v), v=|A|-1
}
where \eqn{qchisq(p=1-\alpha,v)} is the quantile function of a \eqn{\chi^{2}} distribution with \eqn{v} degrees of freedom. The cutoff \eqn{C} is a threshold for the difference of deviances between a tree \eqn{S^{1}} and its subtree \eqn{S^{2}} obtained by pruning the terminal node \eqn{c}. Typical values for \eqn{\alpha} are \eqn{5\%} and \eqn{1\%}, yielding \eqn{p=0.95} and \eqn{p=0.99} respectively. For more details, see \cite{Gabadinho 2016}.
}
\value{
	A probabilistic suffix tree, i.e., an object of class \code{\linkS4class{PSTf}}.
}
\references{
Bejerano, G. & Yona, G. (2001). Variations on probabilistic suffix trees: statistical modeling and prediction of protein families. \emph{Bioinformatics}, 17, pp. 23-43.

Gabadinho, A. & Ritschard, G. (2016). Analyzing State Sequences with Probabilistic Suffix Trees: The PST R Package. \emph{Journal of Statistical Software}, \bold{72}(3), pp. 1-39.

Maechler, M. & Buehlmann, P. (2004). Variable Length Markov Chains: Methodology, Computing, and Software \emph{Journal of Computational and Graphical Statistics}, 13, pp. 435-455.

Ron, D.; Singer, Y. & Tishby, N. (1996). The power of amnesia: Learning probabilistic automata with variable memory length \emph{Machine Learning}, 25, pp. 117-149.
}
\author{
Alexis Gabadinho
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{tune}}, \code{\link{ppplot}}
}
\examples{
data(s1)
s1.seq <- seqdef(s1)
S1 <- pstree(s1.seq, L=3, nmin=2, ymin=0.001)

## --
S1.p1 <- prune(S1, gain="G1", C=1.20, delete=FALSE)
summary(S1.p1)
plot(S1.p1)

## --
C95 <- qchisq(0.95,1)/2
S1.p2 <- prune(S1, gain="G2", C=C95, delete=FALSE)
plot(S1.p2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }

