\name{PSTr-class}
\Rdversion{1.1}
\docType{class}
\alias{PSTr-class}
\alias{[[,PSTr-method}
%\alias{plot,PSTr,ANY-method}
%\alias{print,PSTr-method}
%\alias{summary,PSTr-method}

\title{Nested representation of a probabilistic suffix tree}
\description{
An object of class \code{"PSTr"} is a node of a probabilistic suffix tree (PST). The slot \code{prob} contains one or several probability distributions (if the PST is segmented) and the slot \code{counts} contains the empirical - possibly weighted - counts from which the probabilities are computed. The slot \code{leaf} indicates whether the node (segment) is a terminal node (segment).
The 'flat' representation of a PST is an object of class \code{"\linkS4class{PSTf}"}), that is a list that contains one element for each level of the tree. Each element of the list is itself a list whose elements are nodes, that is objects of class \code{PSTr}.
The 'nested' representation of a probabilistic suffix tree (PST) is a nested list whose elements are children nodes of class \code{"PSTr"}. This representation is used for printing and plotting PST, in which case the flat representation of a PST, i.e., an object of class \code{"\linkS4class{PSTf}"} is turned into an object of class \code{"PSTr"} by using the \code{as} function.
}
\section{Objects from the Class}{
Objects are created when calling the \code{\link{pstree}} function. 
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"}. In the nested representation of a PST, the elements of the list are the children nodes. Otherwise the list is empty.}
    \item{\code{alphabet}:}{Object of class \code{"character"}. Alphabet on which the sequences, and the PST are built. This slot is non-empty only for the root node of the nested representation of a PST. }
    \item{\code{labels}:}{Object of class \code{"character"} containing the long state labels. This slot is non-empty only for the root node of the nested representation of a PST.}
    \item{\code{cpal}:}{Object of class \code{"character"}. Color palette used to represent each state of the alphabet. This slot is non-empty only for the root node of the nested representation of a PST.}
    \item{\code{index}:}{Object of class \code{"matrix"}. When the PST is segmented, indicates the id of the segment corresponding to each group.}
    \item{\code{counts}:}{Object of class \code{"matrix"}. The counts to which the probability distributions are computed. }
    \item{\code{n}:}{Object of class \code{"matrix"}. The number of occurrences of the context in the learning sample, see \code{\link{cprob}}.}
    \item{\code{prob}:}{Object of class \code{"matrix"}. The probability distributions computed from the counts. }
    \item{\code{path}:}{Object of class \code{"character"}. The node label, i.e. the context which is the  path from the node to the root node of the tree. }
    \item{\code{order}:}{Object of class \code{"integer"}. The depth of the node in the tree, i.e., the order of the probability distribution(s) stored in the node. }
    \item{\code{leaf}:}{Object of class \code{"matrix"}. Indicates whether the node (segment) is a terminal node (segment).}
    \item{\code{pruned}:}{Object of class \code{"matrix"}. If the PST was pruned with the \code{delete=FALSE} option, indicates whether the node (segment) is actually pruned. See \code{\link{prune}}.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{list}"}, from data part.
Class \code{"\linkS4class{vector}"}, by class "list", distance 2.
}
\section{Methods}{
  \describe{
    \item{[[}{\code{signature(x = "PSTr")}: extract sub-branches of a nested representation of a PST.}
    \item{plot}{\code{signature(x = "PSTr", y = "ANY")}: plot a PST, see \code{\link{plot,PSTr,ANY-method}}.}
    \item{print}{\code{signature(x = "PSTr")}: print a PST, see \code{\link{print,PSTr-method}}.}
    \item{summary}{\code{signature(object = "PSTr")}: see \code{\link{summary,PSTr-method}}.}
	 }
}

\author{
Alexis Gabadinho
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\linkS4class{PSTf}}
}
\examples{
showClass("PSTr")
}
\keyword{classes}
