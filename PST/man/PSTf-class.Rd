\name{PSTf-class}
\Rdversion{1.1}
\docType{class}
\alias{PSTf-class}
%\alias{cmine,PSTf-method}
%\alias{cplot,PSTf-method}
%\alias{generate,PSTf-method}
%\alias{impute,PSTf,stslist-method}
%\alias{logLik,PSTf-method}
%\alias{nobs,PSTf-method}
%\alias{nodenames,PSTf-method}
%\alias{pdist,PSTf,PSTf-method}
%\alias{plot,PSTf,ANY-method}
%\alias{pmine,PSTf,stslist-method}
%\alias{ppplot,PSTf-method}
%\alias{pqplot,PSTf,stslist-method}
%\alias{predict,PSTf-method}
%\alias{print,PSTf-method}
% \alias{prune,PSTf-method}
%\alias{query,PSTf-method}
%\alias{subtree,PSTf-method}
%\alias{summary,PSTf-method}
% \alias{tune,PSTf-method}

\title{Flat representation of a probabilistic suffix tree}
\description{
The class \code{"PSTf"} is the flat representation of a probabilistic suffix tree (PST) storing a variable length Markov chain model. The flat representation is a list where each element corresponds to a given depth. It is the prefered representation and is used by all functions for model fitting and sequence analysis with PST. The nested representation \code{"\linkS4class{PSTr}"} is used only for printing and plotting PSTs. 
}
\section{Objects from the Class}{
Objects of class \code{"PSTf"} are returned by the \code{\link{pstree}}, \code{\link{prune}} and \code{\link{tune}} function.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"}, a list where each element corresponds to one level of the tree and is itself a list of nodes, i.e., objects of class \code{"\linkS4class{PSTr}"}.}
    \item{\code{data}:}{Object of class \code{"stslist"}. The learning sample to which the PST is fitted, i.e., a sequence object created with the \code{\link[=TraMineR]{seqdef}} function.}
    \item{\code{cdata}:}{Object of class \code{"stslist"}  }
    \item{\code{alphabet}:}{Object of class \code{"character"}. Alphabet on which the sequences, and the PST are built.}
    \item{\code{labels}:}{Object of class \code{"character"} containing the long state labels.}
    \item{\code{cpal}:}{Object of class \code{"character"}. Color palette used to represent each state of the alphabet.}
    \item{\code{segmented}:}{Object of class \code{"logical"} indicating whether the tree is segmented. See \code{\link{pstree}}.}
    \item{\code{group}:}{Object of class \code{"factor"} containing the group membership for each sequence in \code{data}.}
    \item{\code{call}:}{Object of class \code{"call"}.}
    \item{\code{logLik}:}{Object of class \code{"numeric"}, containing the log-likelihood of the VLMC model represented by the PST.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{list}"}, from data part.
Class \code{"\linkS4class{vector}"}, by class "list", distance 2.
}
\section{Methods}{
  \describe{
    \item{cmine}{\code{signature(object = "PSTf")}: context mining, see \code{\link{cmine,PSTf-method}}.}
    \item{cplot}{\code{signature(object = "PSTf")}: plot single nodes of a PST, see \code{\link{cplot,PSTf-method}}.}
    \item{generate}{\code{signature(object = "PSTf")}: generate artificial sequences, see \code{\link{generate,PSTf-method}}. }
    \item{impute}{\code{signature(object = "PSTf", data = "stslist")}: impute missing values in sequence data, see\code{\link{impute,PSTf,stslist-method}}.}
    \item{logLik}{\code{signature(object = "PSTf")}: extract log-likelihood of the VLMC model represented by a PST, see \code{\link{logLik,PSTf-method}}.}
    \item{nobs}{\code{signature(object = "PSTf")}: number of observations (symbols) in the learning sample to which a VLMC model is fitted, see \code{\link{nobs,PSTf-method}}.}
    \item{nodenames}{\code{signature(object = "PSTf")}: retrieve the node labels of a PST, see see \code{\link{nodenames,PSTf-method}}. }
    \item{pdist}{\code{signature(x = "PSTf", y = "PSTf")}: compute probabilistic divergence between two PSTs, see \code{\link{pdist,PSTf,PSTf-method}}. }
    \item{plot}{\code{signature(x = "PSTf", y = "ANY")}: plot a PST, see \code{\link{plot,PSTf,ANY-method}}.}
    \item{pmine}{\code{signature(object = "PSTf", data = "stslist")}: pattern mining, see see \code{\link{pmine,PSTf,stslist-method}}.}
    \item{ppplot}{\code{signature(object = "PSTf")}: plotting a branch of a PST, see \code{\link{ppplot,PSTf-method}}.}
    \item{pqplot}{\code{signature(object = "PSTf", data = "stslist")}: plot the predicted probability of each state in a sequence, see \code{\link{pqplot,PSTf,stslist-method}}. }
    \item{predict}{\code{signature(object = "PSTf")}: predict the likelihood of sequences, see \code{\link{predict,PSTf-method}}. }
    \item{print}{\code{signature(x = "PSTf")}: print a PST, see \code{\link{print,PSTf-method}}.}
    \item{prune}{\code{signature(object = "PSTf")}: prune a PST, see \code{\link{prune,PSTf-method}}. }
    \item{query}{\code{signature(object = "PSTf")}: retrieve counts or next symbol probability distribution from a node in a Probabilistic Suffix Tree, see \code{\link{query,PSTf-method}}. }
    \item{subtree}{\code{signature(object = "PSTf")}: extract a subtree from a segmented PST, see \code{\link{subtree,PSTf-method}}. }
    \item{summary}{\code{signature(object = "PSTf")}: see \code{\link{summary,PSTf-method}}. }
    \item{tune}{\code{signature(object = "PSTf")}: AIC, AICc and BIC based model selection, see \code{\link{tune,PSTf-method}}. }
}
}

\author{
Alexis Gabadinho
}

\seealso{
\code{\linkS4class{PSTr}}
}
\examples{
showClass("PSTf")
}
\keyword{classes}
