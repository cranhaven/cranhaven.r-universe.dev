\name{pqplot}
\alias{pqplot}
\alias{pqplot,PSTf,stslist-method}
\title{ 
	Prediction quality plot
}
\description{
	Plot the predicted probability of each state in a sequence
}

\usage{
\S4method{pqplot}{PSTf,stslist}(object, data, cdata, L, stcol, plotseq=FALSE, 
	ptype="b", cex.plot=1, space=0,
	measure="prob", pqmax, seqscale, ...)
}

\arguments{
	\item{object}{
	a probabilistic suffix tree, i.e., an object of class \code{"\linkS4class{PSTf}"} as returned by the \code{\link{pstree}}, \code{\link{prune}} or \code{\link{tune}} function.
}
	\item{data}{
	a sequence object, i.e., an object of class \code{'stslist'} as created by TraMineR \code{\link[TraMineR]{seqdef}} function, either subsetted with the index of the sequence to predict or containing one sequence. }
	\item{cdata}{
		Not implemented yet.
}
	\item{L}{
		integer. Maximal context length for sequence prediction. This is the same as pruning the PST by removing all nodes of depth<L before prediction. 
}
	\item{stcol}{
		character. Color to use to plot the prediction qualities. 
	}
	\item{plotseq}{ 
		logical. If TRUE, the sequence is displayed separately, and the prediction plot is plotted above.
}
	\item{ptype}{
		character. Type of plot, either \code{'b'} for barplot or \code{'l'} for line.
}
	\item{cex.plot}{
numeric. Expansion factor for setting the size of the font for the axis labels and names. The default value is 1. Values lesser than 1 will reduce the size of the font, values greater than 1 will increase the size.

}
	\item{space}{numeric. Space separating each state in the plot.
}
	\item{measure}{
	character. Measure used for prediction quality. Either \code{'prob'} or \code{'logloss'}.$
}
	\item{pqmax}{
		numeric. Maximum coordinate for the prediction quality plot, i.e. the max of the y axis.
	}
	\item{seqscale}{
		numeric. If \code{plotseq=TRUE}, width of the bar representing the sequence as a proportion of the y axis range.
	}
	\item{\dots}{
		optional graphical parameters to be passed to the plot function.
	}
}

\details{
	The \code{pqplot()} function displays either the predicted probabilities or the log-loss for each position of a single sequence as a series of barplots. For more details, see \cite{Gabadinho 2016}.
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
S1 <- pstree(s1, L=3)

z <- seqdef("a-b-a-a-b")
pqplot(S1, z)
pqplot(S1, z, measure="logloss", plotseq=TRUE)
}

\keyword{ methods }
\keyword{ hplot }
