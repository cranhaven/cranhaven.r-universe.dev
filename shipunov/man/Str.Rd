\name{Str}
\alias{Str}

\title{'str()' enchanced for data frames}

\description{Enhanced 'str()': with variable numbers, row names, missing data indication and possibly more}

\usage{Str(df, as.factor=FALSE)}

\arguments{
\item{df}{Data frame}
\item{as.factor}{Convert character columns to factors?}
}

\details{

'Str()' is an enhanced 'str()'. 'Str()' (1) shows data frame structure
with column indexes, (2) indicates presence of NA(s) with star (*) and
(3) lists first five row names, if they are not default.

If the object is a data frame with atomic columns, this function captures
output of internal 'str()', changes it and outputs the new one. If the
object is not a data frame or is a data frame with non-atomic columns,
then output is not changed.

If 'as.factor=TRUE', converts all character columns to factors before
reporting the structure, thus mimicking pre-R4 behavior of many functions
related with data frames (and also invisibly outputs the new data frame).
Might be useful, for example, to understand the number of unique
character values which will be shown as "factor levels", works well in
conjunction with summary(), please see examples.

Alternative: DescTools::Str() which uses cycles (slower!), has less
features, but works with non-atomic columns.

}

\value{

If 'as.factor=TRUE', invisibly outputs the data frame with all character
columns converted into factors.

}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{str}}}

\examples{
trees1 <- trees
row.names(trees1)[1] <- "a"
trees1[1, 1] <- NA
Str(trees)
Str(trees1)

\dontrun{
trees.crazy <- trees
trees.crazy[[2]] <- trees[, 2, drop=FALSE]
str(trees.crazy)
Str(trees.crazy) # columns non-atomic: output as from str()
}

abc <- data.frame(N=1:26, LETTERS, letters, stringsAsFactors=FALSE)
abc[3, 1] <- NA
Str(abc)
Str(abc, as.factor=TRUE)
summary(Str(abc, as.factor=TRUE))

}

\keyword{environment}
