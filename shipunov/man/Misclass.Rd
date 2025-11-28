\name{Misclass}
\alias{Misclass}
\title{Misclassification (confusion) table}

\description{Misclassification (confusion) table}

\usage{Misclass(pred, obs, best=FALSE, ignore=NULL, quiet=FALSE, force=FALSE, ...)}

\arguments{
\item{pred}{Predicted class labels}
\item{obs}{Observed class labels}
\item{best}{Perform a search for the classification table with minimal misclassification error?}
\item{ignore}{Vector of class labels to ignore (convert into NAs)}
\item{quiet}{Output summary?}
\item{force}{Override the restriction of class number in 'best=TRUE' and speed up code?}
\item{...}{Arguments to 'table'}
}

\details{

'Misclass()' produces misclassification (confusion) 2D table based on two
classifications.

The simple variant ('best=FALSE') assumes that class labels are concerted
(same number of corresponding classes).

Advanced variant ('best=TRUE') can search for the best classification
table (with minimal misclassification rate), this is especially useful in
case of unsupervised classifications which typically return numeric
labels. It therefore assumes that the table is a result of some
non-random process. However, internally it generates all permutations of
factor levels and could be very slow if there are 8 and more class
labels. Therefore, more than 8 classes are not allowed. It is possible
nevertheless to override this restriction with 'force=TRUE'; this option
also uses the experimental code which replaces internal table() with
tabulate() and is much faster with many labels.

Variant with 'best=TRUE' might also add empty rows (filled with zeros) to
the table in case if numbers of classes are not equal.

Additional arguments could be passed for table(), for example,
'useNA="ifany"'. If supplied data contains NAs, there will be also note
in the end. Note that tabulate()-based code (activated with force="TRUE")
does not take table()-specific arguments, so if this is a case, warning
will be issued.

It is possible to ignore (convert into NAs) some class labels with
'ignore=...', this is useful for methods like DBSCAN which output special
label for outliers. In that case, note about missing data is also issued.

Alternatives: confusion matrix from caret::confusionMatrix() which is
more feature rich but much less flexible. See in examples how to
implement some statistics used there.

Note that partial "Misclassification errors" are reverse sensitivities,
and "Mean misclassification error" is a reverse accuracy.

If you want to plot misclassification table, Cohen-Friendly association
plot, assocplot() is probably the best. On this plot, note rectangles
which are big, tall and black (check help(assocplot) to know more).
Diagonal which is black and other cells red indicate low
misclassification rates.

}

\value{Invisibly returns the table of class comparison}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{Adj.Rand}}, \code{link{assocplot}}}

\examples{
iris.dist <- dist(iris[, -5], method="manhattan")
iris.hclust <- hclust(iris.dist)
iris.3 <- cutree(iris.hclust, 3)
Misclass(iris.3, iris[, 5])

set.seed(1)
iris.k <- kmeans(iris[, -5], centers=3)
Misclass(iris.k$cluster, iris[, 5])
Misclass(iris.k$cluster, iris[, 5], best=TRUE)

res <- Misclass(iris.k$cluster, iris[, 5], best=TRUE, quiet=TRUE)
## how to calculate statistics from caret::confusionMatrix()
binom.test(sum(diag(res)), sum(res))$conf.int
mcnemar.test(res) # to avoid NA's, add small number to 'res'
## how to plot misclassification table
assocplot(res)
## how to use Misclass() for Recode()
nn <- Recode(iris.k$cluster, from=dimnames(res)$pred, to=dimnames(res)$obs)
head(nn)

library(dbscan)
iris.db <- dbscan(iris[, -5], eps=0.3)
Misclass(iris.db$cluster, iris$Species, ignore=0, best=TRUE)

set.seed(NULL)
}

\keyword{multivariate}
