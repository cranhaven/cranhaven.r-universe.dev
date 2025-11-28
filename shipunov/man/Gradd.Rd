\name{Gradd}
\alias{Gradd}
\title{Classification grid and decision boundaries}
\description{Adds to the 2D ordination either colored points to make classification grid, or lines to show decision boundaries}

\usage{
Gradd(model2var, data2var, spacing=75, what="points",
 trnsp=0.2, pch=16, cex=0.8, lwd=2, lty=2, lcol="grey", palette=NULL,
 type="ids", User.Predict=function(model2var, X) {}, ...)
}

\arguments{
\item{model2var}{Model based on 'data2var' (see below).}
\item{data2var}{Data with _exactly_ 2 variables, e.g., result of PCA.}
\item{spacing}{Density of points to predict.}
\item{what}{What to draw: either "points" for classification grid, or "lines" for decision boundaries}
\item{trnsp}{Transparency of points.}
\item{pch}{Type of points.}
\item{cex}{Scale of points.}
\item{lwd}{Width of lines.}
\item{lty}{Type of lines.}
\item{lcol}{Color of lines.}
\item{palette}{Palette to use.}
\item{type}{Type of the model: "ids", "lda", "tree", or "user" (see examples).}
\item{User.Predict}{Function to define in case of 'type="user"'.}
\item{...}{Additional arguments to points() or contour().}
}

\details{

Gradd() takes model and its 2D data, makes new data with the same range
but made of dense equidistantly spaced (grid-like) points, then predicts
class labels from this new data, probably also calculates decision
boundaries, and finally plots either points colored by predition, or
lines along boundaries.

Before you run Gradd(), make the model. This model should have 'predict'
method, and use ids (to make colors) and exactly 2 variables with names
same as 'data2var' column names, e.g:

model2var <- somemodel(ids ~ ., data=cbind(ids, data2var))

If the model type is "user", the Gradd() uses predefined
'User.Predict(model2var, X)' function which must return factor ids from
testing X data (see examples).

To plot both lines and grid, use Gradd() twice.

Gradd() is mainly a teching demo. It is useful if the goal is to
illustrate the general properties of the supervised method and/or
underlying data. It uses the entire 2D dataset to learn new data but
learning from training subset is also possible, see the Naive Bayes
example below.

}

% \value{}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
## SVM:
library(e1071)
iris.p <- prcomp(iris[, -5])$x[, 1:2]
iris.svm.pca <- svm(Species ~ ., data=cbind(iris[5], iris.p))
plot(iris.p, type="n", main="SVM")
Gradd(iris.svm.pca, iris.p) # type="ids" (default)
text(iris.p, col=as.numeric(iris[, 5]), labels=abbreviate(iris[, 5], 1,
 method="both.sides"))

## LDA:
library(MASS)
iris.p <- prcomp(iris[, -5])$x[, 1:2]
iris.lda.pca <- lda(Species ~ . , data=cbind(iris[5], iris.p))
plot(iris.p, type="n", main="LDA")
Gradd(iris.lda.pca, iris.p, type="lda")
text(iris.p, col=as.numeric(iris[, 5]), labels=abbreviate(iris[, 5], 1,
 method="both.sides"))

## tree::tree() (note how to draw decision boundaries):
library(tree)
iris.p <- prcomp(iris[, -5])$x[, 1:2]
iris.tree.pca <- tree(Species ~ . , data=cbind(iris[5], iris.p))
plot(iris.p, type="n", main="tree")
Gradd(iris.tree.pca, iris.p, type="tree", what="lines")
text(iris.p, col=as.numeric(iris[, 5]), labels=abbreviate(iris[, 5], 1,
 method="both.sides"))

## randomForest:
library(randomForest)
iris.p <- prcomp(iris[, -5])$x[, 1:2]
iris.rf.pca <- randomForest(Species ~ ., data=cbind(iris[5], iris.p))
plot(iris.p, type="n", main="randomForest")
Gradd(iris.rf.pca, iris.p) # type="ids" (default)
text(iris.p, col=as.numeric(iris[, 5]), labels=abbreviate(iris[, 5], 1,
 method="both.sides"))

## naiveBayes (note how to use training subsample):
library(e1071)
iris.p <- prcomp(iris[, -5])$x[, 1:2]
sel <- 1:nrow(iris) %in% seq(1, nrow(iris), 5)
plot(iris.p, col=iris$Species, pch=ifelse(sel, 19, 1), main="naiveBayes")
iris.nb2 <- naiveBayes(Species ~ ., data=cbind(iris[5], iris.p)[sel, ])
Gradd(iris.nb2, iris.p[sel, ], what="lines")

## rpart (note how to use MDS for the base plot):
iris.dist <- dist(iris[, -5], method="manhattan")
iris.dist[iris.dist == 0] <- abs(jitter(0))
library(MASS)
iris.m <- isoMDS(iris.dist)$points
colnames(iris.m) <- c("Dim1", "Dim2")
library(rpart)
iris.rpart.mds <- rpart(Species ~ . , data=cbind(iris[5], iris.m))
plot(iris.m, type="n", main="rpart + MDS")
Gradd(iris.rpart.mds, iris.m, type="tree")
text(iris.m, col=as.numeric(iris[, 5]), labels=abbreviate(iris[, 5], 1,
 method="both.sides"))

## QDA:
library(MASS)
iris.p <- prcomp(iris[, -5])$x[, 1:2]
iris.qda.pca <- qda(Species ~ . , data=cbind(iris[5], iris.p))
plot(iris.p, type="n", main="QDA")
Gradd(iris.qda.pca, iris.p, type="lda")
text(iris.p, col=as.numeric(iris[, 5]), labels=abbreviate(iris[, 5], 1,
 method="both.sides"))

## nnet:
library(nnet)
iris.p <- prcomp(iris[, -5])$x[, 1:2]
iris.nnet.pca <- nnet(Species ~ . , data=cbind(iris[5], iris.p), size=4)
plot(iris.p, type="n", main="nnet")
Gradd(iris.nnet.pca, iris.p, type="tree")
text(iris.p, col=as.numeric(iris[, 5]), labels=abbreviate(iris[, 5], 1,
 method="both.sides"))

## kNN (note how to employ User.Predict()):
library(class)
iris.p <- prcomp(iris[, -5])$x[, 1:2]
plot(iris.p, type="n", main="kNN")
Gradd(cbind(iris[5], iris.p), iris.p, type="user",
 User.Predict=function(model2var, X) knn(model2var[, 2:3], X, model2var[, 1], k=5))
text(iris.p, col=as.numeric(iris[, 5]), labels=abbreviate(iris[, 5], 1,
 method="both.sides"))
}

\keyword{aplot}
