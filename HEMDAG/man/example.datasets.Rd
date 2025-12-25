\name{example.datasets}
\alias{example.datasets}
\alias{g}
\alias{L}
\alias{S}
\alias{W}
\alias{test.index}
\docType{data}
\title{Small real example datasets}
\description{Collection of real sub-datasets used in the examples of the \pkg{HEMDAG} package}
\usage{
data(graph)
data(labels)
data(scores)
data(wadj)
data(test.index)
}
\details{
The DAG \code{g} contained in \code{graph} data is an object of class \code{graphNEL}. The graph \code{g} has 23 nodes and 30 edges and
represents the "ancestors view" of the HPO term \emph{Camptodactyly of finger} (\code{"HP:0100490"}).

The matrix \code{L} contained in the \code{labels} data is a 100 X 23 matrix, whose rows correspond to genes
(\emph{Entrez GeneID}) and columns to HPO classes.
\eqn{L[i,j]=1} means that the gene \eqn{i} belong to class \eqn{j}, \eqn{L[i,j]=0} means that the gene \eqn{i} does not belong to class \eqn{j}.
The classes of the matrix \code{L} correspond to the nodes of the graph \code{g}.

The matrix \code{S} contained in the \code{scores} data is a named 100 X 23 flat scores matrix, representing the likelihood
that a given gene belongs to a given class: higher the value higher the likelihood. The classes of the matrix \eqn{S} correspond
to the nodes of the graph \code{g}.

The matrix \code{W} contained in the \code{wadj} data is a named 100 X 100 symmetric weighted adjacency matrix, whose rows and
columns correspond to genes.The genes names (\emph{Entrez GeneID}) of the adjacency matrix \code{W} correspond to the genes names of the
flat scores matrix \code{S} and to genes names of the target multilabel matrix \code{L}.

The vector of integer numbers \code{test.index} contained in the \code{test.index} data refers to the index of the examples of the scores
matrix \code{S} to be used in the test set. It is useful only in holdout experiments.
}
\note{
Some examples of full data sets for the prediction of HPO terms are available at the following \href{https://homes.di.unimi.it/notaro/DATA/DATA_BMC/}{link}.
Note that the processing of the full datasets should be done similarly to the processing of the small data examples provided directly in this package.
Please read the \code{README} clicking the link above to know more details about the available full datasets.
}

