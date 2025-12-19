\name{BiclustSet-class}
\docType{class}
\alias{BiclustSet}
\alias{BiclustSet-class}
\alias{show,BiclustSet-method}
\alias{summary,BiclustSet-method}
\title{Class \code{BiclustSet}}
\description{
BiclustSet Class contains the biclustering result in a form: bicluster rows and bicluster columns
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BiclustSet", ...)}. 
The variety of inputs variety of inputs (isa2, fabia, biclust,...) can be used.
}
\section{Slots}{
  \describe{
    \item{\code{GenesMembership}:}{logical, object of class \code{"matrix"}, with row membership within a bicluster }
    \item{\code{ColumnMembership}:}{logical, object of class \code{"matrix"}, with column membership within a bicluster }
    \item{\code{Number}:}{code{"numeric"}, number of biclusters in the set}
  }
}
\author{
Tatsiana Khamiakova
}