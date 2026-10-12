\name{ImportData}
\alias{ImportData}
\title{
Import data file}
\description{
Import the different paired comparison data files in cvs format and create an object\cr
of class \code{DataPairComp}}
\usage{
ImportData(name,labelprod=FALSE,labelconso=NULL, sep =";",dec=".")
}
\arguments{
  \item{name}{
part of name of the different data files (.csv files)}
  \item{labelprod}{
indicate the existence of labels of the different products in data files\cr
(default=\code{FALSE}) given in the header of each column of the data files.}
  \item{labelconso}{
vector of label of consummers given by the user (default=\code{NULL})}
  \item{sep}{
the field separator character. Values on each line of the file are separated by this character.(default=\code{";"})}
  \item{dec}{
the character used in the file for decimal points.(default=\code{"."})}
}
\value{
Object of class \code{DataPairComp} with the following elements:
  
\code{Cons} : corresponding to the label of consummers (default : Number of consummer)
 
\code{Crit} : names of the different criteria contained in the name of the different data files
 
\code{Prod} : names of the different products (default : number of the product)
 
\code{Paircomp} : \code{list} of number of criteria elements each corresponding to the results of paired comparisons performed by the consummers.
}