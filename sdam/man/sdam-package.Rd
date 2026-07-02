\name{sdam-package}
\alias{sdam-package}
\alias{sdam}
\docType{package}
\title{
\packageTitle{sdam}
}
\description{
\packageDescription{sdam}
}
\details{

\tabular{ll}{
Package: \tab sdam \cr
Type: \tab Package \cr
Version: \tab 1.1.4 \cr
Date: \tab 2 September 2022 \cr
License: \tab CC BY-SA 4.0 \cr
}


%The DESCRIPTION file:
%\packageDESCRIPTION{sdam}

%\packageIndices{sdam}
The \code{"sdam"} package is a suite of tools for performing analyses in the history of the Mediterranean world 
and its neighbouring regions in the antiquity period before the Middle Ages. 
Currently, it is possible to access data of writing material from the Epigraphic Database Heidelberg API with function \code{\link{get.edh}} 
and the wrapper function \code{\link{get.edhw}}. Most of the epigraphic data, at least until 10-11-2020, is available 
in the \code{EDH} dataset, which can be transformed in diverse ways by using function \code{\link{edhw}} and function \code{\link{cln}} for re-encoding and 
cleaning portions of text in inscriptions in the \code{EDH} dataset.

With function \code{\link{prex}}, there is also the possibility to compute probabilities of existence of inscriptions and 
other artefacts with either the aoristic sum or count matching for observations for different periodization options. 
Function \code{\link{plot.dates}} allow visualizing interval time events that can be adjusted by the internal function \code{\link{dts}} 
as illustrated in a vignette.

\code{\link{request}} function allows, with user authentication, performing different types of HTTP requests 
aimed to DEiC's servers in \code{https://sciencedata.dk} or another cloud repository with a customized URL address.

A plotting function is found in \code{\link{plot.map}} that allows visualizing cartographical maps of ancient Roman provinces 
that are part of the \code{EDH} dataset. It relies other datasets from the package that are related to the Roman world in 
\code{\link{rp}}, \code{\link{rpd}}, \code{rpmp}, \code{\link{rpmcd}}, \code{\link{rpcp}}, and \code{retn} for a transport network with terrestrial and maritime main routes.

Similarity by simple matching among column vectors in a table or data frame is achieved by function \code{\link{simil}} 
for making analyses of relations between e.g. assemblages and artefacts. 
Function \code{\link{edhwpd}} is to organize the \code{EDH} dataset per province and dates based on a similarity measure, and it is the basis for 
function \code{\link{rmids}} to compute values of missing dates with a restricted imputation on data subsets of artefacts.
}
\author{
Author: \packageAuthor{sdam} 

Maintainer: \packageMaintainer{sdam}
}
\references{
\emph{Epigraphic Database Heidelberg -- Data Reuse Options}, (Online; retrieved on 16 June 2019). URL https://edh-www.adw.uni-heidelberg.de/data

}
\keyword{ package }
\seealso{
\href{../doc/Intro.html}{Datasets in \code{"sdam"} package}

\href{../doc/Encoding.html}{Re-encoding \code{people} in the \code{EDH} dataset}

\href{../doc/Dates.html}{Dates and missing dating data}

\href{../doc/Maps.html}{Cartographical maps and networks}


\href{https://sdam-au.github.io/sdam/articles/}{Articles using "\code{sdam}"}

\href{https://github.com/sdam-au/sdam}{Release candidate version}

}
\examples{
data(package="sdam")
}
