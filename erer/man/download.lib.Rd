\name{download.lib}
\alias{download.lib}
\title{
Downloading source and PDF version of R packages
}
\description{
This function can download three documents for each package: zip version, source version (tar.gz), and PDF manual.
}
\usage{
download.lib(pkgs, destdir = getwd(), mode = "wb", 
  pdf.url = "http://cran.r-project.org/web/packages/", 
  f.zip = TRUE, f.pdf = TRUE)
}

\arguments{
  \item{pkgs}{ A character vector for the names of one or multiple packages}
  \item{destdir}{ The directory where documents are saved (e.g., C:/myFile; the default directory is the current working directory.}
  \item{mode}{ The argument for \code{download.file}; the default is 'wb' for PDF version.}
  \item{pdf.url}{ The url for PDF documents online.}
  \item{f.zip}{ A logical value indicating whether a zip version is downloaded; by default, a zip version is downloaded by \code{download.packages}}
  \item{f.pdf}{ A logical value indicating whether a PDF mannual for a package should be downloaded from the internet.}
}
\details{
This function is mainly used to download the source version of one or multiple packages. A zip version and a PDF manual can also be downloaded at the same time.
}
\value{
Return the packge names.
}

\author{Changyou Sun (\email{cs258@msstate.edu})}

\seealso{\code{\link{download.packages}}; \code{\link{download.file}}.}

\examples{

\dontrun{
  download.lib(pkgs = 'erer', destdir = 'c:/aErer/Rcode', 
    f.zip = FALSE, f.pdf = TRUE)
}
}
\keyword{manip}