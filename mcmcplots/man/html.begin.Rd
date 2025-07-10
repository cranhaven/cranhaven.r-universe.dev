\name{.html.begin}
\alias{.html.begin}
\title{
  Initialize an html file for use in the mcmcplot function.
}
\description{
  Initializes an html file to capture output from the mcmcplot function.  Intended for internal use only.
}
\usage{
.html.begin(outdir = tempdir(), filename = "index", extension = "html", title, cssfile)
}
\arguments{
  \item{outdir}{
    file path for the output directory.
  }
  \item{filename}{
    name of the html file.
  }
  \item{extension}{
    name of the file extension.
  }
  \item{title}{
    title for the html file.
  }
  \item{cssfile}{
    css file name.
  }
}
\value{
  String containing the path to the initialized html file.
}
\author{
  Ilya Goldin
}
\seealso{
  \code{\link{.html.end}}, \code{\link{.html.img}}, \code{\link{mcmcplot}}
}
\examples{
## See examples for function mcmcplot
}
\keyword{ utilities }
