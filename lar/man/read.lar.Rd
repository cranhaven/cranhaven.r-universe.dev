\name{read.lar}
\alias{read.lar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read in labour relations file (read.lar)}
\description{
The function read.lar() reads in a labour relations excel-input-file and
attaches convenience labels for labour relations to be used with draw.lar().
}
\usage{
read.lar(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
An excel file (.xlsx) in labour relations input-format. Required.
  }
}

\references{
Project website: http://historyoflabourrelations.org
}
\author{
Richard L. Zijdeman
}

\seealso{
         \code{\link[lar]{draw.lar}}.
       }

\note{
The creation of this package was made by possible by the International Institute 
of Social History and research grants by the Gerda Henkel Stiftung and the 
Netherlands Organisation for Scientific Research (NWO).
}

\examples{
  \dontrun{
  ## Read in a labour relations pre-defined excel file:
  df <- read.lar("Spain_1900_(JMTL-Sept2011).xlsx")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ labour }
\keyword{ relations } % __ONLY ONE__ keyword per line
