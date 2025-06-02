\name{FILERigaAutumn}
\docType{data}
\alias{FILERigaAutumn}
\title{
  Input text "file" for gulf of Riga autumn planktonic food web
}

\description{
  Input text "file" for the Carbon flux Gulf of Riga planktonic food web
  in autumn as described in Donali et al. (1999).

  The Gulf of Riga is a highly eutrophic system in the Baltic Sea

  The foodweb comprises 7 functional compartments and two external
  compartments, connected with 26 flows.

  Units of the flows are mg C/m3/day

  The "dataset" RigaAutumnFile is included to demonstrate the use of a
  text input file for food web models.

  The original file, RigaAutumn.input can be found in subdirectory
  \file{web} of the packages directory

  In this subdirectory you will find many foodweb example input files

  \itemize{
    \item They can be read using \code{\link{Read}}(file)
    \item Or they can be directly solved using \code{\link{Setup}}(file)
  }

}

\usage{
data(FILERigaAutumn)
}

\format{
  vector of character strings as present in the original file
}

\author{
Karline Soetaert <karline.soetaert@nioz.nl>
}

\examples{
print(FILERigaAutumn)

# RigaAutumnInput is a vector of text strings - 
# here it is first converted to a "File" 
# When using the example files in the LIM directory, 
# this first statement is not necessary
\dontrun{
File <- textConnection(FILERigaAutumn)
RigaAutumn.input <- Read(File)
}

}

\references{
  Donali, E., Olli, K., Heiskanen, A.S., Andersen, T., 1999. Carbon flow
  patterns in the planktonic food web of the Gulf of Riga, the Baltic Sea:
  a reconstruction by the inverse method. Journal of Marine Systems 23,
  251..268.
}

\seealso{
  \code{\link{LIMRigaAutumn}} a list containing the linear inverse model
  specification, generated from file \file{RigaAutumn.input}
}

\keyword{datasets}

