\name{modelSaveState}
\alias{modelSaveState}
\title{Save the model's current state}
\description{This function saves the sate of each chain in OpenBUGS model}
\usage{
modelSaveState(stem)
}
\arguments{
  \item{stem}{The filestem of the files to be generated.}
}
\details{
    Example for argument \code{stem}: 
    If \code{stem = "c:/myFolder/stem"}, the resulting files 
    are called \file{stem1.txt}, ..., \file{stemN.txt}. 
    They are written into the \code{tempdir()} 
    and copied to the path \file{"c:/myFolder"}.
}

\note{This function can be executed once a model has been successfully checked (see \code{\link{modelCheck}}).

If an attempt is made to execute this function in an inappropriate context the generic error message 
\sQuote{command is not allowed (greyed out)} is displayed. 
}
\seealso{\code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}

