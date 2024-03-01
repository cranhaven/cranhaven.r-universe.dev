\name{JGRMessageBox}
\alias{JGRMessageBox}
\title{Dialog box for predicting env. conditions from biological observations}
\description{
  A general Java message box contains information and error message
}
\usage{
JGRMessageBox(w.title = "BiologicalInferences: Error", msg = "Hello!")
}
\arguments{
  \item{w.title}{Title of the message box}
  \item{msg}{Message in the box}
  }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (w.title = "BiologicalInferences: Error", msg = "Hello!") 
{
    a <- .jnew("org.neptuneinc.cadstat.plots.BiologicalInferencesGeneralBox")
    .jcall(a, "Ljavax/swing/JFrame;", "getMyGUI", w.title, msg)
  }
}
\keyword{regression}
