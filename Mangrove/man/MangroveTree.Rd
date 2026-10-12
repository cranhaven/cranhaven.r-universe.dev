\name{MangroveTree}
\alias{MangroveTree}
\alias{initialiseTree}
\alias{getPrevs}
\alias{addPed}
\alias{summary.MangroveTree}
\alias{print.MangroveTree}
\alias{plot.MangroveTree}
\title{
The MangroveTree class
}
\description{
Functions to create, populate, summarise and utilise MangroveTree objects.
}
\usage{
initialiseTree()

# Member functions: 
# tree$addPed(ped,ORs)
# tree$getPrevs(ORs = NULL,K = NULL,overwrite=FALSE,iter=1000)

\method{print}{MangroveTree}(x,...)
\method{summary}{MangroveTree}(object,...)
\method{plot}{MangroveTree}(x,...)
}
\arguments{
  \item{tree}{
      An object of class \code{\link[Mangrove]{MangroveTree}}
}
  \item{x}{
      An object of class \code{\link[Mangrove]{MangroveTree}}
}
  \item{object}{
      An object of class \code{\link[Mangrove]{MangroveTree}}
}
  \item{ped}{
      An object of class \\code{\link[Mangrove]{MangrovePed}}
}
  \item{ORs}{
      An object of class \code{\link[Mangrove]{MangroveORs}}
}
  \item{K}{
      The prevalence of the disease (between 0 and 1, or NULL if not known)
}
  \item{overwrite}{
      Logical. If set to FALSE, and sampling has already been performed, then existing samples will be used. If TRUE, sampling is performed again.
}
  \item{iter}{
      Number of samples to draw from the posterior distribution of number of affecteds.
}
  \item{\dots}{
      Arguments to be passed on to other methods.
 }
}
\details{
The MangroveTree class is used for holding family trees, and for performing risk prediction on them. The \code{initialiseTree} function creates an empty tree, and the \code{tree$addPed} populates it from genetic data. You can then sample from the posterior distribution of number of affecteds using the \code{tree$getPrevs} function.

The \code{print} method writes a text version of the tree to screen, and the \code{summary} method writes some basic information about what the tree contains, along with what calculations have and have not been performed on it. The \code{plot} method is not implemented, and throws an error message.

To see an example of the use of this class, use \code{vignette("Mangrove")}.
}
\value{
For \code{initialiseTree}, an empty object of class \code{MangroveTree}. For the \code{tree$getPrevs} class function, an object of class \code{\link[Mangrove]{MangroveSample}}. 

For all other functions, nothing is returned.
}

\seealso{
\code{\link[Mangrove]{MangrovePed}}, \code{\link[Mangrove]{MangroveORs}}, \code{\link[Mangrove]{MangroveSample}}
}
\examples{
data(famped)
data(exampleORs)

tree <- initialiseTree()
tree$addPed(famped,exampleORs)

print(tree)
summary(tree)

sam <- tree$getPrevs(exampleORs,K=0.02)
summary(sam)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classes}
\keyword{methods}
\keyword{htest}
