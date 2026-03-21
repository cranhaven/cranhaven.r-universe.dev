\name{ACTOR}
\alias{ACTOR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determine actor role
}
\description{
Given two verb roles, which of these is most prominent and hence the actor? 
}
\usage{
ACTOR(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
First verb role
}
  \item{y}{
Second verb role
}
}
\details{
Higher meaning values are more prominent. If tie, first argument is actor
}
\value{
numeric: 1 if first role is actor, 2 if second is.
}
\references{
Van Valin, R. (1999). Generalized semantic roles and the syntax-semantics interface. In F. Corblin, C. Dobrovie-Sorin, & J.-M. Marandin (Eds.), Empirical issues in formal syntax and semantics 2 (pp. 373-389). The Hague: Thesus.}
\author{
Sander Lestrade
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{SITUATION} \code{SELECTACTOR} \code{SELECTUNDERGOER} \code{PROPOSITION} \code{REFCHECK} \code{AGENTFIRST} \code{GENERALIZE} \code{CHECKSUCCESS} \code{WORDORDER} \code{VERBMORPHOLOGY} \code{INTERPRET.INT} \code{FREQUPDATE}
}
\examples{
a=rep(1, 4)
b=rep(0, 4)
ACTOR(a,b)
}
\keyword{misc}

