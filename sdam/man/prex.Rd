\name{prex}
\alias{prex}
\title{
Compute probabilities of existence
}
\description{
A function to compute probabilities of existence of artefacts related events.
}
\usage{
prex(x, type = c("aoristic", "mp", "other"), taq, tpq, vars, 
     bins = NULL, cp, weight = 1, DF, out, plot = FALSE, main = NULL, 
     ylim, keep, ...)
}

\arguments{
  \item{x}{
  list or data frame object of variables and observations.
}
  \item{type}{
      Type of date format to compute or plot: 
        
        \code{aoristic} aoristic sum
        
        \code{mp} mid points and range
        
        \code{other} 
}
  \item{taq}{
  timespan endpoint \emph{terminus ante quem} (TAQ)
}
  \item{tpq}{
  timespan endpoint \emph{terminus post quem} (TPQ)
}
  \item{vars}{
   boundaries of existence of \code{x} (vector for timespan endpoints)
}
  \item{bins}{
  length of the break (integer and optional)
}
  \item{cp}{
        Chronological phase:

         \code{"bin5"} five-bins from the antiquity period

         \code{"bin8"} eight-bins from the antiquity period

         list with a customized chronological phase
}
  \item{weight}{
  value to observations (optional)
}
  \item{DF}{
  return also data frame with observations? (optional and logical)
}
  \item{out}{
  number of outliers to omit (integer or vector where first entry id for latest date)
}
  \item{plot}{
  plot the results?
}
  \item{main}{
  plot's main title (optional)
}
  \item{ylim}{
  limit in \eqn{y}-axis (optional, for plot)
}
  \item{keep}{
  for \code{mp}, keep variables in output? (optional and logical)
}
  \item{\dots}{
  additional optional parameters
}
}
\details{
Currently, the probability of existence of the observations is the \emph{aoristic sum} computed across events for portions of time 
delimited by a TAQ in \code{taq} and TPQ in \code{tpq}, which are endpoints from the stance of the timespan. 
Alternatively, these two boundaries of existence of x are specified in \code{vars}. 

In case the \code{bins} are set to \code{NULL}, then the time breaks take the chronological periods in \code{cp}, which by default 
is \code{"bin5"} or five-periods for the \code{EDH} dataset, which are \code{Arch} (Archaic), \code{Class} (Classical), \code{Hell} 
(Hellenistic), \code{Rom}, (Roman), and \code{Byz} (Byzantine). 
Another built-in option is \code{"bin8"} for eight chronological periods where the Roman period is divided into \code{ERom} (Early Roman), \code{MRom} (Middle Roman), and \code{LRom} (Late Roman) while the Byzantine period is divided into \code{EByz} (Early Byzantine) and \code{MByz} (Middle Byzantine). 
However, option \code{cp} is open for other periodization models as long as the categories of time blocks are components of a list object.
}
\value{
A data frame with values according to the chosen either \code{bins} or \code{cp}. 
If \code{plot} is specified, a bar plot with bars of outcomes.
}
\references{
Crema, E. ``Modelling temporal uncertainty in archaeological analysis,'' \emph{J Archaeol Method Theory}, 19:440--461. (2012). 
(for aoristic sum)

Bevan, \emph{et al}. ``Measuring chronological uncertainty in intensive survey finds: A case study from Antikythera, Greece,'' 
\emph{Archaeometry}, 55, 2, 312--328. (2013). (default chronological periods)
}
\author{
Antonio Rivero Ostoic
}
\note{
When \code{aoristic} is set to \code{FALSE}, then a simple matching of only TAQ and TPQ is computed from \code{x}. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{edhw}}, \code{\link{plot.dates}}, \code{\link{dts}}.
}
\examples{
\dontrun{
# first 100 entries in the EDH dataset (default)
EDHdates <- edhw(vars=c("not_after", "not_before"), as="df", limit=100)

# compute aoristic sum with five-periods
prex(x=EDHdates, taq="not_before", tpq="not_after", cp="bin5")

# compute aoristic sum with 75 year span
prex(x=EDHdates, taq="not_before", tpq="not_after", bins=75, plot=TRUE)}
}
\keyword{ datagen }
\keyword{ metrics }
