\name{plot.dates}
\alias{plot.dates}
\title{
Plot interval dates
}
\description{
A function to plot interval dates with different forms.
}
\usage{
\method{plot}{dates}(x, y, type = c("ts", "mp", "rg"), taq, tpq, id, out, 
    col, cex, lwd, lty, pch, main = NULL, xlab = NULL, ylab = NULL, 
    xlim = NULL, axes = TRUE, alpha, file = NULL, \dots)
%plot.dates(x, y, type = c("ts", "mp", "rg"), taq, tpq, id, out, 
%    col, cex, lwd, lty, pch, main = NULL, xlab = NULL, ylab = NULL, 
%    xlim = NULL, axes = TRUE, alpha, file = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  dataset as a data frame object of variables and observations. 
}
  \item{y}{
  vector of identifiers (optional)
}
  \item{type}{
      Type of date format to plot: 
        
        \code{ts} timespans with endpoints
        
        \code{mp} mid points and range
        
        \code{rg} range only
}
  \item{taq}{
  timespan endpoint \emph{terminus ante quem} (TAQ)
}
  \item{tpq}{
  timespan endpoint \emph{terminus post quem} (TPQ)
}
  \item{id}{
  IDs as variable or rownames in dataset \code{x}
}
  \item{out}{
  integer or vector with number of outliers to omit (first entry id for latest date)
}
  \item{col}{
  color of \code{pch}
}
  \item{cex}{
  size of \code{pch}
}
  \item{lwd}{
  width of time interval segments
}
  \item{lty}{
  shape of time interval segments
}
  \item{pch}{
  symbol for \code{taq} and \code{tpq}
}
  \item{main}{
  plot's main tile
}
  \item{xlab}{
  plot's \code{x} label
}
  \item{ylab}{
  plot's \code{y} label
}
  \item{xlim}{
  plot's \code{x} limits
}
  \item{axes}{
  plot's axes (logical)
}
  \item{alpha}{
  alpha transparency for time interval segments
}
  \item{file}{
  path to produce a file with a PDF format (optional)
}
  \item{\dots}{
  additional optional parameters
}
}
\details{
This plot function is for time interval segments given in the dataset \code{x}, which is 
given as a dataframe or as a ``tibble'' class object.
}
\value{
A graphical plot.
}
%\references{
%}
\author{
Antonio Rivero Ostoic
}
\note{
If \code{x} is \code{NULL}, then \code{EDH} dataset is taken by default.
}
\seealso{
\code{\link{dts}}, \code{\link{get.edh}}, \code{\link{edhw}}, \code{\link{prex}}, \code{\link[tibble]{tibble}}
}
\examples{
\dontrun{
# first 100 entries in the EDH dataset (default)
EDHdates <- edhw(vars=c("not_after", "not_before"), as="df", limit=100)

# timespans
plot.dates(EDHdates, taq="not_before", tpq="not_after")}
}
\keyword{ graphs }
\keyword{ utilities }
