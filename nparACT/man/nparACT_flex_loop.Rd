\name{nparACT_flex_loop}
\alias{nparACT_flex_loop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extended Nonparametric Actigraphy Measures for Multiple Actigraphy Files
}
\description{
Function returns IS, IV, RA, L5, M10 as well as L5 and M10 start times for single actigraphy files. Additionally, it also returns the L value (Lflex) and the start time for a specified number of minutes. The function also returns a classic dual day plot for the data, a plot of minutewise averages across 24 hours as well as a plot for hourly averages across 24 hours. 
}
\usage{
nparACT_flex_loop(path, SR, cutoff = 1, minutes, plot = T, fulldays = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
Character string containing path to location of actigraphy files. Actigraphy files must contain a first column with a date/time vector, which must be of form \code{YYYY-MM-DD HH:MM:SS} or \code{YYYY/MM/DD HH:MM:SS}, i.e. in an unambiguous date format and a second column with numeric actigraphy values. Input files can either be text or comma separated value files.
}
  \item{SR}{
Sampling rate in Hz.
}
  \item{cutoff}{
Can be used to define a cutoff for actigraphy data. Default value is \code{1}, i.e. all values are taken into account.
}
  \item{minutes}{
Number of minutes for which the Lflex value should be computed.
}
  \item{plot}{
If \code{TRUE} a plot is produced. Default is \code{TRUE}.
}
  \item{fulldays}{
If \code{TRUE} only data from multiples of 24 hours (i.e. full days) are included while the rest of the data are discarded. Default is \code{TRUE}.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Data frame containing IS, IV and RA values for each file. Besides this, L5, M10 and Lflex (for the number of minutes specified) values are given along with the respective start times.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Christine Blume <christine.blume@sbg.ac.at>
Nayantara Santhi <n.santhi@surrey.ac.uk>
Manuel Schabus <manuel.schabus@sbg.ac.at>
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
# example function nparACT_flex_loop
data(sleepstudy)
name <- "sleepstudy_example"
newdir <- file.path(tempdir(),name)
dir.create(newdir, showWarnings = FALSE)
olddir <- setwd(newdir)
write.table(sleepstudy, file = "sleepstudy.txt", row.names=FALSE, col.names = FALSE)
r <- nparACT_flex_loop(newdir, SR = 4/60, minutes = 435)
setwd(olddir)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line