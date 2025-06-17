\name{nparACT_base}
\alias{nparACT_base}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Classic Nonparametric Actigraphy Measures for Single Actigraphy Files
}
\description{
Function returns IS, IV, RA, L5, M10 as well as L5 and M10 start times for single actigraphy files. The function also returns a classic dual day plot for the data, a plot of minutewise averages across 24 hours as well as a plot for hourly averages across 24 hours. 
}
\usage{
nparACT_base(name, SR, cutoff = 1, plot = T, fulldays = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{
Character string containing the actigraphy file name. Actigraphy files must contain a first column with a date/time vector, which must be of form \code{YYYY-MM-DD HH:MM:SS} or \code{YYYY/MM/DD HH:MM:SS}, i.e. in an unambiguous date format and a second column with actigraphy values.
}
  \item{SR}{
Sampling rate in Hz.
}
  \item{cutoff}{
Can be used to define a cutoff for actigraphy data. Default value is \code{1}, i.e. all values are taken into account.
}
  \item{plot}{
If \code{TRUE} plots are produced. Default is \code{TRUE}.
}
  \item{fulldays}{
If \code{TRUE} only data from multiples of 24 hours (i.e. full days) are included while the rest of the data are discarded. Default is \code{TRUE}.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Data frame containing IS, IV and RA values. Besides this, L5 and M10 values are given along with the respective start times.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Christine Blume <christine.blume@sbg.ac.at>
Nayantara Santhi <n.santhi@surrey.ac.uk>
Manuel Schabus <manuel.schabus@sbg.ac.at>
}
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line