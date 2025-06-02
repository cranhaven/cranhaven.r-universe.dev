\name{compare_models}
\alias{compare_models}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Compare different models fitted by est_multi_poly}

\description{Given different outputs provided by est_multi_poly, the function compare the
	different models providing a unified table.}

\usage{compare_models(out1, out2, out3=NULL, out4=NULL, out5=NULL,
               nested=FALSE)}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{out1}{output from the 1st fitting}
  \item{out2}{output from the 2nd fitting}
  \item{out3}{output from the 3rd fitting}
  \item{out4}{output from the 4th fitting}
  \item{out5}{output from the 5th fitting}
  \item{nested}{to compare each model with the first in terms of LR test}
  }

%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}

\value{
\item{table}{table summarizing the comparison between the models}
}

\author{Francesco Bartolucci - University of Perugia (IT)}

%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}