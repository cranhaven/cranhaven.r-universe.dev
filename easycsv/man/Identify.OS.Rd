\name{Identify.OS}
\alias{Identify.OS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Returns Operating System
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
returns one obejct which identifies if the OS is supported for use of \link[easycsv]{choose_dir}.
}
\usage{
  Identify.OS()
}
%- maybe also 'usage' for other objects documented here.

\details{
%%  ~~ If necessary, more details than the description above ~~
Internal function of \link[easycsv]{choose_dir}, can be used to identify only one of its' supported operating systems.
}
\value{
A character object.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\link[easycsv]{choose_dir}, \link[base]{.Platform}, \link[base]{Sys.info}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~utilities }% use one of  RShowDoc("KEYWORDS")
