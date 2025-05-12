\name{choose_dir}
\alias{choose_dir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Choose a Folder Interactively
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Use a folder widget to choose a folder interactively.
}
\usage{
choose_dir()
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
  This brings up folder selection widget, it requires no arguments and is implemented into \link[easycsv]{fread_folder} & \link[easycsv]{loadcsv_multi} as the default if no directory is supplied. Currently works only on mac OS,windows and Linux.
    for the windows implementation and further detail see \href{https://www.rdocumentation.org/packages/utils/versions/3.4.1/topics/choose.dir}{choose.dir(remote url)}.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A length-one character vector, character NA if `Cancel` was selected.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\href{https://www.rdocumentation.org/packages/utils/versions/3.4.1/topics/choose.dir}{choose.dir(remote url)}, \link[easycsv]{Identify.OS}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~connection }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~utilities }% __ONLY ONE__ keyword per line
