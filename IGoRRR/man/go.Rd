\name{go}
\alias{go}
\title{IGoR}
\usage{
go(envir=.GlobalEnv, examples=TRUE)
}
\description{
Launches IGoR.
}
\arguments{
\item{envir}{
An environment where to remember tables that will be built by the user with IGoR. Only tables in this environment will be usable by IGoR.
}
\item{examples}{
If \code{TRUE}, load three small tables usefull for testing IGoR. If same named tables exist they will be replaced.
}}
\details{
 \pkg{IGoRRR} (within the application: IGoR for I Go R), generates R code using a limited set of packages, mostly from the 'tidyverse' or inspired by the 'Mosaic' project. 
The generated code is systematically displayed and, in most cases, not activated until the user aks for. Statisticals tables and graphics are the exceptions.
The code may be copied elsewhere to help building a program that will not need IGoR to run. Copied code give some indication of needed packages when it is possible.


There are two small exceptions to the rule, two trivial shortcuts used to produce a readable code. They are both related to weighted statitics.
Their (small) R code is in the file \code{Utilities.R}.

\code{wtd.sum}

\code{wtd.percent}

\bold{Startup}

Before running IGoR, the packages used to generate code are requested to be added to loaded ones. 
These packages will still be available when exiting IGoR, in order to make any copy of the generated code still able to run.

\bold{Configuration}

Configuration files reside in the \code{text} subdirectory of the package.

The file \code{config.json} contains the description of drives used when searching a file or a place to write a file. They should be changed to places that are
meaningfull on the target system. Among them only \code{home} is mandatory but it may reference another place than the working directory.

The file \code{config.json} contains the structure of menus displayed by IGoR. They can be reordered differently and some items may be removed
but there is now provision, today, to add some. The menus names cannot be changed as they reference function names in package or entries
in the file \code{FR.json} where the actual titles are extracted.

The file \code{FR.json} contains every text that is displayed in the user's language. It contains the title of every widget displayed by Shiny and also
every text that is used to help the user in its use of IGoR. Current file is in french language and may be completed by files in some other languess
in the future, although the code todays doen't give any choice.

}

\value{
None. Returns \code{NULL} invisibly.
}

\examples{
\dontrun{
go()           # will retrieve and store tables in .GlobalEnv
 }
\dontrun{
e <- new.env() # will store tables in a specific environment
go(e)          # make some tables then leave
go(e, FALSE)   # go back to IGoR with previously made tables without refreshing example tables
 }
}
