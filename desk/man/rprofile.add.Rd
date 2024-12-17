\name{rprofile.add}
\alias{rprofile.add}
\title{Add a Command to User R Startup File Rprofile.site}
\usage{
rprofile.add(line)
}
\arguments{
\item{line}{a text string specifying the command to be added.}
}
\value{
None.
}
\description{
Adds a specified R command to file "Rprofile.site" for automatic execution during startup.
}
\examples{
if (FALSE) rprofile.add("library(desk)") # Makes package desk to be loaded at startup

}
\concept{R}
\concept{Rprofile.site}
\concept{configuration}
\concept{startup}
