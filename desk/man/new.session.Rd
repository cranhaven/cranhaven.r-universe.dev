\name{new.session}
\alias{new.session}
\title{R Session Reset}
\usage{
new.session(cd = TRUE, sci = FALSE)
}
\arguments{
\item{cd}{if cd = FALSE, the working directory is not be changend. The default, cd = TRUE, sets the working directory to source file loction.}
\item{sci}{if sci = TRUE, the scientific notation is reset to the R standard option.}
}
\value{
None.
}
\description{
\code{new.session} removes all objects from global environment, removes all plots, clears the console, and restores parameter settings. As default, sets the working directory to source file loction in case the function is used from an R script. As an option, resets the scientific notation (e.g., 1e-04).
}
\examples{
# No example available to avoid possibly unwanted object deletion in user environment.
}
\concept{.GlobalEnv}
\concept{delete}
\concept{environment}
\concept{objects}
\concept{remove}
