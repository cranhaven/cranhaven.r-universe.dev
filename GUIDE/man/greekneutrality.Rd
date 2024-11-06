\name{greekneutrality}
\alias{greekneutrality}
\title{
Calculate the hedge positions for achieving greek(s) neutrality for European Call/Put.
}
\description{
Function to calculate the hedge positions for achieving greek(s) neutrality for European Call/Put.
}
\usage{
greekneutrality()
}
\details{
The user inputs are as follows:\cr
Positions: entered with comma separation in case of multiple options. Short positions are entered with a '-' sign prefixed. e.g. -1000, -500, -2000, -500 \cr
Deltas: entered with comma separation in case of multiple options. e.g. 0.5, 0.8, -0.4, 0.7\cr
Gammas: entered with comma separation in case of multiple options. e.g. 2.2, 0.6, 1.3, 1.8\cr
Vegas: entered with comma separation in case of multiple options. e.g. 1.8, 0.2, 0.7, 1.4 \cr
Type of Neutrality desired: chosen amonst Delta, Delta and Gamma, Delta and Vega, Delta Gamma and Vega \cr
Delta, Gamma, Vega of traded option 1: entered with comma separation in case of multiple options. e.g. 0.6, 1.5, 0.8\cr
Delta, Gamma, Vega of traded option 2:entered with comma separation in case of multiple options. e.g. 0.1, 0.5, 0.6  \cr
}


\value{
Positions in the underlying or traded option(s) to achieve the desired greek neutrality
}

\references{
John C. Hull, "Options, Futures, and Other Derivatives", 8/E, Prentice Hall, 2012. 
}
\author{
S Subramanian <ssubramanian@sssihl.edu.in>
}

\seealso{
\code{\link[GUIDE]{calcgreeks}}
}

