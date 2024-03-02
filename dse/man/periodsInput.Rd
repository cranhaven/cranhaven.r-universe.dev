\name{TobsInput}
\alias{TobsInput}
\alias{TobsInput.TSdata}
\alias{TobsInput.TSestModel}

\alias{TobsOutput}
\alias{TobsOutput.TSdata}
\alias{TobsOutput.TSestModel}

\alias{startInput}
\alias{startInput.TSdata}
\alias{startInput.TSestModel}

\alias{startOutput}
\alias{startOutput.TSdata}
\alias{startOutput.TSestModel}

\alias{endInput}
\alias{endInput.TSdata}
\alias{endInput.TSestModel}

\alias{endOutput}
\alias{endOutput.TSdata}
\alias{endOutput.TSestModel}

\alias{frequencyInput}
\alias{frequencyInput.TSdata}
\alias{frequencyInput.TSestModel}

\alias{frequencyOutput}
\alias{frequencyOutput.TSdata}
\alias{frequencyOutput.TSestModel}

\title{TSdata Periods}
\description{Apply a method to the input or output data.}
\usage{
    TobsInput(x)
    \method{TobsInput}{TSdata}(x)
    \method{TobsInput}{TSestModel}(x)

    TobsOutput(x)
    \method{TobsOutput}{TSdata}(x)
    \method{TobsOutput}{TSestModel}(x)

    startInput(x)
    \method{startInput}{TSdata}(x)
    \method{startInput}{TSestModel}(x)

    startOutput(x)
    \method{startOutput}{TSdata}(x)
    \method{startOutput}{TSestModel}(x)

    endInput(x)
    \method{endInput}{TSdata}(x)
    \method{endInput}{TSestModel}(x)

    endOutput(x)
    \method{endOutput}{TSdata}(x)
    \method{endOutput}{TSestModel}(x)

    frequencyInput(x)
    \method{frequencyInput}{TSdata}(x)
    \method{frequencyInput}{TSestModel}(x)

    frequencyOutput(x)
    \method{frequencyOutput}{TSdata}(x)
    \method{frequencyOutput}{TSestModel}(x)

}
\arguments{
    \item{x}{An object containing TSdata.}
}
\value{Depends.}
\details{
    Apply a method to the input or output data so, for example,
    TobsInput(x) in theory does Tobs(inputData(x)), which 
    returns the number of observation periods in input data. The actual implementation
    may not do Tobs(inputData(x)). For example, with TSPADIdata 
    inputData(x) requires a database retrieval which may be fairly slow,
    while the number of periods may be available much more quickly.
}

\examples{
    data("eg1.DSE.data.diff", package="dse")
    TobsOutput(eg1.DSE.data.diff)
}
\concept{DSE}
\keyword{ts}

