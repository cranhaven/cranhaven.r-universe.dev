\name{seir_control}
\alias{seir_control}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    SEIR Parameters Control Data Structure
}
\description{
    The SEIR model uses parameters which are specific to the SEIR model. This data structure affirms that required parameters are supplied, provides default values for optional parameters, and validates the data of each parameter.
}
\usage{
    seir_control(
        input_N_pops=NULL,
        input_S_pops=NULL,
        input_E_pops=NULL,
        input_I_pops=NULL,
        input_R_pops=NULL,
        birth=1/(75*365),
        incubate=1/8.0,
        recov=1/3.0
    )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{input_N_pops}{
        Integer Vector representing the total population for each county.
    }
    \item{input_S_pops}{
        Integer Vector representing the susceptible population for each county.
    }
    \item{input_E_pops}{
        Integer Vector representing the exposed population for each county.
    }
    \item{input_I_pops}{
        Integer Vector representing the infected population for each county.
    }
    \item{input_R_pops}{
        Integer Vector representing the recovered population for each county.
    }
    \item{birth}{
        Default value is 1/(75*365). Must be greater than or equal to zero. Values above one are unusual.
    }
    \item{incubate}{
        Default value is 1/8.0. Must be greater than or equal to zero. Values above one are unusual.
    }
    \item{recov}{
        Default value is 1/3.0. Must be greater than or equal to zero. Values above one are unusual.
    }
}
\details{
    Defines a set of parameters specific to the SEIR model. If an optional parameter is not set, it will use the default value as specified above. If a parameter is outside the specified limits, execution will stop and an error message will be displayed. Some parameters may have values greater than one. While these situations may be unusual, execution will not stop but a warning message will be displayed.

    Note: At least one of \code{input_N_pops} or \code{input_S_pops} must be supplied. If one is not supplied, it will be calculated within the class.

    Note: At least one of \code{input_E_pops} and \code{input_I_pops} must be supplied with a nonzero population. If either of these population parameters or \code{input_R_pops} is not supplied, it will be assumed to be a vector of zeroes.
}
\value{
    Returns a named list of vectors that must be supplied to \code{\link{model_interface}}.
}
\author{
Seth Borkovec, \email{stb224@nau.edu}; Joseph Mihaljevic, \email{joseph.mihaljevic@nau.edu}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
    \code{\link{covid19_control}}
}
\examples{

## Data set for the examples:
S_pops <- rep(100000, 10)
E_pops <- c(0, 1, 0, 3, 2, 0, 13, 3, 0, 0)
I_pops <- c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0)
R_pops <- rep(0, 10)
N_pops <- S_pops + E_pops + I_pops + R_pops

## Example using the default parameters:
seir_control <- seir_control(input_S_pops = S_pops,
                             input_E_pops = E_pops,
                             input_I_pops = I_pops)

## Example specifying one optional parameter:
seir_control <- seir_control(input_N_pops = N_pops,
                             input_I_pops = I_pops,
                             input_R_pops = R_pops,
                             recov          = 1/4.0)

## Example specifying all possible parameters:
seir_control <- seir_control(input_N_pops = N_pops,
                             input_S_pops = S_pops,
                             input_E_pops = E_pops,
                             input_I_pops = I_pops,
                             input_R_pops = R_pops,
                             birth          = 1/(65*365),
                             incubate       = 0.12,
                             recov          = 0.25)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{  }% use one of  RShowDoc("KEYWORDS")
