\name{time_windows}
\alias{time_windows}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Time Windows Data Structure
}
\description{
    The SPARSEMODr models allow for users to dynamically update transmission rates and movement dynamics across the course of the outbreak. These time-varying parameter values must then be compiled into a \code{time_windows} object.

    A \code{time_windows} object is a set of data across multiple vectors or lists including time-varying transmission rate (\code{beta}); a parameter that helps define the range of movement; a parameter that defines the frequency of movement between focal populations; a parameter that constrains the impact of hosts that immigrate from outside of the focal populations; and a method to define the dates over which these parameters fluctuate. 

    When specifying dates for each entry, there are three options, but only one of which may be used. See details below.
    \enumerate{
        \item{Providing a vector for \code{window_length},}
        \item{Providing a vector each for \code{start_dates} and \code{end_dates},}
        \item{Providing a vector for \code{daily}.}
    }

}
\usage{
    time_windows(
        beta=NULL,
        dist_phi=NULL,
        m=NULL,
        imm_frac=NULL,
        hosp_rate=NULL,
        recov_hosp=NULL,
        icu_rate=NULL,
        death_rate=NULL,
        window_length=NULL,
        start_dates=NULL,
        end_dates=NULL,
        daily=NULL,
        r0=NULL
    )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{beta}{
        \emph{required} - \strong{New in version 1.2.0}: A numeric vector of the time-varying transmission rate. If a single vector is provided, the same transmission rate is used in all populations. You can instead provide a list of transmission rate vectors--one for each population. The number of populations must be equal to those used in the \code{\link{covid19_control}} or \code{\link{seir_control}}.
    }
    \item{dist_phi}{
        \emph{required} - A numeric vector of the \code{dist_phi} that is used to calculate the dispersal kernal (see details below).
    }
    \item{m}{
        \emph{required} - A numeric vector of the \code{m} parameter. The inverse of \code{m} is the average time between individuals moving away from their focal population (see details below).
    }
    \item{imm_frac}{
        \emph{required} - A numeric vector. This parameter corresponds to the fraction of the focal population (between 0 and 1) that may be comprised of immigrants from outside of the system (i.e., immigrants that are not from any of the supplied populations in \code{input_pop_N} from \code{\link{model_interface}}); see details below.
    }
    \item{hosp_rate}{
        A numeric vector. Proportion of symptomatic individuals entering hospitalization. Default value is 0.175. Must be greater than zero and less than or equal to one.
    }
    \item{recov_hosp}{
        A numeric vector. Recovery rate of hospitalized individuals. Default value is 1/7.0. Must be greater than or equal to zero. Values above one are unusual.
    }
    \item{icu_rate}{
        A numeric vector. Proportion of hospitalized individuals entering ICU. Default value is 0.20. Must be greater than zero and less than or equal to one.
    }
    \item{death_rate}{
        A numeric vector. Proportion of individuals who do not recover in ICU. Default value is 0.60. Must be greater than zero and less than or equal to one.
    }
    \item{window_length}{
        An integer vector supplying the number of days in each time window (see details below).
    }
    \item{start_dates}{
        A vector of Date objects that corresponds to the starting date of each time window. If supplied, \code{end_dates} must also be supplied (see details below).
    }
    \item{end_dates}{
        A vector of Date objects that corresponds to the ending dates of each time window. If supplied \code{start_dates} must also be supplied (see details below).
    }
    \item{daily}{
        A vector of Date objects that is sequential and complete, encompassing all dates from the start of the outbreak to the end of the outbreak (see details below).
    }
    \item{r0}{
        No longer supported. Gives error message to provide beta instead.
    }
}
\details{

    See \code{\link{Movement}} for descriptions of \code{m} and \code{dist_phi}.

    Defining time window durations. One of the following options is required to define the duration of each time window: \code{window_length}, or \code{start_dates} AND \code{end_dates}, or \code{daily}.

    Use \code{window_length} when you want to specify the length of each time window by the number of days.

    Use \code{start_dates} AND \code{end_dates} when you want to define a time window by its starting and ending dates. A start date may not overlap with an end date, and there can be no gaps between the end date and the subsequent start date.

    Use \code{daily} when you want to update parameters every day of the simulation. In this mode, each time window has a length of one day.
}
\value{
    Returns a named list of vectors that must be supplied to \code{\link{model_interface}}.
}
\author{
Seth Borkovec, \email{stb224@nau.edu}; Joseph Mihaljevic, \email{joseph.mihaljevic@nau.edu}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
    \code{\link{covid19_control}}, \code{\link{seir_control}}
}
\examples{

## Data set for the examples: (All examples include 5 time windows)
input_beta <-           c(  0.30,  0.10,  0.15,  0.15,  0.20)
input_dist_phi <-       c(   200,   200,    20,   150,   150)
input_m <-              c( 0.002, 0.002, 0.002,  0.02,  0.02)
input_imm_frac <-       c(   0.0,   0.0,   0.0,  0.02,  0.02)
input_window_length <-  c(    10,    35,    46,    81,    40)
input_start_dates <-    c(seq(as.Date("2020-07-09"), by=10, len=5))
input_end_dates <-      c(seq(as.Date("2020-07-18"), by=10, len=5))
input_daily <-          c(seq(as.Date("2020-07-09"), by=1, len=5))

## Example using window_length:
### input_window_length defines the number of days
### that each value of the other parameters is repeated.
tw <- time_windows(beta          = input_beta,
                   dist_phi    = input_dist_phi,
                   m             = input_m,
                   imm_frac      = input_imm_frac,
                   window_length = input_window_length)

## Example using start_dates with end_dates:
### Five time windows, each with 10 days
tw <- time_windows(beta         = input_beta,
                   dist_phi   = input_dist_phi,
                   m            = input_m,
                   imm_frac     = input_imm_frac,
                   start_dates  = input_start_dates,
                   end_dates    = input_end_dates)

## Example using daily:
### Parameters are updated daily over 5 days
tw <- time_windows(beta       = input_beta,
                   dist_phi = input_dist_phi,
                   m          = input_m,
                   imm_frac   = input_imm_frac,
                   daily      = input_daily)

## Example with different beta vectors for different populations:
### n_pops should be the total number of populations as used in covid19_control or seir_control
n_pops <- 4
input_beta_list <- vector("list", length = n_pops)
input_beta_list[[1]]      <- c( 0.30,  0.10,  0.10,  0.15,  0.20)
input_beta_list[[2]]      <- c( 0.15,  0.08,  0.15,  0.10,  0.15)
input_beta_list[[3]]      <- c( 0.20,  0.08,  0.10,  0.10,  0.25)
input_beta_list[[n_pops]] <- c( 0.25,  0.12,  0.08,  0.12,  0.10)

tw <- time_windows(beta          = input_beta_list,
                   dist_phi    = input_dist_phi,
                   m             = input_m,
                   imm_frac      = input_imm_frac,
                   window_length = input_window_length)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{  }% use one of  RShowDoc("KEYWORDS")
