\name{Cen}
\alias{Cen}
\title{
Create a Censored Object
}
\description{
Create a censored object, usually used as a response variable in a model
formula.
}
%\synopsis{Cen(obs, obs2, censored, type, origin)}
\usage{
Cen(obs, censored, type = "left")
%or
%Cen(time, time2, event, type=, origin=0)
%is.Cen(x)
}
\arguments{
\item{obs}{
    A numeric vector of observations.  This includes both censored
	and uncensored observations.
}
\item{censored}{
    A logical vector indicating TRUE where an observation in obs is
	censored (a less-than value) and FALSE otherwise.
} 
%\item{x}{
%    A \code{Cen} object constructed using the \code{Cen} function.
%}
%\item{time2}{
%ending time of the interval for interval censored  or counting process 
%data only. 
%Intervals are
%assumed to be open on the left and closed on the right, \code{(start, end]}.
%For counting process data,
%\code{event} indicates whether an event occurred at the end of the interval.
%}

\item{type}{
    character string specifying the type of censoring. Possible values are
    \code{"right"}, \code{"left"}, \code{"counting"}, \code{"interval"},
    or \code{"interval2"}.  The default is \code{"left"}.
    %or \code{"counting"} depending on whether
    %the \code{time2} argument is absent or present, respectively.
}

%\item{origin}{
%for counting process data, the hazard function origin.  This is most often
%used in conjunction with a model containing time dependent strata in order
%to align the subjects properly when they cross over from one strata to
%another.
%}

}
\value{
    An object of class \code{Cen}.  
    
%    There are methods for \code{print},
%    \code{is.na}, and subscripting censored objects.  To include a
%    censored object inside a data frame, use the \code{I()} function.
%    \code{Cen} objects are implemented as a matrix of 2 or 3 columns.

%    In the case of \code{is.Cen}, a logical value \code{T} if \code{x}
%    inherits from class \code{"Cen"}, otherwise an \code{F}.
}

\section{details}{
    This, and related routines, are front ends to routines in the
    \code{survival} package.  Since the survival routines can not handle
    left-censored data, these routines transparently handle ``flipping"
    input data and resultant calculations.  The \code{Cen} function provides
    part of the necessary framework for flipping.
    
}

%\section{DETAILS}{
%    In theory it is possible to represent interval censored data without a
%    third column containing the explicit status.  Exact, right censored,
%    left censored and interval censored observation would be represented
%    as intervals of (a,a), (a, infinity), (-infinity,b), and (a,b)
%    respectively; each specifying the interval within which the event
%    is known to have occurred.
%
%    If \code{type = "interval2"} then the representation given above is
%    assumed, with NA taking the place of infinity.  If `type="interval"
%    \code{event} must be given.  If \code{event} is \code{0}, \code{1},
%    or \code{2}, the relevant information is assumed to be contained in
%    \code{time}, the value in \code{time2} is ignored, and the second column
%    of the result will contain a placeholder.
%
%    Presently, the only methods allowing interval censored data are the
%    parametric models computed by \code{cenreg}, so the distinction
%    between open and closed intervals is unimportant.  The distinction
%    is important for counting process data and the Cox model.
%
%    The function tries to distinguish between the use of 0/1 and
%    1/2 coding for left and right censored data using \code{if
%    (max(status)==2)}.  If 1/2 coding is used and all the subjects are
%    censored, it will guess wrong.  Use 0/1 coding in this case.
%}
\references{
Helsel, Dennis R. (2005).  Nondectects and Data Analysis; Statistics for
censored environmental data.  John Wiley and Sons, USA, NJ.
}

\author{
    R. Lopaka Lee <rclee@usgs.gov>

    Dennis Helsel <dhelsel@practicalstats.com>
} 
\seealso{
    \code{\link{cenfit}},
    \code{\link{flip-methods}}
}
\examples{
    obs      = c(0.5,    0.5,   1.0,  1.5,   5.0,    10,   100)
    censored = c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)

    Cen(obs, censored)
    flip(Cen(obs, censored))
}
\keyword{survival}
