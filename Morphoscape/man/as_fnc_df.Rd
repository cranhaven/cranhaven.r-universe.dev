\name{as_fnc_df}
\alias{as_fnc_df}
\alias{fnc_df}

\title{
Convert a data frame to a \code{fnc_df}
}
\description{
\code{as_fnc_df()} converts a data frame containing coordinates and functional charactertics in a morphological space  to a \code{fnc_df} object for use in later funcitons, most importantly \code{\link{krige_surf}}.
}
\usage{
as_fnc_df(x, func.names = NULL, scale = TRUE)
}
\arguments{
  \item{x}{
a data frame containing coordinates and functional characteristics (and possibly other variables, which are ignored). The first two columns must correspond to the x and y coordinates of the warps in morphological space.
}
  \item{func.names}{
the names of the variables in \code{x} the correspond to functional characteristics. These charcteristics must be numeric variables. If \code{NULL} (the default), all variables other than the first two will be taken to be the functional characteristics under study.
}
  \item{scale}{
whether to scale the functional characteristics to have a minimum of 0 and a maximum of 1. This should generally be left at its default (\code{TRUE}) unless the variables have already been scaled.
}
}

\details{
Input data can be from a sampled grid of locations in morphospace, measured specimen data, species or group means, or a mix. 
}

\value{
A \code{fnc_df} object, which is a \code{data.frame} with the x and y coordinates in the first two columns and the functional characteristics in the other columns. The \code{"func.names"} \link[=attributes]{attribute} contains the names of the functional characteristics.
}

\seealso{
\code{\link{as.data.frame}}

\code{\link{krige_surf}} for using an \code{fnc_df} object to create a kriged surface.
}
\examples{
data("warps")

warps_fnc <- as_fnc_df(warps, 
                       func.names = c("hydro", "curve",
                                       "mech", "fea"))
                                       
str(warps_fnc)
}