\name{lscp_operations}
\alias{sum_lscps}
\alias{sub_lscps}
\alias{div_lscps}
\alias{mult_lscps}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simple Operations on Spatial Data
}
\description{
Perform simple operations (sum, sub, mult, div) on one or multiple landscapes (see details for use cases)

}
\usage{
sum_lscps(lscps, num = NULL, average = TRUE)
mult_lscps(lscps, num = NULL)
sub_lscps(lscps, binary = FALSE)
div_lscps(lscps, binary = FALSE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lscps}{
  A named list containing datasets of spatial data to apply operations (see details). }
  \item{num}{
  Optional. Defaults to NULL. A vector containing a single numeric scalar or numeric vector with length = lscps to scale by when using \code{sum_lscps} and \code{mult_lscps()}. If NULL, these functions will operate between lscps. If num is provided, these functions will operate between lscps and num.  
  }
  \item{binary}{
  If subracting or dividing landscapes, binarize result to obtain logical [0,1] result (see details for use case)
  }
  \item{average}{
  if summation is performed, should the result be averaged for the number of landscapes 
  }
}
\details{
Simple operations are applied to one or more landscapes depending on use case. Spatial datasets can be supplied in an XYZ dataframe, or as any landscape output from Morphoscape. 

sum - sum a single spatial dataset/landscape with a single scalar (\code{lscp1 + num1}); sum two or more spatial datasets/landscapes together (\code{lscp1 + lscp2 ... lscpN}); or sum multiple landscapes with multiple scalars (\code{(lscp1, lscp2 ... lscpN) + (num1, num2, ..., numN)})

mult - multiply a single spatial dataset/landscape with a single scalar (\code{lscp1 * num1}); multiply two or more spatial datasets/landscapes together (\code{lscp1 * lscp2 ... lscpN}); or multiply multiple landscapes with multiple scalars (\code{(lscp1, lscp2 ... lscpN) * (num1, num2, ..., numN)})

sub - substract one spatial dataset/landscape from another (\code{lscp1 - lscp2}). If numeric subtraction is desired, use sum_lscps with negative \code{num} values.

div - divide one spatial dataset by another (\code{lscp1 / lscp2}).

\code{sub_lscps()} and \code{div_lscps} can be used to construct transition landscapes per (Dickson et al 2020) which can compare performance between two adaptive regimes. If binary = T is used, the result will be a spatial representation of which parent landscape dominates. However, it is recommended to use \code{trans_lscps} or \code{adpt_regions} to calculate transtional landscapes or adaptive regions (not yet implemented).

}
\value{
An object of class "combined.surface" containing XYZ spatial data.
}
\author{
Blake V. Dickson}

\seealso{
\code{\link{krige_surf}}, \code{\link{calcGrpWprime}}, \code{\link{calc_lscp}}, \code{\link{calcPoly}}, \code{trans_lscps}, \code{adpt_regions}
}
\examples{
require("Morphoscape")

data("turtles")
data("warps")

fnc_df <- as_fnc_df(warps, 
                       func.names = c("hydro", "curve", "mech", "fea"))

kr_surf <- krige_surf(fnc_df, new_data = turtles)

grid_weights <- generate_weights(n = 10, data = kr_surf)

all_lscps <- calc_all_lscps(kr_surf,
                            grid_weights = grid_weights)

wprime_S <- calcGrpWprime(all_lscps,
                          index = Ecology == "S")

wprime_T <- calcGrpWprime(all_lscps,
                          index = Ecology == "T")
                          
lscps <- list(wprimeS = wprime_S, wprime_T = wprime_T)
                       
# summing multiple landscapes together

summed_surfs <- sum_lscps(lscps, average = TRUE)

# summing landscapes by one or more numeric scalars

summed_surfs <- sum_lscps(lscps, num = c(1.5, -1.15)) # multiple numeric, with subtraction

# multiplying mutliple landscapes together
mult_surfs <- mult_lscps(lscps) # multiply landscapes together

# multiplying landscapes by one or more numeric scalars
mult_surfs <- mult_lscps(lscps, num = 2) # apply numeric multiplier to all landscapes
mult_surfs <- mult_lscps(lscps, num = c(1.2, 0.8)) # apply numeric elements to each landscape

# substract or divide two landscapes

sub_surf <- sub_lscps(lscps)
div_surf <- div_lscps(lscps)

# with binary result

sub_surf <- sub_lscps(lscps, binary = TRUE)
div_surf <- div_lscps(lscps, binary = TRUE)

}

