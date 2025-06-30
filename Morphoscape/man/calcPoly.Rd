\name{calcPoly}
\alias{calcPoly}
\alias{multiPoly}

\title{
Calculate polynomial fits over a surface
}
\description{
\code{calcPoly} calls on the \code{spatial} package to fit rectangular spatial polynomial surface models by least-squares, or GLS. These methods allow the user to test whether data have spatial trends in morphospace. Outputs are a polynomial trend surface, and ANOVA table for the model fit. \code{multiPoly} applies \code{calcPoly} to a \code{fnc_df} with outputs for each trait. For more extensive documentation for model fitting see the \code{spatial} package. 
}
\usage{
calcPoly(fnc, npoly = 3, fnc.name = NULL, 
         gls.covmod = list(covmod = expcov, d = 0.7, alpha = 0, se = 1), 
         pad = 1.2, resample = 100, range = NULL, verbose = FALSE)
                     
multiPoly(fnc_df, npoly = 3, ...)                     
}

\arguments{
  \item{fnc}{
  an XYZ dataframe or matrix of a spatially distributed trait.  
  }
  \item{fnc_df}{
  a functional dataframe from \code{as_fnc_df} with colnames corresponding to X,Y and trait names.
  }
  \item{npoly}{
  singular numeric. Degree of polynomial to fit ragning from 1-4. For \code{multiPoly} this can also be a vector with length equal to the numer of traits in order to specify the degree of polynomial to apply to each trait.
  }
  \item{gls.covmod}{
  Optional list of arguments to pass to \code{surf.gls} if fitting by generalized least-squares is desired. Defaults to NULL, and fitting is performed by least-squares. See \code{\link{surf.gls}} and \code{\link{expcov}} documentation for a full list of arguments and usage.
  }
  \item{fnc.name}{
  Optional speficiation of the trait name. Defaults to \code{NULL}, and will use column names instead.
  }
  \item{pad}{
  Degree by which to extrapolate input data. Defaults to 1.2.
  }
  \item{resample}{
  Resampling density. Corresponds to the number of points calculated along both X and   Y axes. Defaults to 100. If no resampling is desired, set \code{reample = NULL}
  }
  \item{range}{
  Optional. Manually set X and Y ranges. Input is a 2x2 matrix with rows corresponding to X and Y    ranges respectively.
  }
  \item{verbose}{
  Optional. Logical. If \code{TRUE}, will print ANOVA tables.
  }
  \item{...}{
  Arguments to pass onto \code{calcPoly} when using \code{multiPoly}
  }
}

\details{
Fits polynomial trend surfaces using the `spatial` package. First, an \code{npoly} polynomial trend surface is fit by least squares using \code{\link{surf.ls}} or generalized least-squares by \code{\link{surf.gls}}. GLS is fit by one of three covariance functions, exponential (\code{expcov}), gaussian (\code{gaucov}) or spherical (\code{sphercov}) and requires additional parameters to be passed as a list through \code{gls.covmod} (see examples). For a full description of arguments and usage see \code{\link{surf.gls}} and \code{\link{expcov}} documentation.

The surface is then evaluated using \code{\link{trmat}} within limits set by input data, or manually using \code{range}. 
}
\value{
An object of class \code{poly_surf}, or \code{multi_surf} with the following components:
\item{fnc.name}{
name of trait
}
\item{poly}{Polynomial trend fit output from \code{\link{surf.ls}}}
\item{surface}{Evaluated trend surface output from \code{\link{trmat}}}
\item{grid}{Expanded surface in long XZY dataframe format}
\item{peak}{Coordinates and height of the peak of the surface}
}
\references{
Dickson, B.V. and Pierce, S.E. (2019), Functional performance of turtle humerus shape across an ecological adaptive landscape. Evolution, 73: 1265-1277. https://doi.org/10.1111/evo.13747
}
\author{
Blake V. Dickson}


\seealso{
\code{\link{surf.ls}}, 
\code{\link{surf.gls}}, 
\code{\link{expcov}},
\code{\link{trmat}},
}
\examples{
require(spatial)

data("warps")
warps_fnc <- as_fnc_df(warps)

# Make single trait dataframe 
hydro_fnc <- data.frame(warps_fnc[ ,1:2], warps_fnc[ ,"hydro"])

polysurf <- calcPoly(hydro_fnc)
summary(polysurf)
plot(polysurf)

# Fit using gls

polysurf <- calcPoly(hydro_fnc, gls.covmod = list(covmod = expcov, d = 0.7, alpha = 0, se = 1))
\dontrun{
summary(polysurf)
}
plot(polysurf)

# Calculate multiple polynomial surfaces

multi_poly <- multiPoly(warps_fnc)
\dontrun{
summary(multi_poly)
}
plot(multi_poly)

# Set manual range

polysurf <- calcPoly(hydro_fnc, range = rbind(range(warps_fnc$x) * 1.2,
                                              range(warps_fnc$y) * 1.4))
                                              
polysurf <- calcPoly(hydro_fnc, range = rbind(c(-0.2, 0.12),
                                              c(-0.06, 0.1)) )
\dontrun{
summary(polysurf)
}                                              
# 
# Adjust polynomial degree

multiPoly(warps_fnc, npoly = 2)

# Specify multiple degrees

multi_poly <- multiPoly(warps_fnc, npoly = c(2,3,4,3))

\dontrun{
summary(polysurf)
}
plot(polysurf)

}

