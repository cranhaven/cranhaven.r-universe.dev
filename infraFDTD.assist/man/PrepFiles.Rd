\name{PrepFiles}
\alias{PrepFiles}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate Input Files for infraFDTD
}
\description{
  infraFDTD requires text files defining the topography, atmospheric
  sound speed structure, atmospheric density structure, and station
  locations. PrepFiles generates these files from variables in R.
}
\usage{
PrepFiles(X, sta, dh, fn_topo, fn_c, fn_rho, fn_sta, c = 335, rho = 1.2,
xlim = range(X$x), ylim = range(X$y), c_function = NA, rho_function =
NA, z_extent = NaN)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
    DEM. Must include elements x (vector), y (vector), z (matrix: nrow =
    length(x), ncol = length(y))
  }
  \item{sta}{
    Station info. Must include elements x, y (vectors in meters, in
    same coordinate system as X). Station names are optionally stored in
    element sta. z is optional and should only be included if you want 
    microphones in the air. If z is missing, all stations are located on
    surface. Otherwise, NaN elevations are located on the surface, and
    non-NaN elevations are preserved. 
%%     ~~Describe \code{sta} here~~
}
  \item{dh}{
    Grid Spacing (m).
}
  \item{fn_topo}{
    Output file name for DEM.
}
  \item{fn_c}{
    Output file name for sound speed. 
}
  \item{fn_rho}{
    Output file name for density.
}
  \item{fn_sta}{
    Output file name for stations.
}
  \item{c}{
    Sound speed (m/s). If a scalar, sound speed is homogeneous in the
    model. If a vector with length equal to the number of vertical nodes
    in the model, sound speed varies vertically according to the vector
    (this is inconvenient; it's normally better to use c_function for a
    variable atmosphere). This input is ignored if c_function is provided.
}
  \item{rho}{
    Density (kg/m^3). If a scalar, density is homogeneous in the
    model. If a vector with length equal to the number of vertical nodes
    in the model, density varies vertically according to the vector
    (this is inconvenient; it's normally better to use rho_function for a
    variable atmosphere). This input is ignored if rho_function is provided.
}
  \item{xlim}{
    X limits of output DEM--must be contained within the boundaries of DEM.
}
  \item{ylim}{
    Y limits of output DEM--must be contained within the boundaries of DEM.
}
  \item{c_function}{
    Function giving sound speed as a function of height. Consider using
    approxfun() to generate this from an atmospheric profile.
}
  \item{rho_function}{
    Function giving density as a function of height. Consider using
    approxfun() to generate this from an atmospheric profile.
  }
  \item{z_extent}{
    Vertical thickness of model (m); must be greater than total relief in model.
  }

}
\details{
  This function does not generate a param.h file; however, it does print
  values that can be copied to param.h by the user.

  Consider using package rNOMADS to find the atmospheric profile.
}
\value{
  None; print and file output only.
}
\author{
  Jake Anderson
}
\examples{
## generate the DEM
data(volcano) # Maunga Whau (NZ) from built-in package "datasets"
DEM = list(x = 10 * 1:87, y = 10 * 1:61, z = volcano)

## choose the station locations
sta = list(x = c(100, 500), y = c(100, 500))

## show the topo map with stations
image(DEM, col = terrain.colors(20))
contour(DEM, add = TRUE)
points(sta, pch = 25, bg = 1)

## make the input files for infraFDTD, using the default homogeneous
## sound speed and density
PrepFiles(X = DEM, sta = sta, dh = 10, fn_topo = 'topo.txt', fn_c =
'c.txt', fn_rho = 'rho.txt', fn_sta = 'sta.txt')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }% use one of  RShowDoc("KEYWORDS")
\keyword{ file }% __ONLY ONE__ keyword per line
