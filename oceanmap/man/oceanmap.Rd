\name{oceanmap}
\alias{oceanmap}
\alias{oceanmap-package}
\docType{package}
\title{
oceanmap - plot tools for 2D oceanographic data  
}
\description{
\code{oceanmap} is a plotting toolbox for oceanographic data.
Visualizing data is a crucial step in analyzing and exploring data. During the last two decades the statistical programming language R has become a major tool for data analyses and visualization across different fields of science. However, creating figures ready for scientific publication can be a tricky and time consuming task. 

The \code{oceanmap} package provides some helpful functions to facilitate and optimize the visualization of geographic and oceanographic data, such as satellite and bathymetric data sets. Its plotting functions are written in a way that they do not require a large amount of their numerous arguments to be specified but still return nice plots. Its major functions are:

\bold{Major functions:}\cr
\itemize{
\item{\link{plotmap}: plots landmask as basis or overlay}
\item{\link{v}: plots oceanographic data (fronts, SST, chla, bathymetry, etc.) from \link{raster}-objects, ncdf4- or gz-files}
\item{\link{set.colorbar}: adds a colorbar to current figure, allowing several placement methods}
\item{\link{get.bathy}: download bahymetric data at user defined resolution from the NOAA ETOPO1 database}
\item{\link{add.region}: generate region definitions to facilitate land mask and colorbar plotting using \link{plotmap} and \link{v}}
\item{\link{figure} & \link{close_fig}: generate and save graphic devices in flexible file formats (jpeg, png, eps, pdf and eps)}

}

\bold{Getting Started}\cr
Check out some examples of the principle functions, listed above.

}


\author{
Robert K. Bauer
}
