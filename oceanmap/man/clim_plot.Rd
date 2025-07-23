\name{clim_plot}
\alias{clim_plot}
\title{
plots \code{'.gz'}-file climatologies}
\description{
Creates climatology plots of \code{'.gz'}-files. \bold{ATTENTION!} This function requires an ImageMagick installation, but runs also under Windows operating systems. 
}
\usage{
clim_plot(obj, folder,plotfolder=".", plotname, question=T, sst.frontcolor='red', 
          chla.frontcolor='blue', sidelabels = F, Ylab = F, axeslabels = T, v_area, ...)
}
\arguments{
 \item{obj}{
Character string indicating search criteria for climatology \code{'.gz'}-files. 
}
 \item{folder}{
directory where data files are located (optional).
}
 \item{plotfolder}{
directory where image should be saved.
}
 \item{plotname}{
the name of the output file. If not provided, value will be derived from \code{'.gz'}-filenames. 
}
 \item{question}{
whether the user shall be informed about the number of figures to plot before running the procedure (default is \code{TRUE}). 
}
 \item{chla.frontcolor}{
color map to be plotted for chlorophyll fronts (default is blue; obtained from \link{cmap}-dataset)
}

 \item{sst.frontcolor}{
color map to be plotted for sea surface temperature fronts (default is red; obtained from \link{cmap}-dataset)
}
\item{sidelabels}{
  whether an additional y-axis label and title should be added to the plot device (default is \code{FALSE}). If \code{TRUE}, y-axis label is defined by \code{Ylab}, the additional title is derived from the \code{date}-information and gives the month information.
}

\item{Ylab}{
  an additional title for the y axis (default is date information), only used when \code{sidelabels} is set \code{TRUE}. Default value is year-information.
}

\item{axeslabels}{
  whether axeslabels should be shown (default is \code{TRUE}, set as 'longitude' and 'latitude')
}

\item{v_area}{
  character string identifying the region that should be plotted, or in case of \code{obj == 'bathy'}, also a Raster* or Extent object. If missing, region is derived from the \code{'.gz'}-filename. 
  See \link{region_definitions} for area definitions and use \link{add.region} to add new regions.
}

 \item{...}{
Additional arguments to be passed to \link{v} and \link{plotmap} (e.g. \code{main}, \code{sidelabels}, \code{Ylab}, \code{scale_arrow}, \code{minv}, \code{maxv}, \code{adaptive.vals}, \code{cb.xlab}, \code{suffix}, \code{v_area}, \code{v_image}, \code{v_contour}, \code{v_arrows}, \code{fill}, \code{col}, \code{border}, \code{grid}, \code{grid.res}, \code{bwd}, \code{axeslabels}, \code{ticklabels}, \code{cex.lab}, \code{cex.ticks})
}
}              


\seealso{ \code{ \link{v}, \link{readbin}, \link{name_split}, \link{regions}, \link{plotmap}} } 


\author{
Robert K. Bauer
}

\examples{
## Example 1: plot seasonal '.gz'-files, following default plot-procedure
path <- system.file("test_files", package="oceanmap")
gz.file <- Sys.glob(paste0(path,'/*.gz'))[1] # load sample-'.gz'-files
check_gzfiles(folder=path) # return file summary-table
gz.files <- Sys.glob(paste0(path,'/*1s_*.gz')) # load seasonal '.gz'-files
# v(gz.files) # as single plots

## as combined climatology plot, saved in plotfolder
# clim_plot(gz.files,plotname='chla.summary.png') 
}
