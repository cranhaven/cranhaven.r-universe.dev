\name{figure}
\alias{figure}
\title{
generate (and save) graphic devices with flexible fileformat selection
}
\description{
\code{figure} generates graphic devices with flexible fileformat selection.
\bold{Function call with (figure(do.save=T) needs to be finished by close_fig(do.save=T), to close open file connection.}
}

\usage{
figure(filename, folder, type, save=F, do.save=save, 
       width=10, height=10, xpos=-1, do.overwrite=T, delete.old=do.overwrite, ...)
}
\arguments{
  \item{filename}{
  name of the figure to be generated (without file extension)
}
  \item{folder}{
  plot folder (by default current working directory) 
}
  \item{type}{
  character string indicating the graphics format of the figure file. can be:
  \itemize{
  \item{"jpg"}
  \item{"jpeg"}
  \item{"png"}
  \item{"tiff"}
  \item{"eps"}
  \item{"pdf"}
  } 
}
  \item{width, height}{
 width and height of figure to be generated. default units are inches.
 }
 \item{save, do.save}{
 whether file should be saved or not (default is TRUE). 
 if FALSE, new graphic device will be opened inside R.
 }
 \item{xpos}{
 horizontal screen position of graphic device (ignored if do.save == TRUE)
 }
 \item{do.overwrite, delete.old}{
 overwrite existing figure with same filename and extension (default is FALSE)
 }
  \item{\dots}{
additional arguments to be passed to the graphic device
}
}

\seealso{ \code{\link{close_fig}}}

\author{
Robert K. Bauer
}


\examples{
## Example 1: plotmap() and figure()
do.save <- FALSE
figure("Gulf_of_Lions_extended", do.save=do.save, width=5, height=5, type="pdf")
plotmap("lion")
close_fig(do.save)

## now resize figure manually and get new figure dimensions:
width <- dev.size()[1]
height <- dev.size()[2]

# do.save <- TRUE
# figure("Gulf_of_Lions_extended", do.save=do.save, width=width, height=height, type="pdf")
# plotmap("lion")
# close_fig(do.save)




}