\encoding{utf-8}
\name{PlotVariations}
\alias{PlotVariations}
\title{
Plot shapes
}
\description{
Plot the differences between a set of morphometric shapes and a reference 
}
\usage{
PlotVariations(shapes, reference, path, save = F, w = 6, h = 6, d = 100, c_ref = "red", 
 c_target = "black", s_ref = 3, s_target = 2)
}
\arguments{
  \item{shapes}{Landmark data of shapes (list of matrices)}
  \item{reference}{Landmark data of reference shape (matrix type)}
  \item{path}{Path where results will be saved (Default: working directory)}
  \item{c_ref}{Color of the points representing the reference shape (Default \code{c_ref="red"})}
  \item{c_target}{Color of the points representing the target shape (Default \code{c_target="black"})}
  \item{s_ref}{Size of the points representing the reference shape (Default \code{s_ref=3})}
  \item{s_target}{Size of the points representing the target shape (Default \code{s_target=2})}
  \item{save}{A logical value. If TRUE, the function will save the plot as jpeg images in the indicated in \code{path}}
	
	This parameters are used to modify stored graph (\code{save = TRUE}):
	
	\item{w}{Units of width of the graph in inches (Default \code{w = 6})}
	\item{h}{Units of height of the graph in inches(Default \code{h = 6})}
	\item{d}{Number of dpi (resolution) of the graph. (Default \code{d = 6})}

}
\details{The function uses package \pkg{ggplot2} (Wickman 2009) to create multiple plots representing the variations between a list of target shapes and a reference (consensus shape or another specimen data). If \code{save=TRUE} it creates a folder (named \emph{day_hh_mm}) and stores the plots as images (.jpeg) at the provided path (if not provided it will store them in the working directory).
There are several parameters for modify the graph for a suitable representation. This function could be used for example to analyze morphological variations of species over generations.}
\references{H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2009.}
\author{Cabrera Juan Manuel} 
\seealso{ \code{\link{SimEvo}}}
\examples{ 
data("aegla_consensus")
data("simulated_shapes")

#Sample one individual from simulated_shapes dataset

simu<-simulated_shapes[100]

#Create a shape variation graphic

graph<-PlotVariations(simu,aegla_consensus)

#Red dots represent the landmark coordinates of the reference shape and black
# dots represent the target shape. The length of the segments represent 
# distances between pairs of homologous landmarks
  
graph[[1]]
}
