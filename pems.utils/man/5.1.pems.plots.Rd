\name{5.1.pems.plots}
\alias{5.1.pems.plots}
\alias{pems.plots}
\alias{latticePlot}
\alias{panel.PEMSXYPlot}
\alias{XYZPlot}
\alias{pemsPlot}
\alias{panel.pemsPlot}
\alias{preprocess.pemsPlot}
\alias{pemsXYZCondUnitsHandler}
\alias{panel.routePath}
\alias{WatsonPlot}
\alias{preprocess.WatsonPlot}
\alias{panel.WatsonBinPlot}
\alias{panel.WatsonContourPlot}
\alias{panel.WatsonSmoothContourPlot}


\title{
  Various plots for pems.utils
}

\description{
  Various plot functions and visualization tools 
  for use with \code{pems} objects.
}

\usage{

#pemsPlot

pemsPlot(x, y = NULL, z = NULL, groups = NULL, 
         cond = NULL, ..., data = NULL, 
         units = TRUE, multi.y = "special", 
         fun.name="pemsPlot")

#associated functions

pemsXYZCondUnitsHandler(x, y = NULL, z = NULL, 
         cond = NULL, groups = NULL, data = NULL, 
         units = TRUE, ..., 
         fun.name = "pemsXYZCondHandler")

preprocess.pemsPlot(lattice.like = lattice.like, 
         units = units, ...)

panel.pemsPlot(..., loa.settings = FALSE)

panel.routePath(..., loa.settings = FALSE)

#WatsonPlot

WatsonPlot(speed, accel = NULL, z = NULL, ..., 
         data = NULL, cond = NULL, units = TRUE, 
         plot.type = 2, fun.name="WatsonPlot")

#associated functions

preprocess.WatsonPlot(lattice.like = lattice.like, ...)

panel.WatsonBinPlot(..., ref.line = TRUE, 
         process.panel = NULL, plot.panel = NULL, 
         omit.stopped = FALSE, process = TRUE, 
         plot = TRUE, loa.settings = FALSE)

panel.WatsonContourPlot(..., 
         plot.panel=NULL, process = TRUE, 
         plot = TRUE, loa.settings = FALSE)

panel.WatsonSmoothContourPlot(..., 
         plot.panel=NULL, process = TRUE, 
         plot = TRUE, loa.settings = FALSE)
 
#old plots

latticePlot(x = NULL, data = NULL, plot = lattice::xyplot, 
         panel = NULL,  ..., greyscale = FALSE, 
         fun.name = "latticePlot")

panel.PEMSXYPlot(..., grid=NULL)

XYZPlot(x = NULL, ..., data = NULL, statistic = NULL, 
         x.res = 10, y.res = 20, plot = lattice::levelplot,
        fun.name = "XYZPlot")

}




\arguments{

  \item{x, y, z, groups, cond}{
  (Various) The main plot elements. \code{x} and 
  \code{y} are typically plot coordinates. 
  \code{z} is any additional information that could 
  be used , e.g. to modify points plotted at 
  \code{(x,y)} coordinates or generate a third axis 
  for a surface plot. \code{groups} and \code{cond} 
  are plot grouping and conditioning terms that can 
  be used to subset the supplied data and/or generate 
  multiple plot panels. See Below.

  For new plots, these should be supplied individually, 
  e.g. for \code{pemsPlot}:

  \code{pemsPlot(x, y, z, groups, cond)} 

  For old plots, these must be formulae.         

  For \code{latticePlot} the basic formula structure 
  is \code{y ~ x | cond}. 

  For \code{XYZPlot} the basic formula structure is 
  \code{z ~ y * x | cond}. \code{z} is optional, but 
  when it is not supplied \code{z} it is treated 
  as the bin count. 
  
  See Notes and Examples.  
} 

  \item{data}{
  (Optional \code{data.frame} or \code{pems} object) 
  For most plots, the data source for plot elements, 
  e.g \code{x}, \code{y}, \code{z}, etc, if these are 
  not supplied directly or accessible from the 
  current workspace.
} 

  \item{units}{
  (Optional \code{logical} or \code{list}) for 
  \code{pemsPlot} only, unit handling information. 
  By default, \code{pems.utils} adds any known units 
  to plot labels and allows in-plot unit 
  management. Unit management is handled by 
  \code{convertUnits}, and requested conversions 
  need to assigned to an axis. So, for example, the 
  call \code{pemsPlot(..., x.to="m/s")} 
  would generate a plot with the x-axis in units of 
  m/s (assuming \code{pems.utils} knows 
  the unit conversion and the x-axis data series is 
  in units that can be converted). All unit management 
  and associated figure labelling can disabled using 
  \code{units = FALSE}, or unit suffixes 
  can be removed but unit management retained using 
  \code{units.add.to.labels = FALSE}. 
  }
  
  \item{multi.y}{
  (character) \code{pems.plots} accepted multiple 
  \code{y} data-series if passed using \code{cpe}, 
  e.g. \code{y = cpe(a,b)}. \code{multi.y} sets how 
  these are handled, options include \code{'groups'}, 
  \code{'cond'} and \code{'special'} (the default), 
  which selects groups if not used in plot call else 
  cond. 
}

  \item{\dots}{
  (Optional) Other arguments, typically passed on. This 
  includes, \code{scheme} which sets the default 
  coloring scheme for the plot. See Note 
  below.
  }

  \item{fun.name}{
  (Function management argument) \code{pems.utils} 
  management settings, can typically be ignored by 
  most users.
  }

  \item{panel, plot.panel, process.panel}{
  (Functions) These functions are used to generate 
  the content of individual plot panels. 
  Typically, all in-panel data processing and plotting 
  is carried out using \code{panel}. However, these 
  steps can be handled by separate functions if these 
  are supplied as 
  \code{plot.panel} and \code{process.panel}.  
  }

  \item{lattice.like, plot, process, loa.settings}{
  (Various) Plot management elements. These can 
  typically be ignored by most users, but 
  allow plot developers to fine-tune plots. See 
  Details below.
  }

  \item{speed, accel}{
  (Various) For \code{WatsonPlot}, the \code{x} and 
  \code{y} terms, speed and acceleration, 
  respectively. 
  }

  \item{ref.line}{
  (Logical or list) For \code{WatsonPlot}. This 
  argument manages the speed = 0 reference 
  added to WatsonPlots. It can be either a logical 
  (\code{TRUE}/\code{FALSE}), to turn the 
  line on or off, or a list of parameters to set 
  the properties of the line.
  }

  \item{omit.stopped}{
  (Logical or character) For \code{WatsonPlot}, how to 
  handle idling data: \code{FALSE} include 
  in plot; \code{TRUE} or \code{'points'} removes 
  idle points before plotting; \code{'cell'} or 
  \code{'cells'} removes any cells that include 
  idling points before the data is binned.   
  } 

  \item{plot.type}{
  (numeric) For \code{WatsonPlot}, pre-set plot types: 
  \code{1} scatter plot; \code{2} bin plot; 
  \code{3} contour plot of binned data; \code{4} 
  smoothed surface of binned data. 
  See also \code{statistic}     
  } 

  \item{greyscale}{
  (Logical) For older plots only, should the plot 
  be greyscale by default? This option resets 
  the \code{lattice} color themes to greyscale while 
  the plot is beging generated. So: (1) 
  This only effects the plot itself, not subsequent 
  plots; and, (2) any user resets overwrite 
  this, e.g. \code{latticePlot(..., greyscale=TRUE, 
  col="red")} will place red symbols on an 
  overwise greyscale plot. Newer plots use the 
  alternative \code{plot(..., scheme = "greyscale")}.
}

  \item{grid}{
  (List) If supplied, a list of plot parameters to be 
  used to control the appearance of the 
  grid component of the plot. See Below.
}

  \item{statistic}{
   (Function) when binning data with \code{XYZPlot} 
   and \code{WatsonPlot}, the function to use when 
   evaluating the elements of each data bin. By 
   default, \code{length} is used if \code{z} is 
   not supplied to generate a frequency plot and 
   \code{mean} is used if \code{z} is supplied to 
   generate a bin average plot. 
}

  \item{x.res, y.res}{
   (Numerics) when binning data with \code{XYZPlot}, 
   the number of x- and y-axis bins to 
   generate. 
}
    
}


\details{

  \code{pems.utils} includes conventional (generic) 
  \code{plot} methods for \code{pems} and 
  \code{pems.element} objects. See 
  \code{\link{plot.pems}} and 
  \code{\link{plot.pems.element}} for further 
  details. 

  However, it also includes a range of higher-level 
  plotting functions developed for use with 
  \code{pems} data. 

  Early plots, e.g. \code{latticePlot}, only allowed 
  plot arguments using the 
  \code{\link[lattice]{lattice}} formula format. While this is 
  flexible and very powerful system, some users 
  preferred the more conventional \code{plot(x,y,..)} 
  call format. 
  So, newer plots, e.g. \code{pemsPlot}, allow both 
  conventional plot and 
  \code{\link[lattice]{lattice}}-style formula calls. 

  \code{pemsXYZCondUnitsHandler} handles the pems 
  information associated with the plots. This 
  routine is included as a discrete function within 
  this package and others are welcome to 
  use elsewhere for similar purposes. 
  \code{edit(pemsPlot)} to see it.  

  Newer plots use a combination of \code{\link[lattice]{lattice}} 
  and \code{\link[loa]{loa}} functions to provide a range of 
  additional plotting options, such as integrated panel 
  and key management. See \code{\link[loa]{loa}} 
  documentation for further details.   

  \code{preprocess...} and \code{panel...} functions 
  handle pre-plot and in-plot elements 
  of plot generation. These use the \code{\link[loa]{loa}} 
  modification of the \code{\link[lattice]{lattice}} 
  plotting framework.    

  See Notes, Examples and extra documentation: 
  \href{http://pems.r-forge.r-project.org/pems.utils.plots.html}{pems.plots}.
  
}


\value{

  By default, \code{pemsPlot} generates a bubble plot, 
  so it plots \code{(x,y)} points, and by default 
  color-grades and size-scales them according to 
  \code{z} if also supplied.

  When supplied speed and accel data-series as 
  \code{x} and \code{y} cases, the \code{WatsonPlot} 
  generates various forms of Watson's classic 
  speed/accel frequency distribution 
  plot. 
   
  \code{latticePlot} and \code{XYZPlot} are general 
  purpose 'xy' and 'xyz' data plotting functions.  

  \code{fortify} is intended for use by \code{ggplot2} 
  functions when users are plotting data in 
  \code{pems} objects. See Notes.
}

\references{
  
  lattice:

  Sarkar, Deepayan (2008) Lattice: Multivariate 
  Data Visualization with
  R. Springer, New York. ISBN 978-0-387-75968-5

  latticeExtra:

  Deepayan Sarkar and Felix Andrews (2011). 
  latticeExtra: Extra Graphical Utilities Based on 
  Lattice. R package version 0.6-18. 
  http://CRAN.R-project.org/package=latticeExtra

  \code{\link[lattice]{lattice}} is one of number of really 
  nice graphical tools in \code{R}. Others, like 
  \code{ggplot2} and \code{iplot}, help you to very 
  quickly explore your data. But, for me the trellis 
  framework of \code{\link[lattice]{lattice}} 
  has always felt the most flexible.
}

\author{
  Karl Ropkins
}

\note{
         
  General: 

  Like most other plot functions in \code{R}, 
  \code{\link[lattice]{lattice}} functions use a number 
  of common parameter terms. For example, \code{xlab} 
  and \code{ylab} reset the x and y labels of a graph; 
  \code{xlim} and \code{ylim} to set the x- and 
  y-scales of a graph; \code{col} sets the color of a 
  plot element; \code{type} sets the type ('p' for 
  points, 'l' for lines, etc); \code{pch} and 
  \code{cex} set plot symbol type and size, respectively; 
  and, \code{lty} and \code{lwd} set plot line type and 
  thickness, respectively; etc. These terms are passed 
  onto and evaluated by all these plot functions to 
  provide standard plot control. 

  \code{latticePlot}:
  
  The default \code{plot} option for \code{latticePlot} 
  is \code{xyplot}. 

  \code{panel} options for \code{latticePlot}: Default 
  \code{panel.xyplot}. The alternative panel, 
  \code{panel.PEMSXYPlot} supplied as part of this 
  package, adds a grid layer to a standard xy panel. 
  The extra code just allows you to pass specific plot 
  parameters to the grid panel using the argument 
  \code{grid}. You can build almost any plot layout 
  using these and other panels in \code{lattice} and 
  \code{loa} as building blocks. 

  \code{XYZPlot}:

  The default \code{plot} option for \code{latticePlot} 
  is \code{levelplot}. 
   
  \code{pemsPlot}:

  \code{pemsPlot} and subsequent plot functions use an 
  alternative convention. Here, plots include  
  separate \code{process} and \code{plot} steps. 
  This option allows the plot to pass on share the 
  results of in-panel calculations with other panels 
  and keys. The handling mechanism is part of the 
  \code{\link[loa]{loa}} package. 

  The reason for \code{latticePlot}, etc: 

  \code{latticePlot} combines a number of 
  \code{lattice} and \code{latticeExtra} function 
  modifications I regularly use when plotting data. 
  So, it is basically a short cut to save having to 
  write out a lot of code I regularly use. I would 
  encourage anyone to at the very least have a look 
  at \code{lattice}. 
  
  I also hope those learning \code{lattice}, find 
  these functions a helpful introduction and handy 
  'stop gap' while they are getting to grips with 
  the code behind trellis and panel structures.

}

\section{Warning}{
  
  IMPORTANT: Conditioning is currently disabled on 
  \code{XYZPlot}.

  \code{XYZPlot} is a short-term replace for 
  previous function \code{quickPlot}. It will most 
  likely be replaced when \code{pems.utils.0.3} is 
  released.

  \code{pemsPlot} and \code{WatsonPlot} no longer 
  accept formula \code{x}, \code{y}, \code{z} inputs. 
  
  With all these functions I have tried to make 
  the default plotting options as robust as possible. 
  However, it is not always possible to test all the plot
  combines that users might try. So, please 
  let me know if anything is not working nicely for 
  you. Thanks.

}


\seealso{
  See \code{\link[lattice]{lattice}}, \code{latticeExtra}, 
  \code{\link[loa]{loa}}.
}
\examples{

###########
##example 1
###########

\dontrun{
#plotting pems with other packages

#base 
plot(pems.1)
plot(pems.1$velocity)

#with lattice 
require(lattice)
xyplot(velocity~local.time, data = pems.1, type = "l")

#with ggplot2
require(ggplot2)
qplot(time.stamp, velocity, data=pems.1)
ggplot(pems.1, aes(x = time.stamp, y = velocity)) + geom_line() 
#etc

}

###########
##example 2 
###########

#basic usage of latticePlot

latticePlot(velocity~local.time, data = pems.1, type = "l")

latticePlot(velocity~local.time, data = pems.1, col = "red", 
            pch = 20, panel = panel.PEMSXYPlot, 
            grid = list(col ="black", lty=2))

###########
##example 3 
###########

#basic usage of XYZPlot

a <- calcAccel(velocity, local.time, data = pems.1, output="pems")

XYZPlot(~accel*velocity, data=a)

XYZPlot(~accel*velocity, data=a, plot = lattice::wireframe, shade=TRUE)


###########
##example 4 
###########

#basic usage of pemsPlot

pemsPlot(local.time, velocity, data=pems.1, type="l")


###########
##example 5 
###########

#basic usage of WatsonPlot

#Note: using 'a' generated in example 3 
WatsonPlot(velocity, accel, data=a)

\dontrun{
#omit.stopped for different handling of idling data
WatsonPlot(velocity, accel, data=a, omit.stopped="points")
WatsonPlot(velocity, accel, data=a, omit.stopped="cells")

#plot.type for different plot methods
WatsonPlot(velocity, accel, data=a, plot.type=1)
WatsonPlot(velocity, accel, data=a, plot.type=2)
WatsonPlot(velocity, accel, data=a, plot.type=3)
WatsonPlot(velocity, accel, data=a, plot.type=4)
}

}

\keyword{ methods }
