\name{7.1.vsp.code}
\alias{7.1.vsp.code}
\alias{vsp.code}
\alias{vsp}
\alias{calcVSP}
\alias{calcVSP_JimenezPalacios}
\alias{refVSPBin}
\alias{refVSPBin_NCSU.14}
\alias{refVSPBin_MOVES.23}
\alias{VSPPlot}
\alias{VSPBinPlot}

\title{
  Vehicle Specific Power (VSP) related code
}

\description{
  Functions associated with VSP calculations.
}

\usage{

#calculation

calcVSP(speed = NULL, accel = NULL, slope = NULL, 
          time = NULL, distance = NULL, data = NULL,
          calc.method = calcVSP_JimenezPalacios,
          ..., fun.name = "calcVSP", this.call = NULL)

calcVSP_JimenezPalacios(speed = NULL, accel = NULL, 
          slope = NULL, vehicle.weight = NULL, vsp.a = NULL, 
          vsp.b = NULL, vsp.c = NULL, vsp.g = NULL, ..., 
          data = NULL,  
          fun.name = "calcVSP_JimenezPalacios", 
          this.call = NULL)

#VSP binning

refVSPBin(..., bin.method="ncsu.14")

refVSPBin_NCSU.14(vsp = NULL, data = NULL, 
          ..., fun.name="refVSPBin_NSCU.14")

refVSPBin_MOVES.23(vsp = NULL, speed = NULL, data = NULL, 
          ..., fun.name="refVSPBin_MOVES.23")
          
#vsp plotting

VSPPlot(vsp, em = NULL, ..., data = NULL, plot.type = 1, 
          fun.name="VSPPlot")

VSPBinPlot(vspbin, em = NULL, ..., data = NULL, 
          plot.type = 1, stat = NULL, fun.name="VSPBinPlot")
          
}

\arguments{

  \item{speed, accel, slope, time, distance, vsp, vspbin,
  em}{
  (Typically pems.element vectors) \code{speed}, \code{accel}, 
  \code{slope}, \code{time} and \code{distance} are possible 
  inputs for VSP calculation. \code{vsp} and \code{speed} are 
  possible inputs for VSP binning methods. \code{vsp}, 
  \code{vspbin} and \code{em} are x and y inputs for 
  associated plots.
  (See Notes about inputs and methods.)
} 
  \item{data}{
  (Optional, typically \code{pems}) The data source for inputs.
} 
  \item{calc.method, bin.method}{
  (Required functions) \code{calc.method} is the function used 
  to calculate VSP (default \code{calcVSP_JimenezPalaciosCMEM}). 
  \code{bin.method} is the methods used when binning VSP 
  measurements. (See Notes.)
}
  \item{\dots}{
  (Optional) Other arguments, currently passed on as supplied 
  to associated calculation or binning method, or back to 
  pemsPlot.
}
  \item{fun.name}{
  (Optional character) The name of the parent function, to be 
  used in error messaging.
}
  \item{this.call}{
  (Optional) Initial call, should generally be ignored. See 
  \code{\link{common.calculations}} for further details.
}
  \item{vehicle.weight, vsp.a, vsp.b, vsp.c, vsp.g}{
  (Numerics) VSP constants. If not supplied, defaults are 
  applied. See Below.
}
  \item{plot.type}{
  (Optional numeric) For \code{VSPPlot} and \code{VSPBinPlot}, 
  the type of plot to generate. For \code{VSPPlot}, 1 a 
  conventional scatter plot; or 2 a box-and-whisker plot. For 
  \code{VSPBinPlot}, 1 a bar plot; or 2 a box-and-whisker 
  plot. 
}
  \item{stat}{
  (Function) For \code{VSPBinPlot}, the statistic to use when 
  calculating bar scales for \code{plot.type} 1. By default this 
  is mean if \code{em} is supplied or count if not. NOTE: 
  stat is ignore when \code{plot.type} is used
}
    
}


\details{

  \code{calcVSP...} functions calculate VSP:

  \code{calcVSP} is a wrapper function which allows users to 
  supply different combinations of inputs. VSP calculations 
  typically require speed, acceleration and slope inputs. 
  However, This wrapper allows different input combinations, 
  e.g.:

  time and distance (time and distance -> speed, 
  time and speed -> accel)
 
  time and speed (time and speed -> accel)

  speed and accel

  This then passes on \code{speed}, \code{accel} and (if 
  supplied) \code{slope} to the method defined by 
  \code{calc.method}. (This means other VSP functions run 
  via \code{calcVSP(..., calc.method = function)} 
  share this option without needed dedicated code.)

  \code{calcVSP_JimenezPalacios} calculates VSP according 
  to Jimenez Palacios methods. See References and Note below.

  \code{refVSPBin...} functions generate a reference list of 
  VSP bins:
  
  \code{refVSPBin} is a wrapper that generates VSP Mode bins 
  depending on method applied. 

  \code{binVSP_NCSU.14} bins supplied \code{vsp} using the 
  14 bin method described in Frey et al 2002. 
  
  \code{binVSP_MOVES.23} bins supplied \code{vsp} using that 
  and \code{speed} and the 23 bin MOVES method (See Note). 
  
  \code{VSPPlot} generates various plots of VSP (x-axis) and 
  emission (y-axis) data. 
  
  \code{VSPBinPlot} generates various plots of VSP binned data. 
 
}


\value{

  \code{calcVSP} by default uses the Jimenez Palacios method 
  to calculate VSP in kW/metric ton.

  \code{refVSPBin} generates a \code{pems.element factor} 
  vector  of VSP Mode bin assignments. 
  
  \code{VSPPlot} and \code{VSPBinPlot} generate plots as
  \code{lattice} objects.

}


\note{
  \code{calcVSP...} constants can be set/modified in the 
  calculation call, e.g. 
  \code{calcVSP(..., vsp.a = [new.value])}. If not supplied, 
  defaults are used. (See References.) 

  \code{binVSP_MOVES.23} is in-development. Do not use without 
  independent confirmation of values.  

  Unit handling in \code{pems.utils} is via 
  \code{\link{checkUnits}}, 
  \code{\link{getUnits}}, \code{\link{setUnits}} and 
  \code{\link{convertUnits}}. 
  See \code{\link{common.calculations}} for details.
 
}

\seealso{
  See \code{\link{common.calculations}} (and 
  \code{\link{checkUnits}} and 
  \code{\link{convertUnits}}) for details of data management.
}

\author{
  Karl Ropkins
}

\references{
  
 \code{calcVSP_JimenezPalacios} uses methods described in:

 Jimenez-Palacios, J.L. (1999) Understanding and Quantifying 
 Motor Vehicle Emissions with Vehicle Specific Power and TILDAS 
 Remote Sensing. PhD Thesis, Massachusetts Institute of 
 Technology, Cambridge, MA.

 \code{vehicle.weight} is the vehicle mass (in metric tons), 
 and \code{vsp.a}, \code{vsp.b}, \code{vsp.c} and \code{vsp.g} 
 are the calculations constants for:

 \code{vsp = speed * (a * accel + (g * slope) + b) + (c * speed^3)}

 By default: a = 1.1, b = 0.132, c = 0.000302 and g = 0.132 
 (as of Jimenez-Palacios, 1999).
 
 Method ONLY INTENDED FOR vehicles < 3.855 metric tons.

 \code{refVSPBin_NCSU.14} VSP binning as described in:
 
 Frey, H.C., Unal, A., Chen, J., Li, S. and Xuan, C., 2002. 
 Methodology for developing modal emission rates for EPA's 
 multi-scale motor vehicle & equipment emission system. Ann 
 Arbor, Michigan: US Environmental Protection Agency. 
 (EPA420-R-02-027)

}


\examples{

###########
##example 1 
###########

#basic usage

vsp <- calcVSP(velocity, time = local.time, data = pems.1)
#where the returned object, vsp, is vsp values as pems.element

ncsu.14 <- refVSPBin(vsp)
#where the returned object, nscu.14, is the associated modal bin 
# assignments based on the Frey et al (2002) 14 bin method. 

}

\keyword{ methods }
