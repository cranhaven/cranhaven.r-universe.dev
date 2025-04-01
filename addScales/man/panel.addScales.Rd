\name{panel.addScales}
\alias{panel.addScales}
\title{
Default panelFUN For addScales.trellis
}
\description{
Adds Labeled Midline and (Unlabeled) Lines or Shaded Regions Showing
Plot Scaling to Trellis Panels.
}
\usage{
   panel.addScales(
   scaleline = c(0,0),
   scaleType = c("line", "region"),
   ndig.midline = c(h = 2, v = 2),
   col.midline = "red",
   adj.midline = c(-0.1, -0.25),
   midline.aes = list(lwd = if(colCode == "n") 1.5 else 3),
   midline.label.aes =list(
      col = if(colCode == "n")col.midline else "black",
      fontface = "bold",
      cex = .8),
   scaleline.aes = list(lty = "dashed", col = "purple"),
   region.aes = list(fill = "tan", alpha = .20),
   colCode = c("n", "m", "r"),
   palette =  hcl.colors(n = 100, "Viridis"),
   \dots
   )
}
\arguments{
  \item{scaleline}{
      A numeric vector of length 1 or 2 giving the distance from the scalelines
      to the midline(s); or equivalently, the half width(s) of the shaded scale regions.
      Signs are ignored. If two values are given, the first is the
      (vertical) distance to the horizontal midline and the second is the (horizontal)
      distance to the vertical midline. If a single value is given,it is assumed to be the
      first and the second is 0. 0 or NA mean: don't add that scaleline/region and midline.
   }
  \item{scaleType}{
      Whether the scale distance from the midline is shown by scale lines or as a
      shaded region. Note: Use of shaded regions requires that the graphics device
      support alpha transparency. Regions will not be shown properly -- or at all --
      if it does not.
  }
  \item{ndig.midline}{
      Named or unnamed pair of integer arguments, or a single integer that will be replicated.
      The names must be (and are assumed to be if unnamed) "h" and "v" \emph{in that order} and
      give the number of \emph{significant} digits to show in the midline labels for the
      corresponding midlines. Non-integer values are rounded to integer, and values outside the
      range of 0 to 15digits are converted to 2. However, see the \bold{Details} section below
      for a caveat.
}
  \item{col.midline}{
      Midline color. Can be given in any form suitable for the base \code{col}
      parameter of \code{par}.
   }
  \item{adj.midline}{
      \code{numeric}: The \code{adj} vector of length two that will be fed to \code{panel.text}
      to position the midline label with respect to the midline. \code{c(.5,.5)} centers
      the label on the midline and lower limit of the relevant axis
      (x-axis for a horizontal midline, so left-center; and y-axis for a vertical
      midline, so bottom-center). See the base \code{\link{text}} function for details.
   }
  \item{midline.aes}{
      List of \emph{aesthetics} of midline: \code{lwd, lty, alpha}, but
      \bold{not} \code{col}, as this is already specified in the
      \code{col.midline} argument.
   }
  \item{midline.label.aes}{
      List of \emph{aesthetics} of the midline label: \code{col, cex, font,
      fontface, etc.}, but not \code{adj}, as this is handled by
      \code{adj.midline}. See \code{\link{panel.text}} for argument details.
   }
  \item{scaleline.aes}{
      List of \emph{aesthetics} of the scale lines. Same as for
      \code{midline.aes} except that \code{col} can be specified.
   }
   \item{region.aes}{
      \code{fill} and \code{alpha} parameters for shaded scale region.
      }
   \item{colCode}{\code{character}: Should the midlines and possibly also the scale
      regions be color coded by the midline value in the range of all midline values?
      Doing this in addition to labeling their values can improve visual comparison
      of midline levels among the panels. "m" means color code just the midlines; "r" means
      color code both midlines and scale regions; "n", the default, means do not color code.
      }
   \item{palette}{A vector of colors to use for color coding from low to high values.
      See \code{\link{heat.colors}} for how to conveniently specify color palettes.
      }
   \item{\dots}{
   Additional arguments passed down to the panel function, mostly ignored here. But
   see the note below for an exception.
   }
}

\value{
   No value is returned. This panel function is added to the existing panel
   function component of the \code{trellis} object on which \code{addScales}
   dispatches. It adds midline(s) and scale lines or regions to the panels
   when they are plotted.
}
\details{
   Midline values with significant digits that change precision by
   \dQuote{small} amounts relative to the \code{scaleline} values are
   \emph{zapped} by the \code{zapsmall()} function to remove extra
   digits in the display to improve readability. For example,
   a \code{midline} value of 1.23 would be shown as 1 when
\code{ndig.midline = 2} and \code{scaleline} = 100.
}

\author{
Bert Gunter \email{bgunter.4567@gmail.com}
}

\note{A list, \code{all.panel.limits = list(h = obj$y.limits, v = obj$x.limits)}
   with these components of the trellis object list is always passed to the
   panel function (as part of the \dots list). This makes available \emph{all}
   panel limits to the panel function, not just those of its own panel.
   This list is used for color coding midlines and/or scale regions,
   but is ignored otherwise here. Alternative panel functions may choose to use
   this information in other ways.
   }

\note{
   For the \code{xxx.aes} arguments, when the user explicitly specifies the list, any component not specified will default to its formal \verb{panel.addScales} argument value if that exists, or to the relevant panel function default if not, i.e. \code{panel.refline} for the lines and \code{panel.text} for the labels.
}
\seealso{
\code{\link{colors}}, \code{\link{par}}, \code{\link{panel.refline}},
\code{\link{panel.text}}, \code{\link{text}}
}

\keyword{ hplot }
\keyword{ aplot }
