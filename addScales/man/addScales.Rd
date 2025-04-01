\name{addScales}
\alias{addScales}
\alias{addScales.trellis}

\title{
   Add Scaling Information to Panels in Multi-Panel Trellis Plots
}
\description{
   Adds a midline and upper and lower horizontal and/or vertical scale lines
   or shaded regions to all panels. Mostly useful when the
   \code{relation = "free"} option is used in the
   \code{scales} list to avoid loss of detail in plots from data that vary in
   location and scale from panel to panel.
}
\usage{
   addScales(obj, ...)

\method{addScales}{trellis}(obj,
         scaleline = list(h = TRUE, v = FALSE),
         legend = list(h = TRUE, v = TRUE),
         ndig.legend = c(h = 2, v = 2),
         legend.aes = list(),
         legend.loc = c("top","bottom","right","left"),
         panelFUN = panel.addScales,
         \dots)
}

\arguments{
  \item{obj}{
   Object on which method dispatch is carried out. Currently only a \code{trellis}
   method exists.
   }
  \item{scaleline}{
   A two component list with component names "h" and "v". The
   "h" component is for (h)orizontal scale lines/regions and "v" is
   for(v)ertical. Each of these must be a single logical or numeric value
   (and can be mixed in the list, of course). \code{TRUE} means: use a
   calculated default; \code{FALSE} means: don't plot scale lines for
   this component; and a numeric value means: use this value. The calculated
   or user-supplied value is the distance between the midline and scale lines
   = 1/2 the height/width of the shaded region.
   A numeric value of 0 is interpreted as \code{FALSE} -- don't draw.
   However, see the \strong{Details} section below for abbreviated versions that
   are also accepted.

   \bold{Note:} Scale lines/regions are only meaningful for \code{"numeric"} data as defined
   by \code{isTRUE(is.numeric())}. The corresponding \code{scaleline} component --
   "h" for y-axis data and "v" for x-axis data -- is silently set to \code{FALSE} for
   anything else.
   }
  \item{legend}{
   A two component list as in \code{scaleline} for logical values; or
   non-logical values that must either be quoted UTF-8 character strings (which can
   include special characters such as \enc{±}{+/-}, which is unicode U + 00B1,
   or \enc{°}{degrees}, unicode U + 00B0, math symbols, non-ascii language characters,
   etc.); or R \code{language} objects. See the \strong{Legend Details} section for
   further details.
   }

   \item{ndig.legend}{
      Named or unnamed pair of integer arguments, or a single integer that will be
      replicated. The names must be (and are assumed to be if unnamed) "h" and
      "v" \emph{in that order} and give the number of \emph{significant}
      digits to show in the default legend for the corresponding scale lines/region.
      Non-integer values are rounded to integer, and values outside the range
      of 0 to 15 digits are converted to 2.
   }
  \item{legend.aes}{
      List of \emph{aesthetics} of the legend text: \code{cex, font,
      fontface, col, etc.}. See \code{\link{panel.text}} for details.
   }
  \item{legend.loc}{
     One of \code{"top", "bottom", "left",} or \code{"right"} specifying where the
     legend will be placed \emph{outside} the the trellised panels on the page.
     See the \code{legend} section of the \code{\link{xyplot}} man page for
     details, but note that \code{addScales} will turn the legend argument into a
     list of the required form, so that part of the specification on the man page
     can be ignored.
   }
   \item{panelFUN}{
      The function used to add scaling details to the panels. Should use
      standard trellis/grid functionality.
   }
  \item{\dots}{
     Further arguments, controlling \emph{aesthetics} of the lines, labels, and/or
     fill regions such as color, line width, color palette, line type, etc., passed down to the
     \code{panelFUN} function. See \code{\link{panel.addScales}} for details for the
     default panel function.
   }
}

\details{
   As a convenience, abbreviated versions of \code{scaleline} and
   \code{legend} logical arguments can be used instead of the full versions
   described above.The abbreviated versions will be translated into the full
   versions for use by other functions such as \code{scaleline} and
   \code{update.scaledTrellis}.

   Specifically, a single value of \code{TRUE} or \code{FALSE} is replicated to
   both components of the argument. Thus \code{scaleline = FALSE} aborts the
   function, since it says not to add scales in either direction. \code{
   legend = FALSE} is fine, because it specifies only that legends be omitted.
   See the \bold{Legend Specification Details} section below for why this might be useful.

   If an unnamed list with two components (of the correct form) are given, they are
   assumed to be in the order \code{c("h","v")}. If a single named component with
   name "h" or "v" is given, the missing component is assumed
   to be \code{FALSE}. Thus, \code{list(v = TRUE), list(FALSE, TRUE)}, and
   \code{list(h = FALSE, v = TRUE)} are all equivalent. A list with a single
   unnamed component raises an error.

   The default \code{scaleline} calculation assures that all lines/regions fall within the
   axis limits of all panels. A (typically user-supplied) \code{scaleline}
   that fails this criterion will raise a warning and result in some
   panels with missing scale lines when \code{scaleType = "line"}.
}


\value{
   An object of class \code{c("scaledTrellis","trellis")} if successful.
   Because it inherits from class \code{"trellis"}, it can be saved
   and/or (automatically) plotted as usual.\cr

   \code{NULL} invisibly if an error occurs.

   The \code{scaledTrellis} object is the original \code{trellis} object list
   with its \code{panel} and \code{legend} components modified to add the
   scaling information. A new \code{addScales} component is also added
   that is itself a list with (at least) two components named "orig" and "args".
   The first of these contains the original
   \code{panel} and \code{legend} components of \code{obj}. The second
   contains either the names and values of the arguments in the call or the computed
   values of those arguments. The most important of these is the \code{scaleline}
   value, which can be extracted using the \code{\link{scaleline}} function by users
   who wish to construct their own scale line legends.The remaining values are used
   by the update method for \code{scaledTrellis} objects.
}

\section{Legend Specification Details}{
   The default legend is meant to be simple but serviceable. If there are scale lines
   in both directions, it will space them horizontally for \code{"top"} and \code{
   "bottom"} locations and vertically for \code{"right"} and \code{"left"} to
   minimize the space they occupy.

   A user-supplied legend component can be given in two forms: either as a (quoted)
   UTF-8 character string, like this: "Scale lines are at \enc{±}{ +/-}10\enc{°}{ degrees} ";
   or as a so-called \code{language} object. The latter allows the legend to use the
   (shortened to the \code{ndig.legend} number of digits) \code{scaleline} value.
   The former does not.

   A detailed discussion of \code{language} objects is beyond the scope of this Help
   page, but a simple example provides a template that should usually suffice. Suppose,
   instead of the default, the desired legend is:

      \verb{     Scale lines are at} \enc{±}{+/-} \verb{xxx}\enc{°}{ degrees},

   where the \verb{scaleline} value computed by \code{addScales} is to be substituted
   for the xxx. If xxx were available in the environment of the call
   (the \verb{addScales} invocation), then one could use something like
   (as in the previous paragraph):

   \verb{     paste0("Scale lines are at} \enc{±}{+/-}
   \verb{",xxx,"} \enc{°}{ degrees} "\verb{)}

   as the \code{legend} argument. But xxx is not known, because \code{addScales}
   hasn't calculated it yet. So instead, wrap the paste0 call by the
   \code{\link{quote}} function like this:

   \verb{     quote(paste0("Scale lines are at} \enc{±}{+/-}
   \verb{",sl,"} \enc{°}{ degrees} "\verb{))}

   \sQuote{sl} (unquoted) \emph{must} be used to replace the not-yet-known \code{
   scaleline} value. The \code{quote} function will pass the whole unevaluated
   \code{paste0} expression into \code{addScales}
   where the \code{scaleline} value will be calculated and substituted for
   \code{sl} and the whole expression then evaluated.
   Of course, any R expression instead of \code{paste0\dots} can
   be used as long as \code{sl} is substituted wherever the actual \code{scaleline}
   value is wanted.

   Another, perhaps slightly clumsier, way to do this -- but which generalizes to
   arbitrary \code{scaleline} displays as text or graphical objects (so-called
   \emph{grobs}) of any kind -- is simply to run
   \code{addScales} with \code{legend = FALSE} and extract the
   \code{scaleline} value(s) from the resulting object with the \code{scaleline()}
   function. The value(s) can then be used in any construction the user wishes to create.
}

\author{
Bert Gunter \email{bgunter.4567@gmail.com}
}

\seealso{
\code{\link{xyplot}},
\code{\link{panel.refline}},
\code{\link{panel.text}},
\code{\link{scaleline}},
\code{\link{panel.addScales}}
}

\examples{
###### Artificial example to show why addScales() might be useful and
###### how it works
##

###### Create a data set whose panels have different
###### centers and scales for y
x <- rep(0:10, 4)

scaling <- rep(c(1, 2, 5, 10), e = 11)

y <- sin(pi*x/10)*scaling + scaling ## add some structure

f <- factor(rep(LETTERS[1:4], e = 11))

## Now add noise proportional to data mean (= constant CV)
set.seed(91834)

y <- y + rnorm(44,sd = scaling/3)

## Plot this with the default "same" scaling and a loess curve

xyplot(y ~ x| f, layout = c(4,1), col = "darkblue",
        scales = list(alternating = 1, tck = c(1,0)),
        panel = function(...){
           panel.points(...)
           panel.loess(..., span = .6, col.line = "blue")
        })

##
## Because of the different scaling, it's somewhat difficult to
## see the common panel data behavior. With relation = "free", it
## becomes clearer:
##
trellis.par.set(plot.symbol = list(col = "darkblue"),
               plot.line = list(col = "darkblue"))

xyplot(y ~ x| f, layout = c(4,1),
       scales = list(alternating = 1, tck = c(1,0),
                     y = list(relation = "free")),
       panel = function(...){
          panel.points(...)
          panel.loess(..., span = .6, col.line = "blue")
       })

##
##  Unfortunately, the y-scales take up a lot of space and are
##  difficult to read. With more panels, they would completely
##  mess things up. To avoid this, don't draw them and use addScales
##  to layer visual scaling onto the panels.
##
freeplot <- xyplot(y ~ x| f, layout = c(4,1),
          scales = list(alternating = 1, tck = c(1,0),
                        y = list(relation = "free", draw = FALSE)),
          panel = function(...){
             panel.points(...)
             panel.loess(..., span = .6, col.line = "blue")
          })

addScales(freeplot) ## using defaults

##
## The labeled midline allows location comparison among the panels.
## The fixed distance from the dashed scale lines to the midline are given
## by the legend at top. This allows scaling among the panels to be
## compared, because the more y varies within a panel, the closer together
## these fixed scale lines become.
##
## NOTE:
## The addScales object inherits from class "trellis", so can be
## saved and plotted in the same way as 'freeplot' was. That is, the
## following also works:
##
 enhanced <- addScales(freeplot)
 enhanced

## Further panel options, which we use the update() method to change,
## allow for color coded scale regions and midlines:
##
#### Warning: Nothing may display if your graphics device does not support
## alpha transparency

 update(enhanced, scaleType = "region", colCode = "r")

##
## cleanup
rm(scaling, x, y, f, freeplot, enhanced)
##
########  Some real examples   #############
############################################

## Historical daily temperatures for Chicago, New York, and San Francisco.
data(CHITemps, NYCTemps, SFTemps)

preprocess.temps <- function(d){
      meanTemp <- with(d, (TMAX + TMIN)/2)
      Month <- months(as.Date(d$DATE))
      z<- aggregate(meanTemp, list(
         Month = factor(Month, levels = unique(Month)),
         Year = as.numeric(substring(d$DATE,1,4))
      ), FUN = mean)
      names(z)[3] <- "meanTemp"
      z
}

## Create a list containing the preprocessed data for all 3 cities
plotdat <- lapply(
   list(CHI = CHITemps, NYC = NYCTemps, SF = SFTemps),
   preprocess.temps)

## Consider NYC. Because of monthly temperature variation, monthly temperature
## histories are mostly whitespace with the default relation = "same".
## Note also the use of the prepanel.trim function with defaults to remove
## extreme y values.
##
## Consider New York City
nyctemps <-
   xyplot(meanTemp ~ Year|Month, type = "l", layout = c(3,4),
          data = plotdat[[2]],
          as.table = TRUE,
          between = list(x=1, y=0),
          ## reduce strip size
          par.strip.text = list(lines = .8, cex = .7),
          ## remove blank space for top axis
          par.settings = list(layout.heights = list(axis.top = 0)),
          prepanel = prepanel.trim, ## to remove possible extreme values
          panel = function(...){
             panel.grid(v = -1, h = 0, col = "gray70")
             panel.xyplot(...)
             panel.loess(..., span = .5, col = "darkred",
             lwd = 1.5)
          },
          scales = list(axs = "i", alternating = 1, tck = c(1,0)),
          xlab = "Year",
          ylab = "Average Temperature (\u00B0F)",
          main = "Mean Monthly Historical Temperatures in NYC"
   )

nyctemps

## Now try it with y-scale = "free' and addScales
##
nyctemps <- update(nyctemps,
   scales = list(axs = "i", alternating = 1,
   tck = c(1,0),y = list(relation = "free", draw = FALSE)))

addScales(nyctemps)

## The historical temperature trend as the city
## built up and modernized (more concrete and asphalt,people,
## heat sources, etc.) is clearer and quantified by the
## legend and scale lines; and the scale lines also show
## that winter temperatures are clearly more variable than summer.
## This was almost undetectable in the previous plot.

## The same plot using region shading instead of scale lines.
## Warning: May not display if your graphics device does not support
## alpha transparency

addScales(nyctemps, scaleType = "region")

## ... and using color coding for midlines and regions to better visually
## distinguish their values...
##
addScales(nyctemps, scaleType = "region", colCode = "r")

## You can repeat the exercise with the other two cities if you like.
## cleanup
rm(nyctemps, preprocess.temps, plotdat)

#######  Historical Crime Data #####

data(USAcrime)

## We explore the relationship beween property and violent crime over time.
## Point transparency via the 'alpha' setting is used to code year
## and the violent vs. property crime relationship is trellised by state
## for a selection of states.
##
## First with scales = "same", the default..

state.smpl <- c("CA","TX","GA","CO","VA","FL","NY","OH","MO","UT","MA","TN")

wh <- USAcrime$State \%in\% state.smpl

pcols <- hcl.colors(55, rev = TRUE)

crm <-xyplot(allViolent ~ allProperty|State, data = USAcrime[wh,],
             subscripts = TRUE, as.table = TRUE,
             layout = c(4,3), type = c("p", "g"),
             cex= .75,  pch = 19,
             col = pcols[USAcrime[wh,'Year'] -1959],
             par.strip.text = list(lines = .8, cex = .7),
             between = list(x = 1),
             scales = list(axs="i",alternating =1, tck = c(1,0)),
             xlab = "Property Crime Rate (incidents/100,000 population)",
             ylab = "Violent Crime Rate (incidents/100,000 population)",
             main = paste0("Violent vs. Property Crime Rates from 1960-2014 For 12 States"),
             sub = "Point Darkness Codes Years: Darker = Later Years",
             panel = function(subscripts,col,...)
                panel.xyplot(col = col[subscripts],...)
)
crm
## remove the grid and update with
## "free" scales and no axes for both x and y
crm2 <- update(crm, type = "p",
            scales = list(axs="i", relation = "free", draw = FALSE))

## Add scales for both x and y and color code midlines
addScales(crm2, scaleline = TRUE, colCode = "m")

## Some features to note:
##  1. As one might expect, violent and property crime rates are
##   correlated.
##
##  2. Crime rates first increased, peaked, and then decreased over time.
##
##  3. For most states there appears to be a kind of 'hysteresis':
##  the trajectory of the crime decrease is shifted up (higher violent
##  crime rate for the same property rate) from when it increased.
##  This could have been due to a change in reporting procedures,
##  over time, for example.
##
##  4. The midline colors and labels show that NY has the highest
##  violent crime rate, but a modest property crime rate: Tennessee
##  has a middling violent crime rate but the lowest (with VA) property
##  crime rate.
##
##  cleanup
   rm( state.smpl, wh, pcols, crm, crm2)
}

\keyword{ hplot }
\keyword{ aplot }

