#' @rdname sephora-methods
#' @method plot sephora
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggplot_build
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 aes
#' @importFrom ggnewscale new_scale_color
#' @importFrom stats cmdscale
#' @importFrom nlme lme
#' @importFrom MASS mvrnorm
#' @importFrom dplyr '%>%'
#' @importFrom methods setMethod 
#' @importFrom graphics par
#'
#' @section Plotting:
#'
#' This function draws either a graphic based on a \code{\link[ggplot2]{ggplot}} or a \code{\link[base]{plot}} object. 
#' 
#' The default is intended for \code{numeric} vectors and \code{\link[sephora]{sephora-class}} objects.
#' This method employs the \code{ggplot2} system and returns a sort of time series plot.
#' 
#' The method \emph{profiles}, selected when \code{type="profiles"}, is also intended for \code{numeric} 
#' vectors and \code{\link[sephora]{sephora-class}} objects. This method is based on the \code{ggplot2} system
#' and draws \eqn{p} curves, one for each period (\code{p=length(startYear:endYear)}), on 
#' the same time scale (days of the year). 
#' 
#' The method \emph{ms}, selected when \code{type="ms"}, is intended for \code{\link[sephora]{sephora-class}} objects
#' only. Using the \code{ggplot2} system this method draws the result of a multidimensional
#' scaling analysis performed on the smoothed version of the \eqn{p} curves described above.
#' 
#' The method \emph{derivative}, selected when \code{type="derivatives"}, is intended for
#' \code{\link[sephora]{sephora-class}} objects only. A 5-panel plot is drawn showing (from top 
#' to bottom):
#' 
#'  \itemize{
#'    \item FPCA estimate: the \code{fpca} entry of \code{\link[sephora]{sephora-class}} object. See \code{\link[sephora]{phenopar}}.
#'    
#'    \item First, second, third and fourht derivative of FPCA estimate: curve obtained by applying \code{\link[sephora]{ndvi_derivatives}}
#'    to FPCA estimate.
#'  } 
#'
#' @param               x a numeric vector or a \code{sephora} object.
#' @param               y ignored.
#' @param       startYear integer, time series initial year.
#' @param         endYear integer, time series final year.
#' @param       frequency integer giving number of observations per season.
#' @param            type character specifying type of plot. By default, \code{NULL};
#'                        "profiles", "ms" and "derivatives" are also allowed.
#'                        See \bold{Details.}
#' @param        sizeLine integer giving line size
#' @param       sizePoint integer giving point size
#' @param position_legend character. Should a legend be added? Where? See
#'                        \code{\link[ggplot2]{theme}}.
#' @param    title_legend character. Should a legend be added? What would it be? See
#'                        \code{\link[ggplot2]{theme}} and \bold{Details}.
#' @param            xLab character, label to display in x-axis.
#' @param            yLab character, label to display in y-axis. See \bold{Details}.
#' @param            xLim date vector of length 2 indicating limits of x-axis. When no
#'                        supplied, \code{x} will be displayed in the period
#'                        of time defined by \code{startYear}, \code{endYear} and
#'                        \code{frequency}.
#' @param         msTitle character. Default "Cluster". See \bold{Details}.
#' @param      pointShape \code{shape} parameter used in \code{\link[ggplot2]{geom_point}}. Default 16.
#'                        See \bold{Details}.
#' @param       pointSize \code{size} parameter used in \code{\link[ggplot2]{geom_point}}. Default 2.
#'                        See \bold{Details}.
#' @param     pointStroke \code{stroke} parameter used in \code{\link[ggplot2]{geom_point}}. Default 3.
#'                        See \bold{Details}.
#' @param    textFontface \code{fontface} parameter used in \code{\link[ggplot2]{geom_text}}. Default 2.
#'                        See \bold{Details}.
#' @param        textSize \code{size} parameters used in \code{\link[ggplot2]{geom_text}}. Default 5.
#'                        See \bold{Details}.
#' @param      text_hjust \code{hjust} parameter used in \code{\link[ggplot2]{geom_text}}. Default 0.5.
#'                        See \bold{Details}.
#' @param      text_vjust \code{vjust} parameter used in \code{\link[ggplot2]{geom_text}}. Default -0.5.
#'                        See \bold{Details}.
#' @param             ... additional \code{\link[ggplot2]{ggplot}} parameters.
#'                        
#' @details By default, \code{type=NULL} and this option allows for plotting \code{numeric}
#' vectors and \code{sephora} objects; argument \code{title_legend} is only pertinent in this case.
#' Other allowed options for \code{type} are "profiles", "ms" and "derivatives". When 
#' \code{type="profiles"} all the arguments used in the default case are allowed except for
#' \code{title_legend}. When \code{type="ms"}, arguments \code{msTitle}, \code{pointShape},
#' \code{pointSize}, \code{pointStroke}, \code{textFontface}, \code{textSize}, \code{text_hjust}
#' and \code{text_vjust} are pertinent. When \code{type="derivatives"}, the default value of
#' argument \code{yLab} will be used.
#'
#' @return A \code{gg} object (or \code{NULL} (invisible) when \code{type="derivatives"}).
#' 
plot.sephora <- function(x, y, startYear, endYear, frequency, 
                         type=NULL,
                         sizeLine=1, sizePoint=2, position_legend="none", 
                         title_legend=NULL,
                         xLab="Time", yLab="Index", xLim, 
                         msTitle="Cluster", 
                         pointShape=16, pointSize=2, 
                         pointStroke=3, textFontface=2, textSize=5, text_hjust=0.5, 
                         text_vjust=-0.5, ...){
  
  if( inherits(x, "sephora") ){
      toPlot <- x$x
      startYear <- x$startYear
      endYear <- x$endYear
      freq <- x$freq
  } else {
    if( missing(startYear) | missing(endYear) | missing(frequency) ){
      stop("startYear, endYear and frequency must be provided when x is not of class 'sephora'")
    } else {
        toPlot <- x
        freq <- frequency
    }
  }
  
  if(is.null(type)){
    out <- .tsPlot(x=toPlot, startYear = startYear, endYear = endYear, 
                   frequency=freq, 
                   sizeLine=sizeLine, sizePoint=sizePoint, 
                   position_legend=position_legend, 
                   title_legend=title_legend,
                   xLab=xLab, yLab=yLab, xLim=xLim)
  }

  if(!is.null(type)){
    type <- match.arg(type, c("profiles", "ms", "derivatives"))
    
    if( type == "profiles" ){
      out <- .rmPlot(x=toPlot, startYear = startYear, endYear = endYear,
                     frequency=freq, sizeLine=sizeLine, sizePoint=sizePoint,
                     position_legend=position_legend, xLab=xLab, yLab=yLab, 
                     xLim=xLim)
    }
    
    if( type == "ms" ){
      out <- .msPlot(x=x, 
                     legendTitle=msTitle,#"Cluster", 
                     pointShape=pointShape, pointSize=pointSize, 
                     pointStroke=pointStroke, 
                     textFontface=textFontface, textSize=textSize, 
                     text_hjust=text_hjust, text_vjust=text_vjust)
    }
    
    if( type == "derivatives" ){
      out <- .derPlot(x=x, yLab=yLab)
    }
    
  }
    
  out
}

setMethod("plot", signature(x='numeric', y='missing'),
          plot.sephora)
