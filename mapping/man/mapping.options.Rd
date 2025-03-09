\name{mapping.options}
\alias{mapping.options}
\alias{mapping.options.reset}

\title{Default values for \pkg{mapping} functions}

\description{Set or retrieve default values used in mapping functions availables in \pkg{mapping} package.}

\usage{mapping.options(\dots)}

\arguments{
  \item{\dots}{
A single character vector, or a named list. The form \code{name = value} can be used to change a single option or \code{list(name1 = value1, name2 = value2)} can be used to change several arguments. If no arguments are provided, then the function returns all the current options.}
}

\details{

The function change globally the option for the current R session, and locally if used in the \code{mapping} function, with the \code{options} argument, for example, \code{options = mapping.options(legend.frame = FALSE, "title.position" = "left")}.

Many different options are used for the function in \pkg{tmap} package. For more details, look at \code{\link{tm_layout}}, \code{\link{tm_borders}}, and \code{\link{tm_fill}}.

Available options are the following:

\describe{
\item{\code{palette.cont = "YlGnBu"}}{palette for countinuous data}
\item{\code{palette.cat = "Accent"}}{palette for categorical data}
\item{\code{palette.cont.vector = NULL}}{a string vector with color names for countinuous data}
\item{\code{palette.cont.vector = NULL}}{a string vector with color names for categorical data}

\item{\code{nclass = 5}}{number of classes for countinuous data}
\item{\code{check.unit.names = TRUE}}{a ogical value indicating whether the input id names are checked before the link with the coordinates}
\item{\code{use_cache = TRUE}}{a ogical value indicating whether the cache is used to load the shape file}
\item{\code{use_internet = TRUE}}{a ogical value indicating whether the data are downloaded from internet or whether a internet connection is available}

\item{\code{alpha = 1}}{transparency}
\item{\code{breaks = NULL}}{a numerical value indicating the breaks}
\item{\code{interval.closure = "left"}}{a ogical value indicating where the interval are closed}
\item{\code{labels = NULL}}{a character vector with labels of the classes}
\item{\code{NA.color = "grey"}}{color for \code{NA} values}
\item{\code{NA.text = "Missing"}}{label for \code{NA} values}
\item{\code{col.style = "order"}}{type of color scale for numeric data. For other method look ad \code{\link{tm_fill}}}
\item{\code{map.frame = TRUE}}{a logical value indicating whether the frame is drawn}

\item{\code{border.lwd = 1}}{line width of the borders}
\item{\code{border.col = "black"}}{color of the borders}
\item{\code{border.type = "solid"}}{border type}
\item{\code{border.alpha = NA}}{trasparency of the borders}


\item{\code{title = NULL}}{main title}
\item{\code{title.position = "center"}}{main title position}
\item{\code{title.color = "black"}}{color of main title}
\item{\code{title.fontface = 1}}{main title font face}
\item{\code{title.size = 1}}{main title size}

\item{\code{legend.title = NA}}{title of the legend}
\item{\code{legend.show = TRUE}}{a logical value indicating whether include the legend}
\item{\code{legend.only = FALSE}}{a logical value indicating whether include the legend without map}
\item{\code{legend.position = c("right","top")}}{legend position}
\item{\code{legend.digits = 5}}{legend digits}
\item{\code{legend.outside = FALSE}}{a logical value indicating whether the legend is included outside the map}
\item{\code{legend.outside.facetes = TRUE
}}{a logical value indicating whether the legend is included outside the facetes}
\item{\code{legend.width = 1}}{width of the legend}
\item{\code{legend.title.position = c("right","top")}}{legend title position}
\item{\code{legend.title.size = 1}}{legend title size}
\item{\code{legend.title.fontface = 1}}{legend title font space}
\item{\code{legend.title.color = "black"}}{legend title color}
\item{\code{legend.text.color = "black"}}{legend title color}
\item{\code{legend.text.size = 0.5}}{legend title color size}
\item{\code{legend.text.align = "left"}}{}
\item{\code{legend.text.fontface = 1}}{}
\item{\code{legend.frame = TRUE}}{a logical value indicating whether the frame is drawn for the legend}
\item{\code{legend.decimal.mark = "."}}{}
\item{\code{legend.format = "fg"}}{}
\item{\code{legend.big.mark = ","}}{}
\item{\code{legend.text.separator = "-"}}{}

\item{\code{facets.free.scale = FALSE}}{}
\item{\code{facetes.cols = NA}}{}
\item{\code{facetes.rows = NA}}{}

\item{\code{interactive.tiles = "CartoDB.Positron"}}{}
\item{\code{interactive.popup.vars = NULL}}{}
\item{\code{interactive.popup.id = TRUE}}{}
\item{\code{interactive.popup.closeButton = TRUE}}{}
\item{\code{interactive.popup.width.max = 150}}{}
\item{\code{interactive.popup.width.min = 35}}{}
\item{\code{interactive.highlight.weight = 3}}{}
\item{\code{interactive.highlight.color = "black"}}{}
\item{\code{interactive.highlight.alpha = 1}}{}
\item{\code{interactive.highlight.front = TRUE}}{}
\item{\code{interactive.control.collapse = TRUE}}{}
\item{\code{interactive.layer.control.position = c("left", "top")}}{}
\item{\code{interactive.hovered.id = TRUE}}{}
\item{\code{text.size = 0.5}}{}
\item{\code{text.col = "black"}}{}
\item{\code{text.fontface = 1}}{}
\item{\code{text.shadow = FALSE}}{}
\item{\code{text.alpha =  NA}}{}

\item{\code{credits.source = NULL}}{}
\item{\code{credits.author = NULL}}{}
\item{\code{credits.size = 0.7}}{}
\item{\code{credits.fontface = NA}}{}
\item{\code{credits.color = "black"}}{}
\item{\code{credits.align = "left"}}{}
\item{\code{credits.position = c("left", "bottom")}}{}

\item{\code{popup.vars = NA}}{a character vector indicating the variable to popoup in interactive maps}
\item{\code{compass = NULL}}{a character vector indicatin the type of compass (look at \code{\link{tm_layout}})}
\item{\code{style = "white"}}{style (look at \code{\link{tm_style}})}
\item{\code{crs = NULL}}{}
}

Options may be reset using \code{mapping.options()}.
}

\value{Return a list with options.}

\references{
Tennekes M (2018). “tmap: Thematic Maps in R.” _Journalstatisticaltical Software_, *84*(6), 1-39. doi: 10.18637/jss.v084.i06 (URL: https://doi.org/10.18637/jss.v084.i06).
}


\examples{


mapping.options()

# A single options

mapping.options("title.position")

# Globally

mapping.options("title.position" = "left")
mapping.options("title.position")

}
