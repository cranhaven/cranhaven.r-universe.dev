\name{levelMap}
\alias{levelMap}
\alias{lmap}
\title{
Drawing multi-level maps (e.g. semantic maps or linguistic isoglosses)
}
\description{
A multi-level map ("lmap") is a plot of the distribution of nominal data with multiple levels in space. Such visualisations have two direct use-case in linguistics, viz. semantic maps and isoglosses. The drawing if the lines in space is performed by interpolation in this function (see details).

Semantic maps (Haspelmath 2003) are a visualisation of linguistic diversity. A semantic map shows a predefined configuration of functions/senses in two-dimensional space with an overlay of language-specific encoding of these functions/senses. An level-map tries to emulate this linguistic visualisation in an automatic fashion with various options for visual presentation.

Isoglosses show lines surrounding similar phenomena in space. Instead of drawing an exact boundary around measured points, an interpolation-technique is used here to show areas of interest. By only showing boundaries, multiple phenomena can be shown in one graphic.
}
\usage{
levelMap(points, data,
   main = NULL, draw = 5, levels = c(0.41, 0.46, 0.51),
   labels = NULL, cex = 0.7, col = "rainbow", add = FALSE,
   ignore.others = FALSE, normalize.frequency = FALSE, scale.pies = FALSE,
   lambda = NA, legend = TRUE, position = "bottomleft", cex.legend = 0.7,
   font = "", note = TRUE, file.out = NULL, ...)
}
\arguments{
  \item{points}{
  Coordinates of the data points specified as a two-column matrix or dataframe.
}
  \item{data}{
  Language data to be plotted as contour-overlay over the points. Either specified as a vector of language-specific forms, or as a numeric matrix with the forms as columns and the points as rows (the language-specific forms should be specified as \code{colnames}). The values in the matrix designate the occurrence of the forms, allowing for the encoding of frequency/typicality and of overlap of different forms being used in the same function. see Details.
}
  \item{main}{
  Title for the plot
}
  \item{draw}{
  Which forms to be drawn by contours. Specifying a numeric value will only draw the uppermost frequent forms in the data, by default only the topmost five forms are drawn (automatically ordered by frequency). Alternatively, a vector with names or column-indices of the forms to be drawn can be specified.
}
  \item{levels}{
  height of contours to be drawn. Internally, all values are normalized between zero and one, so only values between those extremes are sensible. Line thickness is automatically balanced.
}
  \item{labels}{
  Optionally, character vector with labels for the points, to be drawn instead of symbols in the plot. Should be a vector of the same length as the number of points. Alternatively, a single character-string is repeated for all points.
}
  \item{cex}{
  Character expansion of the labels (see previous option). Also influences the size of symbols or pie-charts.
}
  \item{col}{
  Colour specification, either in the form of the name of a built-in color palettes, like \link{rainbow}, or a manually specified vector of colors. When \link{NULL}, an attempt is made to use grey-scales.
}
  \item{add}{
  Logical: should the plot be added to an existing plot or not?
}
  \item{ignore.others}{
  Logical: ignore all other categories, not selected through \code{draw}.
}
  \item{normalize.frequency}{
  Logical: should rows of data (points in the plot) be normalized to 1? Useful only for data that represent frequency of occurence as columns. Note that setting this to \code{FALSE} influences the behaviour of \code{levels}.
}
\item{scale.pies}{
  logical: for multivalued data: should the size of the pies represent frequencies (\code{TRUE}) or all be of the same size (\code{FALSE}, by default)?
}
\item{lambda}{
  Parameter for the interpolation, passed internally to the function \link[fields]{Krig}. Low values result in more detailed boundaries around the measured points.
}
  \item{legend}{
 Logical: should a legend be added?
}
  \item{position}{
Where should the legend be positioned? Passed internally to \link{legend}.
}
  \item{cex.legend}{
Character expansion passed to \link{legend}, and also used for the indication of the levels in the plot
}
  \item{font}{
Font to be used for the legend and the labels. Passed internally to \code{par(family)}.
}
  \item{note}{
Logical: should a note be added to the bottom of the graphic to document the levels of the countour lines?
  }
  \item{file.out}{
Location for writing the image to a file instead of plotting it on screen
}
  \item{\dots}{
Additional parameters optionally passed to \link{boundary} for the specification of the area of interpolation.
}
}
\details{
The basic idea is to use some kind of interpolation to show areas of high-occurrence of a specific phenomenon. Internally Kriging is used, and then only contour lines are shown of the interpolation. Multiple lines are suggested to indicate the probibalistic interpretation of the lines.
}
\value{
A plot is produces with the different phenomena in space surrounded by lines. When multiple options are possible at each point then pie charts are added.
}
\author{
Michael Cysouw <cysouw@mac.com>
}
\examples{
# isogloss example
# choose one feature from hessen dataset (number 4)
data(hessen)
f4 <- hessen$data[,9:13]

# look for area for interpolation, changing density and grid parameters
# suitable parameters can be passed through to function levelMap below
boundary(hessen$villages, density = 0.1, grid = 10)

# useful size of pies has to be determined by changing cex
plot(hessen$boundary, main = NULL)
levelMap(hessen$villages, f4, draw = 3, cex = 0.8, normalize.frequency = TRUE
  , density = 0.1, grid = 10, add = TRUE, cex.legend = 0.5, scale.pies = TRUE)

\dontrun{
# another isogloss example:
# "f" sounds in German dialects in the words "Kochlöffel"
# might give Unicode-errors because of phonetic symbols
require(mapdata)
map("worldHires", "Germany", fill = TRUE, col = "grey90")

data(dialects)
levelMap(dialects$villages, dialects$data[,21], levels = c(0.20, 0.22, 0.24)
    , add = TRUE, position = "topleft")
title(main = "f-sound in \'Kochlöffel\'")
}

# semantic map example
# location of points via multidimensional scaling of complete data
data(haspelmath)
d <- dist(haspelmath)
p <- MASS::isoMDS(d)$points

# testing boundary parameters
boundary(p)
boundary(p, density = 0.004, box = 0.15, tightness = 8)

# labels to be plotted instead of points
text <- gsub("\\\.", "\n", rownames(haspelmath))

# show a few languages for Haspelmaths indefinite data
# using a quick dummy function to set all parameters
indef <- function(columns) {
  levelMap(p, haspelmath[,columns]
    , levels = 0.1, labels = text
    , density = 0.004, box = 0.15, tightness = 8
    , lambda = 0.1, note = FALSE)
}

oldpar <- par("mfcol")
par(mfcol = c(2,3))

indef(1:3)
indef(4:6)
indef(7:9)
indef(10:12)
indef(13:17)
indef(18:22)

par(mfcol = oldpar)

}
