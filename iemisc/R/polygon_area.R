#' Polygon Area (using the Shoelace Formula)
#'
#' Calculate the area of a polygon using the shoelace formula.
#'
#'
#' @param x numeric vector that contains the x coordinates of the vertices.
#'   Regardless of rather the user starts in a clockwise or a counter-clockwise
#'   direction, the result will be positive.
#' @param y numeric vector that contains the y coordinates of the vertices.
#'   Regardless of rather the user starts in a clockwise or a counter-clockwise
#'   direction, the result will be positive.
#' @param plot integer vector that contains 0, 1 only. 0 represents
#'   do not plot the polygon and 1 is for plotting the polygon.
#' @param fill character vector that contains the inside color of the polygon.
#'   The possible colors are those that are accepted by ggplot2. The default
#'   fill color is black.
#' @param color character vector that the border color of the polygon. The
#'   possible colors are those that are accepted by ggplot2. The default fill
#'   color is black.
#'
#'
#'
#' @return the area of the polygon as a positive numeric vector.
#'
#'
#'
#'
#'
#' @references
#' John D Page, From Math Open Reference: Algorithm to find the area of a polygon. See \url{https://web.archive.org/web/20221006001150/https://www.mathopenref.com/coordpolygonarea2.html}. Provided by Internet Archive: Wayback Machine to avoid the connection timeout.
#'
#'
#'
#'
#'
#' @author John D Page for the JavaScript code, Irucka Embry (R code)
#'
#'
#'
#' @encoding UTF-8
#'
#'
#' @seealso \code{\link[pracma]{polyarea}} and \code{\link[geometry]{polyarea}}
#'
#
#'
#' @examples
#'
#' # Example 1 from Source 2
#'
#' library(iemisc)
#' 
#' x <- c(4,  4,  8,  8, -4, -4)
#' y <- c(6, -4, -4, -8, -8, 6)
#'
#' polygon_area(x, y, plot = 1)
#'
#' # compare with pracma's and geometry's polyarea
#' 
#' pracma::polyarea(x, y)
#'
#' geometry::polyarea(x, y)
#' 
#' 
#' 
#' 
#' 
#' # Example 2
#'
#' library(iemisc)
#' 
#' type38 <- construction_decimal("46'-10 1/2\"", result = "traditional", output = "vector")
#'
#' x38 <- c(0, 25, sum(25, type38, 10), sum(25, type38, 10, 25))
#'
#' y38 <- c(0, rep((3 + 1 / 3), 2), 0)
#'
#' polygon_area(x38, y38, plot = 1, fill = "darkseagreen3", color = "aquamarine4")
#'
#' # compare with pracma's and geometry's polyarea
#'
#' pracma::polyarea(x38, y38)
#' 
#' geometry::polyarea(x38, y38)
#' 
#' 
#' 
#' 
#' 
#' # Example 3
#'
#' install.load::load_package("iemisc", "data.table")
#'
#' coords <- fread("
#' X,   Y
#' 0,	0
#' 34,	4
#' 58,	4
#' 84,	6.7
#' 184,	0", header = TRUE)
#'
#' polygon_area(coords$X, coords$Y, plot = 1, color = "#00abff", fill = NA)
#' # "Use NA for a completely transparent colour." (from ggplot2 color function)
#'
#' # compare with pracma's and geometry's polyarea
#' 
#' pracma::polyarea(coords$X, coords$Y)
#' 
#' geometry::polyarea(coords$X, coords$Y)
#' 
#' 
#' 
#' 
#' 
#' # Example 4 from pracma
#'
#' library(iemisc)
#' 
#' Xx <- c(0, 4, 4, 0)
#'
#' Yy <- c(0, 0, 4, 4)
#'
#' polygon_area(Xx, Yy, 1, color = "goldenrod1", fill = "#00abff")
#'
#' # compare with pracma's and geometry's polyarea
#' 
#' pracma::polyarea(Xx, Yy)
#' 
#' geometry::polyarea(Xx, Yy)
#' 
#' 
#' 
#' 
#' 
#' # Example 5 from pracma
#'
#' library(iemisc)
#'
#' Xx1 <- c(0, 4, 2)
#'
#' Yy1 <- c(0, 0, 4)
#'
#' polygon_area(Xx1, Yy1, 1, color = "rosybrown", fill = "papayawhip")
#'
#' # compare with pracma's and geometry's polyarea
#' 
#' pracma::polyarea(Xx1, Yy1)
#'
#' geometry::polyarea(Xx1, Yy1)
#'
#'
#'
#'
#' @importFrom data.table data.table
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom ggplot2 ggplot aes geom_polygon
#' @importFrom foreach foreach %do%
#'
#' @export
polygon_area <- function (x, y, plot = c(0, 1), fill = NULL, color = NULL) {


plot <- plot

fill <- fill

color <- color


ifelse(is.null(fill), fill <- "black", fill <- fill)

ifelse(is.null(color), color <- "black", color <- color)

ifelse(is.na(fill), fill <- NA_character_, fill <- fill)

ifelse(is.na(color), color <- NA_character_, color <- color)


# check on x
assert_that(!any(qtest(x, "N>=3(,)") == FALSE), msg = "x is NA, NaN, Inf, -Inf, empty, or a string. Or, x has less than 3 values. The smallest polygon is a triangle with 3 vertices and sides. Please check the x values and try again.")
# only process with finite values and provide an error message if the check fails


# check on y
assert_that(!any(qtest(y, "N>=3(,)") == FALSE), msg = "y is NA, NaN, Inf, -Inf, empty, or a string. Or, y has less than 3 values. The smallest polygon is a triangle with 3 vertices and sides. Please check the y values and try again.")


# check on x and y
assert_that(length(x) == length(y), msg = "The length of x and y are not equal. Please check the x and y values and try again.")
# verify that x and y are equivalent lengths


assert_that(qtest(plot, "N==1[0,1]"), msg = "plot should only be a single numeric value of 0 for no plot or 1 for a plot. Please try again.")
# only process with a single numeric value of 0 or 1 and provide an error message if the check fails

assert_that(qtest(fill, "s==1"), msg = "There is not a fill type or more than 1 fill type.")
# only process with enough known variables and provide an error message if the check fails

assert_that(qtest(color, "s==1"), msg = "There is not a color type or more than 1 color type.")
# only process with enough known variables and provide an error message if the check fails


# for the original plot
if (plot == 1) {

# to plot the coordinates
coords <- data.table(X = x, Y = y)

p <- ggplot(coords, aes(X, Y))

p <- p + geom_polygon(fill = fill, color = color)

print(p)
# dev.off()
}



points <- length(x)

area <- 0
# Accumulates area in the loop (Source 1)


i <- 0

j <- points


area <- foreach(i = seq(x), .combine = "+") %do% {

j0 <- j

j <- i

(x[j0] + x[i]) * (y[j0] - y[i])

}


area1 <- abs(area * 0.5)

return(area1)
}
