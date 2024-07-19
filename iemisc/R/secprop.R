#' Section Properties Calculator [GNU Octave/MATLAB]
#'
#' This function "is a short but really useful script that calculates the
#' section properties for an arbitrary shape with holes."
#' Reference: Caprani
#'
#'
#'
#'
#' @param outer_coord numeric matrix that contains the "outer coordinates (x, y)"
#'   Reference: Caprani
#' @param inner_coord numeric matrix that contains the "coordinates for a void"
#'   Reference: Caprani
#' @param original_plot integer vector that contains 0, 1 only. 0 represents
#'   do not print the original plot and 1 is for printing the original plot. The
#'   default value is 0 (no original plot).
#' @param final_plot integer vector that contains 0, 1 only. 0 represents do
#'   not print the final (transformed) plot and 1 is for printing the final
#'   (transformed) plot. The default value is 1 (final plot will be shown).
#'
#'
#' @return SP numeric vector that contains "A (Area), I (Second Moment of Area),
#'   yt and yb properties of the section."
#'   Reference: Caprani
#'
#'
#'
#'
#' @source
#' loops - r - foreach unable to find object within function - Stack Overflow answered by sumshyftw on Jun 7 2019. See \url{https://stackoverflow.com/questions/56498824/r-foreach-unable-to-find-object-within-function/56499056}.
#'
#'
#'
#' @references
#' Colin Caprani, "Section Properties Calculator", \url{https://www.colincaprani.com/programming/matlab/}.
#'
#'
#'
#'
#' @author Colin Caprani (secprop MATLAB function), Irucka Embry (secprop R function)
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#' @note
#' Note: Please refer to the iiemisc: secprop Example (R and GNU Octave)
#' vignette for the examples
#'
#'
#'
#'
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom data.table data.table
#' @importFrom foreach foreach %do%
#' @importFrom ggpubr ggtexttable ttheme ggarrange
#' @importFrom gsubfn list
#' @importFrom pracma zeros
#' @importFrom ramify mat
#'
#'
#' @export
secprop <- function (outer_coord, inner_coord, original_plot = c(0, 1), final_plot = c(0, 1)) {


ifelse(missing(original_plot), original_plot <- 0, original_plot <- original_plot)

ifelse(missing(final_plot), final_plot <- 1, final_plot <- final_plot)


original_plot <- original_plot

final_plot <- final_plot



# Check
assert_that(!any(qtest(outer_coord, "M+[0,)") == FALSE), msg = "An element within the outer_coord matrix is a string, empty, NA, NaN, Inf, and/or -Inf. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(!any(qtest(inner_coord, "M+[0,)") == FALSE), msg = "An element within the inner_coord matrix is a string, empty, NA, NaN, Inf, and/or -Inf. Please try again.")
# only process with finite values for inner_coord and provide an error message if the check fails

assert_that(qtest(original_plot, "N==1[0,1]"), msg = "plot should only be a single numeric value of 0 for no plot or 1 for a plot. Please try again.")
# only process with a single numeric value of 0 or 1 and provide an error message if the check fails

assert_that(qtest(final_plot, "N==1[0,1]"), msg = "plot should only be a single numeric value of 0 for no plot or 1 for a plot. Please try again.")
# only process with a single numeric value of 0 or 1 and provide an error message if the check fails


list[nOC, n] <- size(outer_coord)

list[nIC, n] <- size(inner_coord)

outer_coord <- rbind(outer_coord, outer_coord[1, , drop = FALSE])

inner_coord <- rbind(inner_coord, inner_coord[1, , drop = FALSE])



# for the original plot
if (original_plot == 1) {

ppoly1 <- data.table(x = outer_coord[, 1], y = outer_coord[, 2])

ppoly2 <- data.table(x = inner_coord[, 1], y = inner_coord[, 2])

p <- ggplot(ppoly1, aes(x = x, y = y)) + geom_polygon(fill = "red")

p <- p + geom_polygon(data = ppoly2, aes(x = x, y = y), fill = "white")

print(p)
#dev.off()
}




propsOC <- zeros(1, 3)

propsIC <- propsOC

propsOC <- algor(outer_coord)

if(nIC > 2) propsIC <- algor(inner_coord)

A <- propsOC[1] - propsIC[1]

Ia <- propsOC[2] - propsIC[2]

ybar <- propsOC[3] - propsIC[3]

ybar <- ybar / A

Ia <- Ia - A * ybar ^ 2

A <- 2 * A

Ia <- 2 * Ia

yt <- max(outer_coord[, 2, drop = FALSE]) - ybar

yb <- ybar

SP <- matrix(c(A, Ia, yt, yb), nrow = 4, ncol = 1)



# for the final plot
if (final_plot == 1) {
plot_outer_coord <- outer_coord[-nrow(outer_coord), ]

plot_inner_coord <- inner_coord[-nrow(inner_coord), ]

plot_outer_coord <- cbind(rbind(plot_outer_coord[, 1, drop = FALSE], -plot_outer_coord[seq(nOC, 2, by = -1), 1, drop = FALSE]), rbind(plot_outer_coord[, 2, drop = FALSE], plot_outer_coord[seq(nOC, 2, by = -1), 2, drop = FALSE]))

plot_inner_coord <- cbind(rbind(plot_inner_coord[, 1, drop = FALSE], -plot_inner_coord[seq(nIC, 2, by = -1), 1, drop = FALSE]), rbind(plot_inner_coord[, 2, drop = FALSE], plot_inner_coord[seq(nIC, 2, by = -1), 2, drop = FALSE]))

DT <- cbind(Parameters = c("Area", "Height to Neutral Axis", "Second Moment of Area", "Elastic Modulus, t", "Elastic Modulus, b"), Value = c(sprintf('%d', A), sprintf('%.1f', ybar * 10 / 10), sprintf('%e', Ia), sprintf('%e', Ia / yt), sprintf('%e', Ia / yb)), Units = c("length^2", "length", "length^4", "length^3", "length^3"))

poly1 <- data.table(x = plot_outer_coord[, 1], y = plot_outer_coord[, 2])

poly2 <- data.table(x = plot_inner_coord[, 1], y = plot_inner_coord[, 2])

p1 <- ggplot(poly1, aes(x = x, y = y)) + geom_polygon(fill = "red") + geom_polygon(data = poly2, aes(x = x, y = y), fill = "white")

p2 <- ggtexttable(DT, rows = NULL, theme = ttheme("classic"))

print(ggarrange(p1, p2, widths = c(2, 1)))

# dev.off()
}


# return must come at the end of the function
return(SP)
}





algor <- function (vc) {

  # initialize A, ybar, and Ia
  A <- 0

  ybar <- 0

  Ia <- 0

  # x is the first column of vc
  x <- vc[, 1, drop = FALSE]

  # y is the second column of vc
  y <- vc[, 2, drop = FALSE]

  # n is the length of x
  n <- length(x)

# Source 1 begins
foreach(i = 1:(n-1), .combine = 'c', .export = c("A", "ybar", "Ia", "x", "y")) %do% {

    .GlobalEnv$A <- A

    .GlobalEnv$ybar <- ybar

    .GlobalEnv$Ia <- Ia

    A <- A + 0.5 * (x[i] - x[i+1]) * (y[i] + y[i+1])

    ybar <- ybar + (1 / 6) * (x[i] - x[i+1]) * (y[i] ^ 2 + y[i] * y[i+1] + y[i+1] ^ 2)

    Ia <- Ia + (1 / 12) * (x[i] - x[i+1]) * (y[i] ^ 3 + y[i] ^ 2 * y[i+1] + y[i] * y[i+1] ^ 2 + y[i+1] ^ 3)
  }
# Source 1 ends

  props <- mat("A, Ia, ybar", eval = TRUE)

  return(props)
}
