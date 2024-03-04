#' Thresholds for shading of plots
#' 
#' @title Definition of alpha values and thresholds for plotting ellipse confidence intervals
#' @details Matrix of alpha values. The column names should be character strings 
#' giving the maximum value taken by upDown.sd*degrees.sd for each column. The 
#' rownames should be character strings giving the maximum numeric value taken by
#' the duration for each row.
#' @export 
#' @keywords internal

thresholds <- matrix(data = c(.5, .35, .2, .05, .65, .5, .35, .2, .8, .65, .5, .35, .95, .8, .65, .5), 
    nrow = 4, ncol = 4, byrow = TRUE, 
    dimnames = list (c("95", "240", "720", "INF"), c("25", "60", "700", "INF")))

