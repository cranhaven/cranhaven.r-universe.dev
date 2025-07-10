#' Plots results from the selectSpecies function
#'
#' This function plots results (species probabilities/optimum solutions) from the selectSpecies() function
#'
#' @param result A saved object from function selectSpecies()
#' @param traits A matrix of trait values where traits are columns and rows are species. If one trait is provided, then the function creates a 2D barplot of probabilities for each species. If two traits are provided, then the function creates a 3D barplot that illustrates probabilities of species located within a 2D trait space.
#' @param colors An optional vector of colors for plotting that must include at least two valid color names. The default color scheme is a ramp palette of lightblue to blue.
#' @param xlim Vector of two numbers denoting limits on x-axis.
#' @param ylim Vector of two numbers denoting limits on y-axis.
#' @param xlab Character string to describe x-axis.
#' @param ylab Character string to describe y-axis.
#' @param zlab Character string to describe z-axis. The default axis names is "Probabilities".
#' @param distance An optional number denoting distance between bars in 3d plot.
#' @param cex.lab An optional number denoting the size of the labels. The default is set to 1.5.
#' @param box.col An optional setting for the color of the box. The default setting is transparent.
#' @param xbase The length of the base of each 3d bar along the x-axis
#' @param ybase The length of the base of each 3d bar along the y-axis
#' @param ... Additional arguments to pass to barplot() or lattice::cloud()
#' @return 2D barplot of probabilities for each species or 3D barplot that illustrates probabilities of species located within a 2D trait space
#' @export
#' @examples
#' ### 1 trait constraint with maximum functional diversity and entropy
#' Spp <- 5 #S = number of species
#' trait <- as.matrix(data.frame(trait=c(1:Spp)))
#' rownames(trait) <- c(letters[1:nrow(trait)])
#' result1 <- selectSpecies(t2c=trait, constraints=c(3.5), t2d=trait, obj="QH", capd=FALSE)
#' plotProbs(result1,trait, xlab="Trait")
#'
#' ##### 2 traits: Constrain trait X to value 2.5, diversify trait Y
#' traitX <- matrix(c(rep(1,3),rep(2,3),rep(3,3)))
#' traitY <- matrix(c(rep(c(1,2,3),3)))
#' rownames(traitX) <- c(letters[1:9]); colnames(traitX) <- c("traitX")
#' rownames(traitY) <- c(letters[1:9]); colnames(traitY) <- c("traitY")
#' result2 <- selectSpecies(t2c=traitX,constraints=c(traitX=2.5),t2d=traitY,capd=TRUE,obj="QH")
#' plotProbs(result2,traits = cbind(traitX, traitY))

plotProbs <- function (result, traits, colors = c("lightblue", "blue"), xlim = NULL, ylim = NULL,
                       xlab = NULL, ylab = NULL, zlab = "Probability", distance = 0.3, cex.lab = 1.5, box.col = "transparent",
                       xbase = 0.5, ybase = 0.5,...)
{
  res <- result$prob
  cols <- function(n) {
    (grDevices::colorRampPalette(colors = colors))(20)
  }
  if (ncol(traits) == "1") {
    graphics::barplot(t(res), col = colors[2],
                      names = rownames(res), ylab = ifelse(is.null(ylab), "Probability", ylab),
                      xlab = ifelse(is.null(xlab), "Trait X", xlab),...)
  }
  if (ncol(traits) == "2") {
    if (is.null(xlim)) xlim <- c(min(traits[,1]) - 1, max(traits[, 1]) + 1)
    if (is.null(ylim)) ylim <- c(min(traits[,2]) - 1, max(traits[, 2]) + 1)

    lattice::cloud(res ~ as.vector(t(traits[, 1])) + as.vector(t(traits[,2])), panel.3d.cloud = latticeExtra::panel.3dbars,
                   xbase = xbase, ybase = ybase, scales = list(arrows = FALSE,col = 1), perspective = TRUE,
                   distance = distance, xlim = xlim, ylim = ylim,
                   par.settings = list(axis.line = list(col = box.col)),
                   xlab = list(ifelse(is.null(xlab), "Trait X", xlab), rot = 50),
                   ylab = list(ifelse(is.null(ylab), "Trait Y", ylab), rot = -30),
                   zlab = list(zlab,rot = 90), screen = list(z = 55, x = -55),
                   col.facet = lattice::level.colors(res, at = lattice::do.breaks(range(res), 20), col.regions = cols, colors = TRUE),...)
  }
}
