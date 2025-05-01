#' Plot results of CalculateLocalSens function
#' @description Plot results of the \code{\link{CalculateLocalSens}} function.
#' @param local.out output from \code{\link{CalculateLocalSens}} function.
#' @param type a number to define the type of graphical output. \code{1}: importance index L1; \code{2}:  importance index L2; \code{3}: mean of sensitivity functions; \code{5}: minimum of sensitivity functions; and \code{5}: maximum of sensitivity functions; \code{6}: sensitivity functions and all importance indices are ploted. 
#' @param x.sens string with the name for the x axis.
#' @param y.sens string with the name for the y axis of the sensitivity functions (when \code{type = 6}).
#' @param y.ind string with the name for the y axis of the parameter importance indices.
#' @param bar.colors any valid specification of a color.
#' @param label.size a number to specify the size of axes labels and text.
#' @param x.axis.angle a number with angle of rotation for x axis text. Passed to \code{angle} argument of \code{\link{element_text}}. 
#' @details Font size of saved plots is usually different to the font size seen in graphic browsers. Before changing font sizes, see the final result in saved (or preview) plots.
#' @references Chang W (2012). R Graphics Cookbook. O'Reilly Media, Inc.
#' 
#' Soetaert K, Cash J and Mazzia F (2012). Solving differential equations in R. Springer.
#' 
#' Baquero, O. S., Marconcin, S., Rocha, A., & Garcia, R. D. C. M. (2018). Companion animal demography and population management in Pinhais, Brazil. Preventive Veterinary Medicine.
#' 
#' \url{http://oswaldosantos.github.io/capm}
#' @seealso \link[FME]{plot.sensFun}.
#' @export
#' @examples
#' ## IASA model#' 

#' ## Parameters and intial conditions.
#' data(dogs)
#' dogs_iasa <- GetDataIASA(dogs,
#'                         destination.label = "Pinhais",
#'                         total.estimate = 50444)

#' # Solve for point estimates.
#' solve_iasa_pt <- SolveIASA(pars = dogs_iasa$pars,
#'                          init = dogs_iasa$init,
#'                          time = 0:15,
#'                          alpha.owned = TRUE,
#'                          method = 'rk4')

#' ## Calculate local sensitivities to all parameters.
#' local_solve_iasa2 <- CalculateLocalSens(
#'  model.out = solve_iasa_pt, sensv = "n2")

#' ## Plot local sensitivities
#' PlotLocalSens(local_solve_iasa2)
#' 
PlotLocalSens <- function (local.out = NULL, x.sens = "Time", y.sens = "Sensitivity", y.ind = c("L1", "L2", "Mean", "Min", "Max"), bar.colors = "DarkRed", label.size = 10, x.axis.angle = 90, type = 1) {
  if (length(y.ind) != 5) {
    stop("The length of y.ind must be equal to 5.")
  }
  x <- value <- variable <- loc1 <- NULL
  loc2 <- loc3 <- loc4 <- loc5 <- NULL
  vplayout <- function(x, y) {
    viewport(layout.pos.row = x, layout.pos.col = y)
  }
  tmp <- gather(local.out[, -2], variable, value, -x)
  loc <- ggplot(tmp, aes(x = x, y = value, colour = variable)) + 
    geom_line(size = 0.5) + theme(legend.position = "none") + 
    xlab(x.sens) + ylab(y.sens)
  ordered.summary <- summary(local.out)[
    order(summary(local.out)[, "L1"], decreasing = T), ]
  tmp1 <- cbind(w = factor(rownames(ordered.summary),
                           levels = (rownames(ordered.summary))), ordered.summary)
  coln <- colnames(summary(local.out))[-c(1:2, 8)]
  x.axis.lab <- as.character(tmp1$w)
  x.axis.lab[which(x.axis.lab =='a')] <- 'a'
  options(warn = -1)
  x.axis.lab[which(x.axis.lab =='alpha')] <- expression(alpha)
  for (i in 1:length(coln)) {
    assign(paste0("loc", i),
           ggplot(tmp1, aes_string(x = "w", y = coln[i])) +
             geom_bar(stat = "identity",
                      fill = bar.colors,
                      colour = bar.colors) +
             theme_minimal() + 
             theme(legend.position = "none", 
                   axis.title.x = element_blank(), 
                   axis.text.x = element_text(size = label.size - 1, 
                                              angle = x.axis.angle, vjust = 0.5),
                   axis.title.y = element_text(size = label.size + 2),
                   axis.text.y = element_text(size = label.size)) + 
             ylab(y.ind[i]) +
             scale_x_discrete(labels = x.axis.lab))
  }
  if (type == 6) {
    options(warn = -1)
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(3, 2)))
    print(loc + theme(legend.position = "none"), vp = vplayout(1, 1))
    print(loc1, vp = vplayout(1, 2))
    print(loc2, vp = vplayout(2, 1))
    print(loc3, vp = vplayout(2, 2))
    print(loc4, vp = vplayout(3, 1))
    print(loc5, vp = vplayout(3, 2))
    options(warn = 0)
  }
  if (type == 0) {
    return(loc + theme(legend.position = "right"))
  }
  if (type == 1) {
    return(loc1)
  }
  if (type == 2) {
    return(loc2)
  }
  if (type == 3) {
    return(loc3)
  }
  if (type == 4) {
    return(loc4)
  }
  if (type == 5) {
    return(loc5)
  }
}