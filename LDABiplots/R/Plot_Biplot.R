#' @title Plotting Biplot
#' @description \code{Plot_Biplot} initializes a ggplot2-based visualization of the caracteristics presented in the data analized by the Biplot selected.
#' @usage Plot_Biplot(X, axis = c(1,2), hide = "none",
#'  labels = "auto", ind.shape = 19,
#'  ind.color = "red", ind.size = 2,
#'  ind.label = FALSE, ind.label.size = 4,
#'  var.color = "black", var.size = 0.5,
#'  var.label = TRUE, var.label.size = 4, var.label.angle = FALSE)
#' @param X List containing the output of one of the functions of the package.
#' @param axis Vector with lenght 2 which contains the axis ploted in x and y axis.
#' @param hide Vector specifying the elements to be hidden on the plot. Default value is “none”. Other allowed values are “ind” and “var”.
#' @param labels It indicates the label for points. If it is "auto" the labels are the row names of the coordinates of individuals. If it isn't auto it would be a vector containing the labels.
#' @param ind.shape Points shape. It can be a number to indicate the shape of all the points or a factor to indicate different shapes.
#' @param ind.color Points colors. It can be a character indicating the color of all the points or a factor to use different colors.
#' @param ind.size Size of points.
#' @param ind.label Logical value, if it is TRUE it prints the name for each row of X. If it is FALSE (default) does not print the names.
#' @param ind.label.size Numeric value indicating the size of the labels of points.
#' @param var.color Character indicating the color of the arrows.
#' @param var.size Size of arrow.
#' @param var.label Logical value, if it is TRUE (default) it prints the name for each column of X. If it is FALSE does not print the names.
#' @param var.label.size Numeric value indicating the size of the labels of variables.
#' @param var.label.angle Logical value, if it it TRUE (default) it print the vector names with orentation of the angle of the vector. If it is FALSE the angle of all tags is 0.
#' @return Return a \code{\link{ggplot2}} object.
#' @seealso \code{\link{HJBiplot}}
#' @examples
#' hj.biplot <- HJBiplot(mtcars)
#' Plot_Biplot(hj.biplot, ind.label = TRUE)
#' @import ggplot2
#' @import ggrepel
#' @export
Plot_Biplot <- function(X, axis = c(1,2), hide = "none",
                        labels = "auto", ind.shape = 19,
                        ind.color = "red", ind.size = 2,
                        ind.label = FALSE, ind.label.size = 4,
                        var.color = "black", var.size = 0.5,
                        var.label = TRUE, var.label.size = 4, var.label.angle = FALSE
){
  #### Checkin functions ####
  #### 1. Params ####
  #### >>Axis ploted #####
  axis.x <- axis[1]
  axis.y <- axis[2]
  #### >>Axis labels ####
  PCs <- names(X$eigenvalues)
  var1 <- paste(c("(", X$explvar[axis.x], "%", ")"), collapse = "")
  var2 <- paste(c("(", X$explvar[axis.y], "%", ")"), collapse = "")
  eje1 <- paste(PCs[axis.x], var1)
  eje2 <- paste(PCs[axis.y], var2)
  #### >>Axis names ####
  # It is used to indicate the columns to plot
  x.var <- colnames(X$coord_ind)[axis.x]
  y.var <- colnames(X$coord_ind)[axis.y]
  #### >>Subsect variables ####
  selection <- (X$coord_var[, x.var] == 0) & (X$coord_var[, y.var] == 0)
  new_coord_var <- subset(X$coord_var, subset = !selection)
  ##### 2. Plot ####
  #### >>Empty plot ####
  biplot <-
    ggplot() +
    #theme_minimal() +
    #### >>Draw axis ####
  geom_hline(
    yintercept = 0
  ) +
    geom_vline(
      xintercept = 0
    ) +
    #### >>Axis labels ####
  labs(
    x = eje1,
    y = eje2
  )
  #### 3. Plot points ####
  if(!"ind" %in% hide){
    #### >>Shape params ####
    if(length(ind.shape) == 1){
      shape.aes <- factor(1)
    } else {
      shape.aes <- ind.shape
      hide.point.shape <- "legend"}
    #### >>Add points ####
    biplot <-
      biplot +
      geom_point(
        aes(x = X$coord_ind[, x.var],
            y = X$coord_ind[, y.var],
            colour = ind.color,
            shape = shape.aes
        ),
        size = ind.size
      )
    #### >>Change shape ####
    if(length(ind.shape) == 1){
      hide.point.shape <- FALSE
      biplot <-
        biplot +
        scale_shape_manual(values = ind.shape)
    }
  } else {
    hide.point.color <- FALSE
    hide.point.shape <- FALSE
  }
  #### >>Point names ####
  if (isTRUE(ind.label)){
    # Labels
    if(unique(length(labels) == 1) & unique(labels == "auto")){
      label.used <- rownames(X$coord_ind)
    } else {
      label.used <- labels
    }
    # Add point names
    biplot <-
      biplot +
      geom_text_repel(
        aes(
          x = X$coord_ind[, x.var],
          y = X$coord_ind[, y.var],
          label = label.used,
          colour = ind.color),
        size = ind.label.size
      )
  }
  if(!"ind" %in% hide|isTRUE(ind.label)){
    #### >>Colors ####
    if(length(ind.color) == 1){
      hide.point.color <- FALSE
      biplot <-
        biplot +
        scale_colour_manual(values = ind.color)
    } else {
      if(is.factor(ind.color)) {
        hide.point.color <- "legend"
      } else {
        hide.point.color <- "colorbar"
      }
    }
  }
  ##### 4. Plot arrows #####
  if(!"var" %in% hide){
    #### >>Add arrows ####
    biplot <- biplot +
      geom_segment(
        aes(x = 0,
            y = 0,
            xend = new_coord_var[, x.var],
            yend = new_coord_var[, y.var]
        ),
        arrow = arrow(length = unit(0.5, "cm")),
        colour = var.color,
        size = var.size
      )
    #### >>Angle names ####
    ifelse(
      var.label == TRUE & var.label.angle == TRUE,
      #Angulo para los nombres de las variables
      angle <- atan(new_coord_var[, y.var] / new_coord_var[, x.var]) * 360 / (2 * pi),
      angle <- rep(0, nrow(new_coord_var))
    )
    #### >>Vector names ####
    if(var.label == TRUE){
      biplot <-
        biplot +
        geom_text_repel(
          aes(
            x = new_coord_var[, x.var],
            y = new_coord_var[, y.var],
            label = rownames(new_coord_var),
            angle = angle,
            hjust = ifelse(new_coord_var[, x.var] > 0, 1, 0),
            vjust = ifelse(new_coord_var[, y.var] > 0, 1, 0)
          ),
          col = var.color,
          size = var.label.size,
          box.padding = 0.1,
          point.padding = 0.1
        )
    }
  }
  #### 5. Legend ####
  biplot <- biplot +
    guides(
      colour = hide.point.color,
      shape = hide.point.shape
    )
  biplot
}