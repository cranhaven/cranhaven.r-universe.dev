#' Internal sym.radar.plot the distence between two rows
#' @keywords internal
#' Radar Plot For Symbolic Interval Variables
#'
#' @param dat The symbolic data.
#' @param rad.main the title of the final plot (optional).
#' @param indivs an array that indicates which individuals to use (optional).
#' @param vars an array that indicates which variables to use (optional).
#' @param use.pct a logical value indicating if use percentage or real distance for plot.
#'
#' @return A radar plot.
#' @export
#' @import ggplot2
#' @import ggpolypath
#'
#'
sym.radar.plot <- function(dat, indivs, vars, rad.main = "", rad.legend = "Individuals", use.pct = F) {
  requireNamespace("ggpolypath")

  dat <- to.v2(dat)
  names. <- dat$sym.obj.names
  nom.vars <- dat$sym.var.names # Takes the names of the variables
  dat <- dat$data # Takes the dataset of the symbolic data.
  row.names(dat) <- names.
  if (!missing(indivs)) {
    dat <- dat[indivs, ] # If individuals given, take just that indiduals for the analysis.
  }
  if (!missing(vars)) {
    if (length(vars) <= 2) {
      stop("You must specify 3 or more variables in vars")
    }
    nom.vars <- nom.vars[vars] # If variables given, take just that variables names for the analysis.
    vars <- sort(c(2 * vars - 1, 2 * vars))
    dat <- dat[, vars] # If variables given, take just that variables for the analysis.
  }

  dat <- sym.radar.data(dat, nom.vars, use.pct) # Turn dataset to a molten data.
  # Plot the radar plot with the points of the intervals.
  res.radar <- ggplot2::ggplot(dat, ggplot2::aes(x = Variables, y = value)) +
    ggplot2::geom_point(ggplot2::aes(color = Individuals), size = 1) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "transparent"),
      plot.background = ggplot2::element_rect(fill = "transparent"),
      panel.grid.major = ggplot2::element_line(size = 0.5, linetype = "solid", colour = "#dddddd"),
      axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2)),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    ) +
    ggplot2::scale_y_continuous(limits = c(-0.3, 1.1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    ggplot2::ggtitle(rad.main) + ggplot2::xlab("") + ggplot2::ylab("") +
    coord_radar()

  # Lables with some values of the intervals are added.
  if (use.pct) {
    plots <- list()
    for (i in unique(dat$pos.var)) {
      res.radar <- res.radar +
        ggplot2::geom_text(ggplot2::aes_string(x = i, y = -0.1, label = round(min(dat[dat$pos.var == i, ]$real.value), 3)), size = 3, colour = "gray", family = "Arial") +
        ggplot2::geom_text(ggplot2::aes_string(x = i, y = 1.1, label = round(max(dat[dat$pos.var == i, ]$real.value), 3)), size = 3, colour = "gray", family = "Arial")
    }
  } else {
    res.radar <- res.radar +
      ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0, label = round(min(real.value), 3)), size = 3, colour = "gray", family = "Arial") +
      ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.25, label = inverse.rescale(0.25, real.value)), size = 3, colour = "gray", family = "Arial") +
      ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = inverse.rescale(0.5, real.value)), size = 3, colour = "gray", family = "Arial") +
      ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.75, label = inverse.rescale(0.75, real.value)), size = 3, colour = "gray", family = "Arial") +
      ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 1, label = round(max(real.value), 3)), size = 3, colour = "gray", family = "Arial")
  }
  # Paths which connect the points of the interval are added.
  res.radar + ggpolypath::geom_polypath(data = dat, aes(x = pos.var, y = value, fill = Individuals), alpha = 0.3, rule = "evenodd") +
    scale_fill_discrete(name = rad.legend) + scale_color_discrete(name = rad.legend)
}

#' Internal sym.radar.data
#' @keywords internal
#' Molten data for radar plot
#'
#' @param dat The symbolic data.
#' @param nom.vars Names of the variables.
#' @param use.pct a logical value indicating if use percentage or real distance for plot.
#'
#' @return A molten data.
#' @export
#' @importFrom reshape melt
#' @importFrom scales rescale
#'
sym.radar.data <- function(dat, nom.vars, use.pct = F) {

  n <- ncol(dat) # Number of variables (2 for each variable (minimum and maximum)).
  m <- n / 2 # Number of variables (Real number).
  datos.L <- dat[, seq(1, n, 2)] # Takes the minimum values of the dataset
  datos.R <- dat[, seq(2, n, 2)] # Takes the maximum values of the dataset
  colnames(datos.R) <- nom.vars # Names of the variables are placed to the dataset of minimum.
  colnames(datos.L) <- nom.vars # Names of the variables are placed to the dataset of maximum.
  datos.L <- reshape::melt(t(datos.L), varnames = c("Variables", "Individuals")) # Turn the dataset to a molten data
  datos.R <- reshape::melt(t(datos.R), varnames = c("Variables", "Individuals")) # Turn the dataset to a molten data
  datos.L <- datos.L[order(datos.L$Individuals, datos.L$Variables), ] # Sort the dataset
  datos.R <- datos.R[order(datos.R$Individuals, datos.R$Variables), ] # Sort the dataset
  datos.L <- cbind(datos.L, c(1:m)) # Add a column with the position of each variable
  datos.R <- cbind(datos.R, c(1:m)) # Add a column with the position of each variable
  colnames(datos.L)[ncol(datos.L)] <- "pos.var" # Give a name to the column
  colnames(datos.R)[ncol(datos.R)] <- "pos.var" # Give a name to the column
  # Merge the datasets and adds rows with the values of the first variable for each individual (It is necessary to the radar plot).
  dat <- rbind(datos.R, datos.R[seq(1, nrow(datos.R), m), ], datos.L, datos.L[seq(1, nrow(datos.L), m), ])
  rm(list = c(c("datos.L"), c("datos.R")))
  real.value <- dat$value
  if (use.pct) {
    for (name.var in unique(dat$Variables)) {
      # Rescale the values between 0 and 1 for each variable.
      dat$value[dat$Variables == name.var] <- scales::rescale(dat$value[dat$Variables == name.var], to = c(0, 1))
    }
  } else {
    dat$value <- scales::rescale(dat$value, to = c(0, 1)) # Rescale the values between 0 and 1 for all the dataset.
  }


  dat <- cbind(dat, real.value) # Add a column with the real values.

  # Fix the type of data if is not correctly.
  if (!is.factor(dat$Variables)) dat$Variables <- as.factor(dat$Variables)
  if (!is.factor(dat$Individuals)) dat$Individuals <- as.factor(dat$Individuals)
  if (!is.numeric(dat$value)) dat$value <- as.numeric(dat$value)
  if (!is.numeric(dat$pos.var)) dat$pos.var <- as.numeric(dat$pos.var)

  return(dat)
}

# Turns the rescale value to the real value.
inverse.rescale <- function(pct, values) {
  return(round((pct * (max(values) - min(values))) + min(values), 3))
}

# Function required to make the radar plot
coord_radar <- function(theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, direction = sign(direction), is_linear = function(coord) TRUE)
}
