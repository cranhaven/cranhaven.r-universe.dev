#'@import ggplot2 graphics
#'@export
plot.d1classical <- function(x, ...){

  object <- x
  sm <- summary(object, print = FALSE)

  mtd <- as.numeric(object$mtd)
  data_plot <- data.frame(mtd)

  dens <- density(mtd)
  shade <- data.frame(x = dens$x, y = dens$y)

  label <- paste("Next dose:", round(sm$next_dose, 2))

  out <- ggplot(data_plot, aes_string(x = 'mtd')) + geom_density() +
    geom_vline(xintercept = as.numeric(sm$next_dose),
               linetype = 2, size = 1.2) +
    geom_ribbon(data = shade[shade$x > sm$hpd_dose[1] &
                               shade$x < sm$hpd_dose[2], ],
                aes_string(ymax = 'y', x = 'x'), ymin = 0, fill="red", alpha=0.3) +
    labs(y = "Density", x = "MTD") +
    annotate("text",
             x = sm$next_dose,
             y = max(shade$y)/2, hjust = -0.20,
             label = label) +
    theme_bw()

  return(out)
}

#'@import ggplot2 graphics
#'@export
plot.d1extended <- function(x, ...){

  object <- x
  sm <- summary(object, print = FALSE)

  mtd <- as.numeric(object$mtd)
  data_plot <- data.frame(x = mtd)

  dens <- density(mtd, n = 2^15)
  shade <- data.frame(x = dens$x, y = dens$y)

  label <- paste("Next dose:", round(sm$next_dose, 2))

  out <- ggplot(data_plot, aes_string(x = 'mtd')) + geom_density() +
    geom_vline(xintercept = as.numeric(sm$next_dose),
               linetype = 2, size = 1.2) +
    geom_ribbon(data = shade[
      shade$x > max(sm$hpd_dose[1], object$trial$min_dose) &
        shade$x < min(sm$hpd_dose[2], object$trial$max_dose), ],
      aes_string(ymax = 'y', x = 'x'), ymin = 0, fill = "red", alpha = 0.3) +
    labs(y = "Density", x = "MTD") +
    annotate("text", x = sm$next_dose,
             y = max(shade$y)/2, hjust = -0.20,
             label = label) +
    theme_bw()

  return(out)
}

#'@import ggplot2 graphics
#'@export
plot.d1ph <- function(x, ...){

  object <- x
  sm <- summary(object, print = FALSE)

  mtd <- as.numeric(object$mtd)
  data_plot <- data.frame(mtd)

  dens <- density(mtd)
  shade <- data.frame(x = dens$x, y = dens$y)

  label <- paste("Next dose:", round(sm$next_dose, 2))

  out <- ggplot(data_plot, aes_string(x = 'mtd')) + geom_density() +
    geom_vline(xintercept = as.numeric(sm$next_dose),
               linetype = 2, size = 1.2) +
    geom_ribbon(data = shade[shade$x > sm$hpd_dose[1] &
                               shade$x < sm$hpd_dose[2], ],
                aes_string(ymax = 'y', x = 'x'), ymin = 0, fill="red", alpha=0.3) +
    labs(y = "Density", x = "MTD") +
    annotate("text", x = sm$next_dose,
             y = max(shade$y)/2, hjust = -0.20,
             label = label) +
    theme_bw()

  return(out)
}

