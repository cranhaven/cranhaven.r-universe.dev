#' Plot Results of BRBVS Algorithm
#'
#' This function takes an object containing the results of the BRBVS algorithm
#' and plots the active features against their relative frequencies for the two time to events.
#'
#' @param object An object containing the results of the BRBVS algorithm.
#'               Expected to contain matrices `mtx.act1E` and `mtx.act2E` for feature names,
#'               vectors `freq.rel1E` and `freq.rel2E` for relative frequencies, and
#'               additional elements `Namecondings`, `metric`, `copula`, `margins` for feature decoding and plot annotations.
#' @param ... Additional arguments to be passed to plotting functions.
#'
#' @return A ggplot object representing the BRBVS algorithm results.
#'         Features are displayed on the x-axis and their relative frequencies on the y-axis.
#'         The plot is split into two facets for each survival.
#'
#'
#'
#' @importFrom ggplot2 ggplot aes geom_bar labs xlab ylab facet_grid theme ylim theme_classic
#' @importFrom stats setNames
#' @importFrom stats na.omit
#'
#' @export
#'
#' @examples
#'############################
#'# Toy Example
#'############################
#'BRBVS_object <- list(
#'mtx.act1E = c("z2", "z1", "0", "0"),
#'scores1E = c(20, 1, 1),
#'freq.rel1E = c(1, 1, 1, 1),
#'mtx.act2E = c("z2", "z1", "0", "0"),
#'scores2E = c(20, 1, 1),
#'freq.rel2E = c(1, 1, 1, 1),
#'metric = "FIM",
#'kmax = 4,
#'copula = "C0",
#'margins = c("PO", "PO"),
#'tau = 0.5,
#'Namecondings = matrix(
#'  c("z1", "z1", "z2", "z2", "z3", "z3", "z4", "z4", "z5", "z5", "z6", "z6"),
#'  nrow = 6, ncol = 2, byrow = TRUE,
#'  dimnames = list(NULL, c("Features", "Code"))
#')
#')
#'
#'class(BRBVS_object) <- "BRBVS"
#'plotBRBVS(BRBVS_object)
#'
plotBRBVS <- function(object, ...){
  # Vectorized replacement of '0' with NA and use of na.omit for compact filtering
  object$mtx.act1E <- stats::na.omit(ifelse(object$mtx.act1E == '0', NA, object$mtx.act1E))
  object$mtx.act2E <- stats::na.omit(ifelse(object$mtx.act2E == '0', NA, object$mtx.act2E))

  # Assuming lookup_feature_name function transforms feature names
  object$mtx.act1E <- unname(sapply(object$mtx.act1E, lookup_feature_name, object$Namecondings))
  object$mtx.act2E <- unname(sapply(object$mtx.act2E, lookup_feature_name, object$Namecondings))

  # Combine the two survival states into one data frame
  dta_plot <- data.frame(
    Features = c(object$mtx.act1E, object$mtx.act2E),
    Frequency = c(object$freq.rel1E[1:length(object$mtx.act1E)],
                  object$freq.rel2E[1:length(object$mtx.act2E)]) * 100,
    Survival = rep(c('First Survival', 'Second Survival'),
                   c(length(object$mtx.act1E), length(object$mtx.act2E)))
  )

  # Perform transformations
  dta_plot$Features <- gsub("z", "x", dta_plot$Features)
  dta_plot <- transform(dta_plot, Features = factor(Features, levels = unique(Features)))

  # Ordering of the data frame
  dta_plot <- dta_plot[order(dta_plot$Survival, -dta_plot$Frequency), ]

  # Constructing title and subtitle
  title <- 'Results BRBVS algorithm'
  subtitle <- sprintf('Metric: %s, Copula: %s, Margins: %s %s',
                      object$metric, object$copula,
                      object$margins[1], object$margins[2])

  # Plotting
  p <- ggplot(dta_plot, aes(x = Features, y = Frequency)) +
    geom_bar(stat = "identity", fill = "lightgray", color = "black") +
    labs(title = title, subtitle = subtitle, y = "%", x = "Features") +
    xlab('Features') +
    facet_grid(~Survival, scales = "free_x") +
    ylim(0, 100) +
    theme_classic() +
    theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, face = 'bold'),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_text(size = 14, face = "bold"),
      axis.text.y = ggplot2::element_text(size = 10, face = "bold")
    )
  return(p)
}
globals <- c("Features", "Frequency")
for(g in globals) {
  assign(g, NULL)
}
