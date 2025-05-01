#' Distribution of households according to the number of inhabitants one or more species
#' @description Dodged bar plot of the distribution of households according to the number of inhabitants of one or more species.
#' @param dat \code{\link{data.frame}} with households as observation unit and columns with the number of individuals of the species of interest.
#' @param  species names or positions of columns with species data.
#' @param proportion \code{\link{logical}}. If \code{TRUE} (default), the y axis will represent proportions, if \code{FALSE}, it would represent raw counts.
#' @param x.label title for x axis.
#' @param y.label title for y axis.
#' @param legend \code{\link{logical}}. If \code{TRUE} (default), the legend will be showed, if \code{FALSE}, it will be removed.
#' @references Baquero, O. S., Marconcin, S., Rocha, A., & Garcia, R. D. C. M. (2018). Companion animal demography and population management in Pinhais, Brazil. Preventive Veterinary Medicine.
#' @seealso \link[ggplot2]{geom_bar}.
#' @export
#' @examples
#' data(cluster_sample)
#' PlotHHxSpecies(cluster_sample, c("number_of_persons",
#'                                  "number_of_dogs",
#'                                  "number_of_cats"))
#' 
PlotHHxSpecies <- function(dat = NULL, species = NULL, proportion = TRUE, x.label = "Individuals per household", y.label = "Proportion of households", legend = TRUE) {
  
  # Workaround to the "no visible binding for global variable" note.
  Category <- y <- Species <- NULL
  
  freqs <- data.frame(Category = integer(),
                      Count = integer(),
                      Proportion = integer())
  rows <- c()
  dat <- as.data.frame(dat)
  if (is.numeric(species)) {
    species <- names(dat[, species])
  }
  for (i in 1:length(species)) {
    tmp <- FreqTab(dat[, species[i]])
    freqs <- rbind.data.frame(freqs, tmp)
    rows <- c(rows, nrow(tmp))
  }
  freqs$Species <- rep(species, rows)
  freqs
  if (proportion) {
    freqs <- freqs[, -2]
  } else {
    freqs <- freqs[, -3]
  }
  names(freqs)[2] <- "y"
  freqs$Category <- factor(as.character(freqs$Category),
                           levels = sort(as.integer(levels(freqs$Category))))
  plt <- ggplot(freqs, aes(Category, y, fill = Species)) +
    geom_bar(stat = "identity", position = "dodge") +
    xlab(x.label) +
    ylab(y.label) +
    theme_minimal()
  if (legend) {
    return(plt)
  } else {
    plt +
      theme(legend.position = "none")
  }
}
