#' VSD Options (Internal)
#'
#' Generates lists of lists of options for each graph type, from function call
#'
#' @param arguments Graph-specific arguments
#' @param ... General arguments
#' @keywords internal
#'
#' @return List of list of graph options
.options <- function(arguments = list(), ...)  {
  preset_ggpar <-
    list(
      main = NULL,
      title = NULL,
      submain = NULL,
      subtitle = NULL,
      xlab = "Time",
      legend.title = "Strata",
      size = 1,
      linetype = NULL,
      alpha = 1,
      color = NULL,
      palette = NULL,
      ggtheme = ggpubr::theme_pubr()
    )
  preset_ggsurv <-
    append(preset_ggpar,
           list(
             ylab = NULL,
             censor = NULL,
             censor.shape = NULL,
             censor.size = 4.5,
             conf.int = NULL
           ))
  preset_fit <- append(preset_ggsurv, list(
    conf.int.style = NULL
  ))
  preset_parametric <- append(preset_ggsurv, list(
    conf.int.km = FALSE
  ))
  preset_forest <-
    list(
      main = "Hazard ratio",
      title = NULL,
      cpositions = NULL,
      fontsize = NULL,
      refLabel = NULL,
      noDigits = NULL
    )
  preset_residuals <-
    append(
      preset_ggpar,
      list(
        ylab = NULL,
        resid = NULL,
        se = F,
        df = NULL,
        nsmo = NULL,
        var = NULL,
        point.col = NULL,
        point.size = NULL,
        point.shape = NULL,
        point.alpha = NULL,
        caption = NULL
      )
    )
  preset_hazard <-
    append(preset_ggpar,
           list(ylab = "Hazard rate"))

  preset <- list(
    fit = preset_fit,
    parametric = preset_parametric,
    forest = preset_forest,
    residuals = preset_residuals,
    hazard = preset_hazard
  )

  ellipsis <- list(...)

  for (type in names(preset)) {
    preset[[type]] <- .subset_options(preset[[type]], ellipsis, arguments[[type]])
  }

  return(preset)
}


#' VSD (Sub)Options (Internal)
#'
#' Agglutinates preset, ellipsis, and arguments under a graph type
#'
#' @param subset Graph-specific preset (and allowed) values
#' @param ellipsis General arguments
#' @param subarguments Graph-specific arguments
#' @keywords internal
#'
#' @return subset
.subset_options <- function(subset, ellipsis, subarguments = NULL) {


  # replaces preset with ellipsis arguments, only if they're already named with presets

  to_replace <- ellipsis[names(ellipsis) %in% names(subset)]
  subset[names(to_replace)] <- to_replace

  # does the same for the sublist in arguments named after the object
  if (is.list(subarguments)) {
    to_replace <- subarguments[names(subarguments) %in% names(subset)]
    subset[names(to_replace)] <- to_replace
  }

  # cleanup: main/submain to title/subtitle
  # ggsurv requires it to be title, ggpar allows title by default
  if (!is.null(subset$main) && "title" %in% names(subset)) {
    subset$title <- subset$main
    subset$main <- NULL
  }
  if (!is.null(subset$submain) && "subtitle" %in% names(subset)) {
    subset$subtitle <- subset$submain
    subset$submain <- NULL
  }

  # cleanup: remove NULL values
  subset[sapply(subset, is.null)] <- NULL

  return(subset)
}
