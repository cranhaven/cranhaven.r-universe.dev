#' Returns summary statistics of Financial variables
#'
#' summary_stat allows to get summary statistics for financial variables: it is possible to get the mean, the standard deviation and the spatial autocorrelation
#'
#' @param data Dataset of class 'data.frame'. Specify the dataset obtained from the retrieving and the \code{merge_data} functions.
#' @param corr Logical.  By default set to \code{FALSE}. If spatial autocorrelation is of interest set to \code{TRUE}. To compute the spatial autocorrelation point geometries are required.
#' @param variable character. Specify the name of the variable for which to obtain the spatial autocorrelation.
#' @param d1 See \code{\link{dnearneigh}} function for details.
#' @param d2 See \code{\link{dnearneigh}} function for details.
#' @param plot Logical. By default set to \code{FALSE}. If Moran plot for spatial autocorrelation is of interest set the argument to \code{TRUE}.
#'
#' @returns print summary statistics
#' @import magrittr
#' @import knitr
#' @import spdep
#' @importFrom ("stats", "na.omit", "reorder")
#' @importFrom rlang .data
#' @import PublicWorksFinanceIT
#' @author Lorena Ricciotti
#' @examples
#' data(OBDAPpoint)
#' summary_stat(OBDAPpoint, corr = T, variable = "EuFunding", d1 = 0, d2 = 2)
#'
#'@export
summary_stat <- function(data, corr = FALSE, variable, d1, d2, plot = FALSE) {

  # Filter Financial columns only
  data <- sf::st_as_sf(data, wkt = "geom")
  idx <- grep("Funding|Finance", names(data))
  data <- data %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(idx), ~ as.numeric(.))) %>%
    dplyr::select(dplyr::all_of(idx))
  # %>%
  #   dplyr::mutate(
  #     dplyr::across(
  #       dplyr::everything(),
  #       ~ tidyr::replace_na(., 0)))


  # Calculate mean and standard deviation
  stats_summary <- data %>% sf::st_drop_geometry() %>%
    dplyr::summarise(dplyr::across(dplyr::everything(),
                     list(mean = ~ mean(., na.rm = TRUE),
                          sd = ~ sd(., na.rm = TRUE)),
                     .names = "{.col}.{.fn}")) %>%
    tidyr::pivot_longer(
                        cols = dplyr::everything(),
                        names_to = c("variable", "statistic"),
                        names_sep = "\\.") %>%
    tidyr::pivot_wider(
      names_from = .data$statistic,
      values_from = .data$value)

  results <- list()
  results$variable <- stats_summary$variable
  results$mean <- stats_summary$mean
  results$sd <- stats_summary$sd

  # Display the summary statistics
  cat("### Summary Statistics:\n")
  tab <- stats_summary %>%
    knitr::kable(
      caption = "Mean and Standard Deviation of Financial Variables",
      col.names = c("Variable", "Mean", "Standard Deviation"),
      digits = 2,
      format = "markdown"
    )
  print(tab)

if(corr == TRUE){

  #SPATIAL AUTOCORRELATION
  neighbors <- spdep::dnearneigh(data, d1, d2)
  listw <- spdep::nb2listw(neighbors)
  res <- spdep::moran.test(data[[variable]], listw)

  results$moran.test <- res
  print(res)

  if(plot == TRUE){
  p <- spdep::moran.plot(data[[variable]], listw, ylab = paste("Spatially lagged", variable), xlab = variable)
  print(p)
  }
}
  return(results)
}
