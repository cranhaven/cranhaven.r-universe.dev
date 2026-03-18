#' @title Plot the balance
#' @description Visualize balance of variables between treatment and control groups. Balance plot reflects balance in standardized units.
#'
#' @param .data dataframe
#' @param treatment the column denoted treatment. Must be binary.
#' @param confounders character list of column names denoting the X columns of interest
#' @author Joseph Marlo
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' data(lalonde)
#' plot_balance(lalonde, 'treat', c('re78', 'age', 'educ')) + labs(title = 'My new title')
plot_balance <- function(.data, treatment, confounders){

  if (length(table(.data[[treatment]])) != 2) stop("treatment must be binary")

  p <- .data %>%
    dplyr::select(all_of(c(confounders, treatment))) %>%
    pivot_longer(cols = -treatment) %>%
    group_by(name) %>%
    mutate(value = base::scale(value)[,1]) %>%
    group_by(across(c('name', treatment))) %>%
    summarize(mean = mean(value, na.rm = TRUE),
              .groups = 'drop') %>%
    group_by(name) %>%
    summarize(diff = mean - lag(mean),
              .groups = 'drop') %>%
    na.omit() %>%
    ggplot(aes(x = diff, y = name, color = abs(diff))) +
    geom_vline(xintercept = 0, linetype = 'dashed', color = 'gray60') +
    geom_point(size = 4) +
    scale_colour_gradient(low = 'gray30', high = 'red3') +
    labs(title = 'Treatment and control balance',
         subtitle = 'Zero indicates perfect balance across treatment and control groups',
         x = 'Scaled mean difference',
         y = NULL,
         color = NULL) +
    theme(legend.position = 'none')

 return(p)
}
