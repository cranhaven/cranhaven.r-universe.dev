
#' Just a simple plot function
#'
#' Easy to do, but quicker to have it wrapped up in a plot.
#' @param census a tibble of census counts with columns `year` and
#' `age`, and then the counts of the different sexes in columns
#' named `male`, and `female`.
#' @return  `ggplot_census_by_year_age_sex()` returns a ggplot object which is a
#' stacked barplot with year on the x-axis,
#' counts on the y-axis with fill mapped to age.  It is facet-gridded
#' with sex in the columns and populations in the rows.
#' @export
#' @examples
#' # A single population example
#' g <- ggplot_census_by_year_age_sex(species_1_slurped_results$census_postkill)
#'
#' # a three-population example
#' g3 <- ggplot_census_by_year_age_sex(three_pops_with_mig_slurped_results$census_postkill)
ggplot_census_by_year_age_sex <- function(census) {
  g <- census %>%
    pivot_longer(
      cols = c(male, female),
      names_to = "sex",
      values_to = "n"
    ) %>%
    ggplot(
      aes(
        x = year,
        y = n,
        fill = as.factor(age)
      )
    ) +
    geom_col(colour = "black", size = 0.1) +
    facet_grid(pop ~ sex) +
    guides(
      fill = guide_legend(title = "Age")
    )

  g
}
