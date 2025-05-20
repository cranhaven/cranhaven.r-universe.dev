
#' Count and plot the number of mates each individuals has produced offspring with
#'
#' This just tallies up the information from the pedigree.  It will plot things
#' faceted by pop (over rows) and sexes (over columns).
#' @param P the pedigree from the simulation, like that returned in the `pedigree` component
#' of the list returned by `slurp_spip()`.
#' @export
#' @return A list with two components with names:
#'    - `mate_counts`: A tibble with information about the number of mates with which
#'    a parent produced offspring each year.  It has the columns:
#'        * `sex`: the sex of this parent
#'        * `year`: the year during which the mating occurred
#'        * `pop`: the population this parent was in
#'        * `parent`: the ID of the parent
#'        * `num_offs`: the number of offspring this parent had in total
#'        * `num_mates`: the number of mates this parent had
#'    - `plot_mate_counts`: a ggplot object, faceted on a grid by population in columns
#'    and sex in rows. The x-axis is the number of offspring (in a season), the y-axis is the
#'    number of mates in a season, and the fill color of the grid gives the number of parents
#'    with that number of offspring and mates.
#' @examples
#' result <- count_and_plot_mate_distribution(three_pops_no_mig_slurped_results$pedigree)
#'
#' # have a look at the results:
#' result$mate_counts
#'
#' result$plot_mate_counts
#'
count_and_plot_mate_distribution <- function(P) {

  P2 <- P %>%
    filter(ma != "0" & pa != "0")

  fem_counts <- P2 %>%
    group_by(year, pop, ma) %>%
    summarise(
      num_offs = n(),
      num_mates = n_distinct(pa)
    ) %>%
    ungroup() %>%
    rename(parent = ma)

  male_counts <- P2 %>%
    group_by(year, pop, pa) %>%
    summarise(
      num_offs = n(),
      num_mates = n_distinct(ma)
    ) %>%
    ungroup() %>%
    rename(parent = pa)

  mate_counts <- bind_rows(
    list(
      female = fem_counts,
      male = male_counts
    ),
    .id = "sex"
  )

  # Now, plot this.   Because the values are discrete, we
  # will do it as tiles.
  for_tiles <- mate_counts %>%
    count(pop, sex, num_offs, num_mates)

  gft <- ggplot(
    for_tiles,
    aes(
      x = num_offs,
      y = num_mates,
      fill = n
    )
  ) +
    geom_tile() +
    facet_grid(sex ~ pop) +
    scale_fill_viridis_c(trans = "log10") +
    xlab("Number of offspring (in a season)") +
    ylab("Number of mates (in a season)")


  # return
  list(
    mate_counts = mate_counts,
    plot_mate_counts = gft
  )

}
