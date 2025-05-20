#' Summarize annual sex-and-age-specific survival rates from the census information
#'
#' The prekill census in year t+1 is the post-kill census in year t, so
#' we can use the prekill census to record the realized fraction of individuals
#' of each age and sex that survived the death episode in each year.  In the
#' output survival in year t is the fraction of j-year olds in year t that
#' survive to be j+1 year-olds in year t+1.
#'
#' This function does not track migrants.  Another one is eventually
#' in order that accounts for migrants out of the population.  Also,
#' the plots here might not play well with multiple populations.
#' @param census a tibble of census counts with columns `year` and
#' `age`, and then the counts of the different sexes in columns
#' named `male`, and `female`.
#' @param fem_surv_probs a vector of the parameters used for the simulation.  If present
#' these are put on the histogram plots. If you provide one of these, you have to provide both.
#' @param male_surv_probs a vector of the parameters used for the simulation.  If present
#' these are put on the histogram plots.
#' @param nbins number of bins for the histograms
#' @return A list with components:
#' - `survival_tibble`:  A tibble with the following columns:
#'    - `year`: The year
#'    - `pop`: The population whose census is being counted
#'    - `age`: The age of individuals
#'    - `sex`: The sex of individuals
#'    - `n`: The number of individuals alive and present of sex `sex` and age `age` in year
#'      `year` in pop `pop`.
#'    - `cohort`: The birth year of these individuals
#'    - `surv_fract`: The fraction of the n individuals that survive to have age `age + 1` in
#'      year `year + 1`.
#' - `plot_histos_by_age_and_sex`: A ggplot object of histograms of observed survival fractions
#'   facet-wrapped by age and sex.  Blue vertical lines are the observed means and dashed vertical
#'   red lines are the expected values given the simulation parameters.
#' @export
#' @examples
#' result <- summarize_survival_from_census(
#'   species_1_slurped_results$census_prekill,
#'   species_1_life_history$`fem-surv-probs`,
#'   species_1_life_history$`male-surv-probs`
#' )
#'
#' # print the results if you want
#' result$survival_tibble
#' result$plot_histos_by_age_and_sex
#'
summarize_survival_from_census <- function(
  census,
  fem_surv_probs = NULL,
  male_surv_probs = NULL,
  nbins = 10
) {

  # compute the survival fractions
  surv_fracts <- census %>%
    pivot_longer(
      cols = c(male, female),
      names_to = "sex",
      values_to = "n"
    ) %>%
    mutate(cohort = year - age) %>%
    arrange(pop, sex, cohort, year, age) %>%
    group_by(pop, sex, cohort) %>%
    mutate(
      surv_fract =
        replace_na(
          data = lead(n) / n,
          replace = 0
        )
    ) %>%
    ungroup() %>%
    filter(surv_fract <= 1.0) # there are some at the beginning that are wonky

  num_age_classes <- max(surv_fracts$age)

  if(xor(is.null(fem_surv_probs), is.null(male_surv_probs))) {
    stop("Sorry! You have to provide both male_surv_probs and fem_surv_probs or neither.  Not just one of them")
  }
  if(!is.null(fem_surv_probs)) {
    stopifnot(length(fem_surv_probs) == num_age_classes)
    stopifnot(length(male_surv_probs) == num_age_classes)
    surv_param <- tibble(
      age = (1:length(fem_surv_probs)) - 1L,
      female = fem_surv_probs,
      male = male_surv_probs
    ) %>%
      pivot_longer(
        cols = c(male, female),
        names_to = "sex",
        values_to = "surv_param"
      )
  }


  # compute age means:
  age_means <- surv_fracts %>%
    group_by(sex, age) %>%
    summarise(mean_surv = mean(surv_fract))

  # make the histogram figure
  g <- ggplot(surv_fracts %>% filter(surv_fract > 0)) +
    geom_histogram(
      aes(x = surv_fract),
      bins = nbins,
      fill = "white",
      colour = "black",
      size = 0.2
    ) +
    geom_vline(
      data = age_means %>% filter(mean_surv > 0),
      mapping = aes(xintercept = mean_surv),
      colour = "blue"
    ) +
    facet_wrap(age ~ sex)

  if(!is.null(male_surv_probs)) {
    g <- g +
      geom_vline(
        data = surv_param,
        mapping = aes(xintercept = surv_param),
        colour = "red",
        linetype = "dashed"
      )
  }



  list(
    survival_tibble = surv_fracts,
    plot_histos_by_age_and_sex = g
  )

}
