#' Summarize the distribution of number of offspring and number of mates
#'
#' More later
#' @param census_postkill a tibble with the postkill numbers of individuals.  This
#' is here so we know the total number of individuals that could have reproduced in a given
#' year.
#' @param pedigree a tibble with columns of `year`, `kid`, `pa`, and `ma`. The IDs of the'
#' individuals must by like MX_Y where M means male, X is the birth year, and Y is the unique
#' ID number.
#' @param deaths a tibble with columns of `ID`, `year`, and `age`, giving the years and ages at which
#' different individuals died.
#' @param lifetime_hexbin_width a vector of length two.  The first element is the width in the
#' age direction of each hexbin and the second is the width in the lifetime number of offspring
#' direction for the `plot_lifetime_output_vs_age_at_death` output plot.
#' @param contrib_bin_width width of bins of histogram of contribution of parents of
#' each age and sex to the offspring.
#' @export
#' @return A list with three components, each of them a ggplot object:
#'    - `plot_age_specific_number_of_offspring`: a ggplot object that plots boxplots and jittered points.
#'      The x-axis are the ages of the individuals; the y-axis shows the number of offspring. Summarized
#'      over the entire spip simulation. This is faceted by sex.
#'    - `plot_lifetime_output_vs_age_at_death`: a ggplot object.  This is a hexbin plot. The x-axis
#'      are age-at-death bins, the y axis are bins of total number of offspring produced in a lifetime.
#'      The fill color of each bin gives the number of individuals with that age at death and number
#'      of offspring encountered over the whole simulation.  Plot is faceted by sex.
#'    - `plot_fraction_of_offspring_from_each_age_class`: a ggplot object. This shows the distribution
#'      over all years of the simulation, of the fraction of offspring produced each year that were
#'      produced by males or females of a given age (the plots are facet-wrapped by both age and sex).
#'      The blue vertical line gives the mean.
#' @examples
#' # get stored slurped output for an example
#' X <- species_1_slurped_results
#' g <- summarize_offspring_and_mate_numbers(
#'   X$census_postkill,
#'   X$pedigree,
#'   X$deaths
#' )
#'
#' # Now g is a list holding three plots, accessible like this:
#'
#' # g$plot_age_specific_number_of_offspring
#'
#' # g$plot_lifetime_output_vs_age_at_death
#'
#' # g$plot_fraction_of_offspring_from_each_age_class
#'
summarize_offspring_and_mate_numbers <- function(
  census_postkill,
  pedigree,
  deaths,
  lifetime_hexbin_width = c(1,1),
  contrib_bin_width = 0.01
) {

  # first, we need to just wrangle the data to make pa_year and ma_year columns
  ped2 <- pedigree %>%
    filter(ma != "0" & pa != "0") %>%
    rename(
      kid_year = year,
      kid_id = kid,
      pa_id = pa,
      ma_id = ma
    ) %>%
    extract(
      pa_id,
      into = "pa_year",
      regex = "M([0-9]+)_",
      remove = FALSE,
      convert = TRUE
    ) %>%
    extract(
      ma_id,
      into = "ma_year",
      regex = "F([0-9]+)_",
      remove = FALSE,
      convert = TRUE
    ) %>%
    mutate(trio = 1:n()) %>%
    select(pop, trio, kid_year, pa_year, ma_year, kid_id, pa_id, ma_id) %>%
    mutate(
      pa_age = kid_year - pa_year,
      ma_age = kid_year - ma_year
    )

  # offspring from each parent by age and year
  dads_y <- ped2 %>%
    count(pop, kid_year, pa_age, pa_id)
  moms_y <- ped2 %>%
    count(pop, kid_year, ma_age, ma_id)

  ### This blcok is a bunch of stuff that I ended up not needing for the final plots I wanted to make ###
  if(FALSE) {
    # then we also have to use the census_postkill information to know the
    # number of males and females still alive that produced no offspring.
    # First we need the number of dads and moms with at least one offspring,
    # each year, and of each age.  This is quite the hassle, but we end up with
    # a tibble showing how many males (or females) had nkids = 0.
    zero_males <- dads_y %>%
      group_by(pop, kid_year, pa_age) %>%
      summarise(n_dads = n_distinct(pa_id)) %>%
      ungroup %>%
      left_join(
        x = census_postkill %>% select(-female),
        y = .,
        by = c(
          "pop" = "pop",
          "year" = "kid_year",
          "age" = "pa_age"
        )
      ) %>%
      mutate(n_dads = replace_na(n_dads, 0)) %>%
      mutate(
        nkids = 0,
        n = male - n_dads
      ) %>%
      select(pop, year, age, nkids, n)
    zero_females <- moms_y %>%
      group_by(pop, kid_year, ma_age) %>%
      summarise(n_moms = n_distinct(ma_id)) %>%
      ungroup %>%
      left_join(
        x = census_postkill %>% select(-male),
        y = .,
        by = c(
          "pop" = "pop",
          "year" = "kid_year",
          "age" = "ma_age"
        )
      ) %>%
      mutate(n_moms = replace_na(n_moms, 0)) %>%
      mutate(
        nkids = 0,
        n = female - n_moms
      ) %>%
      select(pop, year, age, nkids, n)

    # now we count up how many males and females had nkids with nkids > 1
    dads_n <- dads_y %>%
      rename(nkids = n, year = kid_year, age = pa_age) %>%
      count(pop, year, age, nkids) %>%
      mutate(parent = "pa")
    moms_n <- moms_y %>%
      rename(nkids = n, year = kid_year, age = ma_age) %>%
      count(pop, year, age, nkids) %>%
      mutate(parent = "ma")
  }

  # put dads_y and mom_y into a single data frame to plot.
  num_offs <- bind_rows(
    male = dads_y %>%
      rename(
        age = pa_age,
        id = pa_id
      ),
    female = moms_y %>%
      rename(
        age = ma_age,
        id = ma_id
      ),
    .id = "sex"
  )

  # we need to add explicit 0s in there for some ages to get the colors
  # consistent with the census plots
  bzs <- num_offs %>%
    group_by(
      sex
    ) %>%
    summarise(
      age = list(setdiff(0:max(age), age))
    ) %>%
    unnest(cols = age) %>%
    mutate(
      id = "big_zero",
      pop = 0,
      n = 0,
      kid_year = min(num_offs$kid_year)
    )

  # now, add those in there:
  num_offs2 <- bind_rows(
    num_offs,
    bzs
  ) %>%
    mutate(age = factor(age, levels = 0:max(age)))

  num_offs_boxplots <- ggplot(
    data = num_offs2,
    mapping = aes(
      x = age,
      y = n
    )
  ) +
    geom_jitter(
      aes(colour = age),
      size = 0.2, width = 0.2, height = 0.3
    ) +
    geom_boxplot(
      aes(fill = age),
      colour = "black",
      alpha = 0.9,
      outlier.shape = 21,
      outlier.fill = NA,
      outlier.stroke = 0.3,
      outlier.colour = "black"
    ) +
    facet_wrap(
      ~ sex,
      ncol = 1
      )



  #### Now we want to make a plot of the lifetime reproductive output as a function of age at death ####
  # We will do this only for the individuals that explicitly died in the simulation.
  # That means that anyone who had not died by the end of the simulation will not be
  # included

  # count up lifetime number of offspring for each individual
  lifetime <- pedigree %>%
    pivot_longer(
      cols = c(pa, ma),
      names_to = "parent",
      values_to = "ID"
    ) %>%
    filter(ID != "0") %>%
    count(ID)

  # now, join that onto the ages at death
  lt2 <- deaths %>%
    left_join(lifetime, by = "ID") %>%
    mutate(
      n = replace_na(n, 0),
      sex = case_when(
        str_detect(ID, "^M") ~ "male",
        str_detect(ID, "^F") ~ "female",
        TRUE ~ NA_character_
      )
    )

  # now we can make hexbin plot of that
  lt_plot <- ggplot(
    lt2,
    aes(x = age, y = n)
  ) +
    geom_hex(binwidth = c(1, 2)) +
    scale_fill_viridis_c(option = "C", trans = "log10") +
    facet_wrap(~ sex, nrow = 1) +
    ylab("Lifetime number of offspring") +
    xlab("Age at death")


  ### Now, making a plot of what fraction of the offspring are from each age class ###
  # We are going to make histograms, faceted by age.

  # let's count up the fraction from each age group each year
  off_fracts <- ped2 %>%
    select(kid_year, pa_age, ma_age) %>%
    pivot_longer(
      cols = c(pa_age, ma_age),
      names_to = c("parent", ".value"),
      names_sep = "_"
    ) %>%
    mutate(
      sex = case_when(
        parent == "pa" ~ "male",
        parent == "ma" ~ "female",
        TRUE ~ NA_character_
      )
    ) %>%
    count(kid_year, sex, age) %>%
    group_by(kid_year, sex) %>%
    mutate(fract = n / sum(n))

  # let's get the means of the fractions across all years
  mean_fracts <- off_fracts %>%
    group_by(sex, age) %>%
    summarise(mean_fract = mean(fract))

  # now we plot that dude, faceting on both sex and age
  age_contrib <- ggplot(
    off_fracts,
    aes(x = fract)
  ) +
    geom_histogram(
      binwidth = contrib_bin_width,
      fill = "gray",
      colour = "black",
      size = 0.2
    ) +
    geom_vline(
      data = mean_fracts,
      mapping = aes(xintercept = mean_fract),
      colour = "blue"
    ) +
    facet_wrap(age ~ sex) +
    theme_bw() +
    xlab("Fraction of annual offspring produced")


  ### Here we want to look at the distribution of number of mates producing offspring ###
  # we want to give a sense of the mean and variability in number of offspring produced with
  # different mates, to be able to verify that the polygamy vs monogamy settings are reasonable.



  list(
    plot_age_specific_number_of_offspring = num_offs_boxplots,
    plot_lifetime_output_vs_age_at_death = lt_plot,
    plot_fraction_of_offspring_from_each_age_class = age_contrib
  )

}
