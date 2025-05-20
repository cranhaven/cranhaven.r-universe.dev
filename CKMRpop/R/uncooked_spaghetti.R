
#' Summarise kin-pair information and use it to create uncooked spaghetti plots
#'
#'  This gives a nice graphical summary of all the kin pairs along with when they
#'  were sampled and their age at the time of sampling and their sex.
#'  #'  In order to visually summarize all the kin pairs that were found,
#' with specific reference to their age, time of sampling, and sex, I find it
#' helpful to use what I have named the "Uncooked Spaghetti Plot".  There are multiple
#' subpanels on this plot.  Here is how to read/view these plots:
#' - Each row of subpanels is for a different dominant relationship, going from
#'   closer relationships near the top and more distant ones further down.  You can
#'   find the abbreviation for the dominant relationship at the right edge of the panels.
#' - In each row, there are four subpanels: `F->F`, `F->M`, `M->F`, and `M->M`.  These
#'   refer to the different possible combinations of sexes of the individuals in the pair.
#'     + For the non-symmetrical relationships these are naturally defined with the
#'       first letter (`F` for female or `M` for male) denoting the sex of the "upper_member"
#'       of the relationship.  That is, if it is PO, then the sex of the parent is the first letter.
#'       The sex of the non-upper-member is the second letter.  Thus a `PO` pair that consists of
#'       a father and a daughter would appear in a plot that is in the `PO` row in the `M->F` column.
#'     + For the symmetrical relationships, there isn't a comparably natural way of
#'       ordering the individuals' sexes for presentation.  For these relationships, the
#'       first letter refers to the sex of the individual that was sampled in the earliest
#'       year.  If both individuals were sampled in the same year, and they are of different
#'       sexes, then the female is considered the first one, so those all go on the `F->M` subpanel.
#' - On the subpanels, each straight line (i.e., each piece of uncooked spaghetti) represents
#'   a single kin pair.  The two endpoints represent the year/time of sampling (on the x-axis)
#'   and the age of the individual when it was sampled (on the y-axis) of the two members of
#'   the pair.
#'     + If the relationship is non-symmetrical, then the line is drawn as an arrow pointing
#'       from the upper member to the lower member.
#'     + The color of the line gives the number of shared ancestors (`max_hit`) at the level
#'       of the dominant relationship. This is how you can distinguish full-sibs from half-sibs, etc.
#' @param Pairs  The tibble of kin pairs that comes out of `compile_related_pairs()`.
#' @param Samples The tibble of samples that comes out of `slurp_spip()`.
#' @param jitter_age half the width of the uniform jitter window around age
#' @param jitter_year half the width of the uniform jitter window around sampling year
#' @return `uncooked_spaghetti()` returns a list with two components: `input_data` and `plot`.
#' `plot` is a ggplot object of the plot described above in "Description."  `input_data` is,
#' itself, another list with the following named components:
#'    - `P5`: A tibble that is a processed version of the `Pairs` input.  This is what
#'      goes into making the ggplot.
#'    - `age_grid`: A tibble giving the coordinates for placing the alternating pink and white
#'      horizontal background rectangles on the plot.
#'    - `year_grid`: A tibble giving the coordinates for placing the alternating pink and white
#'      vertical background rectangles on the plot.
#' @export
#' @examples
#' # get the input variables
#' # only take the first 50 samples to reduce time for example
#' Samples <- species_1_slurped_results$samples[1:50, ]
#' Pairs <- compile_related_pairs(Samples)
#' result <- uncooked_spaghetti(Pairs, Samples)
#'
#' # produce the plot with:
#' # result$plot
uncooked_spaghetti <- function(
  Pairs,
  Samples,
  jitter_age = 0.2,
  jitter_year = 0.2
) {
  # First, grab just the colums that we want to use for this
  P <- Pairs %>%
    select(id_1, id_2, dom_relat, max_hit, upper_member, pop_post_1:samp_years_list_2)

  # now, we get all combinations of the different sampling years through
  # a couple of left_joins
  s1 <- P %>%
    select(id_1, samp_years_list_1) %>%
    filter(!duplicated(id_1)) %>%
    unnest(cols = c(samp_years_list_1)) %>%
    rename(samp_year_1 = samp_years_list_1)

  s2 <- P %>%
    select(id_2, samp_years_list_2) %>%
    filter(!duplicated(id_2)) %>%
    unnest(cols = c(samp_years_list_2)) %>%
    rename(samp_year_2 = samp_years_list_2)

  # here we left_join to expand things over the sampling years and compute
  # the ages at sampling. We also determine which column each pair should be in.
  P2 <- P %>%
    left_join(s1, by = "id_1") %>%
    left_join(s2, by = "id_2") %>%
    select(-samp_years_list_1, -samp_years_list_2) %>%
    mutate(
      age_1 = samp_year_1 - born_year_1,
      age_2 = samp_year_2 - born_year_2,
      sex_config = case_when(
        (is.na(upper_member) | upper_member == 0) & (samp_year_1 < samp_year_2) ~ str_c(sex_1, "->", sex_2),
        (is.na(upper_member) | upper_member == 0) & (samp_year_1 > samp_year_2) ~ str_c(sex_2, "->", sex_1),
        (is.na(upper_member) | upper_member == 0) & (samp_year_1 == samp_year_2) & (sex_1 != sex_2) ~ "F->M",
        (is.na(upper_member) | upper_member == 0) & (samp_year_1 == samp_year_2) & (sex_1 == sex_2) ~ str_c(sex_1, "->", sex_2),
        upper_member == 1 ~ str_c(sex_1, "->", sex_2),
        upper_member == 2 ~ str_c(sex_2, "->", sex_1),
        TRUE ~ "WTF?!"
      )
    )

  # now, we really need to add the "Se" category to that. We do this by just expanding
  # the individuals that were sampled more than once.
  Selfies <- Samples %>%
    filter(map_int(samp_years_list, length) > 1) %>%
    mutate(
      first = map(
        .x = samp_years_list,
        .f = function(x) combn(x, 2)[1,]
      ),
      second = map(
        .x = samp_years_list,
        .f = function(x) combn(x, 2)[2,]
      )
    ) %>%
    mutate(id_1 = ID, id_2 = ID) %>%
    unnest(cols = c(first, second)) %>%
    rename(
      samp_year_1 = first,
      samp_year_2 = second
    ) %>%
    mutate(
      dom_relat = "Se",
      max_hit = 1,
      upper_member = NA,
      pop_1 = pop_post,     # this is going to need fixing when we start keeping track of which populations they were sampled from
      pop_2 = pop_post,
      sex_1 = sex,
      sex_2 = sex,
      born_year_1 = born_year,
      born_year_2 = born_year,
      age_1 = samp_year_1 - born_year_1,
      age_2 = samp_year_2 - born_year_2,
      sex_config = str_c(sex, "->", sex)
    ) %>%
    select(intersect(names(P2), names(.)))  # hacky, dealing with all the extra columns after I added other census sizes in there

  if(nrow(Selfies) > 0) {
    P3 <- bind_rows(
      Selfies,
      P2
    )
  } else {
    P3 <- P2
  }



  # now, we are going to want to jitter the endpoints consistently for these different individuals,
  # so that we can better see overlapping pairs.  Also, if you get half-sibs of the same age in the
  # same year, you couldn't see them without some jittering.  So, we are going to jitter things within jitter_age and jitter_year
  # of either side of their actual value, then we will throw down some light gray rectangles over those
  # areas so that we know which integer each corresponds to.  This is going to involve some joins.
  xys <- bind_rows(
    P3 %>%
      select(id_1, samp_year_1, age_1) %>%
      rename(id = id_1, sy = samp_year_1, age = age_1),
    P3 %>%
      select(id_2, samp_year_2, age_2) %>%
      rename(id = id_2, sy = samp_year_2, age = age_2)
  ) %>%
    distinct() %>%
    mutate(
      x = sy + runif(n(), min = -jitter_year, max = jitter_year),
      y = age + runif(n(), min = -jitter_age, max = jitter_age)
    ) %>%
    select(id, x, y)

  # join those coordinates on, and, for a last hurrah,
  # make a factor out of dom_relat so the rows are in the proper order,
  # arrange everything by dom_relat and
  # max_hit so that the interesting ones get plotted on top.
  Relats <- c("Se", "PO", "Si", "GP", "A", "FC")  # we will have to update this when we have more than two generations

  P4 <- P3 %>%
    left_join(
      xys,
      by = c("id_1" = "id")
    ) %>%
    rename(
      x_1 = x,
      y_1 = y
    ) %>%
    left_join(
      xys,
      by = c("id_2" = "id")
    ) %>%
    rename(
      x_2 = x,
      y_2 = y
    )

  P5 <- P4 %>%
    mutate(dom_relat = factor(dom_relat, levels = Relats)) %>%
    arrange(
      dom_relat,
      max_hit
    )



  # now, we are pretty much ready to start plotting this, except for figuring out the arrow
  # direction for the non-symmetrical relationships...

  # make a data frame for the background grids
  age_grid <- tibble(
    ymin = seq(
      from = min(c(P5$age_1, P5$age_2)) - jitter_age,
      to = max(c(P5$age_1, P5$age_2)) - jitter_age,
      by = 1
    )
  ) %>%
    mutate(
      ymax = ymin + jitter_age * 2
    )

  year_grid <- tibble(
    xmin = seq(
      from = min(c(P5$samp_year_1, P5$samp_year_2)) - jitter_year,
      to = max(c(P5$samp_year_1, P5$samp_year_2)) - jitter_year,
      by = 1
    )
  ) %>%
    mutate(
      xmax = xmin + jitter_age * 2
    )

  g <- ggplot(P5) +
    geom_rect(
      data = age_grid,
      aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = ymin,
        ymax = ymax
      ),
      fill = "pink",
      alpha = 0.2
    ) +
    geom_rect(
      data = year_grid,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "pink",
      alpha = 0.2
    ) +
    geom_segment(
      aes(
        x = x_1,
        y = y_1,
        xend = x_2,
        yend = y_2,
        colour = as.factor(max_hit)
      )
    ) +
    facet_grid(dom_relat ~ sex_config) +
    theme_bw() +
    scale_colour_manual(
      values = c(`1` = "gray", `2` = "red", `3` = "blue", `4` = "orange"),
      name = "Number of\nShared Ancestors"
    ) +
    xlab("Sampling Year") +
    ylab("Age at Time of Sampling") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )


  # return the plot and also the data that went into it
  list(
    input_data = list(
      P5 = P5,
      age_grid = age_grid,
      year_grid = year_grid
    ),
    plot = g
  )

}

