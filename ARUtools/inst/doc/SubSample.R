## ----include = FALSE----------------------------------------------------------
library(ARUtools)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ----code_folding = 'hide'----------------------------------------------------
library(dplyr)
library(stringr)
library(lubridate)

simple_deploy <-
  tidyr::expand_grid(
    site_id = unique(example_sites$Sites),
    doy = seq(121, 191, by = 2),
    times = seq(-30, 120, by = 30)
  ) |>
  tidyr::separate(site_id, into = c("plot", "site"), sep = "_", remove = F) |>
  left_join(example_sites, join_by(site_id == Sites)) |>
  mutate(
    # aru_id = glue::glue("BARLT-000{as.numeric(as.factor(site_id))}"),
    date = ymd("2028-01-01") + doy,
    date_time = ymd_hm(glue::glue("{date} 06:00")) + minutes(times),
    date_time_chr = str_replace(as.character(date_time), "\\s", "T"),
    file_name = glue::glue("{plot}/{site_id}/{ARU}_{date_time_chr}.wav")
  )

simple_deploy

## -----------------------------------------------------------------------------
site_info <- simple_deploy |>
  slice_min(order_by = date_time, n = 1, by = site_id) |>
  dplyr::select(site_id, ARU, lon, lat, date_time)

## -----------------------------------------------------------------------------
sites <- clean_site_index(site_info,
  name_aru_id = "ARU",
  name_site_id = "site_id",
  name_date_time = c("date_time"),
  name_coords = c("lon", "lat")
)
metadata <- clean_metadata(project_files = simple_deploy$file_name) |>
  add_sites(sites) |>
  calc_sun() |>
  dplyr::mutate(doy = lubridate::yday(date))
dplyr::glimpse(metadata)

## ----fig.height=6, out.height=8, warning=FALSE--------------------------------
p <- sim_selection_weights(
  min_range = c(-70, 240),
  day_range = c(120, lubridate::yday(lubridate::ymd("2021-07-20"))),
  min_mean = 30, min_sd = 60,
  day_mean = lubridate::yday(lubridate::ymd("2021-06-10")),
  day_sd = 20, offset = 0,
  return_log = TRUE, selection_fun = "norm"
)

## -----------------------------------------------------------------------------
full_selection_probs <-
  metadata |>
  calc_selection_weights(
    col_site_id = site_id,
    col_min = t2sr,
    col_day = doy,
    params = p
  )

## ----echo=FALSE---------------------------------------------------------------
library(ggplot2)
ggplot(full_selection_probs, aes(doy, t2sr, colour = psel_normalized)) +
  geom_point() +
  scale_colour_viridis_c() +
  facet_wrap(~site_id)

## -----------------------------------------------------------------------------
sample_size <- count(full_selection_probs, site_id) |>
  transmute(site_id,
    n = floor(n * .02),
    n_os = ceiling(n * .3)
  )

## ----echo=F-------------------------------------------------------------------
p_obs <-
  tidyr::expand_grid(
    p_obs = seq(0.01, 0.99, by = 0.01),
    n_obs = 1:20
  ) |>
  mutate(p_total = 1 - exp(-p_obs * n_obs)) |>
  ggplot(aes(n_obs, p_obs, fill = p_total), colour = NA) +
  geom_raster() +
  scale_fill_viridis_c() +
  labs(
    x = "Number of minutes observed", y = "Probably of observing per minute",
    fill = "Total\nprobability\nof\ndetection"
  )
p_obs

## -----------------------------------------------------------------------------
grts_res <- sample_recordings(full_selection_probs,
  n = sample_size,
  col_site_id = site_id,
  seed = 2024,
  col_sel_weights = psel_normalized
)

dplyr::glimpse(grts_res$sites_base)

## -----------------------------------------------------------------------------
withr::with_seed(2024, {
  random_sample <-
    full_selection_probs |>
    dplyr::slice_sample(
      n = 4, by = site_id,
      weight_by = psel_normalized,
      replace = F
    )
})

## ----eval=F-------------------------------------------------------------------
# withr::with_seed(2024, {
#   random_sample_stratified <-
#     full_selection_probs |>
#     left_join(sample_size, by = join_by(site_id)) |>
#     nest_by(site_id, n) |>
#     rowwise() |>
#     mutate(sample = list(dplyr::slice_sample(
#       .data = data,
#       n = .data$n,
#       weight_by = psel_normalized,
#       replace = F
#     ))) |>
#     dplyr::select(site_id, sample) |>
#     tidyr::unnest(sample)
# })

## -----------------------------------------------------------------------------
oversample <- filter(
  full_selection_probs,
  !path %in% random_sample$path
) |>
  dplyr::slice_sample(
    n = 2, by = site_id,
    weight_by = psel_normalized,
    replace = F
  )

## ----echo=F-------------------------------------------------------------------
filter(full_selection_probs, path %in% grts_res$sites_base$path) |>
  ggplot(aes(doy, t2sr)) +
  geom_point() +
  xlim(range(full_selection_probs$doy)) +
  ylim(range(full_selection_probs$t2sr)) +
  labs(x = "Day of Year", y = "Time to sunrise", title = "GRTS selection")

## ----echo=F-------------------------------------------------------------------
random_sample |>
  mutate(z = 1) |>
  ggplot(aes(doy, t2sr)) +
  geom_point() +
  xlim(range(full_selection_probs$doy)) +
  ylim(range(full_selection_probs$t2sr)) +
  labs(x = "Day of Year", y = "Time to sunrise", title = "Random selection")

## -----------------------------------------------------------------------------
withr::with_seed(6546, {
  random_sample$length <- sample(
    x = c("5min", "3min", "1min"),
    size = nrow(random_sample), replace = T
  )
})

## ----echo=F-------------------------------------------------------------------
count(random_sample, length, site_id) |>
  tidyr::pivot_wider(
    names_from = site_id,
    values_from = n, values_fill = list(n = 0),
    values_fn = as.numeric
  )

## -----------------------------------------------------------------------------
withr::with_seed(569, {
  sample5min <- slice_sample(random_sample,
    n = 1, by = site_id, weight_by = psel_normalized
  )

  sample3min <- slice_sample(
    random_sample |>
      filter(!path %in% sample5min$path),
    n = 1, by = site_id, weight_by = psel_normalized
  )


  random_sample_with_lengths <- random_sample |>
    mutate(
      Length_group =
        case_when(
          path %in% sample5min$path ~ "5min",
          path %in% sample3min$path ~ "3min",
          TRUE ~ "1min"
        ),
      length_clip = as.numeric(str_extract(Length_group, "^\\d")) * 60
    )
})

## ----echo=F-------------------------------------------------------------------
count(random_sample_with_lengths, Length_group, site_id) |>
  tidyr::pivot_wider(
    names_from = site_id,
    values_from = n, values_fill = list(n = 0),
    values_fn = as.numeric
  )

## ----eval=F-------------------------------------------------------------------
# random_sample_with_lengths$length_clip <-
#   purrr::map(
#     1:nrow(random_sample_with_lengths),
#     ~ get_wav_length(
#       path = random_sample_with_lengths$path[[.x]],
#       return_numeric = T
#     )
#   )

## -----------------------------------------------------------------------------
random_sample_with_lengths$length <- 5 * 60

## ----eval=T-------------------------------------------------------------------
random_sample_with_lengths <-
  random_sample_with_lengths |>
  rowwise() |>
  mutate(StartTime = case_when(
    Length_group == "5min" ~ 0,
    TRUE ~ runif(
      1, 0,
      pmax(
        0,
        length - length_clip
      )
    )
  )) |>
  ungroup()

## ----echo=F-------------------------------------------------------------------
ggplot(random_sample_with_lengths, aes(StartTime, fill = Length_group)) +
  geom_histogram(binwidth = 10)

## -----------------------------------------------------------------------------
final_selection <- random_sample_with_lengths |>
  add_wildtrax()

final_selection |>
  head() |>
  dplyr::select(path, wildtrax_file_name)

## ----eval=F-------------------------------------------------------------------
# out_directory <- "/path/to/upload/directory/"
# dir.create(out_directory, recursive = T)
# ul_tab <- expand_grid(
#   period = c("Dawn"), # Add 'Dusk' if using more than one time period
#   length = unique(selected_recordings$Length_group)
# )
# purrr::map(glue::glue("{out_directory}/{ul_tab$period}/{ul_tab$length}"),
#   dir.create,
#   recursive = T
# )

## ----eval=F-------------------------------------------------------------------
# log_output <-
#   format_clip_wave(
#     segment_df = final_selection,
#     in_base_directory = "", out_base_directory = out_directory,
#     length_clip_col = "length_clip",
#     sub_dir_out_col = c("Time_period", "Length_group"),
#     filepath_in_col = "path",
#     out_filename_col = "wildtrax_file_name",
#     use_job = F, filewarn = F
#   )

