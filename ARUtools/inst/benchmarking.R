library(bench)
library(fs)

project_dir <- "../ARUtools - Extra/ARUtools_file_examples"

# fs vs. base -------------------------------

# fs package faster
bench::mark(t1 <- list.files(project_dir, recursive = TRUE),
            t2 <- fs::dir_ls(project_dir, all = FALSE, recurse = TRUE, type = "file"),
            check = FALSE)


# pattern checking faster in dir_ls rather than after - For selecting small
bench::mark(t1 <- fs::dir_ls(project_dir, all = FALSE, recurse = TRUE, type = "file",
                             regexp = "P71") |> as.character(),
            t2 <- fs::dir_ls(project_dir, all = FALSE, recurse = TRUE, type = "file") |>
              stringr::str_subset("P71")
)

# no real different if selecting large
bench::mark(t1 <- fs::dir_ls(project_dir, all = FALSE, recurse = TRUE, type = "file",
                             regexp = "P71", invert = TRUE) |> as.character(),
            t2 <- fs::dir_ls(project_dir, all = FALSE, recurse = TRUE, type = "file") |>
              stringr::str_subset("P71", negate = TRUE)
)


# sun_diff faster but equivalent to using intervals ---------------------
t1 <- Sys.time()
t2 <- t1 - lubridate::hours(6)

mark(sun_diff(t1, t2),
     abs(lubridate::int_length(lubridate::interval(t1, t2))/60),
     min_time = 5)


# pmin with abs ------------------------------
s <- dplyr::tibble(
  t2sr_day_of = c(269, 269, 897, 927, 957),
  t2sr_before = c(1709, 269, 2337, 2367,2397),
  t2sr_after = c(-1172, -544, -544, -514, -484),
  t2ss_day_of = c(-660, -31.6, -31.6, -1.62, 28.4),
  t2ss_before = c(780, 1408, -31.6, 1438, 1468),
  t2ss_after = c(-2100, -1472, -1442, -1442, -1412))

bench::mark(
  s1 <- s |>
    dplyr::rowwise() |> # This is slow and ungainly, but pmin didn't work if not taking abs time to sunrise.
    dplyr::transmute(t2sr = c(t2sr_day_of, t2sr_before, t2sr_after)[which.min(c(abs(t2sr_day_of), abs(t2sr_before),
                                                                             abs(t2sr_after)))],
                  t2ss = c(t2ss_day_of, t2ss_before, t2ss_after)[which.min(c(abs(t2ss_day_of), abs(t2ss_before),
                                                                             abs(t2ss_after)))]) |>
    dplyr::ungroup(),

  s2 <- s |>
    dplyr::transmute(t2sr = purrr::pmap_dbl(
      list(.data$t2sr_day_of, .data$t2sr_before, .data$t2sr_after),
      ~c(..1, ..2, ..3)[abs(c(..1, ..2, ..3)) == pmin(abs(..1), abs(..2), abs(..3))][1]),
      t2ss = purrr::pmap_dbl(
        list(.data$t2ss_day_of, .data$t2ss_before, .data$t2ss_after),
        ~c(..1, ..2, ..3)[abs(c(..1, ..2, ..3)) == pmin(abs(..1), abs(..2), abs(..3))][1])),

  # Slightly faster than the above
  s3 <- s |>
    dplyr::transmute(
      t2sr = purrr::pmap_dbl(
        list(.data$t2sr_day_of, .data$t2sr_before, .data$t2sr_after), min_abs),
      t2ss = purrr::pmap_dbl(
        list(.data$t2ss_day_of, .data$t2ss_before, .data$t2ss_after), min_abs)),

  min_time = 3
)






# GPX - gpx package vs sf ----------------------------
# SF is MUCH faster and doesn't convert datetimes to timezone specific
p <- "../ARUtools - Extra/ARUtools_file_examples/James_Bay_Lowlands_Boreal_Shield_Transition_2022/P028/1A_BARLT10962/GPS_log.gpx"
p <- "../ARUtools - Extra/ARUtools_file_examples/James_Bay_Lowlands_Boreal_Shield_Transition_2022/P028/2A_BARLT13383/GPS_log.gpx"
bench::mark(
  {
    g1 <- gpx::read_gpx(p)$waypoints |>
      dplyr::select("time" = Time, "longitude" = "Longitude", "latitude" = "Latitude") |>
      dplyr::mutate(time = lubridate::with_tz(time, "America/Toronto"),
             time = lubridate::force_tz(time, "UTC"))
  },
  {
    g2 <- sf::st_read(p, layer = "waypoints")
    g2 <- dplyr::bind_cols(sf::st_drop_geometry(g2), sf::st_coordinates(g2)) |>
      dplyr::select("time", "longitude" = "X", "latitude" = "Y") |>
      dplyr::mutate(time2 = lubridate::force_tz(time, "UTC"))
  }, check = FALSE
)
