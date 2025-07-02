
library(dplyr)

tz_locs <- dplyr::tribble(
  ~tz_rough, ~lat, ~lon,~site_id,
  "Central",54.31810, -90.16277, "ARU1",
  "Eastern", 54.31409, -89.80434, "ARU2",
  "Eastern", 54.20901, -89.69448, "SettingLoc"
) %>%
  left_join(x = sf::st_as_sf(x=., coords = c("lon", "lat"), crs= 4326), by = join_by(tz_rough, site_id)) %>%
  dplyr::mutate(tz_actual = lutz::tz_lookup(x = ., method = 'accurate') )


rec_schedule <- tz_locs |>
  filter(site_id == "SettingLoc") |>
  tidyr::expand_grid(date = seq(lubridate::ymd("2023-05-01"),
                                lubridate::ymd("2023-07-15"), by = "3 days")
                     ) %>%
  left_join(suncalc::getSunlightTimes(data = ., keep = c("sunrise"), tz = unique(.$tz_actual) ),
            by = join_by(lon, lat, date)) |>
  dplyr::select(-site_id) |>
  tidyr::expand_grid(t2sr = c(-30, 0, 30, 60),
                     site_id = c("ARU1", "ARU2")) |>
  mutate(recording_datetime = sunrise + lubridate::minutes(t2sr),
          recording_datetime_local = lubridate::force_tz(recording_datetime, "UTC")) |>
  rename(sunrise_local = sunrise)


recordings_arus <- dplyr::left_join(
  tz_locs[1:2,],
  dplyr::select(rec_schedule, site_id, date, t2sr_settings = t2sr, recording_datetime_settings = recording_datetime,
                recording_datetime_local_settings=recording_datetime_local),
  by= dplyr::join_by(site_id), multiple='all',
) |> sf::st_as_sf() %>%
  nest_by(tz_actual) |>
  rowwise() |>
  mutate(ss_sr = list(suncalc::getSunlightTimes(data = .data$data, keep = c("sunrise"), tz = .data$tz_actual)  |>
                        mutate(sunrise_utc = lubridate::with_tz(sunrise, "UTC"),
                               sunrise_edt = lubridate::with_tz(sunrise, "America/Toronto"),
                               sunrise = lubridate::force_tz(sunrise_edt, "UTC")) |>
           dplyr::select(-date, -lon, -lat))
           ) |>
  tidyr::unnest(c(data, ss_sr)) |>
  mutate(true_time_to_sunrise = as.numeric(difftime( recording_datetime_local_settings,sunrise, units='mins')) ,
         error = t2sr_settings - true_time_to_sunrise,
         longitude = lon,
         latitude = lat,
         aru_id = site_id,
         date_time = recording_datetime_local_settings
  )

library(ggplot2)
ggplot(recordings_arus, aes(tz_actual, error)) +
  geom_jitter(width = .1, height = 0.01, aes(colour = t2sr_settings))


ARUtools_sr <-
  calc_sun(recordings_arus |> rename(sunrise_me  = sunrise,
                                     tz_me = tz_actual),
           aru_tz = "America/Toronto")

ARUtools_sr_local <-
  calc_sun(recordings_arus |>
             rename(sunrise_me  = sunrise,
                                     tz_me = tz_actual),
           aru_tz = "local")

hist(ARUtools_sr$t2sr-ARUtools_sr$t2sr_settings )
hist(ARUtools_sr_local$t2sr-ARUtools_sr_local$t2sr_settings)



ggplot(ARUtools_sr, aes(t2sr, t2sr_settings)) +
 geom_abline(slope =1, intercept = 1, linetype =2)+
  geom_point(aes(colour = tz_me))

ggplot(ARUtools_sr_local, aes(t2sr, t2sr_settings)) +
  geom_abline(slope =1, intercept = 1, linetype =2)+
  geom_point(aes(colour = tz_me))
