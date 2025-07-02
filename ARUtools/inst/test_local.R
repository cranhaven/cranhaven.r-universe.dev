library(dplyr)
library(stringr)

# eg File output ----------
out <- readr::read_rds("../Boreal_Shield_Lowlands_transition_Selection/Meta_data_cleaned.rds")

out2 <- out |>
  select(path = filename,
         file_name = WaveFile, date, date_time, type = wav,
         site_id = Plot, aru_type = ARU_type,
         aru_id = Site_ARUid,
         doy, longitude = longitude_decimal_degrees, latitude = latitude_decimal_degrees,
         tz, t2sr = t2sr_min, t2ss = t2ss_min) |>
  mutate(aru_id_old = aru_id,
         id = str_extract(aru_id_old, "^\\d(b|A|C){0,2}(?=_)"),
         aru_id = str_remove(aru_id_old, paste0(id, "_"))) |>
  # Fix id
  mutate(aru_id = if_else(aru_id == "SM401221", "S4A01221", aru_id))



m <- clean_metadata(project_files = f,
                    pattern_site = "(P|Q)\\d+",
                    pattern_dt_sep = create_pattern_dt_sep(sep = c("T", "_")))

# If had gps files...
#g <- clean_gps(m)
#m2 <- add_sites(m, g, buffer_before = 24, buffer_after = 24)

# But since none, just add in coords
g <- select(out2, site_id, aru_id, longitude, latitude, date) |>
  distinct()
m2 <- left_join(m, g)

filter(m2, is.na(latitude)) # check


m3 <- calc_sun(m2) # All Eastern Timezone



# Compare ---------------------------------------

# Ensure column/row orders are the same, add
m_comp <- arrange(m3, site_id, aru_id, date_time)

out_comp <- out2[names(m_comp)] |>
  arrange(site_id, aru_id, date_time) |>
  mutate(file_name = paste0(file_name, ".wav"),
         date_time = lubridate::force_tz(date_time, "UTC"),
         across(starts_with("t2"), abs))

# Compare using waldo package
waldo::compare(out_comp[1:50,], m_comp[1:50,])
