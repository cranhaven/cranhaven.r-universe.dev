# Workflows
#
# m <- clean_metadata()
# s <- clean_site_index() # From user-supplied file
# m <- add_sites(s)
# m <- calc_sun(m)
#
# m <- clean_metadata()
# g <- clean_gps(m)    # From GPS summary log files
# m <- add_sites(s)
# m <- calc_sun(m)


# eg File output ----------
out <- readr::read_rds("../Boreal_Shield_Lowlands_transition_Selection/Meta_data_cleaned.rds")

# - Only wave files
# - GPS data added
# - Sunset/sunrise added

# eg 1 - Date sep ------------------------------------------------
d <- "../ARUtools - Extra/ARUtools_file_examples/HudsonBayCoast_2021/"

count_files(d)

m <- clean_metadata(project_dir = d,
                    pattern_site = "BP_ARU0(1|2)",
                    pattern_dt_sep = "(_|T)")

check_meta(m)
check_meta(m, date = TRUE)

# OR
m <- clean_metadata(project_dir = d,
                    pattern_site = "BP_ARU0(1|2)",
                    pattern_dt_sep = create_pattern_dt_sep(c("T", "_")))

g <- clean_gps(m) # Warning
g <- clean_gps(m, dist_cutoff = Inf)

m <- add_sites(m, g)

filter(m, is.na(longitude)) |> select(-path) # Look at where no GPS match
filter(g, site_id == "BP_ARU01") # Ah, no date overlap where actually have GPS

# eg 2 - GPS files / Site Index ------------------------------------
d <- "../ARUtools - Extra/ARUtools_file_examples/LutherMarsh_2021/"
count_files(d)

m <- clean_metadata(project_dir = d)

# Hmm, check for problems
check_problems(m)              # Show relevant columns
check_problems(m, path = TRUE) # Show just paths
check_meta(m)

# Try again
m <- clean_metadata(project_dir = d,
                    pattern_dt_sep = "_")

check_problems(m) # No site-related information in names
check_meta(m)

# Create site index
sites <- readr::read_csv("../ARUtools - Extra/Scripts/LutherMarsh_2021/2022-02-23_Locations_LutherMarsh_2021.csv") |>
  mutate(aru_id = stringr::str_remove(location, "LutherMarsh2021-"),
         date_start = min(m$date_time, na.rm = TRUE),
         date_end = max(m$date_time, na.rm = TRUE)) |>
  clean_site_index(name_site_id = "location",
                   name_date_time = c("date_start", "date_end"))

# GPS from logs
g <- clean_gps(m, dist_by = "aru_id")

# Compare
select(g, aru_id, longitude, latitude) |> distinct()
sites

# Join
m1 <- add_sites(m, sites)
# Could easily just use a simple join, because not dependent on dates
m2 <- add_sites(m, sites, by_date = NULL)

waldo::compare(m1, m2) # ✔ No differences!

check_problems(m1) # No problems!

# Get time to sunrise/sunset
m1 <- calc_sun(m1, aru_tz = "America/Toronto")
m1



# eg 3 - Big set of files -------------------------
d <- "../ARUtools - Extra/ARUtools_file_examples/JamesBayLowlands_2021/"
m <- clean_metadata(project_dir = d)

check_problems(m)
check_problems(m, path = TRUE) # No ARU types or IDs in file names, ignore.
check_meta(m)

g <- clean_gps(m, dist_by = "site_id") # No aru_ids

check_problems(g, check = "date")
check_problems(g, check = "date", path = TRUE)
check_file(check_problems(g, check = "date", path = TRUE))

# Ah ha! Missing headings (as clean_gps() said)

m1 <- add_sites(m, g, by_date = "date") # Faster, but averages over coordinates
m2 <- add_sites(m, g)                   # Slow but matches more precisely

check_problems(m2, check = "longitude")
check_problems(m2, check = "longitude", date = TRUE)

filter(g, site_id == "P68_2") # No GPS log for this site (it was the one skipped earlier)

# eg 4 - File lists -----------------------------
files <- readr::read_rds("../Boreal_Shield_Lowlands_transition_Selection/Transition_all_files.rds")

m <- clean_metadata(
  project_files = files,
  pattern_site = "(P|Q)\\d{3}",
  pattern_dt_sep = create_pattern_dt_sep(sep = c("T", "_")),
  subset = "ZOOM|Zoom", subset_type = "omit"
  )
m

# eg 5 - Site from file ----------------------
files <- readr::read_rds("../RiverTrips_DataPrep/all_files_vec.rds")
i <- "../RiverTrips_DataPrep/NRT2022_SMM_Logfile_Compile.xlsx"

m <- clean_metadata(project_files = files,
                    pattern_aru_id = create_pattern_aru_id(prefix = "CWS-"),
                    pattern_dt_sep = "_")

check_problems(m) # Some funny files 'zoom'

# Omit 'zoom' files (no sites)
m <- clean_metadata(project_files = files,
                    subset = "Zoom", subset_type = "omit",
                    pattern_aru_id = create_pattern_aru_id(prefix = "CWS-"),
                    pattern_dt_sep = "_")

check_problems(m)
check_meta(m)

# Add sites by date
sites <- clean_site_index(i) # Expect standard column names - No good
sites <- clean_site_index(i, # Supply column names
                          name_aru_id = "ARU_ID",
                          name_site_id = "SiteID_WildTrax",
                          name_date_time = c("Date_Deploy", "Date_Retrieve"),
                          name_extra = c("river" = "NorthernRiverTrip"))

f <- add_sites(m, sites) # See that it omits "site_id" from by (included by default)

f <- add_sites(m, sites, quiet = TRUE) # To omit non-essential messages

# Check Problems (non-matched) - These are either
# a) not in the date range in the sites index, or
# b) missing a record in the sites index
filter(f, is.na(site_id)) |>
  check_meta(date = TRUE)

check_problems(f, date = TRUE)

# Look at relevant ARU ids in site file to compare dates
# - 2022-06-24 before the first recording
# - 2022-07-02 occurs in the morning, perhaps that ARU was set out before noon?
semi_join(sites, check_problems(f, date = TRUE), by = c("aru_id"))

# Add time to sunrise/sunset
s <- calc_sun(f, aru_tz = "America/Toronto")
s

# No times where no coords
filter(s, !is.na(longitude))

# eg 6 - BAR-LT GPS file examples ------------------------------------------
d <- "../ARUtools - Extra/ARUtools_file_examples/James_Bay_Lowlands_Boreal_Shield_Transition_2022/"
#file.create(file.path(d, "/P028/1A_BARLT10962/", "0000.wav")) # Just for testing

m <- clean_metadata(project_dir = d,
                    pattern_site_id = create_pattern_site_id(p_digits = 3,
                                                             s_digits = 0))

g <- clean_gps(m)
check_problems(g, check = "date")

# Let's take a look - No wonder, it's not GPX!
check_problems(g, check = "date", path = TRUE) |>
  check_file()

# We could fix this by changing the extension to txt or csv

# A GPX file would look something like...
check_file(g$path[481078])


# eg 7 - Site Index with timezones ------------------------------------
d <- "../ARUtools - Extra/ARUtools_file_examples/LutherMarsh_2021/"
m <- clean_metadata(project_dir = d, pattern_dt_sep = "_")

# Create site index
sites <- "../ARUtools - Extra/Scripts/LutherMarsh_2021/2022-02-23_Locations_LutherMarsh_2021.csv" |>
  readr::read_csv(show_col_types = FALSE) |>
  mutate(aru_id = stringr::str_remove(location, "LutherMarsh2021-"),
         date_start = min(m$date_time, na.rm = TRUE),
         date_end = max(m$date_time, na.rm = TRUE),
         date_start = lubridate::force_tz(date_start, "America/Toronto"),
         date_end = lubridate::force_tz(date_end, "America/Toronto")) |>
  clean_site_index(name_site_id = "location",
                   name_date_time = c("date_start", "date_end"))

# GPS from logs
g <- clean_gps(m, dist_by = "aru_id")

# Compare
select(g, aru_id, longitude, latitude) |> distinct()
sites

# Join (although could easily just use a simpler join because not dependent on dates)
m1 <- add_sites(m, sites)

# Join by sites only
m2 <- add_sites(m, sites, by_date = NULL)

# Compare
waldo::compare(m1, m2) # ✔ No differences


m1 <- calc_sun(m1, aru_tz = "America/Toronto")

m1

# eg 8 - GPSX files with wonky times ------------------------------------
d <- "../ARUtools - Extra/ARUtools_file_examples/James_Bay_Lowlands_Boreal_Shield_Transition_2022/"

m <- clean_metadata(project_dir = d) # No dates/sites, as expected

g <- dplyr::filter(m, stringr::str_detect(file_name, "gpx")) |>
  clean_gps(dist_by = "aru_id")

p <- check_problems(g, check = "date")
p

# - First file is another problem (NA's across the board)
load_gps(p$path[1], skip = 0, gps_ext = "gpx")

# - Second file has wonky time (originates from -1-01-01 dates)
load_gps(p$path[2], skip = 0, gps_ext = "gpx") |>
  arrange(time) |>
  select(time, name)

