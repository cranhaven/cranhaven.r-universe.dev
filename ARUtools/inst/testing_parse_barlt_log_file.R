# filename <- "J:/RecordStor20222023/James_Bay_Lowlands_Boreal_Shield_Transition_2022/P059/3A_BARLT17084/logfile_00017084_SD1.txt"

full_file <- readr::read_lines(file = filename)
breaks <- grep("^\\s*--------------", full_file)

scrape_barlt_log <- function(i){
  j <- breaks[i]
  if((i+2)>length(breaks) ) next_break <- length(full_file)
  else  next_break <- breaks[min(i+2)]

    this_group <- dplyr::tibble(string = full_file[(j+6): min(length(full_file), next_break-1)]) |>
      dplyr::filter(string!="") |>
      dplyr::mutate(dt_raw = stringr::str_sub(string, 1L, 19L),
                    event = stringr::str_sub(string, 20L, -1L))
  suppressWarnings({
    dt <-     this_group$dt_raw |> lubridate::dmy_hms()
    if(all(is.na(dt)) )dt <- this_group$dt_raw |> lubridate::ymd_hms()
    if(all(is.na(dt))) abort(c(glue::glue("Cannot parse dates for line {i} in {filename}"),
                               "i" = glue::glue("Date read is {full_file[i+6] |> stringr::str_sub(1L, 19L)}")
    ))
  })

    this_group$Date_timeStart <- dt




    lines_ <- full_file[(j+1):(j+4)] |>
      stringr::str_remove( "\\w+\\s*\\w+:\\s*") |>
      stringr::str_remove("^\\s")
    names(lines_) <- c("ARU_type","Serial", "Config", "Firmware")


    dplyr::tibble(ARU_type = lines_["ARU_type"],
                   Serial_number = lines_["Serial"],
                   Config = lines_["Config"],
                   Firmware_num = stringr::str_extract(lines_["Firmware"], "\\d+\\.\\d+"),
                   Firmware_date = stringr::str_extract(lines_["Firmware"], "\\([\\w\\d\\s\\:]+\\)") |>
                     stringr::str_remove_all("[\\(\\)]") ,
                   Date_time = this_group$Date_timeStart[[1]]) |>
      dplyr::mutate(
                   Year = lubridate::year(Date_time),
                   Month = lubridate::month(Date_time),
                   Day = lubridate::day(Date_time),
                   Hour = lubridate::hour(Date_time),
                   Minute = lubridate::minute(Date_time),
                   Second = lubridate::second(Date_time),
                   this_report_metadata = list(this_group))


}

res <- purrr::map_df(seq(1, length(breaks), by=2), scrape_barlt_log)
