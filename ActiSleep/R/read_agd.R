#' @title Read in AGD filedata
#'
#' @description Obtain a list of AGD filenames and data
#'
#' @param file AGD data file
#' @param tz timezone, default is GMT
#' @param sec time interval used to set date
#'
#' @return list of AGD filenames and data
#' @export
#'
read_agd <- function(file, tz = "GMT", sec = 10)
  {
  con <- dbConnect(SQLite(), file)
  ad_set <- dbReadTable(con, "settings")
  ad_set <<- ad_set
  raw_start <- ad_set$settingValue[ad_set$settingName == "startdatetime"]
  origen <- as.POSIXlt(
    (as.numeric(raw_start) / 1e+07),
    origin = "0001-01-01 00:00:00",
    tz = "GMT")
  origen <- as.POSIXct(as.character(origen), tz = tz)
  longitud <- ad_set$settingValue[ad_set$settingName == "epochcount"]
  longitud <- as.numeric(longitud)
  base <- dbReadTable(con, "data")
  base <- tibble::as_tibble(base)
  date = origen + seq(from = 0, by = sec, length.out = longitud)
  base$date = date
  base <- select(base, date, everything(), -dataTimestamp)
  ad_set <- tibble::as_tibble(ad_set)
  res <- list(settings = ad_set, raw.data = base)
  dbDisconnect(con)
  return(res)
  }
