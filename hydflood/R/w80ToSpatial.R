#' @name w80ToSFL
#' @rdname w80ToSFL
#' 
#' @title Function to convert w80-files to \code{sfc_LINESTRING}.
#' 
#' @description This function converts w80-files, an ascii-format with 80
#'   characters per line for spatial point data used by the German Waterways and
#'   Shipping Administration (WSV). Every single row codes for one point:
#'   
#'   \code{|_1_|2_|_3______|4|____________5______________|_______6_______|_____7___|_8_|} \cr
#'   
#'   \code{W0701 55 2594611   1330938065557502425901108035 5795591108035         Bu.15   01} \cr
#'   
#'   \code{W0701 57 2594611   7330932961457502484041108035 5538181108035         Bu.15   01} \cr
#'   
#'   Within each row very specific sections code for specific attributes:
#'   
#'   | **section** | **column(s)** | **attribute** | **column name in result** |
#'   | :---: | :--- | :--- | ---: |
#'   | 1 | 1 | state id, here W=WSV | sid |
#'   | 1 | 2-5 | Federal Waterway ID | fwid |
#'   | 2 | 6-8 | WSV point type | wsvpt |
#'   | 3 | 9 | blank | - |
#'   | 3 | 10-15 | river station (km) | station |
#'   | 4 | 16 | bank: 1 left, 2 right | bank |
#'   | 4 | 17-20 | continuous id | id |
#'   | 5 | 21-30 | easting in GK-coordinates | x |
#'   | 5 | 31-40 | northing in GK-coordinates | y |
#'   | 6 | 41-46 | datum of measurement | date_coor |
#'   | 6 | 47 | accuracy | acc_coor |
#'   | 6 | 48-54 | elevation | z |
#'   | 6 | 55-60 | date of the elevation measurement | date_z |
#'   | 6 | 61 | accuracy of the elevation measurement | acc_z |
#'   | 6 | 62-64 | type of measurement | tom |
#'   | 7 | 65-84 | comment | comment |
#'   | 8 | 85-86 | point status | status |
#'   
#'   In a second step these points are aggregated to a
#'   \code{sfc_LINESTRING} using the grouping column \code{id}.
#' 
#' @md
#' 
#' @param filename argument of length 1 and type \code{character} specifying
#'   an existing w80-file.
#' @param crs argument of type \code{\link[sf:st_crs]{crs}} or \code{\link[terra]{crs}}.
#' @param id argument of type \code{character} specifying a grouping column.
#' 
#' @return \code{sfc_LINESTRING}.
#' 
#' @examples
#'   options("hydflood.datadir" = tempdir())
#'   library(hydflood)
#'   c <- st_crs("EPSG:25833")
#'   filename <- tempfile(fileext = ".w80")
#'   
#'   # write temporary w80 file
#'   cat("W0701 55 2594611   1330938065557502425901108035 5795591108035         Bu.15   01\n",
#'       file = filename)
#'   cat("W0701 57 2594611   7330932961457502484041108035 5538181108035         Bu.15   01\n",
#'       file = filename, append = TRUE)
#'   
#'   # import temporary w80 file as sf LINESTRING
#'   sl <- w80ToSFL(filename, c, "station_int")
#' 
#' @export
#' 
w80ToSFL <- function(filename, crs, id = c("sid", "fwid", "wsvpt",
                                           "station", "bank", "id", "x",
                                           "y", "date_coor", "acc_coor",
                                           "z", "date_z", "acc_z", "tom",
                                           "comment", "status", "lat",
                                           "lon", "station_int",
                                           "station_c")) {
    
    # check input
    stopifnot(is.character(filename),length(filename) == 1,
              file.exists(filename))
    stopifnot(inherits(crs, "crs"))
    
    # read all lines
    lines <- readLines(con <- file(filename, encoding="UTF-8"), warn = FALSE)
    close(con)
    l <- length(lines)
    
    # create an empty df
    df <- data.frame(sid = as.character(rep(NA, l)),
                     fwid = as.integer(rep(NA, l)),
                     wsvpt = as.integer(rep(NA, l)),
                     station = as.numeric(rep(NA, l)),
                     bank = as.character(rep(NA, l)),
                     id = as.integer(rep(NA, l)),
                     x = as.numeric(rep(NA, l)),
                     y = as.numeric(rep(NA, l)),
                     date_coor = as.Date(rep(NA, l)),
                     acc_coor = as.integer(rep(NA, l)),
                     z = as.numeric(rep(NA, l)),
                     date_z = as.Date(rep(NA, l)),
                     acc_z = as.integer(rep(NA, l)),
                     tom = as.character(rep(NA, l)),
                     comment = as.character(rep(NA, l)),
                     status = as.character(rep(NA, l)),
                     lat = as.numeric(rep(NA, l)),
                     lon = as.numeric(rep(NA, l)),
                     row.names = 1:l,
                     stringsAsFactors = FALSE)
    bank <- c("left", "right")
    
    # read data to df
    df$sid <- substring(lines, 1, 1)
    df$sid[which(df$sid == " ")] <- NA
    df$fwid <- as.integer(substring(lines, 2, 5))
    df$fwid[which(df$fwid == "    ")] <- NA
    df$wsvpt <- as.integer(substring(lines, 6, 8))
    df$station <- as.numeric(substring(lines, 10, 15)) / 1000
    df$bank <- bank[as.integer(substring(lines, 16, 16))]
    df$id <- as.integer(substring(lines, 17, 20))
    df$x <- as.numeric(substring(lines, 21, 30)) / 1000
    df$y <- as.numeric(substring(lines, 31, 40)) / 1000
    df$date_coor <- as.Date(strptime(substring(lines, 41, 46), format="%d%m%y"))
    df$acc_coor <- substring(lines, 47, 47)
    df$acc_coor[which(df$acc_coor == " ")] <- NA
    df$z <- as.numeric(substring(lines, 48, 54))/10000
    df$date_z <- as.Date(strptime(substring(lines, 55, 60), format="%d%m%y"))
    df$acc_z <- substring(lines, 61, 61)
    df$acc_z[which(df$acc_z == " ")] <- NA
    df$tom <- substring(lines, 62, 64)
    df$tom[which(df$tom == "   ")] <- NA
    df$comment <- substring(lines, 65, 78)
    df$comment[which(df$comment == "              ")] <- NA
    df$status <- substring(lines, 79, 80)
    df$station_int <- as.integer(df$station * 1000)
    df$station_c <- as.character(df$station_int)
    
    ## list of Lines per id, each with one Line in a list
    ids <- unique(df[, id])
    l <- vector("list", length(ids))
    names(l) <- paste0("i", ids)
    station <- numeric(length(ids))
    names(station) <- paste0("i", ids)
    station_int <- numeric(length(ids))
    names(station_int) <- paste0("i", ids)
    for (i in ids) {
        id <- which(df[, id] == i)
        d <- df[id, c("x", "y")]
        l[[paste0("i", i)]] <- sf::st_linestring(as.matrix(d))
        station[paste0("i", i)] <- unique(df$station[id])
        station_int[paste0("i", i)] <- unique(df$station_int[id])
    }
    
    lines <- sf::st_sfc(l, crs = crs)
    lines <- sf::st_sf(lines, data.frame(ids, station, station_int))
    return(lines)
}


#' @name w80ToSFP
#' @rdname w80ToSFP
#' 
#' @title Function to convert w80-files to \code{sfc_POINT}.
#' 
#' @description This function converts w80-files, an asci-format with 80
#'   characters per line for spatial point data used by the German Waterways and
#'   Shipping Administration (WSV). Every single row codes for one point:
#'   
#'   \code{|_1_|2_|_3______|4|____________5______________|_______6_______|_____7___|_8_|} \cr
#'   
#'   \code{W0701 55 2594611   1330938065557502425901108035 5795591108035         Bu.15   01} \cr
#'   
#'   \code{W0701 57 2594611   7330932961457502484041108035 5538181108035         Bu.15   01} \cr
#'   
#'   Within each row very specific sections code for specific attributes:
#'   
#'   | **section** | **column(s)** | **attribute** | **column name in result** |
#'   | :---: | :--- | :--- | ---: |
#'   | 1 | 1 | state id, here W=WSV | sid |
#'   | 1 | 2-5 | Federal Waterway ID | fwid |
#'   | 2 | 6-8 | WSV point type | wsvpt |
#'   | 3 | 9 | blank | - |
#'   | 3 | 10-15 | river station (km) | station |
#'   | 4 | 16 | bank: 1 left, 2 right | bank |
#'   | 4 | 17-20 | continuous id | id |
#'   | 5 | 21-30 | easting in GK-coordinates | x |
#'   | 5 | 31-40 | northing in GK-coordinates | y |
#'   | 6 | 41-46 | datum of measurement | date_coor |
#'   | 6 | 47 | accuracy | acc_coor |
#'   | 6 | 48-54 | elevation | z |
#'   | 6 | 55-60 | date of the elevation measurement | date_z |
#'   | 6 | 61 | accuracy of the elevation measurement | acc_z |
#'   | 6 | 62-64 | type of measurement | tom |
#'   | 7 | 65-84 | comment | comment |
#'   | 8 | 85-86 | point status | status |
#' 
#' @md
#' 
#' @param filename argument of length 1 and type \code{character} specifying
#'   an existing w80-file.
#' @param crs argument of type \code{\link[sf:st_crs]{crs}} or \code{\link[terra]{crs}}.
#' 
#' @return \code{sfc_POINT}.
#' 
#' @examples
#'   options("hydflood.datadir" = tempdir())
#'   library(hydflood)
#'   c <- st_crs("EPSG:25833")
#'   filename <- tempfile(fileext = ".w80")
#'   
#'   # write temporary w80 file
#'   cat("W0701 55 2594611   1330938065557502425901108035 5795591108035         Bu.15   01\n",
#'       file = filename)
#'   cat("W0701 57 2594611   7330932961457502484041108035 5538181108035         Bu.15   01\n",
#'       file = filename, append = TRUE)
#'   
#'   # import temporary w80 file as sf POINT
#'   sf <- w80ToSFP(filename, c)
#' 
#' @export
#' 
w80ToSFP <- function(filename, crs) {
    
    # check input
    stopifnot(is.character(filename),length(filename) == 1,
              file.exists(filename))
    stopifnot(inherits(crs, "crs"))
    
    # read all lines
    lines <- readLines(con <- file(filename, encoding="UTF-8"), warn = FALSE)
    close(con)
    l <- length(lines)
    
    # create an empty df
    df <- data.frame(sid = as.character(rep(NA, l)),
                     fwid = as.integer(rep(NA, l)),
                     wsvpt = as.integer(rep(NA, l)),
                     station = as.numeric(rep(NA, l)),
                     bank = as.character(rep(NA, l)),
                     id = as.integer(rep(NA, l)),
                     x = as.numeric(rep(NA, l)),
                     y = as.numeric(rep(NA, l)),
                     date_coor = as.Date(rep(NA, l)),
                     acc_coor = as.integer(rep(NA, l)),
                     z = as.numeric(rep(NA, l)),
                     date_z = as.Date(rep(NA, l)),
                     acc_z = as.integer(rep(NA, l)),
                     tom = as.character(rep(NA, l)),
                     comment = as.character(rep(NA, l)),
                     status = as.character(rep(NA, l)),
                     lat = as.numeric(rep(NA, l)),
                     lon = as.numeric(rep(NA, l)),
                     row.names = 1:l,
                     stringsAsFactors = FALSE)
    bank <- c("left", "right")
    
    # read data to df
    df$sid <- substring(lines, 1, 1)
    df$sid[which(df$sid == " ")] <- NA
    df$fwid <- as.integer(substring(lines, 2, 5))
    df$fwid[which(df$fwid == "    ")] <- NA
    df$wsvpt <- as.integer(substring(lines, 6, 8))
    df$station <- as.numeric(substring(lines, 10, 15)) / 1000
    df$bank <- bank[as.integer(substring(lines, 16, 16))]
    df$id <- as.integer(substring(lines, 17, 20))
    df$x <- as.numeric(substring(lines, 21, 30)) / 1000
    df$y <- as.numeric(substring(lines, 31, 40)) / 1000
    df$date_coor <- as.Date(strptime(substring(lines, 41, 46), format="%d%m%y"))
    df$acc_coor <- substring(lines, 47, 47)
    df$acc_coor[which(df$acc_coor == " ")] <- NA
    df$z <- as.numeric(substring(lines, 48, 54))/10000
    df$date_z <- as.Date(strptime(substring(lines, 55, 60), format="%d%m%y"))
    df$acc_z <- substring(lines, 61, 61)
    df$acc_z[which(df$acc_z == " ")] <- NA
    df$tom <- substring(lines, 62, 64)
    df$tom[which(df$tom == "   ")] <- NA
    df$comment <- substring(lines, 65, 78)
    df$comment[which(df$comment == "              ")] <- NA
    df$comment <- trimws(df$comment)
    df$status <- substring(lines, 79, 80)
    df$station_int <- as.integer(df$station * 1000)
    df$station_c <- as.character(df$station_int)
    
    # make spatial
    sf <- sf::st_as_sf(df, coords = c("x", "y"), crs = crs)
    
    # add columns lat and lon
    sf$lat <- sf::st_coordinates(
        sf::st_transform(sf, sf::st_crs("EPSG:4326")))[, "Y"]
    sf$lon <- sf::st_coordinates(
        sf::st_transform(sf, sf::st_crs("EPSG:4326")))[, "X"]
    
    return(sf)
}

