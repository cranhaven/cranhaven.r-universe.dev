# File getData.R                                                               #
# Part of the camelsCL R package, https://gitlab.com/hzambran/camelsCL ;       #
#                                 https://CRAN.R-project.org/package=camelsCL  #
# Copyright 2024-2025 Hector Garces-Figueroa and Mauricio Zambrano-Bigiarini   #
# Distributed under GPL 2 or later                                             #
################################################################################
#                                 'getData'                                    #
################################################################################
# Function that facilitates the retrieval of data for a specific catchment     #
# from the CAMELS-CL dataset                                                   #
################################################################################
# Author : Hector Garces-Figueroa and Mauricio Zambrano-Bigiarini              #
################################################################################
# Started: May 2024                                                            #
# Updates: 2024-May-06, 2024-May-08, 2024-May-14, 2024-May-27, 2024-Aug-04     #
#          2024-Oct-04, 2024-Oct-10, 2024-Oct-11, 2024-Oct-13, 2025-Jan-30,    #
#          2025-Feb-07, 2025-May-13, 2025-May-16, 2025-Jun-02, 2025-Jun-10     #
#          2025-Aug-26                                                         #
################################################################################
# 'x'               :  Numeric, indicating the ID of the catchment for which   #
#                      all the data will be downloaded                         #
#                                                                              #
# 'tscale'          :  Character, indicating the temporal scale to be used for #
#                      the output time series. Valid values of tscale are:     #
#                                                                              #
#                      1) "daily": All the output time series have a daily     #
#                         temporal frequency, directly read from the           #
#                         CAMELS-CL dataset.                                   #
#                      2) "monthly": All the output time series have a monthly #
#                         temporal frequency, aggregated from the original     #
#                         daily time series in the CAMELS-CL dataset.          #
#                      3) "annual": All the output time series have an annual  #
#                         temporal frequency, aggregated from the original     #
#                         daily time series in the CAMELS-CL dataset.          #
#                                                                              #
# 'from'            :  Date, indicating the starting date of the time series   #
#                      to be returned. Its default value is "auto", which      #
#                      automatically selects the first date with at least one  #
#                      non-missing value among all variables.                  #
#                                                                              #
# 'to'              :  Date, indicating the ending date of the time series     #
#                      to be returned. Its default value is "auto", which      #
#                      automatically selects the last date with at least one   #
#                      non-missing value among all variables.                  #
#                                                                              #
# 'na.rm'           :  Logical. Should missing values be removed?              #
#                      -) TRUE : the monthly/annual values are computed only   #
#                                for months/years with a percentage of missing #
#                                values less than na.rm.max.                   #
#                      -) FALSE : if there is AT LEAST one NA within a         #
#                                 month/year, the corresponding monthly/annual #
#                                 values in the output object will be NA.      #
#                                                                              #
# 'na.rm.max'       :  Numeric in [0, 1]. It is used to define the maximum     #
#                      percentage of missing values allowed in each month/year #
#                      to keep the temporally aggregated value                 #
#                      (i.e., monthly/annual) in the output object of this     #
#                      function. In other words, if the percentage of missing  #
#                      values in a given month/year is larger or equal than    #
#                      na.rm.max, the corresponding monthly or annual value    #
#                      will be NA.                                             #
#                                                                              #
# 'include.shp'     :  (Optional) Logical, indicating whether to include a     #
#                      shapefile of the catchment in the function's output.    #
#                      Its default value is FALSE.                             #
#                                                                              #
# 'include.attr'    :  (Optional) Logical, indicating whether to include all   #
#                      the attributes of the catchment in the function's       #
#                      output. Its default value is FALSE.                     #
#                                                                              #
# 'include.extra.P' :  (Optional) Logical, indicating whether to include all   #
#                      the additional precipitation datasets available in the  #
#                      CAMELS-CL database (i.e., MSWEP, CHIRPSv2, and TMPA).   #
#                      Its default value is FALSE.                             #
#                                                                              #
# 'verbose'         :  Logical; if TRUE, progress messages are printed.

#getData <- function(x, ...) UseMethod("getData")
getData <- function(x, tscale, from, to, na.rm, na.rm.max, include.shp, include.attr, include.extra.P, verbose) UseMethod("getData")


getData.default <- function(x, tscale = c("daily", "monthly", "annual"),
                            from="auto", to="auto", na.rm=TRUE, na.rm.max=0,
                            include.shp=FALSE, include.attr=FALSE,
                            include.extra.P=FALSE, verbose=TRUE) {


  # checking that 'x' is provided
  if (missing(x)) stop("'x' argument must be specified!")
  if (!is.numeric(x)) stop("'x' must be numeric!")

  # checking 'from'
  if (!identical(from, "auto") && !inherits(from, "Date")) {
    stop("'from' must be a Date object!")
  } # IF end

  # checking 'to'
  if (!identical(to, "auto") && !inherits(to, "Date")) {
    stop("'to' must be a Date object!")
  } # IF end

  # checking 'tscale'
  tscale <- match.arg(tscale)

  # creating urls
  baseURL  <- "https://kimunkodata.ufro.cl/camelsCL/"
  openUser <- "camelsCL"
  openPass <- "CaMeLs*2025$"

  URLs <- list(ts = paste0(baseURL, "X", x, "_ts.csv"),
               shp = paste0(baseURL, "X", x, ".rds"))

  # retrieving content from urls
  if (verbose) message("[ Getting CAMELS-CL data for catchment ", x, " ... ]")

  URLcontent <- try(lapply(URLs,
                           function(x) httr::GET(x, httr::authenticate(user = openUser, password = openPass))),
                    silent = TRUE)

  # these errors are usually related to client connection...
  if (inherits(URLcontent, "try-error")) {
    stop("The server could not be reached. Please check your internet connection !")
  }

  if (httr::status_code(URLcontent[["ts"]]) == 404) {
    stop("'x' is not available in the CAMELS-CL database")
  }

  if (verbose) message(paste("[ CAMELS-CL data for catchment", x, "has been successfully downloaded ! ]"))
  csvContent <- lapply(URLcontent[grep(pattern = "\\.csv$", x = URLs)],
                       function(x) httr::content(x, as = "text", encoding = "UTF-8"))
  # reading text from content

  ts        <- zoo::read.csv.zoo(text = csvContent$ts, format = "%Y-%m-%d")

  # excluding extra precipitation datasets
  if (!include.extra.P) {
    extra.p.names <- c("CHIRPSv2", "MSWEP", "TMPA")
    aux.pos       <- grep(pattern = paste0(extra.p.names, collapse = "|"),
                          x = colnames(ts))
    ts            <- ts[, -aux.pos]
  } # IF end

  # selection of an appropiate period with available data for ts
  with.data <- ts[apply(ts, MARGIN = 1, function(x) any(!is.na(x)))]
  ts.start  <- stats::start(with.data)
  ts.end    <- stats::end(with.data)

  if (!identical(from, "auto") && !identical(to, "auto")) {
    if (from > to) stop("'from' must be less than 'to'")
  } # IF end

  if (!identical(from, "auto")) {
    if (from > ts.end) stop("'from' must be less than", ts.end)
    ts.start <- from
  } # IF end

  if (!identical(to, "auto")) {
    if (to < ts.start) stop("'to' must be greater than", ts.start)
    ts.end <- to
  } # IF end

  ts        <- stats::window(ts, start = ts.start, end = ts.end)
  names(ts) <- paste0(names(ts), "_day")

  # time series aggregation
  if (tscale != "daily") {

    # Selecting the aggregation function to be used for each case (i.e.,
    # 'sum' for p, pet and q (in mm), and mean for q (in m3s-1) and temp)

    agg.fun <- ifelse(grepl(pattern = "^P|^Qobs_mm", x = colnames(ts)),
                      "sum", "mean")

    ts.list <- lapply(ts, "[")

    # monthly aggregation
    if (tscale == "monthly") {

      if (verbose) message("[ Agreggating into monthly time series ... ]")

      names(ts.list) <- gsub("day", "mon", names(ts.list))

      ts.list <- mapply(function(zoo.data, agg.fun) {
        hydroTSM::daily2monthly(x = zoo.data, FUN = agg.fun, na.rm = na.rm,
                                na.rm.max = na.rm.max)
      }, zoo.data = ts.list, agg.fun = agg.fun, SIMPLIFY = FALSE)

      ts <- do.call(zoo::merge.zoo, ts.list)
    } # IF end

    # annual aggregation
    if (tscale == "annual") {

      if (verbose) message("[ Agreggating into annual time series ... ]")

      names(ts.list) <- gsub("day", "year", names(ts.list))

      ts.list <- mapply(function(zoo.data, agg.fun) {
        hydroTSM::daily2annual(x = zoo.data, FUN = agg.fun, na.rm = na.rm,
                               na.rm.max = na.rm.max)
      }, zoo.data = ts.list, agg.fun = agg.fun, SIMPLIFY = FALSE)

      ts <- do.call(zoo::merge.zoo, ts.list)
    } # IF end

  } # IF end

  # creating the output of the function
  out <- ts

  # optional outputs
  if (include.shp || include.attr) {

    nelements <- 1
    out       <- vector("list", nelements)
    out[[1]]  <- ts
    names(out)[[1]] <- "CatchmentTS"

    # reading the .rds file containing the catchment shapefile
    if (include.shp) {
      vect.tmp    <- tempfile()
      vectContent <- httr::content(URLcontent[[grep(pattern = "\\.rds$", x = URLs)]], as = "raw")
      writeBin(vectContent, con = vect.tmp)
      out[[2]]        <- terra::readRDS(vect.tmp)
      names(out)[[2]] <- "CatchmentBorders"
      unlink(vect.tmp)
    } # IF end

    # attributes of the selected catchment
    if (include.attr) {
      catchAttr.pos  <- which(.catchAttr$gauge_id == x)
      localCatchAttr <- .catchAttr[catchAttr.pos, ]
      rownames(localCatchAttr) <- NULL
      out[[length(out) + 1]]      <- localCatchAttr
      names(out)[[length(out)]] <- "CatchmentAttributes"
    } # IF end

  } # IF end

  return(out)

} # 'getData' END