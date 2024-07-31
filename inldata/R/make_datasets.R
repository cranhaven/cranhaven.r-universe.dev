#' Create Package Datasets
#'
#' @description Create datasets for the \pkg{inldata} package and save each as an
#'   R-data file with the `.rda` extension, which is a format native to \R.
#'   The \pkg{stats} \pkg{dataRetrieval}, and \pkg{stringi} packages must be available.
#'   This function is intended for use by \pkg{inldata}-package developers.
#'
#' @param path 'character' string.
#'   Path to the package's source directory, with tilde-expansion performed.
#'   Defaults to the working directory.
#'   Ensure that under the `path` is a folder named `data-raw`
#'   that contains the raw data files required for the build process.
#' @param destdir 'character' string.
#'   Destination directory to write R-data files, with tilde-expansion performed.
#'   Defaults to the `data` directory located under `path`.
#' @param clean 'logical' flag.
#'   Whether to delete all pre-existing R-data files in the destination directory.
#' @param tz 'character' string.
#'   [Time zone][base::timezones] specification.
#'   Defaults to Mountain Standard Time (North America).
#'   See [`OlsonNames`] for time zone information.
#' @param census_yr 'integer' number.
#'   United States census year.
#' @param buffer_dist 'numeric' number.
#'   Buffer distance for the study area defined by the bounding of the sample [`sites`] dataset.
#'   Specified in units of the coordinate reference system (`crs$units`).
#' @param resolution 'numeric' number.
#'   Spatial resolution of the raster grid, in meters.
#'   Specify in units of the coordinate reference system (`crs$units`).
#' @param warn 'integer' value.
#'   Sets the handling of warning messages.
#'   Choose value of less than 0 to show no warnings, 1 to print warnings (default),
#'   and 2 to error on warnings.
#' @param timeout 'integer' number.
#'  Timeout for some of the internet operations, in minutes.
#'  Defaults to 10 minutes.
#' @param compress 'logical' flag or 'character' string.
#'   Whether compression should be used when saving a dataset to file.
#'   Character strings "auto", "gzip", "bzip2" and "xz" (default) are accepted.
#'   See the [`save`] function for details on compression types.
#' @param seed 'integer' count.
#'   Random number generator state, used to create reproducible results.
#' @param quiet 'logical' flag.
#'   Whether to suppress printing of debugging information.
#'
#' @details This function retrieves and parses datasets from local and remote sources.
#'   Access to the internet is required to download data from the following remote sources:
#'   - National Elevation Dataset ([NED](https://www.usgs.gov/publications/national-elevation-dataset))
#'     on [Amazon's Cloud](https://prd-tnm.s3.amazonaws.com/).
#'   - Spatial data from the
#'     [TIGER/Line Geodatabase](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-geodatabase-file.html)
#'     that contains spatial extracts from the U.S. Census Bureau's
#'     [MAF/TIGER database](https://www2.census.gov/geo/tiger/TGRGDB22/).
#'   - National Hydrography Dataset
#'     ([NHD](https://www.usgs.gov/national-hydrography/national-hydrography-dataset))
#'     data from the USGS NHD file geodatabase on [Amazon's Cloud](https://dmap-data-commons-ow.s3.amazonaws.com/).
#' @details Each of the package dataset's represents a snapshot of the data at a specified point in time.
#'   While geospatial datasets may change very little over time
#'   (such as the boundary of the Idaho National Laboratory),
#'   other datasets continue to grow as new data becomes available
#'   (such as water-quality data measured in [`samples`] collected from wells).
#' @details To ensure that the function retrieves the most recent data versions,
#'   it is recommended to periodically check the URLs of remote sources and update them within the function.
#'   It is also advisable to document any changes in the datasets and update their help documentation accordingly.
#' @details Files downloaded during intermediate stages of the build process
#'   are cached on your computer to speed up future builds.
#'   You can specify the path to the cache directory by setting an environment variable named `CACHE_DIR`.
#'   By default the location of the cache directory is determined by the [`get_cache_dir()`] command.
#'
#' @return Returns the paths to the newly created R Data files invisibly.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @examples
#' # Example requires that the 'path' argument be specified as
#' # the top-level directory of the inldata package repository.
#' \dontrun{
#'   make_datasets(destdir = tempfile(""))
#' }

make_datasets <- function(path = getwd(),
                          destdir = file.path(path, "data"),
                          clean = FALSE,
                          tz = "America/Denver",
                          census_yr = 2023,
                          buffer_dist = 1000,
                          resolution = 100,
                          warn = 1,
                          timeout = 10,
                          compress = "xz",
                          seed = 0L,
                          quiet = FALSE) {

  # track computation time
  dt <- Sys.time()
  message("TIMESTAMP: ", format(dt, usetz = TRUE), "\n")

  # check arguments
  path <- path.expand(path) |>
    normalizePath(winslash = "/", mustWork = FALSE)
  checkmate::assert_directory_exists(path, access = "r")
  destdir <- path.expand(destdir) |>
    normalizePath(winslash = "/", mustWork = FALSE)
  checkmate::assert_flag(clean)
  checkmate::assert_choice(tz, choices = OlsonNames())
  checkmate::assert_int(census_yr, lower = 2000)
  checkmate::assert_number(buffer_dist, lower = 0, finite = TRUE)
  checkmate::assert_number(resolution, lower = 1, finite = TRUE)
  checkmate::assert_int(warn)
  checkmate::assert_number(timeout, lower = 1, finite = TRUE)
  if (!is.logical(compress)) {
    checkmate::assert_choice(compress,
      choices = c("auto", "gzip", "bzip2", "xz")
    )
  }
  checkmate::assert_count(seed, null.ok = TRUE)
  checkmate::assert_flag(quiet)

  # check packages
  for (pkg in c("dataRetrieval", "stats", "stringi")) {
    check_package(pkg, msg = "Making datasets")
  }

  # check system dependencies
  if (!capabilities("libcurl")) {
    stop("libcurl is unavailable", call. = FALSE)
  }

  # set global options
  op <- options(warn = warn, timeout = timeout * 60, useFancyQuotes = FALSE)
  on.exit(expr = options(op))

  # set terra-package options
  local({
    terra::terraOptions(progress = 0L, verbose = !quiet)
  })

  # make temporary directory
  tmpdir <- tempfile("")
  dir.create(tmpdir, showWarnings = FALSE)
  on.exit(
    expr = unlink(tmpdir, recursive = TRUE),
    add = TRUE
  )

  # set U.S. Census URL
  census_url <- paste0("https://www2.census.gov/geo/tiger/TIGER", census_yr)
  assert_url(census_url)

  # set National Hydrography Dataset (NHD) URL
  nhd_url <- "https://dmap-data-commons-ow.s3.amazonaws.com"
  assert_url(nhd_url)

  # set The National Map (TNM) URL
  tnm_url <- "https://prd-tnm.s3.amazonaws.com"
  assert_url(tnm_url)

  # test data-raw folder exists
  paste0(path, "/data-raw") |>
    checkmate::assert_directory_exists(access = "r")

  # make coordinate reference system dataset (crs)
  message("STATUS: making 'crs' dataset ...")
  file <- file.path(path, "data-raw/misc/projection.txt")
  checkmate::assert_file_exists(file, access = "r")
  crs <- readLines(con = file, encoding = "UTF-8") |>
    sf::st_crs()
  save(crs, file = file.path(tmpdir, "crs.rda"), compress = FALSE)

  # make parameter dataset (parameters)
  message("STATUS: making 'parameters' dataset ...")
  file <- file.path(path, "data-raw/qwdata/pcodes.txt")
  checkmate::assert_file_exists(file, access = "r")
  pcodes <- utils::read.delim(file = file, header = FALSE, colClasses = "character")[[1]]
  parameters <- mds_parameters(pcodes = pcodes)

  # make laboratory detection limits dataset (dl)
  message("STATUS: making 'dl' dataset ...")
  file <- file.path(path, "data-raw/misc/detection-limits.tsv")
  checkmate::assert_file_exists(file, access = "r")
  data <- utils::read.delim(file, comment.char = "#", colClasses = "character")
  dl <- mds_dl(data, parameters)
  save(dl, file = file.path(tmpdir, "dl.rda"), compress = FALSE)

  # make water-quality samples dataset (samples)
  message("STATUS: making 'samples' dataset ...")
  file <- file.path(path, "data-raw/qwdata/output.rdb")
  checkmate::assert_file_exists(file, access = "r")
  data <- read_rdb(file)
  file <- file.path(path, "data-raw/misc/alpha-codes.tsv")
  checkmate::assert_file_exists(file, access = "r")
  alpha_codes <- utils::read.delim(file, colClasses = "character")
  file <- file.path(path, "data-raw/misc/counting-error.tsv")
  checkmate::assert_file_exists(file, access = "r")
  cnt_error <- utils::read.delim(file, colClasses = "character")
  samples <- mds_samples(data, alpha_codes, cnt_error, tz, dl, parameters, seed)
  save(samples, file = file.path(tmpdir, "samples.rda"), compress = FALSE)

  # continue making parameter dataset (parameters)
  d <- tabulate_parm_data(parameters, samples)
  parameters <- merge(parameters, d, by = "pcode", sort = FALSE)
  save(parameters, file = file.path(tmpdir, "parameters.rda"), compress = FALSE)

  # make benchmark concentrations dataset (benchmarks)
  message("STATUS: making 'benchmarks' dataset ...")
  file <- file.path(path, "data-raw/misc/benchmarks.csv")
  checkmate::assert_file_exists(file, access = "r")
  bm <- utils::read.csv(file,
    na.strings = c("NA", ""),
    strip.white = TRUE,
    colClasses = "character",
    check.names = FALSE
  )
  file <- file.path(path, "data-raw/misc/benchmarks-extras.tsv")
  checkmate::assert_file_exists(file, access = "r")
  mcl_extras <- utils::read.delim(file,
    strip.white = TRUE,
    colClasses = "character"
  )
  benchmarks <- mds_benchmarks(bm, mcl_extras, parameters)
  save(benchmarks, file = file.path(tmpdir, "benchmarks.rda"), compress = FALSE)

  # make site information dataset (sites)
  message("STATUS: making 'sites' dataset ...")
  file <- file.path(path, "data-raw/qwdata/siteids.txt")
  checkmate::assert_file_exists(file, access = "r")
  data <- utils::read.delim(file = file, header = FALSE, colClasses = "character")
  colnames(data) <- c("agency_cd", "site_no", "station_nm", "network_cd", "pos")
  sites <- mds_sites(data, crs)

  # make digital elevation model dataset (dem)
  message("STATUS: making 'dem' dataset ...")
  ids <- c("n44w113", "n44w114", "n45w113", "n45w114")
  urls <- sprintf("%s/StagedProducts/Elevation/13/TIFF/current/%s/USGS_13_%s.tif",
    tnm_url, ids, ids
  )
  data <- do.call(terra::merge,
    lapply(urls,
      FUN = function(url) {
        file <- download_file(url, quiet = quiet)
        terra::rast(file)
      }
    )
  )
  extent <- sf::st_buffer(sites, dist = buffer_dist * 10) |> sf::st_bbox()
  dem <- mds_dem(data, crs, extent, resolution)

  # make mountains dataset (mountains)
  message("STATUS: making 'mountains' dataset ...")
  file <- file.path(path, "data-raw/misc/mountain-names.geojson")
  checkmate::assert_file_exists(file, access = "r")
  data <- sf::st_read(dsn = file, quiet = quiet)
  mountains <- mds_mountains(data, crs, dem)

  # crop dem and mountains datasets
  extent <- sf::st_buffer(sites, dist = buffer_dist) |> sf::st_bbox()
  dem <- terra::crop(dem, extent)
  extent <- sf::st_bbox(dem)
  mountains <- clean_sf(mountains, crs = crs, extent = extent, type = "POLYGON")
  dem <- terra::wrap(dem)
  save(dem, file = file.path(tmpdir, "dem.rda"), compress = FALSE)
  save(mountains, file = file.path(tmpdir, "mountains.rda"), compress = FALSE)

  # make surface-water measurements dataset (swm)
  message("STATUS: making 'swm' dataset ...")
  swm <- mds_swm(sites, tz)
  save(swm, file = file.path(tmpdir, "swm.rda"), compress = FALSE)

  # make groundwater levels dataset (gwl)
  message("STATUS: making 'gwl' dataset ...")
  gwl <- mds_gwl(sites, tz)
  save(gwl, file = file.path(tmpdir, "gwl.rda"), compress = FALSE)

  # continue making site information dataset (sites)
  d <- tabulate_site_data(sites, samples, gwl, swm)
  sites <- merge(sites, d, by = "site_no", sort = FALSE)
  save(sites, file = file.path(tmpdir, "sites.rda"), compress = FALSE)

  # make units of measurment dataset (units)
  message("STATUS: making 'units' dataset ...")
  file <- file.path(path, "data-raw/misc/units.tsv")
  checkmate::assert_file_exists(file, access = "r")
  data <- utils::read.delim(file,
    comment.char = "#",
    na.strings = "",
    colClasses = "character"
  )
  units <- mds_units(data)
  save(units, file = file.path(tmpdir, "units.rda"), compress = FALSE)

  # make background concentrations dataset (background)
  message("STATUS: making 'background' dataset ...")
  file <- file.path(path, "data-raw/misc/background.tsv")
  checkmate::assert_file_exists(file, access = "r")
  data <- utils::read.delim(file,
    strip.white = TRUE,
    colClasses = "character"
  )
  background <- mds_background(data, parameters)
  save(background, file = file.path(tmpdir, "background.rda"), compress = FALSE)

  # make eastern Snake River Plain boundary dataset (esrp)
  message("STATUS: making 'esrp' dataset ...")
  file <- file.path(path, "data-raw/misc/esrp.geojson")
  checkmate::assert_file_exists(file, access = "r")
  data <- sf::st_read(dsn = file, quiet = quiet)
  esrp <- clean_sf(data, cols = "geometry", crs = crs)
  save(esrp, file = file.path(tmpdir, "esrp.rda"), compress = FALSE)

  # make Idaho National Laboratory boundary dataset (inl)
  message("STATUS: making 'inl' dataset ...")
  file <- file.path(path, "data-raw/misc/inl.geojson")
  checkmate::assert_file_exists(file, access = "r")
  data <- sf::st_read(dsn = file, quiet = quiet)
  inl <- clean_sf(data, cols = "geometry", crs = crs)
  save(inl, file = file.path(tmpdir, "inl.rda"), compress = FALSE)

  # make industrial waste ditch dataset (iwd)
  message("STATUS: making 'iwd' dataset ...")
  file <- file.path(path, "data-raw/misc/nrfiwd.geojson")
  checkmate::assert_file_exists(file, access = "r")
  data <- sf::st_read(dsn = file, quiet = quiet)
  iwd <- clean_sf(data, cols = "geometry", crs = crs)
  save(iwd, file = file.path(tmpdir, "iwd.rda"), compress = FALSE)

  # make Idaho National Laboratory facilities dataset (facilities)
  message("STATUS: making 'facilities' dataset ...")
  file <- file.path(path, "data-raw/misc/facilities.geojson")
  checkmate::assert_file_exists(file, access = "r")
  data <- sf::st_read(dsn = file, quiet = quiet)
  facilities <- mds_facilities(data, crs = crs)
  save(facilities, file = file.path(tmpdir, "facilities.rda"), compress = FALSE)

  # make percolation ponds dataset (percponds)
  message("STATUS: making 'percponds' dataset ...")
  file <- file.path(path, "data-raw/misc/percponds.geojson")
  checkmate::assert_file_exists(file, access = "r")
  data <- sf::st_read(dsn = file, quiet = quiet)
  percponds <- mds_percponds(data, crs = crs)
  save(percponds, file = file.path(tmpdir, "percponds.rda"), compress = FALSE)

  # make state of Idaho boundary dataset (idaho)
  message("STATUS: making 'idaho' dataset ...")
  url <- sprintf("%s/STATE/tl_%s_us_state.zip", census_url, census_yr)
  files <- download_file(url, quiet = quiet) |> extract_archive()
  file <- grep(".shp$", files, value = TRUE)
  data <- sf::st_read(dsn = file, quiet = quiet)
  idaho <- mds_idaho(data, crs)
  save(idaho, file = file.path(tmpdir, "idaho.rda"), compress = FALSE)

  # make cities and towns dataset (cities)
  message("STATUS: making 'cities' dataset ...")
  url <- sprintf("%s/PLACE/tl_%s_16_place.zip", census_url, census_yr)
  files <- download_file(url, quiet = quiet) |> extract_archive()
  file <- grep(".shp$", files, value = TRUE)
  data <- sf::st_read(dsn = file, quiet = quiet)
  cities <- mds_cities(data, crs, extent)
  save(cities, file = file.path(tmpdir, "cities.rda"), compress = FALSE)

  # make county boundaries dataset (counties)
  message("STATUS: making 'counties' dataset ...")
  url <- sprintf("%s/COUNTY/tl_%s_us_county.zip", census_url, census_yr)
  files <- download_file(url, quiet = quiet) |> extract_archive()
  file <- grep(".shp$", files, value = TRUE)
  data <- sf::st_read(dsn = file, quiet = quiet)
  counties <- mds_counties(data, crs, extent)
  save(counties, file = file.path(tmpdir, "counties.rda"), compress = FALSE)

  # make road netowrk dataset (roads)
  message("STATUS: making 'roads' dataset ...")
  urls <- sprintf("%s/ROADS/tl_%s_%s_roads.zip", census_url, census_yr, counties$id)
  all_data <- do.call(rbind,
    lapply(urls, function(url) {
      files <- download_file(url, quiet = quiet) |> extract_archive()
      file <- grep(".shp$", files, value = TRUE)
      sf::st_read(dsn = file, quiet = quiet)
    })
  )
  url <- sprintf("%s/PRISECROADS/tl_%s_16_prisecroads.zip", census_url, census_yr)
  files <- download_file(url, quiet = quiet) |> extract_archive()
  file <- grep(".shp$", files, value = TRUE)
  prisec_data <- sf::st_read(dsn = file, quiet = quiet)
  roads <- mds_roads(all_data, prisec_data, crs, extent)
  save(roads, file = file.path(tmpdir, "roads.rda"), compress = FALSE)

  # make lakes and ponds dataset (lakes)
  message("STATUS: making 'lakes' dataset ...")
  url <- paste0(nhd_url, "/NHDPlusV21/Data/NHDPlusPN/NHDPlusV21_PN_17_NHDSnapshot_08.7z")
  files <- download_file(url, quiet = quiet) |> extract_archive()
  file <- grep("NHDWaterbody.shp$", files, value = TRUE)
  data <- sf::st_read(dsn = file, quiet = quiet)
  lakes <- mds_lakes(data, crs, extent)
  save(lakes, file = file.path(tmpdir, "lakes.rda"), compress = FALSE)

  # make rivers and streams dataset (streams)
  message("STATUS: making 'streams' dataset ...")
  url <- paste0(nhd_url, "/NHDPlusV21/Data/NHDPlusPN/NHDPlusV21_PN_17_NHDSnapshot_08.7z")
  files <- download_file(url, quiet = quiet) |> extract_archive()
  file <- grep("NHDFlowline.shp$", files, value = TRUE)
  data <- sf::st_read(dsn = file, quiet = quiet)
  streams <- mds_streams(data, crs, extent)
  save(streams, file = file.path(tmpdir, "streams.rda"), compress = FALSE)

  # compress temporary files
  message("STATUS: compress dataset files ...")
  if (is.character(compress) || compress) {
    tools::resaveRdaFiles(paths = tmpdir, compress = compress, version = 3L)
  }

  # copy temporary files to destination directory
  message("STATUS: copy dataset files to destination directory ...")
  dir.create(destdir, showWarnings = FALSE, recursive = TRUE)
  if (clean) {
    sprintf("%s/*.rda", destdir) |> unlink()
  }
  files <- list.files(path = tmpdir, full.names = TRUE)
  file.copy(from = files, to = destdir, overwrite = TRUE)

  # get paths to files in destination directory
  paths <- file.path(destdir, basename(files), fsep = "/")

  # print computation time
  message("\n", "DURATION: ", format(Sys.time() - dt, usetz = TRUE))

  invisible(paths)
}


# Parameter Information for Analytes (parameters) -----

mds_parameters <- function(pcodes) {

  # check arguments
  checkmate::assert_character(pcodes, any.missing = FALSE, min.len = 1)

  # check for duplicated values
  if (any(is <- duplicated(pcodes))) {
    txt <- pcodes[is] |> sQuote() |> paste(collapse = ", ")
    stop("Duplicated parameter codes: ", txt, call. = FALSE)
  }

  # download data
  d <- dataRetrieval::readNWISpCode(pcodes)

  cols <- c(
    "parm_nm" = "parameter_nm",
    "pcode" = "parameter_cd",
    "parm_group_nm" = "parameter_group_nm",
    "casrn" = "casrn",
    "srsname" = "srsname",
    "unit_cd" = "parameter_units"
  )
  d <- d[, cols]
  colnames(d) <- names(cols)

  d$casrn <- trimws(d$casrn)
  d$casrn[d$casrn == ""] <- NA_character_

  d$srsname <- trimws(d$srsname)
  d$srsname[d$srsname == "Trihalomethanes (four), total, from SDWA NPDWR"] <- "TTHM4"
  d$srsname[d$srsname == ""] <- NA_character_

  d$unit_cd <- trimws(d$unit_cd)
  subs <- c(
    "mg/L" = "^mg/l$",
    "mg/L as CaCO3" = "^mg/l CaCO3$",
    "mg/L " = "^mg/l ",
    "ug/L" = "^ug/l$",
    "ug/L " = "^ug/l "
  )
  for (i in seq_along(subs)) {
    d$unit_cd <- sub(
      pattern = subs[i],
      replacement = names(subs)[i],
      x = d$unit_cd
    )
  }
  d$unit_cd[d$unit_cd == ""] <- NA_character_

  # convert classes
  d$parm_group_nm <- as.factor(d$parm_group_nm)
  d$unit_cd <- as.factor(d$unit_cd)

  idxs <- tolower(d$parm_nm) |> stringi::stri_order(numeric = TRUE)
  d <- d[idxs, ]
  rownames(d) <- NULL

  d
}


# Laboratory Detection Limits (dl) -----

mds_dl <- function(data, parameters) {

  # check arguments
  checkmate::assert_data_frame(data, types = "character", min.rows = 1, col.names = "named")
  checkmate::assert_data_frame(parameters, min.rows = 1, col.names = "named")

  # merge in parameter name
  d <- merge(x = data, y = parameters[, c("pcode", "parm_nm")], by = "pcode")

  # convert column class
  d$unit_cd <- as.factor(d$unit_cd)
  d$lab_det_lim_va <- as.numeric(d$lab_det_lim_va)
  d$min_dt <- as.Date(d$min_dt)
  d$reference <- as.factor(d$reference)

  # unit conversion
  conversion <- convert_units(d, parameters)
  d$lab_det_lim_va <- d$lab_det_lim_va * conversion$mult

  # order and remove row names
  cols <- c("parm_nm", "pcode", "lab_det_lim_va", "min_dt", "reference")
  idxs <- tolower(d$srsname) |>
    stringi::stri_order(numeric = TRUE)
  d <- d[idxs, cols]
  rownames(d) <- NULL

  d
}


# Water-Quality Data Records (samples) -----

mds_samples <- function(d, alpha_codes, cnt_error, tz, dl, parameters, seed) {

  # check arguments
  checkmate::assert_data_frame(d,
    types = "character",
    min.rows = 1,
    col.names = "named"
  )
  checkmate::assert_data_frame(alpha_codes,
    types = "character",
    min.rows = 1,
    col.names = "named"
  )
  checkmate::assert_data_frame(cnt_error,
    types = "character",
    min.rows = 1,
    col.names = "named"
  )
  checkmate::assert_string(tz)
  checkmate::assert_data_frame(dl, min.rows = 1, col.names = "named")
  checkmate::assert_data_frame(parameters, min.rows = 1, col.names = "named")
  checkmate::assert_count(seed, null.ok = TRUE)

  # translate alpha codes
  dic <- alpha_codes$nwis_cd
  names(dic) <- alpha_codes$alpha_cd
  colnames(d) <- dic[colnames(d)]
  idxs <- which(dic[colnames(d)] == "")
  if (length(idxs) > 0) {
    txt <- colnames(d)[idxs] |> sQuote() |> paste(collapse = ", ")
    warning("Missing database names for ALPHA codes:\n  ", txt, call. = FALSE, immediate. = TRUE)
    d <- d[, -idxs]
  }

  # set result units
  idxs <- match(d$pcode, parameters$pcode)
  if (any(is <- is.na(idxs))) {
    txt <- d$pcode[idxs][is] |> sQuote() |> paste(collapse = ", ")
    stop("Samples contains unrecognized parameter codes: ", txt, call. = FALSE)
  }
  d$unit_cd <- parameters$unit_cd[idxs]

  # set site name
  site_nm <- parse_station_nm(d$station_nm)
  d <- data.frame(site_nm, d)

  # account for missing time
  is <- d$sample_tm %in% c("", NA)
  d$sample_tm[is] <- "1200"
  d$sample_dt <- paste(d$sample_dt, d$sample_tm) |>
    as.POSIXct(tz = tz, format = "%Y%m%d %H%M")
  d$sample_tm <- NULL

  # get the number of digits to the right of the decimal point in a number
  d$result_scale_va <- sub("\\d+\\.?(.*)$", "\\1", d$result_va) |>
    nchar()

  d$result_va <- as.numeric(d$result_va)
  d$anl_stat_cd <- NULL

  for (i in grep("_va$", colnames(d))) {
    d[[i]] <- as.numeric(d[[i]])
  }

  # remove unnecessary columns
  cols <- c("station_nm", "parm_nm")
  idxs <- match(cols, colnames(d)) |>
    stats::na.omit()
  if (length(idxs) > 0) {
    d <- d[, -idxs]
  }

  # remove records that are duplicated in the NWIS and QWDATA databases
  idxs <- which(colnames(d) %in% c("db_no", "dqi_cd"))
  is <- duplicated(d[, -idxs])
  d <- d[!is, ]

  # remove contaminated results
  is <- d$remark_cd %in% "V"
  d <- d[!is, ]

  # initialize remark
  d$remark <- nrow(d) |> character()

  # report zero and negative results as nondetects
  is <- d$remark_cd %in% "<" & !is.na(d$result_va) & d$result_va <= 0
  d$remark_cd[is] <- NA_character_
  txt <- "Change remark code from '<' (nondetect) to '' because result value is less than or equal to 0"
  d$remark[is] <- paste_strings(d$remark[is], txt, collapse = "; ", recycle0 = TRUE)

  # identify radiochemical parameter codes
  is <- parameters$pcode %in% d$pcode & parameters$parm_group_nm %in% "Radiochemical"
  radchem_pcode <- parameters$pcode[is]

  # report radiochemical nondetects as less than the reporting level
  is <- d$remark_cd %in% "R" & d$pcode %in% radchem_pcode
  d$result_va[is] <- d$rpt_lev_va[is]
  d$remark_cd[is] <- "<"
  txt <- "Substitute result value with reporting level value and change remark code from 'R' to '<'"
  d$remark[is] <- paste_strings(d$remark[is], txt, collapse = "; ", recycle0 = TRUE)

  # check sample type codes
  choices <- c(
    "blank" = "2",
    "reference material" = "6",
    "replicate" = "7",
    "regular" = "9",
    "not determined" = "A",
    "other QA" = "B",
    "composite (time)" = "H"
  )
  unique(d$sample_type_cd) |>
    checkmate::assert_subset(choices = choices, empty.ok = FALSE)

  # assume unrecorded sample types are regular environmental samples
  is <- d$sample_type_cd %in% "A"
  d$sample_type_cd[is] <- "9"
  txt <- "Change sample type code from 'A' (not recorded) to '9' (regular sample)"
  d$remark[is] <- paste_strings(d$remark[is], txt, collapse = "; ", recycle0 = TRUE)

  # add column that identifies a unique sample
  d$sample_id <- paste0(
    d$site_no,
    d$medium_cd,
    format(d$sample_dt, format = "%Y%m%d%H%M", tz = "GMT")
  )

  # convert counting error to standard deviaiton
  is_lt_1989 <- d$sample_dt < as.POSIXct("2008-01-01")
  for (i in seq_len(nrow(cnt_error))) {
    cd <- cnt_error$pcode[i]
    ce <- cnt_error$pcode_ce[i]

    is_cd <- d$pcode == cd & is.na(d$lab_sd_va)
    is_ce <- d$pcode == ce

    d0 <- d[is_cd, "sample_id", drop = FALSE]
    d0$idx <- rownames(d0) |> as.integer()
    d1 <- d[is_ce, c("sample_id", "result_va")]
    d0 <- merge(d0, d1, by = "sample_id", all.x = TRUE)
    d$lab_sd_va[is_cd] <- d0[order(d0$idx), "result_va"]

    txt <- "Determine laboratory standard deviation from counting error"
    d$remark[is_cd] <- paste_strings(d$remark[is_cd], txt, collapse = "; ", recycle0 = TRUE)

    # workaround for data entered into NWIS with uncertainty of 2s
    is <- is_cd & is_lt_1989
    d$lab_sd_va[is] <- d$lab_sd_va[is] / 2

    txt <- "Divide laboratory standard deviation by 2 because sample collected before Jan 1, 2008"
    d$remark[is] <- paste_strings(d$remark[is], txt, collapse = "; ", recycle0 = TRUE)
  }

  # set identifier for replicate-sample pairs
  d$id <- seq_len(nrow(d))

  # subset replicate samples
  env_samp <- d[d$sample_type_cd %in% "9", ]
  rep_samp <- d[d$sample_type_cd %in% "7", ]

  # convert to day
  env_day <- as.Date(env_samp$sample_dt) |> as.integer()
  rep_day <- as.Date(rep_samp$sample_dt) |> as.integer()

  # convert to numeric in seconds
  env_sec <- as.numeric(env_samp$sample_dt)
  rep_sec <- as.numeric(rep_samp$sample_dt)

  # identify replicate pair samples
  ids <- vapply(seq_along(rep_sec), function(i) {
    idxs <- {
      env_samp$site_no == rep_samp$site_no[i] & # match site number
      env_samp$pcode == rep_samp$pcode[i] & # match parameter code
      env_day == rep_day[i] # same day
    } |> which()
    if (length(idxs) == 0) {
      return(NA_integer_)
    }
    tdiff <- env_sec[idxs] - rep_sec[i]
    idx <- abs(tdiff) |> which.min()
    if (length(idx) == 0) {
      return(NA_integer_)
    }
    env_samp$id[idxs][idx]
  }, integer(1))

  # warn if replicate record is unpaired
  orphans <- rep_samp[is.na(ids), ]
  ignore_pcode <- c(
    "99111", # type of quality assurance data associated with sample, code
    "99105", # type of replicate, code
    "99102", # type of blank sample, code
    "99101", # source of blank solution, code
    "99100", # type of blank solution, code
    "84164", # sampler type, code
    "82398", # sampling method, code
    "71999" # sample purpose, code
  )
  is <- orphans$pcode %in% ignore_pcode
  orphans <- orphans[!is, ]
  if (nrow(orphans) > 0) {
    txt <- sprintf("  %s '%s' %s %s",
      orphans$site_no, orphans$site_nm, orphans$pcode, orphans$sample_dt
    ) |>
      unique() |>
      sort()
    sprintf("Unable to pair %d replicate records:", length(txt)) |>
      warning(call. = FALSE, immediate. = TRUE)
    paste(txt, collapse = "\n") |> message()
  }

  # pair replicate samples
  m <- cbind("env" = ids, "rep" = rep_samp$id)
  m <- m[!is.na(ids), ]
  d$rep_pair_id <- NA_integer_
  d$rep_pair_id[m[, "env"]] <- m[, "env"]
  d$rep_pair_id[m[, "rep"]] <- m[, "env"]
  d$id <- NULL

  # set laboratory detection limits
  d$lab_det_lim_va <- NA_real_
  for (i in seq_len(nrow(dl))) {
    is <- d$pcode == dl$pcode[i] & as.Date(d$sample_dt) >= dl$min_dt[i]
    d$lab_det_lim_va[is] <- dl$lab_det_lim_va[i]
  }

  # represent radionuclide result value using confidence interval
  conf <- 0.95
  set.seed(seed)
  error <- d$lab_sd_va * stats::qnorm(1 - (1 - conf) / 2)
  error[is.na(error)] <- 0
  li <- d$result_va - error
  ui <- d$result_va + error
  is <- is.finite(li) & is.finite(d$lab_det_lim_va) & li < d$lab_det_lim_va
  li[is] <- 0
  is <- is.finite(ui) & is.finite(d$lab_det_lim_va) & ui < d$lab_det_lim_va
  li[is] <- 0
  ui[is] <- d$lab_det_lim_va[is]
  is <- is.finite(li) & li < 0
  li[is] <- 0
  is <- is.finite(ui) & ui < 0
  li[is] <- 0
  ui[is] <- 0
  li <- round_numbers(li, digits = d$result_scale_va)
  ui <- round_numbers(ui, digits = d$result_scale_va)
  d$lab_li_va <- li
  d$lab_ui_va <- ui

  # represent nondetect result value using confidence interval
  is <- d$remark_cd %in% "<"
  d$lab_li_va[is] <- 0
  d$lab_ui_va[is] <- d$result_va[is]

  # set correct date type for analysis date
  d$anl_dt <- as.POSIXct(d$anl_dt, tz = tz, format = "%Y%m%d")

  # convert character to factor class
  cols <- c(
    "medium_cd",
    "db_no",
    "remark_cd",
    "dqi_cd",
    "rpt_lev_cd",
    "sample_type_cd",
    "unit_cd"
  )
  for (col in cols) {
    d[[col]] <- as.factor(d[[col]])
  }

  # sort records
  idxs <- order(
    stringi::stri_rank(tolower(d$site_nm), numeric = TRUE),
    stringi::stri_rank(tolower(d$parm_short_nm), numeric = TRUE),
    d$sample_dt
  )
  cols <- c(
    "site_nm",
    "sample_dt",
    "parm_short_nm",
    "unit_cd",
    "remark_cd",
    "result_va",
    "lab_sd_va",
    "lab_li_va",
    "lab_ui_va",
    "rpt_lev_va",
    "rpt_lev_cd",
    "medium_cd",
    "anl_ent_cd",
    "dqi_cd",
    "meth_cd",
    "sample_type_cd",
    "db_no",
    "sample_id",
    "site_no",
    "pcode",
    "rep_pair_id",
    "result_tx",
    "remark",
    "anl_dt"
  )
  d <- d[idxs, cols]
  rownames(d) <- NULL

  d
}


# Benchmark Concentrations (benchmarks) -----

mds_benchmarks <- function(bm, mcl_extras, parameters) {

  # check arguments
  checkmate::assert_data_frame(bm,
    types = "character",
    min.rows = 1,
    col.names = "named"
  )
  checkmate::assert_data_frame(mcl_extras,
    types = "character",
    min.rows = 1,
    col.names = "named"
  )
  checkmate::assert_data_frame(parameters,
    min.rows = 1,
    col.names = "named"
  )

  # set column names
  cols <- c(
    "srsname" = "Chemical Name",
    "casrn" = "CAS Registry Number",
    "pcode" = "USGS Parameter Code",
    "class" = "Chemical Class",
    "mcl" = "MCL (micrograms/L)",
    "hhbp_noncancer" = "Chronic Noncancer HHBP (micrograms/L)",
    "hhbp_cancer" = "Carcinogenic HHBP (micrograms/L)",
    "hbsl_noncancer" = "Noncancer HBSL (micrograms/L)",
    "hbsl_cancer" = "Cancer HBSL (micrograms/L)",
    "remark" = "Benchmark Remarks"
  )
  d <- bm[, cols]
  colnames(d) <- names(cols)

  d$srsname <- NULL
  d$mcl <- as.numeric(d$mcl)
  d$hhbp_noncancer <- as.numeric(d$hhbp_noncancer)
  d$hbsl_noncancer <- as.numeric(d$hbsl_noncancer)
  FUN <- function(x, idx) {
    vapply(strsplit(x, split = "-"),
      FUN = function(y) {
        as.numeric(y[idx])
      },
      FUN.VALUE = numeric(1)
    )
  }
  d$hhbp_cancer_min <- FUN(d$hhbp_cancer, 1)
  d$hhbp_cancer_max <- FUN(d$hhbp_cancer, 2)
  d$hbsl_cancer_min <- FUN(d$hbsl_cancer, 1)
  d$hbsl_cancer_max <- FUN(d$hbsl_cancer, 2)

  is <- grepl("^mrem/yr", d$remark)
  d$mcl[is] <- 50 # screening level

  l <- strsplit(d$pcode, split = ", ")
  idxs <- lapply(seq_along(l),
    FUN = function(i) {
      rep(i, length(l[[i]]))
    }
  ) |>
    unlist()
  d <- d[idxs, ]
  d$pcode <- unlist(l)

  d$unit_cd <- "ug/L"

  p <- parameters[, c("pcode", "parm_nm")]
  d <- merge(p, d, by = "pcode", all.x = TRUE, sort = FALSE)

  mcl_extras$mcl <- as.numeric(mcl_extras$mcl)
  idxs <- match(mcl_extras$pcode, d$pcode)
  d[idxs, colnames(mcl_extras)] <- mcl_extras[]
  d$mcl[idxs] <- mcl_extras$mcl |> as.numeric()
  d$unit_cd[idxs] <- mcl_extras$unit_cd

  cols <- c(
    "mcl",
    "hhbp_noncancer",
    "hhbp_cancer_min",
    "hhbp_cancer_max",
    "hbsl_noncancer",
    "hbsl_cancer_min",
    "hbsl_cancer_max"
  )
  is <- is.na(d[, cols]) |>
    apply(MARGIN = 1, FUN = all)
  d <- d[!is, ]
  conversion <- convert_units(d, parameters)
  d[, cols] <- d[, cols] * conversion$mult

  idxs <- tolower(d$parm_nm) |>
    stringi::stri_order(numeric = TRUE)
  cols <- c("parm_nm", "pcode", cols, "remark")
  d <- d[idxs, cols]
  rownames(d) <- NULL

  d
}


# Site Information (sites) -----

mds_sites <- function(data, crs) {

  # check arguments
  checkmate::assert_data_frame(data,
    types = "character",
    min.rows = 1,
    col.names = "named"
  )
  checkmate::assert_class(crs, "crs")

  # check for duplicated values
  if (any(is <- duplicated(data$site_no))) {
    txt <- sQuote(data$site_no[is]) |> paste(collapse = ", ")
    stop("Duplicated site numbers: ", txt, call. = FALSE)
  }

  # download data from NWIS
  d <- dataRetrieval::readNWISsite(siteNumbers = unique(data$site_no))

  # change class
  data$pos <- as.integer(data$pos)

  # merge in metadata
  d <- merge(d, data[, c("site_no", "network_cd", "pos")], by = "site_no")

  # check altitude datum
  is <- !(d$alt_datum_cd %in% "NAVD88")
  if (any(is)) {
    warning(
      "Unrecognized altitude datum code in sites, will assume NAVD88:",
      call. = FALSE,
      immediate. = TRUE
    )
    sprintf("  %s '%s' datum '%s'",
      d$site_no[is], d$station_nm[is], d$alt_datum_cd[is]
    ) |>
      paste(collapse = "\n") |> message()
    d$alt_datum_cd[is] <- "NAVD88"
  }

  d$site_nm <- parse_station_nm(d$station_nm)
  d$station_nm <- trim_station_nm(d$station_nm)

  d$completion_cd <- "O"
  is <- grepl(" PORT", d$station_nm) & grepl(" ZONE", d$station_nm)
  d$completion_cd[is] <- "M"
  x <- d[, c("lat_va", "long_va")]
  is <- !is & (duplicated(x) | duplicated(x, fromLast = TRUE))
  d$completion_cd[is] <- "P"
  is <- d$network_cd == "S"
  d$completion_cd[is] <- NA_character_

  d$construction_dt <- as.character(d$construction_dt) |>
    as.Date(tryFormats = c("%Y%m%d", "%Y%m", "%Y"))

  coord_accuracies <- c(
    "H" = 0.01,
    "1" = 0.1,
    "5" = 0.5,
    "S" = 1,
    "T" = 10,
    "R" = 3,
    "F" = 5
  )
  checkmate::assert_subset(d$coord_acy_cd,
    choices = names(coord_accuracies)
  )
  d$coord_acy_va <- coord_accuracies[d$coord_acy_cd]

  cols <- c(
    "coord_meth_cd",
    "coord_acy_va",
    "alt_meth_cd",
    "reliability_cd",
    "aqfr_type_cd",
    "nat_aqfr_cd",
    "aqfr_cd",
    "depth_src_cd",
    "completion_cd",
    "huc_cd",
    "network_cd"
  )
  for (i in cols) {
    d[[i]][d[[i]] == ""] <- NA_character_
    d[[i]] <- as.factor(d[[i]])
  }

  idxs <- order(
    d$network_cd,
    stringi::stri_rank(tolower(d$site_nm), numeric = TRUE),
    d$well_depth_va
  )
  cols <- c(
    "dec_long_va",
    "dec_lat_va",
    "site_nm",
    "station_nm",
    "site_no",
    "coord_meth_cd",
    "coord_acy_va",
    "alt_va",
    "alt_meth_cd",
    "alt_acy_va",
    "huc_cd",
    "construction_dt",
    "reliability_cd",
    "nat_aqfr_cd",
    "aqfr_cd",
    "aqfr_type_cd",
    "well_depth_va",
    "hole_depth_va",
    "depth_src_cd",
    "completion_cd",
    "network_cd",
    "pos"
  )
  d <- d[idxs, cols]
  rownames(d) <- NULL
  d <- droplevels(d)

  sp <- sf::st_as_sf(d,
    coords = c("dec_long_va", "dec_lat_va"),
    crs = sf::st_crs("+proj=longlat +datum=NAD83")
  ) |>
    sf::st_make_valid() |>
    sf::st_transform(crs = crs)

  sp
}


# Surface-Water Measurements (swm) -----

mds_swm <- function(sites, tz) {

  # check arguments
  checkmate::assert_class(sites, classes = "sf")
  checkmate::assert_string(tz)

  # download data from NWIS
  d <- dataRetrieval::readNWISmeas(
    siteNumbers = sites$site_no,
    tz = tz,
    convertType = TRUE
  )

  # set measurement date-time value
  d$stage_dt <- d$measurement_dateTime
  if (anyNA(d$stage_dt)) {
    stop("Missing timestamp", call. = FALSE)
  }

  # add local site names
  idxs <- match(d$site_no, sites$site_no)
  d$site_nm <- sites$site_nm[idxs]

  # remove duplicated timestamp records
  is <- duplicated(d[, c("site_no", "stage_dt")])
  if (any(is)) {
    warning("Removed duplicated timestamp records:", call. = FALSE, immediate. = TRUE)
    sprintf("  %s '%s' %s",
      d$site_no[is], d$site_nm[is], d$stage_dt[is]
    ) |>
      paste(collapse = "\n") |>
      message()
    d <- d[!is, ]
  }

  # add local site names
  idxs <- match(d$site_no, sites$site_no)
  d$site_nm <- sites$site_nm[idxs]

  # set measurement values
  d$stage_va <- as.numeric(d$gage_height_va)
  d$disch_va <- as.numeric(d$discharge_va)
  is <- is.na(d$stage_va) & is.na(d$disch_va)
  d <- d[!is, ]

  # add measurment accuracy
  qual <- as.character(d$measured_rating_diff)
  per_unc <- c(
    "Excellent" = 2,
    "Good" = 5,
    "Fair" = 8,
    "Poor" = 10,
    "Unknown" = NA_real_,
    "Unspecified" = NA_real_
  )
  idxs <- match(qual, names(per_unc))
  d$frac_unc <- per_unc[idxs] / 100
  d$stage_acy_va <- (d$stage_va * d$frac_unc) |> round_numbers(digits = 2)
  d$disch_acy_va <- (d$disch_va * d$frac_unc) |> round_numbers(digits = 2)

  # sort records
  idxs <- tolower(d$site_nm) |>
    stringi::stri_rank(numeric = TRUE) |>
    order(d$site_no, d$stage_dt)
  cols <- c(
    "site_nm",
    "site_no",
    "stage_dt",
    "stage_va",
    "disch_va",
    "stage_acy_va",
    "disch_acy_va"
  )
  d <- d[idxs, cols]
  rownames(d) <- NULL

  d
}


# Groundwater Levels (gwl) -----

mds_gwl <- function(sites, tz) {

  # check arguments
  checkmate::assert_class(sites, classes = "sf")
  checkmate::assert_string(tz)

  # download data from NWIS
  d <- dataRetrieval::readNWISgwl(
    siteNumbers = sites$site_no,
    parameterCd = c(
      "72019", # depth to water level, in feet below land surface.
      "62611" # groundwater level above NAVD 88, in feet
    ),
    convertType = FALSE
  )

  # add local site names
  idxs <- match(d$site_no, sites$site_no)
  d$site_nm <- sites$site_nm[idxs]

  # set measurement date-time value
  datetime <- as_posix_ct(dt = d$lev_dt, tm = d$lev_tm, tz = tz)
  is <- is.na(datetime)
  if (any(is)) {
    warning("Unknown date-time format, removed records:", call. = FALSE, immediate. = TRUE)
    sprintf("  %s '%s' date '%s' time '%s'",
      d$site_no[is], d$site_nm[is], d$lev_dt[is], d$lev_tm[is]
    ) |>
      paste(collapse = "\n") |>
      message()
  }
  d$lev_dt <- datetime
  d <- d[!is, ]

  # set measurement values
  d$lev_va <- as.numeric(d$lev_va)
  d$sl_lev_va <- as.numeric(d$sl_lev_va)

  # place water-level depth and elevation on same row
  d$id <- paste(d$site_no, d$site_tp_cd, d$lev_dt)
  sl <- d[d$parameter_cd == "62611", c("id", "sl_lev_va")]
  cols <- c(
    "id",
    "site_nm",
    "site_no",
    "lev_dt",
    "lev_acy_cd",
    "lev_meth_cd",
    "lev_status_cd",
    "lev_age_cd",
    "lev_va"
  )
  d <- d[d$parameter_cd == "72019", cols]
  d <- merge(d, sl, by = "id")
  d$id <- NULL

  # remove missing values
  is <- is.na(d$lev_va) | is.na(d$sl_lev_va)
  d <- d[!is, ]

  # average daily values when missing time and all other columns are equal
  is <- colnames(d) %in% c("lev_va", "sl_lev_va")
  ids <- apply(d[, !is], MARGIN = 1, FUN = paste, collapse = " ")
  agg <- stats::aggregate(d[, is], by = list("ids" = ids), FUN = mean)
  idxs <- match(ids, agg$ids)
  d$lev_va <- agg$lev_va[idxs]
  d$sl_lev_va <- agg$sl_lev_va[idxs]
  d <- d[!duplicated(ids), ]

  # add groundwater level accuracy values
  d$lev_acy_va <- NA_real_
  lev_acy <- c("0" = 1, "1" = 0.1, "2" = 0.01)
  d$lev_acy_va <- lev_acy[match(d$lev_acy_cd, names(lev_acy))]
  d$lev_acy_cd <- NULL
  d$sl_lev_acy_va <- sites$alt_acy_va[match(d$site_no, sites$site_no)] + d$lev_acy_va

  d$lev_meth_cd <- as.factor(d$lev_meth_cd)
  d$lev_status_cd <- as.factor(d$lev_status_cd)
  d$lev_age_cd <- as.factor(d$lev_age_cd)

  # sort records
  idxs <- order(
    stringi::stri_rank(tolower(d$site_nm), numeric = TRUE),
    d$site_no,
    d$lev_dt
  )
  d <- d[idxs, ]
  rownames(d) <- NULL

  d
}


# Units of Measurment (units) -----

mds_units <- function(data) {

  # check arguments
  checkmate::assert_data_frame(data, min.rows = 1, col.names = "named")

  idxs <- tolower(data$unit_cd) |> order()
  data <- data[idxs, ]
  rownames(data) <- NULL

  data
}


# Background Concentrations (background) -----

mds_background <- function(data, parameters) {

  # check arguments
  checkmate::assert_data_frame(data, min.rows = 1, col.names = "named")
  checkmate::assert_data_frame(parameters, min.rows = 1, col.names = "named")

  # merge in parameters
  d <- merge(x = data, y = parameters[, c("pcode", "parm_nm")], by = "pcode")

  d$bkgrd_min <- as.numeric(d$bkgrd_min)
  d$bkgrd_max <- as.numeric(d$bkgrd_max)
  d$reference <- as.factor(d$reference)
  d$unit_cd <- as.factor(d$unit_cd)

  # unit conversion
  conversion <- convert_units(d, parameters)
  cols <- c("bkgrd_min", "bkgrd_max")
  d[, cols] <- d[, cols] * conversion$mult

  cols <- c("parm_nm", "pcode", "bkgrd_min", "bkgrd_max", "reference")
  idxs <- tolower(d$parm_nm) |> stringi::stri_order(numeric = TRUE)
  d <- d[idxs, cols]
  rownames(d) <- NULL

  d
}


# Idaho National Laboratory Facilities (facilities) -----

mds_facilities <- function(data, crs) {

  # check arguments
  checkmate::assert_multi_class(data, classes = c("sf", "data.frame"))
  checkmate::assert_class(crs, classes = "crs")

  features <- c(
    "ATRC" = "Advanced Test Reactor Complex",
    "CFA" = "Central Facilities Area",
    "INTEC" = "Idaho Nuclear Technology and Engineering Center",
    "MFC" = "Materials and Fuels Complex",
    "NRF" = "Naval Reactors Facility",
    "RWMC" = "Radioactive Waste Management Complex",
    "TAN" = "Test Area North "
  )
  idxs <- match(data[["NAME"]], names(features))
  data$name <- features[idxs]

  data <- clean_sf(data,
    cols = c(
      "name" = "name",
      "id" = "NAME",
      "geometry" = "geometry"
    ),
    agr = "identity",
    crs = crs,
    type = "POLYGON"
  )

  idxs <- tolower(data$name) |>
    stringi::stri_order(numeric = TRUE)
  data <- data[idxs, ]
  rownames(data) <- NULL

  data
}


# Percolation Ponds (percponds) -----

mds_percponds <- function(data, crs) {

  # check arguments
  checkmate::assert_multi_class(data, classes = c("sf", "data.frame"))
  checkmate::assert_class(crs, classes = "crs")

  data <- clean_sf(data,
    cols = c(
      "name" = "Name",
      "facility_id" = "Pond_loc",
      "min_dt" = "Start_date",
      "max_dt" = "End_date",
      "geometry" = "geometry"
    ),
    agr = "identity",
    crs = crs,
    type = "POLYGON"
  )

  data$facility_id <- as.factor(data$facility_id)
  data$min_dt <- as.integer(data$min_dt)
  data$max_dt <- as.integer(data$max_dt)

  idxs <- order(
    stringi::stri_rank(tolower(data$name), numeric = TRUE),
    stringi::stri_rank(tolower(data$facility_id), numeric = TRUE)
  )
  data <- data[idxs, ]
  rownames(data) <- NULL

  data
}


# State of Idaho Boundary (idaho) -----

mds_idaho <- function(data, crs) {

  # check arguments
  checkmate::assert_multi_class(data, classes = c("sf", "data.frame"))
  checkmate::assert_class(crs, classes = "crs")

  is <- data[["NAME"]] == "Idaho"
  data <- data[is, ]

  data <- clean_sf(data, cols = "geometry", crs = crs)

  data <- sf::st_simplify(data,
    preserveTopology = TRUE,
    dTolerance = 100
  )

  data
}


# Cities and Towns (cities) -----

mds_cities <- function(data, crs, extent) {

  # check arguments
  checkmate::assert_multi_class(data, classes = c("sf", "data.frame"))
  checkmate::assert_class(crs, classes = "crs")
  checkmate::assert_class(extent, "bbox")

  data <- clean_sf(data,
    cols = c(
      "name" = "NAME",
      "id" = "GEOID",
      "geometry" = "geometry"
    ),
    agr = "identity",
    crs = crs,
    extent = extent
  )

  data <- suppressWarnings(sf::st_centroid(data))

  idxs <- tolower(data$name) |>
    stringi::stri_order(numeric = TRUE)
  data <- data[idxs, ]
  rownames(data) <- NULL

  data
}


# County Boundaries (counties) -----

mds_counties <- function(data, crs, extent) {

  # check arguments
  checkmate::assert_multi_class(data, classes = c("sf", "data.frame"))
  checkmate::assert_class(crs, classes = "crs")
  checkmate::assert_class(extent, "bbox")

  data <- clean_sf(data,
    cols = c(
      "name" = "NAME",
      "id" = "GEOID",
      "geometry" = "geometry"
    ),
    agr = "identity",
    crs = crs,
    extent = extent,
    type = "POLYGON"
  )

  idxs <- tolower(data$name) |>
    stringi::stri_order(numeric = TRUE)
  data <- data[idxs, ]
  rownames(data) <- NULL

  data
}


# Road Netowrk (roads) -----

mds_roads <- function(all_data, prisec_data, crs, extent) {

  # check arguments
  checkmate::assert_multi_class(all_data, classes = c("sf", "data.frame"))
  checkmate::assert_multi_class(prisec_data, classes = c("sf", "data.frame"))
  checkmate::assert_class(crs, classes = "crs")
  checkmate::assert_class(extent, "bbox")

  cols <- c(
      "name" = "FULLNAME",
      "id" = "LINEARID",
      "geometry" = "geometry"
  )
  all_data <- clean_sf(all_data,
    cols = cols,
    agr = "identity",
    crs = crs,
    extent = extent
  )
  prisec_data <- clean_sf(prisec_data,
    cols = cols,
    agr = "identity",
    crs = crs,
    extent = extent
  )

  data <- stats::aggregate(
    all_data[, "geometry"],
    by = list(all_data$id),
    FUN = mean
  )
  colnames(data) <- c("id", "geometry")
  idxs <- match(data$id, all_data$id)
  data$name <- all_data$name[idxs]
  data$prisec_fl <- data$id %in% prisec_data$id

  idxs <- tolower(data$name) |>
    stringi::stri_rank(numeric = TRUE) |>
    order(data$id)
  cols <- c("name", "id", "prisec_fl", "geometry")
  data <- data[idxs, cols]
  rownames(data) <- NULL

  data <- sf::st_make_valid(data)

  data
}


# Lakes and Ponds (lakes) -----

mds_lakes <- function(data, crs, extent) {

  # check arguments
  checkmate::assert_multi_class(data, classes = c("sf", "data.frame"))
  checkmate::assert_class(crs, classes = "crs")
  checkmate::assert_class(extent, "bbox")

  data <- clean_sf(data,
    cols = c(
      "gnis_nm" = "GNIS_NAME",
      "id" = "COMID",
      "reach_cd" = "REACHCODE",
      "gnis_id" = "GNIS_ID",
      "feature_tp" = "FTYPE",
      "geometry" = "geometry"
    ),
    agr = c(
      "gnis_nm" = "identity",
      "id" = "identity",
      "reach_cd" = "identity",
      "gnis_id" = "identity",
      "feature_tp" = "constant"
    ),
    crs = crs,
    extent = extent,
    type = "POLYGON"
  )

  data$id <- as.character(data$id)
  data$feature_tp <- as.factor(data$feature_tp)

  idxs <- tolower(data$gnis_nm) |>
    stringi::stri_rank(numeric = TRUE) |>
    order(data$id)
  data <- data[idxs, ]
  rownames(data) <- NULL

  data
}


# Rivers and Streams (streams) -----

mds_streams <- function(data, crs, extent) {

  # check arguments
  checkmate::assert_multi_class(data, classes = c("sf", "data.frame"))
  checkmate::assert_class(crs, classes = "crs")
  checkmate::assert_class(extent, "bbox")

  data <- clean_sf(data,
    cols = c(
      "gnis_nm" = "GNIS_NAME",
      "id" = "COMID",
      "reach_cd" = "REACHCODE",
      "gnis_id" = "GNIS_ID",
      "feature_tp" = "FTYPE",
      "geometry" = "geometry"
    ),
    agr = c(
      "gnis_nm" = "identity",
      "id" = "identity",
      "reach_cd" = "identity",
      "gnis_id" = "identity",
      "feature_tp" = "constant"
    ),
    crs = crs,
    extent = extent
  )

  is <- is.na(data$gnis_id)
  data <- data[!is, ]

  data$id <- as.character(data$id)
  data$feature_tp <- as.factor(data$feature_tp)

  idxs <- tolower(data$gnis_nm) |>
    stringi::stri_rank(numeric = TRUE) |>
    order(data$id)
  data <- data[idxs, ]
  rownames(data) <- NULL

  data
}


# Digital Elevation Model (dem) -----

mds_dem <- function(data, crs, extent, resolution) {

  # check arguments
  checkmate::assert_class(data, classes = "SpatRaster")
  checkmate::assert_class(crs, classes = "crs")
  checkmate::assert_class(extent, "bbox")
  checkmate::assert_number(resolution, lower = 1, finite = TRUE)

  # make raster grid
  grd <- terra::rast(
    crs = crs$input,
    extent = terra::ext(extent),
    resolution = resolution
  )

  # set aggregation factor
  fact <- 5L

  # disaggregate raster cells
  grd <- terra::disagg(grd, fact = fact)

  # project data into raster grid
  r <- terra::project(data, grd)

  # aggregate raster cells
  r <- terra::aggregate(r, fact = fact, fun = stats::median)

  # convert elevation from meters to feet
  r[] <- r[] * 3.2808399
  r[] <- round_numbers(r[], digits = 0)

  # set field name
  names(r) <- "elevation"

  r
}


# Mountains and Buttes (mountains) -----

mds_mountains <- function(data, crs, dem) {

  # check arguments
  checkmate::assert_multi_class(data, classes = c("sf", "data.frame"))
  checkmate::assert_class(crs, classes = "crs")
  checkmate::assert_class(dem, classes = "SpatRaster")

  # clean spatial feature
  data <- clean_sf(data,
    cols = c("name", "geometry"),
    agr = "identity",
    crs = crs
  )

  # calculate the terrain slope
  r <- terra::terrain(dem, v = "slope")

  # calculate slope threshold
  threshold <- terra::values(r) |>
    as.vector() |>
    stats::na.omit() |>
    terra::quantile(probs = 0.80)

  # find slope greater than or equal to the threshold
  r <- r >= threshold

  # identify areas of connected steep areas
  r <- terra::patches(r, zeroAsNA = TRUE)

  # convert patches to polygons
  p <- terra::as.polygons(r)

  # fill holes in the polygons
  p <- terra::fillHoles(p)

  # convert polygons to spatial feature
  p <- sf::st_as_sf(p)

  # add polygon names
  p <- sf::st_join(p, data, left = FALSE)

  # set polygon tolerance
  tol <- min(terra::res(r)) / 2

  # simplify polygons
  p <- sf::st_simplify(p, preserveTopology = TRUE, dTolerance = tol)

  # get spatial extent
  extent <- terra::ext(r) |> sf::st_bbox(crs = crs)

  # clean spatial feature
  p <- clean_sf(p,
    cols = c("name", "geometry"),
    crs = crs,
    extent = extent,
    type = "POLYGON"
  )

  p
}


# Tabulate Parameter Data ----

tabulate_parm_data <- function(parameters, samples) {

  # check arguments
  checkmate::assert_data_frame(parameters, min.rows = 1, col.names = "named")
  checkmate::assert_data_frame(samples, min.rows = 1, col.names = "named")

  tbl <- parameters

  x <- samples$sample_dt
  by <- list("pcode" = samples$pcode)
  d <- stats::aggregate(x, by = by, FUN = min)
  names(d)[2] <- "min_dt"
  tbl <- merge(tbl, d, by = "pcode")
  d <- stats::aggregate(x, by = by, FUN = max)
  names(d)[2] <- "max_dt"
  tbl <- merge(tbl, d, by = "pcode")

  tbl$min_dt <- as.Date(tbl$min_dt)
  tbl$max_dt <- as.Date(tbl$max_dt)

  x <- table(samples$pcode) |> as.array()
  tbl$nrecords <- x[match(tbl$pcode, names(x))] |> as.integer()

  tbl$nsites <- vapply(tbl$pcode,
    FUN = function(cd) {
      samples$site_no[samples$pcode %in% cd] |>
        unique() |>
        length()
    },
    FUN.VALUE = integer(1)
  )

  ranks <- tolower(tbl$parm_nm) |>
    stringi::stri_rank(numeric = TRUE)
  idxs <- order(tbl$parm_group_nm, ranks)
  cols <- c(
    "pcode",
    "min_dt",
    "max_dt",
    "nrecords",
    "nsites"
  )
  tbl <- tbl[idxs, cols]

  tbl
}

# Tabulate Site Data ----

tabulate_site_data <- function(sites, samples, gwl, swm) {

  # check arguments
  checkmate::assert_class(sites, classes = "sf")
  checkmate::assert_data_frame(samples, min.rows = 1, col.names = "named")
  checkmate::assert_data_frame(gwl, min.rows = 1, col.names = "named")
  checkmate::assert_data_frame(swm, min.rows = 1, col.names = "named")

  dat <- sites |> as.data.frame()
  dat$geometry <- NULL

  x <- c(samples$sample_dt, gwl$lev_dt)
  by <- list("site_no" = c(samples$site_no, gwl$site_no))

  d <- stats::aggregate(x, by = by, FUN = min)
  names(d)[2] <- "min_dt"
  dat <- merge(dat, d, by = "site_no", all.x = TRUE)

  d <- stats::aggregate(x, by = by, FUN = max)
  names(d)[2] <- "max_dt"
  dat <- merge(dat, d, by = "site_no", all.x = TRUE)

  dat$min_dt <- as.Date(dat$min_dt)
  dat$max_dt <- as.Date(dat$max_dt)

  x <- table(gwl$site_no) |> as.array()
  ngwl <- x[match(dat$site_no, names(x))]

  x <- table(swm$site_no) |> as.array()
  nswm <- x[match(dat$site_no, names(x))]

  dat$nmeas <- cbind(ngwl, nswm) |>
    apply(MARGIN = 1, FUN = sum, na.rm = TRUE)

  x <- table(samples$site_no) |> as.array()
  dat$nsamples <- x[match(dat$site_no, names(x))] |> as.integer()
  dat$nsamples[is.na(dat$nsamples)] <- 0L

  is <- !is.na(samples$rep_pair_id) &
    samples$sample_type_cd %in% "7"
  x <- table(samples$site_no[is]) |> as.array()
  dat$nreps <- x[match(dat$site_no, names(x))] |> as.integer()
  dat$nreps[is.na(dat$nreps)] <- 0L

  ranks <- tolower(dat$site_nm) |> stringi::stri_rank(numeric = TRUE)
  idxs <- order(dat$network_cd, ranks, dat$well_depth_va)
  cols <- c(
    "site_no",
    "min_dt",
    "max_dt",
    "nmeas",
    "nsamples",
    "nreps"
  )
  dat <- dat[idxs, cols]
  rownames(dat) <- NULL

  dat
}


# Convert Units ----

convert_units <- function(data, parameters) {

  # define possible conversions (add values as necessary)
  conversions <- c(
    "ug/L to mg/L" = 0.001,
    "ug/L to mg/L as N" = 0.001,
    "ug/L to mg/L as P" = 0.001
  )

  # check arguments
  checkmate::assert_data_frame(data, min.cols = 2, col.names = "named")
  cols <- c("pcode", "unit_cd")
  checkmate::assert_subset(cols, choices = colnames(data))
  checkmate::assert_vector(data$pcode, any.missing = FALSE)
  checkmate::assert_vector(data$unit_cd, any.missing = FALSE)
  checkmate::assert_data_frame(parameters, min.rows = 1, col.names = "named")

  # set to and from
  d <- data[, cols]
  colnames(d) <- c("pcode", "from")
  d$from <- as.character(d$from)
  idxs <- match(d$pcode, parameters$pcode)
  d$to <- parameters$unit_cd[idxs] |> as.character()

  # initialize multiplier
  d$mult <- NA_real_
  d$mult[d$from == d$to] <- 1

  # get conversion multipliers
  types <- paste(d$from, d$to, sep = " to ")
  convert <- types[d$from != d$to] |> unique()
  checkmate::assert_subset(convert, choices = names(conversions))
  for (type in convert) {
    d$mult[types == type] <- conversions[type]
  }

  d
}
