#' Create Data Release
#'
#' @description Create U.S. Geological Survey (USGS) data release product
#'   from R-package datasets and their documentation.
#'   Requires that the \pkg{xml2} and \pkg{jsonlite} packages are available.
#'
#' @param metadata 'character' string or named 'list'.
#'   Either the path to a JSON formatted metadata file that contains general information
#'   for the USGS data release (see *Examples* section),
#'   or a named list with the equivalent information.
#' @param ...
#'   Additional arguments to be passed to the [`write_datasets`] function.
#'   The `formats` argument, which is specified within the function, is the exception.
#' @param bounding 'bbox', 'sf', 'SpatRaster', or 'PackedSpatRaster' spatial feature.
#'   Object to compute spatial bounding coordinates from, see [`sf::st_bbox`] function.
#' @param rngdates 'Date' or 'POSIXct' vector.
#'   Object to compute the date range of observations from.
#' @param validate 'logical' flag.
#'   Whether to perform a metadata validation and stop execution if errors are found.
#'   See [`validate_metadata`] function for system requirements.
#' @inheritParams write_datasets
#'
#' @details Citation entries for the \R package (software release) and accompanying data release
#'   should be included in the package CITATION file, and documented in that order.
#'
#' @return Named list of metadata created for spatial and non-spatial datasets.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#' @author A.R. Trcka, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso [`parse_rd_db`] function for reading and parsing R-package documentation.
#'
#' @export
#'
#' @examples
#' destdir <- tempfile("")
#' rngdates <- c(samples$sample_dt, gwl$lev_dt) |> range()
#' l <- make_data_release(
#'   metadata = system.file("extdata/metadata.json", package = "inldata"),
#'   package = "inldata",
#'   destdir = destdir,
#'   include = "crs",
#'   quiet = TRUE,
#'   bounding = sites,
#'   rngdates = rngdates
#' )
#' str(l, 1)
#'
#' unlink(destdir, recursive = TRUE)

make_data_release <- function(metadata,
                              package,
                              destdir = getwd(),
                              ...,
                              bounding = NULL,
                              rngdates = NULL,
                              validate = FALSE) {

  # check packages
  check_package(pkg = "xml2", msg = "Creating a data release")
  check_package(pkg = "jsonlite", msg = "Creating a data release")

  # check arguments
  if (checkmate::test_string(metadata)) {
    metadata <- path.expand(metadata) |> normalizePath(winslash = "/") |>
      jsonlite::read_json()
  } else {
    checkmate::assert_list(metadata, min.len = 1, names = "named")
  }
  checkmate::assert_string(package)
  checkmate::assert_string(destdir)
  destdir <- path.expand(destdir) |> normalizePath(winslash = "/", mustWork = FALSE)
  dirname(destdir) |> dir.create(recursive = TRUE, showWarnings = FALSE)
  checkmate::assert_path_for_output(destdir, overwrite = TRUE)
  checkmate::assert_multi_class(bounding,
    classes = c("PackedSpatRaster", "SpatRaster", "sf", "bbox"),
    null.ok = TRUE
  )
  checkmate::assert_multi_class(rngdates, classes = c("Date", "POSIXct"), null.ok = TRUE)

  # write package datasets to JSON files
  ds_paths <- write_datasets(
    package = package,
    destdir = destdir,
    formats = c("geojson", "json", "tiff", "txt"),
    ...
  )
  ds_files <- basename(ds_paths)
  ds_names <- tools::file_path_sans_ext(ds_files)

  # check if working in package directory
  is_pkg_dir <- test_pkg_dir(package)

  # parse help documentation for package datasets
  if (is_pkg_dir) {
    rds <- parse_rd_db(dir = getwd(), stages = "install")
  } else {
    rds <- parse_rd_db(package = package)
  }

  # subset help documents
  rds <- rds[ds_names]

  # loop through datasets
  eainfo <- lapply(seq_along(rds), function(i) {

    # get help documentation
    rd <- rds[[i]]

    # initialize list for detailed node
    detailed <- list()

    # make dataset description
    desc <- c(rd$description, rd$references) |> paste(collapse = "\n\n")

    # make dataset source
    src <- if (is.null(rd$source)) "Producer defined." else rd$source

    # add dataset description
    detailed$enttyp <- list(
      "enttypl" = list(ds_files[i]),
      "enttypd" = list(desc),
      "enttypds" = list(src)
    )

    # describe dataset fields
    d <- rd$format_table
    if (!is.null(d)) {

      # get dataset
      envir <- new.env()
      if (is_pkg_dir) {
        nm <- load(
          file = sprintf("data/%s.rda", ds_names[i]),
          envir = envir,
          verbose = FALSE
        )
      } else {
        nm <- utils::data(
          list = ds_names[i],
          package = package,
          envir = envir,
          verbose = FALSE
        )
      }
      ds <- envir[[nm[1]]]

      # uncompress packed spatial raster objects
      if (inherits(ds, "PackedSpatRaster")) {
        ds <- terra::unwrap(ds)
      }

      # try to convert to a data frame
      ds <- try(as.data.frame(ds), silent = TRUE)

      # describe column data
      if (is.data.frame(ds)) {
        if (!checkmate::test_subset(d$name, choices = colnames(ds))) {
          stop("Column name(s) do not match in dataset ", ds_names[i], call. = FALSE)
        }
        ds <- ds[, d$name, drop = FALSE]

        # add no-data flag
        d$is_na <- vapply(ds, FUN = anyNA, FUN.VALUE = logical(1))

        # add categorical levels
        d$levels <- lapply(ds, FUN = levels) |> I()

        # add data range
        d$range <- lapply(ds, function(x) {
          if (checkmate::test_double(x, all.missing = FALSE)) {
            ran <- range(x, na.rm = TRUE, finite = TRUE)
            vapply(ran, FUN = format, FUN.VALUE = character(1), scientific = FALSE)
          } else {
            NULL
          }
        }) |> I()
      }

      # add attributes
      attrs <- apply(d, 1L, function(x) {
        l <- list()
        l$attrlabl <- list(x["name"])
        l$attrdef <- list(x["value"])
        l$attrdefs <- list("Producer defined.")

        # add no-data value
        if (x$is_na) {
          l <- c(l, list(
            "attrdomv" = list(
              "edom" = list(
                "edomv" = list("null"),
                "edomvd" = list("No data."),
                "edomvds" = list("Producer defined.")
              )
            )
          ))
        }

        # add categorical type
        if (!is.null(x$levels)) {
          levels <- x$levels[nchar(x$levels) > 0]
          for (level in levels) {
            l <- c(l, list(
              "attrdomv" = list(
                "edom" = list(
                  "edomv" = list(level),
                  "edomvd" = list("See attribute definition."),
                  "edomvds" = list("Producer defined.")
                )
              )
            ))
          }

        # add numeric-data type
        } else if (!is.null(x$range)) {
          l <- c(l, list(
            "attrdomv" = list(
              "rdom" = list(
                "rdommin" = list(x$range[1]),
                "rdommax" = list(x$range[2])
              )
            )
          ))

        # add unknown-data type
        } else {
          l <- c(l, list(
            "attrdomv" = list(
              "udom" = list("See attribute description.")
            )
          ))
        }

        l
      })
      names(attrs) <- rep("attr", nrow(d))
      detailed <- c(detailed, attrs)
    }

    detailed
  })
  names(eainfo) <- rep("detailed", length(eainfo))

  # add datasets to metadata
  metadata[[1]]$eainfo <- eainfo

  # add data quality process date
  metadata[[1]]$dataqual$lineage$procstep$procdate <- Sys.Date() |> format(format = "%Y") |> list()

  # get package citations
  if (is_pkg_dir) {
    citations <- utils::readCitationFile("inst/CITATION")
  } else {
    citations <- utils::citation(package = package)
  }

  # set URL data is available online
  url <- paste0("https://doi.org/", citations[[2]]$doi)
  metadata[[1]]$distinfo$stdorder$digform$digtopt$onlinopt$computer$networka$networkr <- list(url)

  # add metadata date
  metadata[[1]]$metainfo$metd <- Sys.Date() |> format(format = "%Y%m%d") |> list()

  # add data-release citation info
  metadata[[1]]$idinfo$citation <- parse_citeinfo(
    citation = citations[[2]]
  )

  # add bounding coordinates
  if (!is.null(bounding)) {
    if (inherits(bounding, "PackedSpatRaster")) {
      bounding <- terra::unwrap(bounding)
    }
    if (inherits(bounding, "SpatRaster")) {
      bounding <- terra::as.points(bounding) |> sf::st_as_sf()
    }
    if (inherits(bounding, "sf")) {
      bounding <- sf::st_bbox(bounding)
    }
    bb <- sf::st_as_sfc(bounding) |>
      sf::st_transform(crs = "EPSG:4326") |>
      sf::st_bbox()
    l <- as.vector(bb) |> formatC(format = "f", digits = 4) |> lapply(FUN = list)
    names(l) <- c("westbc", "southbc", "eastbc", "northbc")
    l <- l[c(1, 3, 4, 2)]

    metadata[[1]]$idinfo$spdom$bounding <- l
  }

  # add date range of observations
  if (!is.null(rngdates)) {
    x <- range(rngdates, na.rm = TRUE) |> format(format = "%Y%m%d")
    metadata[[1]]$idinfo$timeperd$timeinfo$rngdates <- list(
      "begdate" = list(x[1]),
      "enddate" = list(x[2])
    )
  }

  # get package URLs
  if (is_pkg_dir) {
    urls <- read.dcf("DESCRIPTION", fields = "URL") |> as.character()
  } else {
    urls <- utils::packageDescription(pkg = package)$URL
  }
  urls <- gsub("[\r\n]|[ ]", "", urls) |> strsplit(split = ",") |> unlist()

  # add cross-reference info
  metadata[[1]]$idinfo$crossref <- parse_citeinfo(
    citation = citations[[1]],
    pubplace = urls[1]
  )

  # write XML file
  file <- file.path(destdir, "metadata.xml", fsep = "/")
  xml2::as_xml_document(metadata) |> xml2::write_xml(file = file)

  # metadata validation
  if (validate) {
    file.path(destdir, "metadata.xml") |> validate_metadata(error = TRUE)
  }

  invisible(metadata[[1]])
}


# Function to parse citation entry
parse_citeinfo <- function(citation, pubplace = NULL) {
  checkmate::assert_class(citation, classes = c("citation", "bibentry"))
  checkmate::assert_string(pubplace, null.ok = TRUE)

  authors <- lapply(citation$author, function(x) {
    c(x$given, x$family) |> paste(collapse = " ") |> list()
  })
  names(authors) <- rep("origin", length(authors))

  l <- authors
  l$pubdate <- list(citation$year)
  l$title <- list(citation$title)
  l$edition <- list(citation$version)
  l$geoform <- list("publication")

  if (!is.null(pubplace)) {
    l$pubinfo <- list(
      "pubplace" = list(pubplace),
      "publish" = list(citation$institution)
    )
  }
  l$onlink <- paste0("https://doi.org/", citation$doi) |> list()

  list("citeinfo" = l)
}
