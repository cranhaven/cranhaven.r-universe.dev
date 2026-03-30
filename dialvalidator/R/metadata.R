#' Access Phone Number Metadata
#'
#' @description
#' Functions to access and update the bundled libphonenumber metadata.
#'
#' @name metadata
NULL

#' Get the full metadata object
#'
#' Returns the parsed libphonenumber metadata used internally by all
#' validation functions. Useful for inspection and debugging.
#'
#' @return A list with elements `cc_to_regions`, `territories`,
#'   `example_numbers`, `version`, and `built`.
#'
#' @examples
#' meta <- dv_metadata()
#' meta$version
#' length(meta$territories)
#'
#' @export
dv_metadata <- function() {
  ensure_metadata()
  the$metadata
}

#' Get metadata for a specific territory
#'
#' @param region ISO 3166-1 alpha-2 region code (e.g., `"NZ"`, `"US"`).
#'
#' @return A list containing the territory's country code, national prefix,
#'   phone type patterns, formatting rules, and example numbers.
#'   Returns `NULL` if the region is not found.
#'
#' @examples
#' nz <- dv_territory("NZ")
#' nz$country_code
#' nz$mobile$example
#'
#' @export
dv_territory <- function(region) {
  ensure_metadata()
  region <- toupper(region)
  the$metadata$territories[[region]]
}

#' Update metadata from upstream
#'
#' Downloads the latest `PhoneNumberMetadata.xml` from Google's libphonenumber
#' repository, parses it, and saves the result to the user's cache directory.
#' The updated metadata is used for the remainder of the session.
#'
#' Requires the \pkg{xml2} package.
#'
#' @param url URL to the raw PhoneNumberMetadata.xml file. Defaults to the
#'   master branch on GitHub.
#'
#' @return Invisibly returns the path to the cached metadata file.
#'
#' @examples
#' \donttest{
#' dv_update_metadata()
#' }
#'
#' @export
dv_update_metadata <- function(
  url = paste0(
    "https://raw.githubusercontent.com/google/libphonenumber/master/",
    "resources/PhoneNumberMetadata.xml"
  )
) {
  rlang::check_installed("xml2", reason = "to download and parse metadata XML")

  cli::cli_alert_info("Downloading PhoneNumberMetadata.xml...")
  xml <- xml2::read_xml(url)

  cli::cli_alert_info("Parsing metadata...")
  metadata <- parse_metadata_xml(xml)

  cache_dir <- tools::R_user_dir("dialvalidator", "cache")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_path <- file.path(cache_dir, "metadata.rds")

  saveRDS(metadata, cache_path, compress = "xz")
  the$metadata <- metadata

  cli::cli_alert_success(
    "Updated to version {metadata$version} ({length(metadata$territories)} territories). Cached at {cache_path}"
  )
  invisible(cache_path)
}


# --- Internals ---

#' Ensure metadata is loaded
#' @noRd
ensure_metadata <- function() {
  if (is.null(the$metadata)) {
    # Try user cache first
    cache_path <- file.path(
      tools::R_user_dir("dialvalidator", "cache"), "metadata.rds"
    )
    if (file.exists(cache_path)) {
      the$metadata <- readRDS(cache_path)
    } else {
      rlang::abort(
        "dialvalidator metadata not found. This should not happen if the package was installed correctly.",
        class = "dialvalidator_no_metadata"
      )
    }
  }
}

#' Parse PhoneNumberMetadata.xml into an R list
#'
#' This is the core metadata parser, shared between `data-raw/build_metadata.R`
#' and `dv_update_metadata()`.
#'
#' @param xml An `xml2::xml_document` of PhoneNumberMetadata.xml.
#' @return A metadata list.
#' @noRd
parse_metadata_xml <- function(xml) {
  phone_types <- c(
    "generalDesc", "fixedLine", "mobile", "tollFree", "premiumRate",
    "sharedCost", "personalNumber", "voip", "pager", "uan", "voicemail",
    "noInternationalDialling"
  )
  type_names_r <- c(
    "general_desc", "fixed_line", "mobile", "toll_free", "premium_rate",
    "shared_cost", "personal_number", "voip", "pager", "uan", "voicemail",
    "no_international_dialling"
  )

  territory_nodes <- xml2::xml_find_all(xml, "//territory")
  territories <- list()
  cc_to_regions <- list()
  example_numbers <- list()

  for (tnode in territory_nodes) {
    id <- xml2::xml_attr(tnode, "id")
    cc <- xml2::xml_attr(tnode, "countryCode")
    national_prefix <- xml2::xml_attr(tnode, "nationalPrefix")
    international_prefix <- xml2::xml_attr(tnode, "internationalPrefix")
    main_country <- xml2::xml_attr(tnode, "mainCountryForCode")
    leading_digits <- xml2::xml_attr(tnode, "leadingDigits")

    territory <- list(
      id = id,
      country_code = cc,
      national_prefix = if (!is.na(national_prefix)) national_prefix,
      international_prefix = if (!is.na(international_prefix)) international_prefix,
      main_country_for_code = if (!is.na(main_country)) as.logical(main_country),
      leading_digits = if (!is.na(leading_digits)) str_remove_all(leading_digits, "\\s+")
    )

    for (i in seq_along(phone_types)) {
      parsed <- parse_type_node(tnode, phone_types[i])
      if (!is.null(parsed)) {
        territory[[type_names_r[i]]] <- parsed
        if (!is.null(parsed$example) && type_names_r[i] != "general_desc") {
          example_numbers[[length(example_numbers) + 1]] <- list(
            region = id, country_code = cc,
            type = type_names_r[i], example = parsed$example
          )
        }
      }
    }

    territory$formats <- parse_format_nodes(tnode)
    territories[[id]] <- territory

    if (is.null(cc_to_regions[[cc]])) {
      cc_to_regions[[cc]] <- id
    } else {
      cc_to_regions[[cc]] <- c(cc_to_regions[[cc]], id)
    }
  }

  # Sort: main country first

for (cc in names(cc_to_regions)) {
    regions <- cc_to_regions[[cc]]
    if (length(regions) > 1) {
      main_first <- vapply(regions, function(r) {
        isTRUE(territories[[r]]$main_country_for_code)
      }, logical(1))
      cc_to_regions[[cc]] <- c(regions[main_first], regions[!main_first])
    }
  }

  list(
    cc_to_regions = cc_to_regions,
    territories = territories,
    example_numbers = example_numbers,
    version = "live",
    built = Sys.Date()
  )
}

#' @noRd
parse_type_node <- function(territory_node, type_name) {
  node <- xml2::xml_find_first(territory_node, type_name)
  if (is.na(node)) return(NULL)

  pattern_node <- xml2::xml_find_first(node, "nationalNumberPattern")
  pattern <- if (!is.na(pattern_node)) {
    raw <- str_remove_all(xml2::xml_text(pattern_node), "\\s+")
    paste0("^(?:", raw, ")$")
  }

  lengths_node <- xml2::xml_find_first(node, "possibleLengths")
  possible_lengths <- if (!is.na(lengths_node)) {
    len_str <- xml2::xml_attr(lengths_node, "national")
    if (!is.null(len_str) && !is.na(len_str)) {
      parts <- str_split_1(len_str, ",")
      unlist(lapply(parts, function(p) {
        if (str_detect(p, fixed("["))) {
          bounds <- as.integer(str_extract_all(p, "[0-9]+")[[1]])
          seq(bounds[1], bounds[2])
        } else {
          as.integer(p)
        }
      }))
    }
  }

  example_node <- xml2::xml_find_first(node, "exampleNumber")
  example <- if (!is.na(example_node)) xml2::xml_text(example_node)

  list(pattern = pattern, possible_lengths = possible_lengths, example = example)
}

#' @noRd
parse_format_nodes <- function(territory_node) {
  format_nodes <- xml2::xml_find_all(
    territory_node, "availableFormats/numberFormat"
  )
  if (length(format_nodes) == 0) return(list())

  lapply(format_nodes, function(fmt) {
    pattern <- xml2::xml_attr(fmt, "pattern")
    format_text <- xml2::xml_text(xml2::xml_find_first(fmt, "format"))

    leading_digits <- xml2::xml_find_all(fmt, "leadingDigits")
    leading_digits_patterns <- if (length(leading_digits) > 0) {
      vapply(leading_digits, function(ld) {
        str_remove_all(xml2::xml_text(ld), "\\s+")
      }, character(1))
    }

    npfr <- xml2::xml_attr(fmt, "nationalPrefixFormattingRule")
    intl_format_node <- xml2::xml_find_first(fmt, "intlFormat")
    intl_format <- if (!is.na(intl_format_node)) xml2::xml_text(intl_format_node)

    result <- list(
      pattern = pattern,
      format = format_text,
      leading_digits_patterns = leading_digits_patterns,
      national_prefix_formatting_rule = npfr
    )
    if (!is.null(intl_format)) result$intl_format <- intl_format
    result
  })
}
