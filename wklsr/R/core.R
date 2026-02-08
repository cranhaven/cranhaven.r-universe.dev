# wkls.R - R implementation of the wkls library

#' @importFrom DBI dbConnect dbDisconnect dbExecute dbGetQuery
#' @importFrom duckdb duckdb
#' @importFrom utils head
NULL


# Package environment to store connection
.wkls_env <- new.env()

# Overture Maps dataset version
OVERTURE_VERSION <- "2025-11-19.0"
HF_PARQUET_PATH <- sprintf("hf://datasets/wherobots/overturemaps-us-west-2/release/%s/theme=divisions/type=division_area/*",
                           OVERTURE_VERSION)

# ============================================================================
# SQL Query Templates
# ============================================================================
# REFACTOR REASON: Centralized query definitions make it easier to maintain
# and understand the data access patterns. Pattern queries are separated
# from exact-match queries for clarity.

COUNTRY_OR_DEPENDENCY_QUERY <- "
    SELECT * FROM wkls
    WHERE country = ?
      AND (subtype = 'country' OR subtype = 'dependency')
"

REGION_QUERY <- "
    SELECT * FROM wkls
    WHERE country = ?
      AND region = ?
      AND subtype = 'region'
"

# Exact match queries - use space removal for fuzzy matching
CITY_QUERY_EXACT <- "
    SELECT * FROM wkls
    WHERE country = ?
      AND region = ?
      AND subtype IN ('county', 'locality', 'localadmin')
      AND (
        REPLACE(name_primary, ' ', '') ILIKE REPLACE(?, ' ', '')
        OR
        REPLACE(name_en, ' ', '') ILIKE REPLACE(?, ' ', '')
    )
"

DEPENDENCY_CITY_QUERY_EXACT <- "
    SELECT * FROM wkls
    WHERE country = ?
      AND subtype IN ('county', 'locality', 'localadmin')
      AND (
        REPLACE(name_primary, ' ', '') ILIKE REPLACE(?, ' ', '')
        OR
        REPLACE(name_en, ' ', '') ILIKE REPLACE(?, ' ', '')
    )
"

# Pattern queries - use wildcards directly for LIKE searches
CITY_QUERY_PATTERN <- "
    SELECT * FROM wkls
    WHERE country = ?
      AND region = ?
      AND subtype IN ('county', 'locality', 'localadmin')
      AND (name_primary ILIKE ? OR name_en ILIKE ?)
"

DEPENDENCY_CITY_QUERY_PATTERN <- "
    SELECT * FROM wkls
    WHERE country = ?
      AND subtype IN ('county', 'locality', 'localadmin')
      AND (name_primary ILIKE ? OR name_en ILIKE ?)
"

# ============================================================================
# Database Initialization
# ============================================================================

#' Initialize the wkls table
#' @keywords internal
.initialize_table <- function() {
  # Check if connection exists AND is valid
  if (exists("con", envir = .wkls_env)) {
    tryCatch({
      # Test if connection is still valid
      dbExecute(.wkls_env$con, "SELECT 1")
      return(invisible(NULL))
    }, error = function(e) {
      # Connection is invalid, remove it and recreate
      rm("con", envir = .wkls_env)
    })
  }

  # Create DuckDB connection
  .wkls_env$con <- dbConnect(duckdb::duckdb())

  # URL to the data file on GitHub
  data_url <- "https://raw.githubusercontent.com/tbanken/wklsr/refs/heads/main/inst/extdata/overture.zstd18.parquet"

  # Install and load extensions, configure S3
  # Wrap in tryCatch to avoid segfaults during R CMD check on some systems
  tryCatch({
    dbExecute(.wkls_env$con, "INSTALL spatial")
    dbExecute(.wkls_env$con, "LOAD spatial")
  }, error = function(e) {
    warning("Could not load DuckDB spatial extension. Geometry functions may not work: ", e$message)
  })

  tryCatch({
    dbExecute(.wkls_env$con, "INSTALL httpfs")
    dbExecute(.wkls_env$con, "LOAD httpfs")
  }, error = function(e) {
    warning("Could not load DuckDB httpfs extension. Remote file access may not work: ", e$message)
  })

  dbExecute(.wkls_env$con, "SET s3_region='us-west-2'")
  dbExecute(.wkls_env$con, "SET s3_access_key_id=''")
  dbExecute(.wkls_env$con, "SET s3_secret_access_key=''")
  dbExecute(.wkls_env$con, "SET s3_session_token=''")
  dbExecute(.wkls_env$con, "SET s3_endpoint='s3.amazonaws.com'")
  dbExecute(.wkls_env$con, "SET s3_use_ssl=true")

  # Create table from remote parquet file
  query <- sprintf("
    CREATE TABLE IF NOT EXISTS wkls AS
    SELECT id, country, region, subtype, name_primary, name_en
    FROM '%s'
  ", data_url)

  dbExecute(.wkls_env$con, query)
}

#' Package load hook
#' @param libname Library name
#' @param pkgname Package name
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Skip initialization during R CMD check to avoid segfaults with extensions
  # The table will initialize lazily on first use
  if (identical(Sys.getenv("_R_CHECK_PACKAGE_NAME_"), "")) {
    # Not in R CMD check - try to initialize but don't fail if it doesn't work
    tryCatch({
      .initialize_table()
    }, error = function(e) {
      # Silently fail - will initialize on first actual use
      invisible(NULL)
    })
  }
}

# ============================================================================
# Helper Functions
# ============================================================================
# REFACTOR REASON: Extracted common logic to reduce duplication and improve
# maintainability. Each helper function has a single, clear responsibility.

#' Check if a country code represents a dependency
#' @keywords internal
.is_dependency <- function(country_iso) {
  .initialize_table()
  result <- dbGetQuery(.wkls_env$con,
                       COUNTRY_OR_DEPENDENCY_QUERY,
                       params = list(country_iso))
  nrow(result) > 0 && result$subtype[1] == "dependency"
}

#' Build a query for listing entities with deduplication
#' @keywords internal
.build_list_query <- function(country_iso, region_iso = NULL, subtype_filter, distinct_col = "name_primary") {
  if (is.null(region_iso)) {
    # Country-level query
    sprintf("
      SELECT DISTINCT ON (%s) id, country, region, subtype, name_primary as name
      FROM wkls
      WHERE country = '%s' AND subtype IN (%s)
      ORDER BY %s, id
    ", distinct_col, country_iso, subtype_filter, distinct_col)
  } else {
    # Region-level query
    sprintf("
      SELECT DISTINCT ON (%s) id, country, region, subtype, name_primary as name
      FROM wkls
      WHERE country = '%s' AND region = '%s' AND subtype IN (%s)
      ORDER BY %s, id
    ", distinct_col, country_iso, region_iso, subtype_filter, distinct_col)
  }
}

# ============================================================================
# Core Resolution Logic
# ============================================================================

#' Internal function to resolve a chain
#' @keywords internal
#' REFACTOR REASON: Simplified branching logic by extracting dependency check
#' and using consistent parameter passing patterns.
.resolve_chain <- function(chain, is_pattern = FALSE) {
  .initialize_table()

  if (length(chain) == 0) {
    stop("No attributes in the chain. Use wkls$country or wkls$country$region, etc.")
  } else if (length(chain) == 1) {
    # Country or dependency lookup
    country_iso <- toupper(chain[1])
    query <- COUNTRY_OR_DEPENDENCY_QUERY
    params <- list(country_iso)

  } else if (length(chain) == 2) {
    country_iso <- toupper(chain[1])

    if (.is_dependency(country_iso)) {
      # Dependency → city lookup
      query <- if (is_pattern) DEPENDENCY_CITY_QUERY_PATTERN else DEPENDENCY_CITY_QUERY_EXACT
      params <- list(country_iso, chain[2], chain[2])
    } else {
      # Country → region lookup
      region_iso <- paste0(country_iso, "-", toupper(chain[2]))
      query <- REGION_QUERY
      params <- list(country_iso, region_iso)
    }

  } else if (length(chain) == 3) {
    # Country → region → city lookup
    country_iso <- toupper(chain[1])
    region_iso <- paste0(country_iso, "-", toupper(chain[2]))
    query <- if (is_pattern) CITY_QUERY_PATTERN else CITY_QUERY_EXACT
    params <- list(country_iso, region_iso, chain[3], chain[3])

  } else {
    stop("Too many chained attributes (max = 3)")
  }

  result <- dbGetQuery(.wkls_env$con, query, params = params)

  # Debug helper: show available cities if no match found
  if (nrow(result) == 0 && length(chain) == 3 && !is_pattern) {
    debug_query <- sprintf("
      SELECT DISTINCT name_primary FROM wkls
      WHERE country = '%s' AND region = '%s'
      AND subtype IN ('county', 'locality', 'localadmin')
      LIMIT 10
    ", country_iso, region_iso)
    available <- dbGetQuery(.wkls_env$con, debug_query)
    if (nrow(available) > 0) {
      message(sprintf("No match for '%s'. Available cities in %s-%s include: %s",
                      chain[3], country_iso, region_iso,
                      paste(head(available$name_primary, 5), collapse=", ")))
    }
  }

  return(result)
}

#' Internal function to get geometry
#' @keywords internal
.get_geom_expr <- function(chain, expr) {
  df <- .resolve_chain(chain)
  if (nrow(df) == 0) {
    stop(sprintf("No result found for: %s", paste(chain, collapse = ".")))
  }

  geom_id <- df$id[1]
  query <- sprintf("
    SELECT %s
    FROM parquet_scan('%s')
    WHERE id = '%s'
  ", expr, HF_PARQUET_PATH, geom_id)

  result_df <- dbGetQuery(.wkls_env$con, query)
  if (nrow(result_df) == 0) {
    stop(sprintf("No geometry found for ID: %s", geom_id))
  }

  result <- result_df[1, 1]

  # For WKB, the result might be wrapped in a list, extract the raw bytes
  if (is.list(result) && length(result) == 1) {
    result <- result[[1]]
  }

  return(result)
}

# ============================================================================
# Proxy Object Pattern
# ============================================================================

#' Create a wkls proxy object
#' @keywords internal
.make_wkls_proxy <- function(chain = character(0)) {
  obj <- list()  # Empty list - don't store chain here!
  class(obj) <- "wkls_proxy"
  attr(obj, "wkls_chain") <- chain
  obj
}

#' Print method for wkls_proxy
#'
#' Prints the resolved data frame for a wkls_proxy chain
#'
#' @param x A wkls_proxy object
#' @param ... Additional arguments (unused)
#' @return Invisibly returns the resolved data frame
#' @export
print.wkls_proxy <- function(x, ...) {
  chain <- attr(x, "wkls_chain", exact = TRUE)
  df <- .resolve_chain(chain)
  print(df)
  invisible(df)
}

# ============================================================================
# Data Frame Compatibility Methods
# ============================================================================
# REFACTOR REASON: These methods make the proxy behave like a data frame
# for common operations without requiring explicit conversion.

#' Check if object is a wkls_proxy
#'
#' S3 method that identifies wkls_proxy objects as data frames
#'
#' @param x A wkls_proxy object
#' @return A logical value: always returns TRUE for wkls_proxy objects
#' @export
is.data.frame.wkls_proxy <- function(x) {
  TRUE
}

#' Convert wkls_proxy to data frame
#'
#' Explicitly converts a wkls_proxy to a data frame by resolving the chain
#'
#' @param x A wkls_proxy object
#' @param ... Additional arguments (unused)
#' @return A data frame with columns: id, country, region, subtype, name
#' @export
as.data.frame.wkls_proxy <- function(x, ...) {
  chain <- attr(x, "wkls_chain", exact = TRUE)
  .resolve_chain(chain)
}

#' Get number of rows
#'
#' Returns the number of rows in the resolved data
#'
#' @param x A wkls_proxy object
#' @return An integer representing the number of rows, or 0L for empty chains
#' @export
nrow.wkls_proxy <- function(x) {
  chain <- attr(x, "wkls_chain", exact = TRUE)
  if (length(chain) == 0) {
    return(0L)
  }
  df <- .resolve_chain(chain)
  nrow(df)
}

#' Get number of columns
#'
#' Returns the number of columns in the resolved data
#'
#' @param x A wkls_proxy object
#' @return An integer representing the number of columns, or 0L for empty chains
#' @export
ncol.wkls_proxy <- function(x) {
  chain <- attr(x, "wkls_chain", exact = TRUE)
  if (length(chain) == 0) {
    return(0L)
  }
  df <- .resolve_chain(chain)
  ncol(df)
}

#' Get dimensions
#'
#' Returns the dimensions (rows and columns) of the resolved data
#'
#' @param x A wkls_proxy object
#' @return An integer vector of length 2 giving rows and columns,
#'   or c(0L, 0L) for empty chains
#' @export
dim.wkls_proxy <- function(x) {
  chain <- attr(x, "wkls_chain", exact = TRUE)
  if (length(chain) == 0) {
    return(c(0L, 0L))
  }
  df <- .resolve_chain(chain)
  dim(df)
}

#' Get column names
#'
#' Returns the column names of the resolved data
#'
#' @param x A wkls_proxy object
#' @return A character vector of column names, typically:
#'   "id", "country", "region", "subtype", "name"
#' @export
names.wkls_proxy <- function(x) {
  chain <- attr(x, "wkls_chain", exact = TRUE)
  if (length(chain) == 0) {
    return(character(0))
  }
  df <- .resolve_chain(chain)
  names(df)
}

#' Extract rows/columns like a data frame
#'
#' Allows subsetting the resolved data
#'
#' @param x A wkls_proxy object
#' @param i Row indices
#' @param j Column indices
#' @param drop Whether to drop dimensions
#' @return A subset of the resolved data frame
#' @export
`[.wkls_proxy` <- function(x, i, j, drop = TRUE) {
  chain <- attr(x, "wkls_chain", exact = TRUE)
  if (length(chain) == 0) {
    stop("Cannot extract from empty wkls object")
  }
  df <- .resolve_chain(chain)
  df[i, j, drop = drop]
}

# ============================================================================
# Geometry Method Builders
# ============================================================================
# REFACTOR REASON: Extracted method creation into helper functions to reduce
# duplication in the $ operator. Each geometry method follows the same pattern.

#' Create a geometry method closure
#' @keywords internal
.make_geometry_method <- function(chain, expr_template, ...) {
  function(...) {
    expr <- if (is.function(expr_template)) {
      expr_template(...)
    } else {
      expr_template
    }
    .get_geom_expr(chain, expr)
  }
}

#' Create a helper method (countries, regions, etc.)
#' @keywords internal
#' REFACTOR REASON: Helper methods follow similar validation and query patterns.
#' This function reduces duplication while keeping the validation logic clear.
.make_helper_method <- function(chain,
                                expected_levels,
                                error_msg,
                                query_builder) {
  function() {
    if (length(chain) != expected_levels) {
      stop(error_msg)
    }
    query_builder(chain)
  }
}

# ============================================================================
# Main Operators
# ============================================================================

#' Dollar operator for wkls_proxy
#'
#' Provides access to chaining attributes and methods
#'
#' @param x A wkls_proxy object
#' @param name Name of attribute or method to access
#' @return Depends on usage: a wkls_proxy object for chaining, a function
#'   for methods (wkt, wkb, etc.), or a data frame for helper methods
#'   (countries, regions, etc.)
#' @export
`$.wkls_proxy` <- function(x, name) {
  chain <- attr(x, "wkls_chain", exact = TRUE)

  if (name == "chain") {
    return(chain)
  }

  # Method names
  method_names <- c("wkt", "wkb", "hexwkb", "geojson", "svg",
                    "countries", "dependencies", "regions", "counties", "cities", "subtypes",
                    "overture_version")

  if (name %in% method_names) {
    # GEOMETRY METHODS
    # REFACTOR REASON: Using helper function reduces repetitive closure creation
    if (name == "wkt") {
      return(.make_geometry_method(chain, "ST_AsText(geometry)"))
    } else if (name == "wkb") {
      return(.make_geometry_method(chain, "ST_AsWKB(geometry)"))
    } else if (name == "hexwkb") {
      return(.make_geometry_method(chain, "ST_AsHEXWKB(geometry)"))
    } else if (name == "geojson") {
      return(.make_geometry_method(chain, "ST_AsGeoJSON(geometry)"))
    } else if (name == "svg") {
      return(function(relative = FALSE, precision = 15L) {
        expr <- sprintf("ST_AsSVG(geometry, %s, %d)",
                        tolower(as.character(relative)),
                        as.integer(precision))
        .get_geom_expr(chain, expr)
      })
    }

    # HELPER METHODS
    # REFACTOR REASON: Similar validation patterns extracted into helper
    else if (name == "countries") {
      return(function() {
        if (length(chain) > 0) {
          stop("countries() can only be called on the root object.")
        }
        .initialize_table()
        dbGetQuery(.wkls_env$con, "
          SELECT DISTINCT id, country, subtype, name_primary as name
          FROM wkls
          WHERE subtype = 'country'
        ")
      })
    } else if (name == "dependencies") {
      return(function() {
        if (length(chain) > 0) {
          stop("dependencies() can only be called on the root object.")
        }
        .initialize_table()
        dbGetQuery(.wkls_env$con, "
          SELECT DISTINCT id, country, subtype, name_primary as name
          FROM wkls
          WHERE subtype = 'dependency'
        ")
      })
    } else if (name == "regions") {
      return(function() {
        if (length(chain) != 1) {
          stop("regions() requires exactly one level of chaining. Use wkls$country$regions()")
        }
        .initialize_table()
        country_iso <- toupper(chain[1])

        if (.is_dependency(country_iso)) {
          stop(sprintf("The country '%s' does not have regions in the dataset", country_iso))
        }

        query <- .build_list_query(country_iso, NULL, "'region'", "region")
        dbGetQuery(.wkls_env$con, query)
      })
    } else if (name == "counties") {
      return(function() {
        if (length(chain) != 2) {
          stop("counties() requires exactly two levels of chaining. Use wkls$country$region$counties()")
        }
        .initialize_table()
        country_iso <- toupper(chain[1])
        region_iso <- paste0(country_iso, "-", toupper(chain[2]))

        query <- .build_list_query(country_iso, region_iso, "'county'", "name_primary")
        dbGetQuery(.wkls_env$con, query)
      })
    } else if (name == "cities") {
      return(function() {
        if (length(chain) == 0) {
          stop("cities() requires exactly two levels of chaining. Use wkls$country$region$cities()")
        }

        .initialize_table()
        country_iso <- toupper(chain[1])

        if (length(chain) == 1) {
          if (.is_dependency(country_iso)) {
            # Dependency - get cities directly
            query <- .build_list_query(country_iso, NULL, "'locality', 'localadmin'", "name_primary")
            return(dbGetQuery(.wkls_env$con, query))
          } else {
            stop("cities() requires exactly two levels of chaining. Use wkls$country$region$cities()")
          }
        } else if (length(chain) == 2) {
          if (.is_dependency(country_iso)) {
            stop("cities() requires exactly two levels of chaining. Use wkls$country$region$cities()")
          } else {
            region_iso <- paste0(country_iso, "-", toupper(chain[2]))
            query <- .build_list_query(country_iso, region_iso, "'locality', 'localadmin'", "name_primary")
            return(dbGetQuery(.wkls_env$con, query))
          }
        } else {
          stop("cities() requires exactly two levels of chaining. Use wkls$country$region$cities()")
        }
      })
    } else if (name == "subtypes") {
      return(function() {
        if (length(chain) > 0) {
          stop("subtypes() can only be called on the root object.")
        }
        .initialize_table()
        dbGetQuery(.wkls_env$con, "SELECT DISTINCT subtype FROM wkls")
      })
    } else if (name == "overture_version") {
      return(function() {
        if (length(chain) > 0) {
          stop("overture_version() is only available at the root level.")
        }
        OVERTURE_VERSION
      })
    }
  }

  # Chain another attribute
  new_chain <- c(chain, tolower(name))
  if (length(new_chain) > 3) {
    stop("Too many chained attributes (max = 3)")
  }

  .make_wkls_proxy(new_chain)
}

#' Double bracket operator for wkls_proxy
#'
#' Allows dictionary-style access and pattern matching with percent signs
#'
#' @param x A wkls_proxy object
#' @param name Name to access (supports \% for pattern matching)
#' @return A wkls_proxy object for regular lookups, or a data frame
#'   when pattern matching is used (names containing \%)
#' @export
`[[.wkls_proxy` <- function(x, name) {
  chain <- attr(x, "wkls_chain", exact = TRUE)

  if (grepl("%", name)) {
    # Pattern search - keep original case
    new_chain <- c(chain, name)

    if (length(new_chain) > 3) {
      stop("Too many chained attributes (max = 3)")
    }

    return(.resolve_chain(new_chain, is_pattern = TRUE))
  } else {
    # Regular lookup - lowercase for consistency
    name_lower <- tolower(name)
    new_chain <- c(chain, name_lower)

    if (length(new_chain) > 3) {
      stop("Too many chained attributes (max = 3)")
    }

    return(.make_wkls_proxy(new_chain))
  }
}

# ============================================================================
# Package Exports
# ============================================================================

#' Well-Known Locations Object
#'
#' The main entry point for accessing global administrative boundaries.
#' Chain country, region, and city codes to retrieve geographical data.
#'
#' @format A wkls_proxy object
#' @examples
#' \donttest{
#' # Get country geometry
#' wkls$us$wkt()
#'
#' # Get region geometry
#' wkls$us$ca$geojson()
#'
#' # Get city geometry
#' wkls$us$ca$sanfrancisco$wkt()
#'
#' # List all countries
#' wkls$countries()
#'
#' # List regions in a country
#' wkls$us$regions()
#' }
#' @export
wkls <- .make_wkls_proxy()

# Cleanup function
.onUnload <- function(libpath) {
  if (exists("con", envir = .wkls_env)) {
    dbDisconnect(.wkls_env$con, shutdown = TRUE)
  }
}
