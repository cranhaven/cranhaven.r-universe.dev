nisra_data_portal_request <- function(method, ...) {
  path_order <- c(
    "datefrom",
    "matrix",
    "format_type",
    "format_version",
    "language"
  )
  params <- rlang::list2(...)
  params <- purrr::list_c(params[path_order])

  req <- httr2::request("https://ws-data.nisra.gov.uk/public/api.restful") |>
    httr2::req_user_agent("nisrarr (http://github.com/MarkPaulin/nisrarr)") |>
    httr2::req_throttle(getOption("nisrarr.throttle", 60)) |>
    httr2::req_url_path_append(method)

  if (!is.null(params)) {
    req <- httr2::req_url_path_append(req, params)
  }

  resp <- req |>
    httr2::req_perform()

  resp
}


nisra_data_portal <- function(method, ..., flush_cache = FALSE) {
  params <- list(...)
  key <- rlang::hash(list(method, params))

  cached_resp <- nisrarr_cache$get(key)
  if (!cachem::is.key_missing(cached_resp) && !flush_cache) {
    resp_body <- cached_resp |>
      httr2::resp_body_string()

    return(resp_body)
  }

  resp <- nisra_data_portal_request(method, ...)

  resp_body <- resp |>
    httr2::resp_body_string()

  nisrarr_cache$set(key, resp)

  resp_body
}


#' Read NISRA data portal dataset
#'
#' Fetch a dataset from the NISRA data portal using the dataset code. You
#' can search for a dataset using [nisra_search()].
#'
#' @param dataset_code Dataset code
#' @param flush_cache Ignore cached values
#'
#' @return A tibble with the requested dataset. If `dataset_code`
#' is not found, an error will be thrown.
#' @export
#'
#' @examples
#' claimant_count_lgd <- nisra_read_dataset("CCMLGD")
nisra_read_dataset <- function(dataset_code, flush_cache = FALSE) {
  response <- nisra_data_portal(
    "PxStat.Data.Cube_API.ReadDataset",
    matrix = dataset_code,
    format_type = "JSON-stat",
    format_version = "2.0",
    language = "en",
    flush_cache = flush_cache
  )

  resp_list <- jsonlite::fromJSON(response)
  meta <- c(
    resp_list[["extension"]],
    note = resp_list[["note"]],
    label = resp_list[["label"]],
    updated = resp_list[["updated"]]
  )

  nisra_df(
    rjstat::fromJSONstat(response),
    meta = meta
  )
}


nisra_read_collection <- function(datefrom = NULL, flush_cache = FALSE) {
  if (missing(datefrom) || is.null(datefrom)) {
    datefrom <- lubridate::today() - lubridate::dmonths(3)
    datefrom <- format(datefrom, format = "%Y-%m-%d")
  }

  resp <- nisra_data_portal(
    "PxStat.Data.Cube_API.ReadCollection",
    language = "en",
    datefrom = datefrom,
    flush_cache = flush_cache
  )
  resp <- jsonlite::fromJSON(resp, simplifyDataFrame = FALSE)

  codes <- vapply(
    resp$link$item,
    \(x) {
      x[["extension"]][["matrix"]]
    },
    character(1)
  )

  labels <- vapply(
    resp$link$item,
    \(x) {
      x[["label"]]
    },
    character(1)
  )

  frequencies <- vapply(
    resp$link$item,
    \(x) {
      dims <- x[["dimension"]]
      dplyr::coalesce(
        dims[["TLIST(A1)"]][["label"]],
        dims[["TLIST(H1)"]][["label"]],
        dims[["TLIST(Q1)"]][["label"]],
        dims[["TLIST(M1)"]][["label"]],
        dims[["TLIST(W1)"]][["label"]],
        dims[["TLIST(D1)"]][["label"]]
      )
    },
    character(1)
  )

  dimensions <- lapply(resp$link$item, \(x) {
    dims <- x[["dimension"]]
    vapply(
      dims,
      \(dim) {
        dim[["label"]]
      },
      character(1)
    )
  })

  updated_dates <- lubridate::ymd_hms(vapply(
    resp$link$item,
    \(x) {
      x[["updated"]]
    },
    character(1)
  ))

  tibble::tibble(
    dataset_code = codes,
    dataset_label = labels,
    frequency = frequencies,
    dataset_dimensions = dimensions,
    updated = updated_dates
  )
}


#' Search for a NISRA dataset
#'
#' Search the NISRA data portal for a dataset. You can search dataset titles
#' either for a keyword or with a regular expression, using a dataset code, or
#' by variables that appear in dataset. You can also specify how recently the
#' dataset must have been updated.
#'
#' @param keyword Text to search for in dataset titles
#' @param regex Regular expression for searching dataset titles
#' @param dataset_code Dataset to find
#' @param variables Variables to search for in datasets
#' @param datefrom Date to search from. Search is limited to datasets updated in
#' the last three months if not specified.
#' @param flush_cache Ignore cached values
#'
#' @return A tibble of dataset information matching the search
#' terms. This will include dataset codes, label, frequency, dimensions,
#' and dimensions.
#' @export
#'
#' @examples
#' population_datasets <- nisra_search(keyword = "population")
#' age_datasets <- nisra_search(variables = "age")
nisra_search <- function(
  keyword = NULL,
  regex = NULL,
  dataset_code = NULL,
  variables = NULL,
  datefrom = NULL,
  flush_cache = FALSE
) {
  coll <- nisra_read_collection(datefrom = datefrom, flush_cache = flush_cache)
  if (!missing(keyword)) {
    pattern <- stringr::fixed(keyword, ignore_case = TRUE)
    coll <- coll[stringr::str_detect(coll[["dataset_label"]], pattern), ]
  }

  if (!missing(regex)) {
    pattern <- stringr::regex(regex, ignore_case = TRUE)
    coll <- coll[stringr::str_detect(coll[["dataset_label"]], regex), ]
  }

  if (!missing(dataset_code)) {
    coll <- coll[coll[["dataset_code"]] == dataset_code, ]
  }

  if (!missing(variables)) {
    found <- vapply(
      coll[["dataset_dimensions"]],
      \(x) {
        all(vapply(
          variables,
          \(y) {
            any(stringr::str_detect(
              stringr::str_to_lower(x),
              stringr::coll(y, ignore_case = TRUE)
            ))
          },
          logical(1)
        ))
      },
      logical(1)
    )

    coll <- coll[found, ]
  }

  coll
}

nisrarr_cache <- cachem::cache_mem(
  max_age = getOption("nisrarr.cache_max_age", Inf)
)
