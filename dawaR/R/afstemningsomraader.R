afstemningsomraader <- function(func, ...) {
  if (func == "reverse") {
    vote_rev(!!!...)
  } else if (func == "autocomplete") {
    vote_ac(!!!...)
  }
}

vote <- function(...) {
  afstemningsomraader(!!!...)
}

vote_rev <- function(x, y, type = NULL, as_df = FALSE, as_list = FALSE) {
  if (!is.null(type)) {
    if (check_coordinate_type(type)) {
      coord <- coordinate_type(type)
    }
  } else {
    coord <- type
  }

  response <- reverse(
    section = "afstemningsomraader",
    x = x,
    y = y,
    srid = coord
  )

  if (is.null(response)) {
    return(NULL)
  }

  name <- response$afstemningssted$navn
  address <- response$afstemningssted$adgangsadresse$adressebetegnelse
  municipality <- response$kommune$navn
  region <- response$region$navn
  constituency <- response$opstillingskreds$navn

  if (as_df) {
    output <- data.frame(name, address, municipality, region, constituency)
  } else if (as_list) {
    output <- list(
      "name" = name,
      "address" = address,
      "municipality" = municipality,
      "region" = region,
      "constituency" = constituency
    )
  } else {
    output <- name
  }

  return(output)
}

vote_ac <- function(input) {
  if (!is.character(input)) {
    cli::cli_abort("{.var input} is not of type {.var character}")
  }

  response <- autocomplete(
    section = "afstemningsomraader",
    input = input
  )

  if (is.null(response)) {
    return(NULL)
  }

  if (length(response) > 1) {
    output <- response[[1]][[2]]$navn
  } else if (length(response == 1)) {
    output <- response[[2]]$navn
  } else if (length(response) < 1) {
    cli::cli_alert_danger("No entry found for {.var {input}}")
    output <- NA
  }

  return(output)
}
