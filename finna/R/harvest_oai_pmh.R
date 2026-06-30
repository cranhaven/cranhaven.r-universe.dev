#' @title Harvest Metadata from an OAI-PMH Server
#'
#' @description
#' This function harvests metadata records from an OAI-PMH-compliant server in batches,
#' using a custom User-Agent string to identify the service and returns them in a tibble format.
#'
#' @param base_url A string. The base URL of the OAI-PMH server.
#' @param metadata_prefix A string. The metadata format to request (e.g., "oai_dc", "marc21").
#' @param set A string. Optional. A set specifier to limit the harvested records (e.g., "non_dedup").
#' @param verbose A logical. Whether to display progress messages. Default is `TRUE`.
#' @param record_limit limits the number of records that the user wants to fetch
#' @param output_file output file to be saved as a csv file.
#' @param user_agent A string. A custom User-Agent string to identify the service. Default is "FinnaHarvester/1.0".
#'
#' @return A tibble with the harvested records containing selected metadata fields.
#' @importFrom xml2 read_xml xml_find_all xml_find_first xml_text xml_attr xml_name xml_ns_strip
#' @importFrom httr GET content user_agent
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble tibble
#' @importFrom readr write_csv
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Example for oai_dc (Dublin Core)
#' records_oai_dc <- harvest_oai_pmh(
#' base_url = "https://api.finna.fi/OAI/Server",
#' metadata_prefix = "oai_dc",
#' user_agent = "MyCustomHarvester/1.0"
#' )
#' # Example for marc21 (MARC 21)
#' records_marc21 <- harvest_oai_pmh(
#' base_url = "https://api.finna.fi/OAI/Server",
#' metadata_prefix = "marc21",
#' user_agent = "MyCustomHarvester/1.0"
#' )
#'
#' # Example for oai_vufind_json (VuFind JSON)
#' records_oai_vufind_json <- harvest_oai_pmh(
#' base_url = "https://api.finna.fi/OAI/Server",
#' metadata_prefix = "oai_vufind_json",
#' user_agent = "MyCustomHarvester/1.0"
#' )
#'
#' # Example for oai_ead (Encoded Archival Description)
#' records_oai_ead <- harvest_oai_pmh(
#' base_url = "https://api.finna.fi/OAI/Server",
#' metadata_prefix = "oai_ead",
#' user_agent = "MyCustomHarvester/1.0"
#' )
#' # Example for oai_ead3 (Encoded Archival Description version 3)
#' records_oai_ead3 <- harvest_oai_pmh(
#' base_url = "https://api.finna.fi/OAI/Server",
#' metadata_prefix = "oai_ead3",
#' user_agent = "MyCustomHarvester/1.0"
#' )
#'
#' # Example for oai_forward (Forward metadata format)
#' records_oai_forward <- harvest_oai_pmh(
#' base_url = "https://api.finna.fi/OAI/Server",
#' metadata_prefix = "oai_forward",
#' user_agent = "MyCustomHarvester/1.0"
#' )
#'
#' # Example for oai_lido (Lightweight Information Describing Objects)
#' records_oai_lido <- harvest_oai_pmh(
#' base_url = "https://api.finna.fi/OAI/Server",
#' metadata_prefix = "oai_lido",
#' user_agent = "MyCustomHarvester/1.0"
#' )
#'
#' # Example for oai_qdc (Qualified Dublin Core)
#' records_oai_qdc <- harvest_oai_pmh(
#' base_url = "https://api.finna.fi/OAI/Server",
#' metadata_prefix = "oai_qdc",
#' user_agent = "MyCustomHarvester/1.0"
#' )
#' }

harvest_oai_pmh <- function(base_url, metadata_prefix, set = NULL,
                            verbose = TRUE, user_agent = "FinnaHarvester/1.0",
                            output_file = NULL, record_limit = NULL) {
  if (missing(base_url) || !nzchar(base_url)) {
    stop("The 'base_url' parameter must be provided and non-empty.")
  }

  url <- paste0(base_url, "?verb=ListRecords")
  params <- list(metadataPrefix = metadata_prefix)
  if (!is.null(set)) {
    params$set <- set
  }

  all_records <- list()
  page <- 1
  fetched_records <- 0
  pb <- NULL
  complete_list_size <- NULL

  repeat {
    if (verbose) message("Fetching page ", page, "...")
    response <- tryCatch({
      httr::GET(url, query = params, httr::user_agent(user_agent))
    }, error = function(e) {
      warning("Request failed: ", e$message)
      return(NULL)
    })

    if (is.null(response) || response$status_code != 200) {
      warning("Failed to fetch data. HTTP status code: ", response$status_code)
      break
    }

    raw_content <- httr::content(response, as = "text", encoding = "UTF-8")
    #print(raw_content)
    xml <- read_xml(raw_content)
    xml <- strip_namespaces(xml)

    records <- xml_find_all(xml, "//record")
    if (length(records) > 0) {
      record_list <- lapply(records, function(record) {
        tryCatch({
          metadata <- xml_find_first(record, "metadata")
          raw_xml <- as.character(record)

          # Extract setSpec (Collection Name)
          set_spec_nodes <- xml_find_all(record, ".//setSpec")
          set_spec <- if (length(set_spec_nodes) > 0) paste(xml_text(set_spec_nodes), collapse = "|") else NA

          if (!is.null(metadata)) {
            # Extract all child elements dynamically with unique names
            fields <- xml_find_all(metadata, ".//*[local-name()]")
            field_data <- lapply(fields, xml_text)
            field_names <- sapply(fields, xml_name)

            # Ensure unique names for tibble
            unique_field_names <- make.unique(field_names)

            # Return a named list with metadata fields and raw XML
            metadata_list <- setNames(as.list(field_data), unique_field_names)
            metadata_list$raw_xml <- raw_xml
            metadata_list$setSpec <- set_spec  # Store collection name
            metadata_list
          } else {
            list(raw_xml = raw_xml)  # Raw XML only if no metadata
            list(setSpec = set_spec)
          }
        }, error = function(e) {
          warning("Error parsing record: ", e$message)
          return(NULL)
        })
      })

      # Fetch identifiers and setSpec
      id_set_map <- fetch_identifiers_with_sets(base_url, metadata_prefix, set, user_agent)

      # Filter out NULL results
      record_list <- record_list[!sapply(record_list, is.null)]
      all_records <- c(all_records, record_list)
      fetched_records <- fetched_records + length(record_list)

      # Stop if the record limit is reached
      if (!is.null(record_limit) && fetched_records >= record_limit) {
        all_records <- all_records[1:record_limit]  # Trim to exact limit
        break
      }
    }

    token_node <- xml_find_first(xml, "//resumptionToken")
    resumption_token <- xml_text(token_node)

    if (is.null(complete_list_size)) {
      complete_list_size <- as.numeric(xml_attr(token_node, "completeListSize"))
      if (!is.na(complete_list_size) && verbose) {
        pb <- progress::progress_bar$new(
          format = "[:bar] :percent ETA: :eta",
          total = ifelse(is.null(record_limit), complete_list_size, record_limit),
          clear = FALSE
        )
      }
    }

    # Update Progress Bar Safely
    if (!is.null(pb)) {
      pb$tick(min(length(record_list), pb$total - pb$current))
    }

    # Stop if the record limit is reached
    if (!is.null(record_limit) && fetched_records >= record_limit) {
      all_records <- all_records[1:record_limit]
      break
    }

    params <- list(resumptionToken = resumption_token)
    page <- page + 1
  }

  # Combine all records into a tibble
  if (length(all_records) > 0) {
    if (metadata_prefix == "marc21") {
      df <- dplyr::bind_rows(lapply(all_records, function(x) {
        if (!is.null(x$raw_xml)) {
          parsed <- parse_marc_fields(x$raw_xml)
          if (!is.null(x$setSpec)) parsed$setSpec <- x$setSpec
          return(tibble::as_tibble(parsed, .name_repair = "unique"))
        } else {
          return(NULL)
        }
      }))
    } else {
      df <- dplyr::bind_rows(lapply(all_records, function(x) tibble::as_tibble(x, .name_repair = "unique")))
    }

  } else {
    df <- tibble::tibble()
  }

  if (!is.null(output_file)) {
    readr::write_csv(df, file = output_file, row.names = FALSE)
    if (verbose) message("Records saved to: ", output_file)
  }

  if (verbose) message("Finished harvesting ", nrow(df), " records.")
  return(df)
}

#' @title Fetch Available OAI-PMH Sets
#' @description Fetches and lists the available sets (collections) from an OAI-PMH server.
#' @param base_url A string. The base URL of the OAI-PMH server.
#' @param user_agent A string. Custom User-Agent string. Default is "OAIHarvester/1.0".
#' @return A tibble with setSpec and setName columns.
#' @importFrom httr GET content user_agent
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @importFrom tibble tibble
#' @export
fetch_oai_sets <- function(base_url, user_agent = "FinnaHarvester/1.0") {
  url <- paste0(base_url, "?verb=ListSets")
  response <- tryCatch({
    httr::GET(url, httr::user_agent(user_agent))
  }, error = function(e) {
    warning("Failed to fetch sets: ", e$message)
    return(NULL)
  })

  if (is.null(response) || response$status_code != 200) {
    stop("Failed to fetch sets. HTTP status code: ", response$status_code)
  }

  raw_content <- httr::content(response, as = "text", encoding = "UTF-8")
  xml <- read_xml(raw_content)
  xml <- strip_namespaces(xml)

  sets <- xml_find_all(xml, "//set")
  if (length(sets) == 0) {
    warning("No sets found.")
    return(tibble::tibble(setSpec = character(), setName = character()))
  }

  tibble::tibble(
    setSpec = xml_text(xml_find_first(sets, "setSpec")),
    setName = xml_text(xml_find_first(sets, "setName"))
  )
}
strip_namespaces <- function(doc) {
  xml_ns_strip(doc)
  return(doc)
}



#' @title Parse a MARC21 Record from Raw XML
#' @description Converts MARC21 XML to a named list with field+subfield keys (e.g., "245a").
#' @param xml_string A string of MARCXML for one record.
#' @return A named list of parsed fields.
parse_marc_fields <- function(xml_string) {
  doc <- xml2::read_xml(xml_string)
  xml2::xml_ns_strip(doc)

  leader <- xml2::xml_text(xml2::xml_find_first(doc, ".//leader"))
  ctrl_fields <- xml2::xml_find_all(doc, ".//controlfield")
  ctrl_list <- stats::setNames(xml2::xml_text(ctrl_fields), xml2::xml_attr(ctrl_fields, "tag"))

  data_fields <- xml2::xml_find_all(doc, ".//datafield")
  data_list <- list()

  for (df in data_fields) {
    tag <- xml2::xml_attr(df, "tag")
    subfields <- xml2::xml_find_all(df, ".//subfield")
    for (sf in subfields) {
      code <- xml2::xml_attr(sf, "code")
      key <- paste0(tag, code)
      val <- xml2::xml_text(sf)

      # Collapsing multiple values into one string (with | separator)
      if (!is.null(data_list[[key]])) {
        data_list[[key]] <- paste(data_list[[key]], val, sep = " | ")
      } else {
        data_list[[key]] <- val
      }
    }
  }

  result <- c(list(leader = leader), ctrl_list, data_list)
  return(result)
}
