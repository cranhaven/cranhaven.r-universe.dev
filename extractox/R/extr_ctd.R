#' Extract Data from the CTD API
#'
#' This function queries the Comparative Toxicogenomics Database API to retrieve data related to chemicals, diseases, genes, or other categories.
#'
#' @param input_terms A character vector of input terms such as CAS numbers or IUPAC names.
#' @param category A string specifying the category of data to query. Valid options
#'   are "all", "chem", "disease", "gene", "go", "pathway", "reference", and "taxon".
#'   Default is "chem".
#' @param report_type A string specifying the type of report to return. Default is
#'   "genes_curated". Valid options include:
#'   \describe{
#'     \item{"cgixns"}{Curated chemical-gene interactions. Requires at least one
#'        \code{action_types} parameter.}
#'     \item{"chems"}{All chemical associations.}
#'     \item{"chems_curated"}{Curated chemical associations.}
#'     \item{"chems_inferred"}{Inferred chemical associations.}
#'     \item{"genes"}{All gene associations.}
#'     \item{"genes_curated"}{Curated gene associations.}
#'     \item{"genes_inferred"}{Inferred gene associations.}
#'     \item{"diseases"}{All disease associations.}
#'     \item{"diseases_curated"}{Curated disease associations.}
#'     \item{"diseases_inferred"}{Inferred disease associations.}
#'     \item{"pathways_curated"}{Curated pathway associations.}
#'     \item{"pathways_inferred"}{Inferred pathway associations.}
#'     \item{"pathways_enriched"}{Enriched pathway associations.}
#'     \item{"phenotypes_curated"}{Curated phenotype associations.}
#'     \item{"phenotypes_inferred"}{Inferred phenotype associations.}
#'     \item{"go"}{All Gene Ontology (GO) associations. Requires at least one
#'        \code{ontology} parameter.}
#'     \item{"go_enriched"}{Enriched GO associations. Requires at least one
#'        \code{ontology} parameter.}
#'   }
#' @param input_term_search_type A string specifying the search method to use.
#'   Options are "hierarchicalAssociations" or "directAssociations". Default is
#'   "directAssociations".
#' @param action_types An optional character vector specifying one or more interaction
#'   types for filtering results. Default is "ANY".
#'   Other acceptable inputs are "abundance", "activity", "binding", "cotreatment",
#'   "expression", "folding", "localization", "metabolic processing"...
#'   See https://ctdbase.org/tools/batchQuery.go for a full list.
#' @param ontology An optional character vector specifying one or more ontologies
#'   for filtering GO reports. Default NULL.
#' @param verify_ssl Boolean to control of SSL should be verified or not.
#' @param verbose A logical value indicating whether to print detailed messages.
#'   Default is TRUE.
#' @param ... Any other arguments to be supplied to `req_option` and thus to `libcurl`.
#' @return A data frame containing the queried data in CSV format.
#' @seealso \href{https://ctdbase.org}{Comparative Toxicogenomics Database}
#' @references
#' - Davis, A. P., Grondin, C. J., Johnson, R. J., Sciaky, D., McMorran, R.,
#'   Wiegers, T. C., & Mattingly, C. J. (2019).
#' The Comparative Toxicogenomics Database: update 2019. Nucleic acids research,
#'   47(D1), D948–D954. \doi{10.1093/nar/gky868}
#' @export
#' @examples
#' \donttest{
#' input_terms <- c("50-00-0", "64-17-5", "methanal", "ethanol")
#' dat <- extr_ctd(
#'   input_terms = input_terms,
#'   category = "chem",
#'   report_type = "genes_curated",
#'   input_term_search_type = "directAssociations",
#'   action_types = "ANY",
#'   ontology = c("go_bp", "go_cc")
#' )
#' str(dat)
#'
#' # Get expresssion data
#' dat2 <- extr_ctd(
#'   input_terms = input_terms,
#'   report_type = "cgixns",
#'   category = "chem",
#'   action_types = "expression"
#' )
#'
#' str(dat2)
#' }
extr_ctd <- function(
    input_terms,
    category = "chem",
    report_type = "genes_curated",
    input_term_search_type = "directAssociations",
    action_types = NULL,
    ontology = NULL,
    verify_ssl = FALSE,
    verbose = TRUE,
    ...) {
  if (base::missing(input_terms)) {
    cli::cli_abort("The argument {.field {input_terms}} is required.")
  }


  base_url <- "https://ctdbase.org/tools/batchQuery.go"
  check_internet(verbose = verbose)


  col_names <- c(
    "chemical_name", "chemical_id", "casrn", "gene_symbol",
    "gene_id", "organism", "organism_id", "pubmed_ids", "query"
  )

  params <- list(
    inputType = category,
    inputTerms = NULL, # this is add after
    report = report_type,
    format = "csv", # Fixed to CSV format
    inputTermSearchType = input_term_search_type
  )

  # Add optional parameters and collapse is vector of length > 1
  params_to_collapse <- list(
    inputTerms = input_terms,
    actionTypes = action_types,
    ontology = ontology
  )

  # this can be fixed using .multi in req_url_query
  for (param_name in names(params_to_collapse)) {
    param_value <- params_to_collapse[[param_name]]
    if (!is.null(param_value)) {
      params[[param_name]] <- paste(param_value, collapse = "|")
    }
  }

  # Perform the request and get a response
  if (isTRUE(verbose)) {
    cli::cli_alert_info("Sending request to CTD database...")
  }

  libcurl_opt <- set_ssl(verify_ssl = verify_ssl, other_opt = ...)

  resp <- tryCatch(
    {
      httr2::request(base_url) |>
        httr2::req_url_query(!!!params) |>
        httr2::req_options(!!!libcurl_opt) |>
        httr2::req_perform()
    },
    error = function(e) {
      cli::cli_abort("Failed to perform the request: {conditionMessage(e)}")
    }
  )

  check_status_code(resp, verbose = verbose)

  csv_file <- tempfile(fileext = "csv")

  resp |>
    httr2::resp_body_raw() |>
    writeBin(csv_file)

  out <- utils::read.csv(csv_file) |>
    janitor::clean_names()

  names(out)[names(out) == "x_input"] <- "query"
  out <- out[c(setdiff(names(out), "query"), "query")]
  names(out) <- col_names

  out[out == ""] <- NA

  # Lets clean up
  out$query <- gsub(" \\[Object not found\\]", "", out$query)

  check_na_warn(dat = out, col_to_check = "gene_id", verbose = verbose)

  unlink(csv_file)
  out
}

#' Extract Tetramer Data from the CTD API
#'
#' This function queries the Comparative Toxicogenomics Database API to retrieve
#' tetramer data based on chemicals, diseases, genes, or other categories.
#'
#' @param chem A string indicating the chemical identifiers such as CAS number or
#'   IUPAC name of the chemical.
#' @param disease A string indicating a disease term. Default is an empty string.
#' @param gene A string indicating a gene symbol. Default is an empty string.
#' @param go A string indicating a Gene Ontology term. Default is an empty string.
#' @param input_term_search_type A string specifying the search method to use.
#'   Options are "hierarchicalAssociations" or "directAssociations". Default is
#'   "directAssociations".
#' @param qt_match_type A string specifying the query type match method. Options
#'   are "equals" or "contains". Default is "equals".
#' @param verify_ssl Boolean to control if SSL should be verified or not.
#'   Default is FALSE.
#' @param verbose A logical value indicating whether to print detailed messages.
#'   Default is TRUE.
#' @param ... Any other arguments to be supplied to `req_option` and thus to `libcurl`.
#' @return A data frame containing the queried tetramer data in CSV format.
#' @seealso \href{https://ctdbase.org}{Comparative Toxicogenomics Database}
#' @references
#' - Comparative Toxicogenomics Database: \url{https://ctdbase.org}
#' - Davis, A. P., Grondin, C. J., Johnson, R. J., Sciaky, D., McMorran, R.,
#'   Wiegers, T. C., & Mattingly, C. J. (2019).
#' The Comparative Toxicogenomics Database: update 2019. Nucleic acids research,
#'   47(D1), D948–D954. \doi{10.1093/nar/gky868}
#' - Davis, A. P., Wiegers, T. C., Wiegers, J., Wyatt, B., Johnson,
#'   R. J., Sciaky, D., Barkalow, F., Strong, M., Planchart, A.,
#'   & Mattingly, C. J. (2023). CTD tetramers: A new online tool that computationally
#'   links curated chemicals, genes, phenotypes, and diseases to inform molecular
#'    mechanisms for environmental health. Toxicological Sciences, 195(2), 155–168.
#' \doi{10.1093/toxsci/kfad069}
#' @export
#' @examples
#' \donttest{
#' tetramer_data <- extr_tetramer(
#'   chem = c("50-00-0", "ethanol"),
#'   disease = "",
#'   gene = "",
#'   go = "",
#'   input_term_search_type = "directAssociations",
#'   qt_match_type = "equals"
#' )
#' str(tetramer_data)
#' }
extr_tetramer <- function(
    chem,
    disease = "",
    gene = "",
    go = "",
    input_term_search_type = "directAssociations",
    qt_match_type = "equals",
    verify_ssl = FALSE,
    verbose = TRUE,
    ...) {
  if (base::missing(chem)) {
    cli::cli_abort("The argument {.field {chem}} is required.")
  }

  check_internet(verbose = verbose)

  if (length(chem) > 1) {
    dat <- lapply(chem, extr_tetramer_,
      disease = disease,
      gene = gene,
      go = go,
      input_term_search_type = input_term_search_type,
      qt_match_type = qt_match_type,
      verify_ssl = verify_ssl,
      verbose = verbose,
      ...
    )
    out <- do.call(rbind, dat)
  } else {
    base_url <- "https://ctdbase.org/query.go"

    out <- extr_tetramer_(
      chem = chem,
      disease = disease,
      gene = gene,
      go = go,
      input_term_search_type = input_term_search_type,
      qt_match_type = qt_match_type,
      verify_ssl = verify_ssl,
      verbose = verbose,
      ...
    )
  }

  check_na_warn(dat = out, col_to_check = "gene_id", verbose = verbose)

  out
}

#' Extract Tetramer Data from the CTD API
#'
#' @inherit extr_tetramer title description params return references
#' @noRd
#' @keywords internal
extr_tetramer_ <- function(
    chem,
    disease = "",
    gene = "",
    go = "",
    input_term_search_type = "directAssociations",
    qt_match_type = "equals",
    verify_ssl = FALSE,
    verbose = verbose,
    ...) {
  # Define the base URL
  base_url <- "https://ctdbase.org/query.go"

  # Define the parameters for the request
  params <- list(
    chem = chem,
    disease = disease,
    `d-3572529-e` = "1",
    gene = gene,
    chemqt = qt_match_type,
    go = go,
    type = "tetramer",
    diseaseInputTermSearchTypeName = input_term_search_type,
    goInputTermSearchTypeName = input_term_search_type,
    diseaseqt = qt_match_type,
    action = "Search",
    chemInputTermSearchTypeName = input_term_search_type,
    goqt = qt_match_type,
    `6578706f7274` = "1",
    geneqt = qt_match_type
  )

  # Perform the request and get a response
  if (isTRUE(verbose)) {
    cli::cli_alert_info("Sending request to CTD database for tetramer data for
                        {.field {chem}}...")
  }

  libcurl_opt <- set_ssl(verify_ssl = verify_ssl, other_opt = ...)

  resp <- tryCatch(
    {
      httr2::request(base_url) |>
        httr2::req_url_query(!!!params) |>
        httr2::req_options(!!!libcurl_opt) |>
        httr2::req_perform()
    },
    error = function(e) {
      cli::cli_abort("Failed to perform the request: {conditionMessage(e)}")
    }
  )

  check_status_code(resp, verbose = verbose)

  tab_file <- tempfile(fileext = "csv")

  resp |>
    httr2::resp_body_raw() |>
    writeBin(tab_file)

  out <- utils::read.csv(tab_file) |>
    janitor::clean_names()

  unlink(tab_file)

  # if no chem is found it returns a dataframe with one single row
  if (ncol(out) == 1) {
    out <- data.frame(
      query = chem,
      chemical = NA,
      chemical_id = NA,
      gene = NA,
      gene_id = NA,
      phenotype = NA,
      phenotype_id = NA,
      disease = NA,
      disease_id = NA
    )
  } else {
    out <- cbind(query = chem, out)
  }

  out <- out[c(setdiff(names(out), "query"), "query")]

  out
}
