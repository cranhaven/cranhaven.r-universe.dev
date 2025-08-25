#' Generate a dataframe with Specified Columns and NAs
#'
#' Used internally to handle no results queries of extr_casrn_from_cid
#'
#' @param missing_chem Vector of missing chemical names.
#'
#' @return A dataframe with the specified columns and all NAs.
#' @keywords internal
#' @noRd
create_na_df <- function(missing_chem) {
  column_names <- c(
    "cid",
    "iupac_name",
    "casrn",
    "cid_all",
    "casrn_all",
    "molecular_formula",
    "molecular_weight",
    "smiles",
    "connectivity_smiles",
    "inchi",
    "inchi_key",
    "iupac_name_2",
    "x_log_p",
    "exact_mass",
    "monoisotopic_mass",
    "tpsa",
    "complexity",
    "charge",
    "h_bond_donor_count",
    "h_bond_acceptor_count",
    "rotatable_bond_count",
    "heavy_atom_count",
    "isotope_atom_count",
    "atom_stereo_count",
    "defined_atom_stereo_count",
    "undefined_atom_stereo_count",
    "bond_stereo_count",
    "defined_bond_stereo_count",
    "undefined_bond_stereo_count",
    "covalent_unit_count",
    "volume3d",
    "x_steric_quadrupole3d",
    "y_steric_quadrupole3d",
    "z_steric_quadrupole3d",
    "feature_count3d",
    "feature_acceptor_count3d",
    "feature_donor_count3d",
    "feature_anion_count3d",
    "feature_cation_count3d",
    "feature_ring_count3d",
    "feature_hydrophobe_count3d",
    "conformer_model_rmsd3d",
    "effective_rotor_count3d",
    "conformer_count3d",
    "fingerprint2d",
    "title",
    "patent_count",
    "patent_family_count",
    "literature_count",
    "annotation_types",
    "annotation_type_count",
    "source_categories",
    "query"
  )

  # Create the dataframe with all NAs
  out <- data.frame(matrix(NA,
    nrow = length(missing_chem),
    ncol = length(column_names)
  ))
  names(out) <- column_names

  out$query <- missing_chem

  out
}

#' Retrieve CASRN for PubChem CIDs
#'
#' This function retrieves the CASRN for a given set of PubChem Compound Identifiers (CID).
#' It queries PubChem through the `webchem` package and extracts the CASRN from
#' the depositor-supplied synonyms.
#'
#' @param pubchem_ids A numeric vector of PubChem CIDs. These are unique identifiers
#'   for chemical compounds in the PubChem database.
#' @param verbose A logical value indicating whether to print detailed messages.
#'   Default is TRUE.
#' @return A data frame containing the CID, CASRN, and IUPAC name of the compound.
#' The returned data frame includes three columns:
#' \describe{
#'   \item{CID}{The PubChem Compound Identifier.}
#'   \item{casrn}{The corresponding CASRN of the compound.}
#'   \item{iupac_name}{The IUPAC name of the compound.}
#'   \item{query}{The pubchem_id queried.}
#' }
#' @seealso \href{https://pubchem.ncbi.nlm.nih.gov/}{PubChem}
#' @export
#' @examples
#' \donttest{
#' # Example with formaldehyde and aflatoxin
#' cids <- c(712, 14434) # CID for formaldehyde and aflatoxin B1
#' extr_casrn_from_cid(cids)
#' }
extr_casrn_from_cid <- function(pubchem_ids, verbose = TRUE) {
  if (base::missing(pubchem_ids)) {
    cli::cli_abort("The argument {.field pubchem_ids} is required.")
  }
  check_internet(verbose = verbose)

  if (isTRUE(verbose)) {
    cli::cli_alert_info("Querying {.field pubchem_ids}.")
  }

  # casrn_data <- webchem::pc_sect(pubchem_ids, "Depositor-Supplied Synonyms")
  casrn_data <- webchem::pc_sect(pubchem_ids, "CAS")

  col_names <- c("cid", "iupac_name", "casrn", "source_name", "source_id", "query")


  if (ncol(casrn_data) == 0) {
    casrn_data <- stats::setNames(
      as.data.frame(
        matrix(ncol = length(col_names), nrow = length(pubchem_ids))
      ),
      col_names
    )
    casrn_data$query <- pubchem_ids
  } else {
    names(casrn_data) <- col_names
    casrn_data[, "query"] <- casrn_data$cid
    casrn_data$cid[is.na(casrn_data$casrn)] <- NA
  }

  check_na_warn(casrn_data, col_to_check = "cid", verbose = verbose)

  casrn_data
}


#' Query Chemical Information from IUPAC Names
#'
#' This function takes a vector of IUPAC names and queries the PubChem database
#' (using the `webchem` package) to obtain the corresponding CASRN and CID for
#' each compound. It reshapes the resulting data, ensuring that each compound
#' has a unique row with the CID, CASRN, and additional chemical properties.
#'
#' @param iupac_names A character vector of IUPAC names. These are standardized
#'   names of chemical compounds that will be used to search in the PubChem
#'   database.
#' @param verbose A logical value indicating whether to print detailed messages.
#'   Default is TRUE.
#' @param domain A character string specifying the PubChem domain to query.
#'   One of `"compound"` or `substance`. Default is `compound`.
#' @param delay A numeric value indicating the delay (in seconds) between API
#'   requests. This controls the time between successive PubChem queries.
#'   Default is 0. See Details for more info.
#' @return A data frame with phisio-chemical information on the queried
#'   compounds, including but not limited to:
#' \describe{
#'   \item{iupac_name}{The IUPAC name of the compound.}
#'   \item{cid}{The PubChem Compound Identifier (CID).}
#'   \item{isomeric_smiles}{The SMILES string (Simplified Molecular Input Line
#'       Entry System).}
#' }
#' @details
#' The function performs two queries to PubChem:
#' 1. The first query retrieves the PubChem Compound Identifier (CID) for each
#'    IUPAC name.
#' 2. The second query retrieves additional information using the
#'    obtained CIDs.
#' In cases of multiple rapid successive requests, the PubChem server may
#' deny access. Introducing a delay between requests (using the `delay`
#' parameter) can help prevent this issue.
#' @export
#' @examples
#' \donttest{
#' # Example with formaldehyde and aflatoxin
#' extr_chem_info(iupac_names = c("Formaldehyde", "Aflatoxin B1"))
#' }
extr_chem_info <- function(
    iupac_names,
    verbose = TRUE,
    domain = "compound",
    delay = 0) {
  if (base::missing(iupac_names)) {
    cli::cli_abort("The argument {.field {iupac_names}} is required.")
  }

  check_internet(verbose = verbose)

  iupac_cid <- webchem::get_cid(
    iupac_names,
    domain = domain,
    verbose = verbose
  )

  if (all(is.na(iupac_cid$cid))) {
    out <- create_na_df(missing_chem = iupac_names)
    return(out)
  }

  iupac_cid$cid <- as.numeric(iupac_cid$cid)

  # Handle missing CIDs
  if (any(is.na(iupac_cid$cid))) {
    missing_c <- iupac_cid$query[is.na(iupac_cid$cid)]
    missing_df <- create_na_df(missing_c)
  }

  iupac_cid_clean <- iupac_cid[!is.na(iupac_cid$cid), ]

  # we know that the cid exist because they came from get_cid
  cid_cas <- extr_casrn_from_cid(iupac_cid_clean$cid, verbose = verbose)
  cid_cas$query <- NULL

  # Ensure unique rows and summarize
  iupac_cid_cas_unique <- stats::aggregate(cbind(cid, casrn) ~ iupac_name, data = cid_cas, function(x) list(unique(x))) # nolint
  iupac_cid_cas_unique$cid <- sapply(iupac_cid_cas_unique$cid, function(x) x[!is.na(x)][1]) # nolint
  iupac_cid_cas_unique$casrn <- sapply(iupac_cid_cas_unique$casrn, function(x) x[!is.na(x)][1]) # nolint
  iupac_cid_cas_unique$cid_all <- sapply(iupac_cid_cas_unique$cid, paste, collapse = ", ") # nolint
  iupac_cid_cas_unique$casrn_all <- sapply(iupac_cid_cas_unique$casrn, paste, collapse = ", ") # nolint


  Sys.sleep(delay)
  all_prop <- webchem::pc_prop(iupac_cid_cas_unique$cid)
  all_prop$CID <- as.numeric(all_prop$CID)

  # get the original query value
  all_prop_query <- merge(all_prop, iupac_cid, by.x = "CID", by.y = "cid")

  out <- merge(
    iupac_cid_cas_unique,
    all_prop_query,
    by.x = "cid",
    by.y = "CID",
    all.x = TRUE
  )

  names(out) <- names(create_na_df(1))

  if (any(is.na(iupac_cid$cid))) {
    out <- rbind(missing_df, out)
  }

  check_na_warn(out, col_to_check = "cid", verbose = verbose)

  out
}


#' Extract FEMA from PubChem
#'
#' This function retrieves FEMA (Flavor and Extract Manufacturers Association)
#'   flavor profile information for a list of CAS Registry Numbers (CASRN) from
#'   the PubChem database using the `webchem` package.
#' @param casrn A vector of CAS Registry Numbers (CASRN) as atomic vectors.
#' @param verbose A logical value indicating whether to print detailed messages.
#'    Default is TRUE.
#' @param delay A numeric value indicating the delay (in seconds) between API
#'   requests. This controls the time between successive PubChem queries.
#'   Default is 0. See Details for more info.
#' @inherit extr_chem_info details
#' @return A data frame containing the FEMA flavor profile information for each
#'   CASRN. If no information is found for a particular CASRN, the output will
#'   include a row indicating this.
#' @seealso \href{https://pubchem.ncbi.nlm.nih.gov/}{PubChem}
#' @export
#' @examples
#' \donttest{
#' extr_pubchem_fema(c("83-67-0", "1490-04-6"))
#' }
extr_pubchem_fema <- function(casrn, verbose = TRUE, delay = 0) {
  extr_pubchem_section(
    casrn = casrn,
    section = "FEMA Flavor Profile",
    verbose = verbose,
    delay = delay
  )
}


#' Extract GHS Codes from PubChem
#'
#' This function extracts GHS (Globally Harmonized System) codes from PubChem.
#' It relies on the `webchem` package to interact with PubChem.
#'
#' @param casrn Character vector of CAS Registry Numbers (CASRN).
#' @param verbose A logical value indicating whether to print detailed messages.
#'    Default is TRUE.
#' @param delay A numeric value indicating the delay (in seconds) between API
#'   requests. This controls the time between successive PubChem queries.
#'   Default is 0. See Details for more info.
#' @inherit extr_chem_info details
#' @return A dataframe containing GHS information.
#' @seealso \href{https://pubchem.ncbi.nlm.nih.gov/}{PubChem}
#' @export
#' @examples
#' \donttest{
#' extr_pubchem_ghs(casrn = c("50-00-0", "64-17-5"))
#' }
extr_pubchem_ghs <- function(casrn, verbose = TRUE, delay = 0) {
  extr_pubchem_section(
    casrn = casrn,
    section = "GHS Classification",
    verbose = verbose,
    delay = delay
  )
}


#' Extract PubChem Section Data
#'
#' A generalized function to extract specific section data
#' (e.g., FEMA or GHS) from PubChem for a given CASRN.
#'
#' @param casrn A character vector of CAS Registry Numbers (CASRN).
#' @param section A character string specifying the PubChem section to query
#'    (e.g., "FEMA Flavor Profile" or "GHS Classification").
#' @param verbose A logical value indicating whether to print detailed messages.
#'    Default is TRUE.
#' @param delay A numeric value indicating the delay (in seconds) between API
#'   requests. This controls the time between successive PubChem queries.
#'   Default is 0. See Details for more info.
#' @inherit extr_chem_info details
#' @return A dataframe containing the queried section information for each
#'    CASRN.
#' @keywords internal
#' @noRd
extr_pubchem_section <- function(casrn, section, verbose = TRUE, delay = 0) {
  if (base::missing(casrn)) {
    cli::cli_abort("The argument {.field {casrn}} is required.")
  }

  check_internet(verbose = verbose)

  dat <- lapply(casrn, function(cas) {
    extr_pubchem_section_(
      casrn = cas,
      section = section,
      verbose = verbose,
      delay = delay
    )
  })

  out <- do.call(rbind, dat)
  check_na_warn(dat = out, col_to_check = "IUPAC_name", verbose = verbose)
  out
}

#' Internal Helper Function for `extr_pubchem_section`
#'
#' @param casrn A single CASRN.
#' @param section A character string specifying the PubChem section to query.
#' @param verbose A logical value indicating whether to print detailed
#'    messages. Default is TRUE.
#' @param delay A numeric value indicating the delay (in seconds) between API
#'   requests. This controls the time between successive PubChem queries.
#'   Default is 0. See Details for more info.
#' @inherit extr_chem_info details
#' @noRd
#' @keywords internal
extr_pubchem_section_ <- function(casrn, section, verbose = TRUE, delay = 0) {
  dat_cid <- webchem::get_cid(casrn, match = "first", verbose = verbose)

  col_out <- c(
    "cid",
    "casrn",
    "IUPAC_name",
    "result",
    "source_name",
    "source_id",
    "other",
    "query"
  )

  Sys.sleep(delay)

  # Handle no CID retrieved
  if (is.na(dat_cid$cid)) {
    names_casrn <- webchem::pc_sect(dat_cid$cid, "Depositor-Supplied Synonyms")
    na_matrix <- matrix(NA, nrow = 1, ncol = length(col_out))
    out_df <- as.data.frame(na_matrix)
    colnames(out_df) <- col_out
    out_df$casrn <- NA
    out_df$query <- casrn
    out_df$other <- "CASRN not found"

    return(out_df)
  }

  dat_section <- webchem::pc_sect(
    dat_cid$cid,
    section = section,
    verbose = verbose
  ) |>
    janitor::clean_names()

  # Handle empty results for section
  if (ncol(dat_section) == 0) {
    name_casrn <- webchem::pc_sect(dat_cid$cid, "Depositor-Supplied Synonyms")
    na_matrix <- matrix(NA, nrow = 1, ncol = length(col_out))
    out_df <- as.data.frame(na_matrix)
    colnames(out_df) <- col_out
    out_df$IUPAC_name <- name_casrn$Name[[1]]
    out_df$cid <- dat_cid$cid
    out_df$casrn <- casrn
    out_df$query <- casrn
    out_df$other <- paste(section, "info not found")

    if (isTRUE(verbose)) {
      cli::cli_warn("{section} for {.field {casrn}} not found!")
    }
    return(out_df)
  }

  # Clean and format the results
  out_df <- merge(dat_cid, dat_section, by = "cid")
  out_df$result <- gsub(paste0("^", section, ".*"), "", out_df$result)
  out_df[, "other"] <- NA

  names(out_df)[names(out_df) %in% c("query", "name")] <- c(
    "casrn",
    "IUPAC_name"
  )
  out_df[, "query"] <- casrn
  out_df
}
