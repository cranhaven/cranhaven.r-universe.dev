#' Polymorphism pattern
#' @noRd

snp_pattern <- "rs\\d{3,20}"

#' Extract polymorphism from string - helper.
#'
#' Helper function. Extract polymorphism from string.
#' @param string String. Specifies string to search for polymorphisms.
#' @param pattern Regex pattern. Specifies which pattern to use to look for
#' polymorphisms.
#'
#' @importFrom purrr is_empty
#'
#' @noRd
retrieve_polymorphism <- function(string,
                                  pattern = snp_pattern) {
  polymorph <- stringr::str_extract_all(stringr::str_to_lower(string),
                                        stringr::str_to_lower(pattern)) %>%
    unlist() %>%
    unique()


  polymorph <- polymorph[!is.na(polymorph)]

  if (!is_empty(polymorph)) {
    return (polymorph)
  } else {
    return(NA)
  }
}


#' Extract SNPs from abstracts in data frame
#'
#' Extract SNPs from abstracts in a data frame.
#'
#' Extract SNPs from abstracts in a data frame. SNPs are added to the data
#' frame in a separate column. Furthermore, an optional column can indicate if
#' SNPs are generally present in an abstract.
#'
#' @param df Data frame containing abstracts.
#' @param pattern String. Regex pattern to identify SNPs.
#' @param col.abstract Symbol. Column containing abstracts.
#' @param indicate Boolean. If `indicate = TRUE`, add another column called
#' "SNP_present", verbally indicating if a SNP is present in an abstract.
#' @param discard Boolean. If `discard = TRUE`, only abstracts containing a
#' SNP are kept.
#'
#' @return Data frame. If `discard = FALSE`, return the data frame with
#' an additional column for SNPs.
#' If `discard = TRUE`, return only abstracts containing SNPs.
#'
#' @seealso [count_snp()],
#' [get_snp()],
#' [subset_snp()]
#' @family extract functions
#'
#' @importFrom magrittr %>%
#'
#' @export
extract_snp <- function(df,
                                 pattern = snp_pattern,
                                 col.abstract = Abstract,
                                 indicate = FALSE,
                                 discard = FALSE) {
  # Probably better to keep the SNPs in a list rather than unnest that column.
  # If the column was unnested, it would distort the miRNA count as multiple SNPs
  # might be reported with multiple miRNAs in an abstract.
  # When counting SNPs, this column needs to be unnested, though.
  df_polymorphism <- df %>%
    dplyr::mutate(SNPs = purrr::map({{col.abstract}}, ~ retrieve_polymorphism(string = .x,
                                                                            pattern = snp_pattern)))

  if (indicate == TRUE) {
    df_polymorphism <- df_polymorphism %>%
      dplyr::mutate(SNP_present = ifelse(!is.na(SNPs), "Yes", "No"))
  }

  if (discard == TRUE) {
    df_polymorphism <- df_polymorphism %>%
      dplyr::filter(!is.na(SNPs))

    return(df_polymorphism)
  } else {
    return(df_polymorphism)
  }
}

#' Count SNPs in a data frame
#'
#' Count occurrence of SNPs in a data frame.
#'
#' Count occurrence of SNPs in  a data frame. The count of SNPs
#' is returned as a separate data frame, only listing the SNPs
#' and their respective frequency.
#'
#' @param df Data frame containing SNPs and PubMed IDs.
#' @param col.snp Symbol. Column containing SNPs.
#' @param col.pmid Symbol. Column containing PubMed IDs.
#'
#' @return Data frame. Data frame containing SNPs and their
#' respective frequency.
#'
#' @seealso [extract_snp()],
#' [get_snp()],
#' [subset_snp()]
#'
#' @family count functions
#'
#' @importFrom dplyr desc
#'
#' @export
count_snp <- function(df,
                      col.snp = SNPs,
                      col.pmid = PMID) {
  df_count <- df %>%
    dplyr::distinct({{col.pmid}}, .keep_all = TRUE) %>%
    tidyr::unnest({{col.snp}}) %>%
    dplyr::add_count({{col.snp}}) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::rename("Mentioned_n" = n) %>%
    dplyr::select({{col.snp}}, Mentioned_n) %>%
    dplyr::distinct()
  return(df_count)
}

#' Subset data frame for specific SNPs
#'
#' Subset data frame for specific SNPs only.
#'
#' Subset data frame for specific SNPs only.
#'
#' @param df Data frame containing SNPs.
#' @param snp.retain Character vector. Vector specifying which SNPs to keep.
#' SNPs in `snp.retain` must match SNPs in `col.snp` in `df`.
#' @param col.snp Symbol. Column containing SNPs.
#'
#' @return Data frame containing only specified SNPs.
#' If no SNP in `snp.retain` matches a SNP in `col.snp`, `subset_snp()` stops
#' with a warning saying *"No SNP in 'snp.retain' matches a SNP in 'col.snp'.
#' Could not filter for SNP."*.
#'
#' @seealso [extract_snp()],
#' [count_snp()],
#' [get_snp()]
#'
#' @family subset functions
#'
#' @export
subset_snp <- function(df,
                       snp.retain,
                       col.snp = SNPs) {
  # Etracts all miRNA names in df
  vec_snp <- df %>%
    dplyr::select({{col.snp}}) %>%
    dplyr::pull() %>%
    unique()
  # Test if miRNA can be extracted
  no_shared_snp <- get_shared_mir_vec(vec_snp, snp.retain) %>%
    length()

  if(no_shared_snp == 0) {
    stop("No SNP in 'snp.retain' matches a SNP in 'col.snp'.
            Could not filter for SNP.")
  }

  df <- df %>%
    tidyr::unnest({{col.snp}}) %>%
    dplyr::filter({{col.snp}} %in% snp.retain)

  return(df)
}


#' Get SNPs from a data frame
#'
#' Get SNPs from a data frame.
#'
#' Get SNPs from a data frame.
#'
#' * If a data frame containing SNP counts as from [count_snp()] is provided,
#' these SNPs are specified by the row they are listed in. To get the SNPs by
#' row, set the `row` argument.
#' * If a data frame with PubMed IDs is provided, these SNPs are specified by
#' their top occurrence. To get the SNPs by frequency, set the `top` argument.
#'
#' If neither `row` nor `top` is provided, `row` is automatically set to `1`.
#'
#' @param df Data frame containing SNPs. If `top` is set, `df` must also
#' contain PubMed IDs.
#' @param row Integer. Optional. Specifies row from which SNP shall be obtained. Works best
#' with a data frame listing counts only as from [count_snp()].
#' If neither `row` nor `top` is given, `row` is automatically set to `1`.
#' @param top Integer. Optional. Specifies number of most frequent SNPs to return.
#' @param col.snp Symbol. Column containing SNPs.
#' @param col.pmid Symbol. Column containing PubMed IDs. Necessary if the data
#' frame provided is not a count data frame.
#'
#' @return String or character vector containing SNPs.
#'
#' @seealso [extract_snp()],
#' [count_snp()],
#' [subset_snp()]
#'
#' @family get functions
#'
#' @export
get_snp <- function(df,
                    row = NULL,
                    top = NULL,
                    col.snp = SNPs,
                    col.pmid = PMID) {

  if(is.null(row) & is.null(top)) {
    row <- 1
  }

  # check that top and row are not null at the same time
  if(!is.null(top) & !is.null(row)) {
    stop("'row' and 'top' cannot be set simultaneously. Please set either
         'top' or 'row'.")
  }

  # get snp in row number 'row' if row is set
  if(!is.null(row)) {
    snp_row <- df %>%
      dplyr::select({{col.snp}}) %>%
      dplyr::pull()

    snp_row <- snp_row[row]
    return(snp_row)
  }

  # get top snp if top is set
  # PubMed ID is necessary here as the SNPs are often extracted after miRs have
  # been extracted. If miRs are extracted and then SNPs and one would unnest
  # the SNP column, the SNPs would explode per abstract if more than one miRNA
  # was extracted from an abstract. This would distort the count. PubMed IDs
  # are therefore necessary to objectively count the number of SNPs per abstract.
  if(!is.null(top)) {
    top_snps <- df %>%
      dplyr::distinct({{col.pmid}}, .keep_all = TRUE) %>%
      tidyr::unnest({{col.snp}}) %>%
      dplyr::add_count({{col.snp}}) %>%
      dplyr::select({{col.snp}}, n) %>%
      dplyr::distinct() %>%
      dplyr::top_n(top, n) %>%
      dplyr::select({{col.snp}}) %>%
      dplyr::pull()

    return(top_snps)
  }
}
