#' Convert JATS-file from PubMed into a data frame
#'
#' Convert JATS-file from PubMed into a data frame.
#'
#' Converts an JATS-file from PubMed into a data
#' frame.
#' The JATS-file should contain PubMed-IDs, abstracts from research articles,
#' abstract title, publication year, abstract language, and article type.
#' The data frame created holds at least six columns, namely
#'
#' * `PMID`, containing the PubMed-ID,
#' * `Year`, containing the publication year,
#' * `Title`, containing the title of the abstracts,
#' * `Abstract`, containing the actual abstract,
#' * `Language`, containing the language(s) of the paper,
#' * `Type`, containing the article type.
#'
#' If `topic` is provided, a "Topic" column is added, assigning all abstracts in
#' `df` to `topic`.
#'
#' `read_pubmed()` is faster than `read_pubmed_jats()` and thus
#' recommended.
#'
#' @param jats_file JATS-file, downloaded from PubMed.
#' @param topic String. Optional. If provided, adds a "Topic" column containing
#' `topic`.
#'
#' @return Data frame containing PubMed-IDs, abstracts, abstract titles,
#' publication years, languages, and article types.
#'
#' @seealso [read_pubmed()]
#'
#' @family external data functions
#'
#' @importFrom magrittr %>%
#'
#' @export
read_pubmed_jats <- function(jats_file, topic = NULL) {
  #Read in jats-file
  mirna_file <- xml2::read_xml(jats_file)
  #Find Pubmed articles
  node_articles <- xml2::xml_find_all(mirna_file, "//PubmedArticle")
  #Create empty df
  df <- data.frame("PMID" = integer(),
                   "Year" = integer(),
                   "Title" = character(),
                   "Abstract" = character(),
                   "Language" = character(),
                   "Type" = character(),
                   stringsAsFactors = FALSE)

  df <- dplyr::as_tibble(df)
  #Extract PMID, Year, Title, Abstract, Language
  for (i in sequence(length(node_articles))) {
    node_abstr <- node_articles[i]
    pmid <- xml2::xml_find_all(node_abstr, ".//PMID") %>%
      xml2::xml_text() %>%
      as.numeric()
    year <- xml2::xml_find_first(node_abstr, ".//Year") %>%
      xml2::xml_text() %>%
      as.numeric()
    title <- xml2::xml_find_all(node_abstr, ".//ArticleTitle") %>%
      xml2::xml_text()
    abstr <- xml2::xml_find_all(node_abstr, ".//Abstract") %>%
      xml2::xml_text()
    lang <- xml2::xml_find_all(node_abstr, ".//Language") %>%
      xml2::xml_text()
    type <- xml2::xml_find_all(node_abstr, ".//PublicationType") %>%
      xml2::xml_text()
    df[i, "PMID"] <- pmid[1]
    df[i, "Year"] <- year[1]
    df[i, "Title"] <- title[1]
    df[i, "Abstract"] <- abstr[1]
    df[i, "Language"][[1]] <- list(lang)
    df[i, "Type"][[1]] <- list(type)
  }

  df$Topic <- topic

  #Return df
  return(df)
}

#' Convert PubMed-file from PubMed into a data frame
#'
#' Convert PubMed-file from PubMed into a data frame.
#'
#' Convert an PubMed-file from PubMed into a data
#' frame.
#' The PubMed-file should contain PubMed-IDs, abstracts from research articles,
#' abstract title, publication year, abstract language, and article type.
#' The data frame created holds at least six columns, namely
#'
#' * `PMID`, containing the PubMed-ID,
#' * `Year`, containing the publication year,
#' * `Title`, containing the title of the abstracts,
#' * `Abstract`, containing the actual abstract,
#' * `Language`, containing the language(s) of the paper,
#' * `Type`, containing the article type.
#'
#' If `topic` is provided, a "Topic" column is added, assigning all abstracts in
#' `df` to `topic`.
#'
#' `read_pubmed()` is faster than `read_pubmed_jats()` and thus
#' recommended.
#'
#' @param pubmed_file PubMed-file as .txt, downloaded from PubMed.
#' @param topic String. Optional. If provided, adds a "Topic" column containing
#' `topic`.
#'
#'
#' @return Data frame containing PubMed-IDs, abstracts, abstract titles,
#' publication years, languages, and article types.
#'
#' @seealso [read_pubmed_jats()]
#'
#' @family external data functions
#'
#' @importFrom dplyr mutate
#'
#' @export
read_pubmed <- function(pubmed_file, topic = NULL) {

  . = NULL

  # Read in .txt file
  df <- suppressMessages(readr::read_fwf(pubmed_file))

  # Clean Term column
  # Replace "" with NA and fill up
  df <- df %>%
    dplyr::rename("Term" = 1,
                  "Content" = 2) %>%
    mutate(Term = stringr::str_replace_all(Term, "-", ""),
           Term = stringr::str_trim(Term),
           Term = ifelse(Term == "", NA, Term),
           Term = zoo::na.locf(Term))

  # Filter for terms of interest
  terms_interest <- c("PMID", "DP", "TI", "AB", "LA", "PT")
  df <- dplyr::filter(df, Term %in% terms_interest)

  # Enumerate abstracts
  indices_pubmed <- which(df$Term == "PMID")
  abstract_names <- stringr::str_c("Abstract_", seq_along(indices_pubmed))
  df[indices_pubmed, "Abstract_no"] <- abstract_names

  # Fill abstract enumeration
  # Spread data frame
  # Combine character vectors abstracts and titles
  # Clean data frame
  df <- df %>%
    mutate(Abstract_no = zoo::na.locf(Abstract_no)) %>%
    {suppressWarnings(tidyr::pivot_wider(., names_from = Term,
                                  values_from = Content))} %>%
    mutate(AB = purrr::map(AB, ~ stringr::str_c(.x, collapse = " "))) %>%
    mutate(TI = purrr::map(TI, ~ stringr::str_c(.x, collapse = " "))) %>%
    mutate(LA = purrr::map(LA, ~ stringr::str_c(.x, collapse = ""))) %>%
    {suppressWarnings(tidyr::unnest(., PMID, DP, AB, TI, LA, keep_empty = TRUE))} %>%
    mutate(Year = substr(DP, 1, 4)) %>%
    dplyr::select(PMID, Year, TI, AB, LA, PT) %>%
    mutate(PT = purrr::map(PT,~ stringr::str_c(.x))) %>%
    dplyr::rename(Title = TI,
           Abstract = AB,
           Language = LA,
           Type = PT) %>%
    {suppressWarnings(mutate(.,PMID = as.double(PMID),
           Year = as.double(Year)))}

  # Add topic column
  df$Topic <- topic

  return(df)
}

#' Save data frame(s) as xlsx-file
#'
#' Save data frame(s) locally as an xlsx-file.
#'
#' Saves data frame locally as an xlsx-file. If more than
#' one data frame is provided, data frames are saved
#' in an xlsx-file with one sheet per data frame.
#'
#' Wrapper function of `write.xlsx()` from \pkg{openxlsx}.
#'
#' @param ... Data frame(s) to save.
#' @param excel_file String. File name that `...` shall
#' be saved to. Must end in ".xlsx".
#'
#' @return xlsx-file, locally saved.
#'
#' @seealso [openxlsx::write.xlsx()]
#'
#' @family external data functions
#'
#' @export
save_excel <- function(...,
                       excel_file = "miRetrieve_data.xlsx") {
  # Save as excel-file
  openxlsx::write.xlsx(x = list(...),
                       file = excel_file)
}

#' Save the last generated figure
#'
#' Save the last generated figure locally.
#'
#' Saves the last generated figure locally. Wrapper
#' function of `ggsave()` from \pkg{ggplot2}. For further details, please
#' see ?ggplot2::ggsave.
#'
#' @param plot_file String. File name that the figure
#' shall be saved to. Can end in either ".png", ".tiff",
#' ".pdf", ".jpeg", or ".bmp". For more information, see the documentation
#' of [ggplot2::ggsave()].
#' @param width Integer. Optional. Plot width. If `width = NULL`, `width` is
#' set to the width of the plotting window.
#' @param height Integer. Optional. Plot height If `height = NULL`, `height` is
#' set to the height of the plotting window.
#' @param units String. Units for `width` and `height`.
#' @param dpi Integer. Resolution for raster graphics such as .pdf-files.
#' @param device String or function. Specifies which device to use (such as
#' "pdf" or `cairo_pdf`)
#'
#' @return Plot, locally saved.
#'
#' @importFrom graphics par
#'
#' @seealso [ggplot2::ggsave()]
#'
#' @family external data functions
#'
#' @export
save_plot <- function(plot_file,
                      width = NULL,
                      height = NULL,
                      units = "in",
                      dpi = 300,
                      device = NULL) {
  if(is.null(width)) {
    width <- par("din")[1]
  }

  if(is.null(height)) {
    height <- par("din")[2]
  }

  ggsave(filename = plot_file,
         width = width,
         height = height,
         units = units,
         dpi = dpi,
         device = device)
}
