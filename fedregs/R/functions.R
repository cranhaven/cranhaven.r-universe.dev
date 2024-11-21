#' cfr_part
#'
#' @title Parse the Relevant Details for CFR xml files
#' @description \code{cfr_part} returns a data_frame year, title, volume, chapters, parts, and file path for each xml file
#' @details Since we're after more refined data than a single volume, we need to figure out what chapters and parts are associated with each volume. This function parses the xml and scrapes the Table of Contents for the information held in each volume.
#'
#' @param verbose logical. Will return "helpful" messages regarding the status of the file path.
#' @param file_path file path coming from the bulk download
#'
#' @return Numeric (year, title, volume, and chapters) and characters (parts and file path).
#' @importFrom magrittr %>%
#' @keywords internal
#'

cfr_part <- function(file_path, verbose = FALSE){

  res <- xml2::read_xml(file_path, as = "parsed", encoding = "UTF-8")

  parts <- res %>%
    xml2::xml_find_all(".//FMTR/TITLEPG/PARTS") %>%
    xml2::xml_text()

  chapters <- res %>%
    xml2::xml_find_all(".//FMTR/TOC/TITLENO/CHAPTI/SUBJECT") %>%
    xml2::xml_text()

  if(length(chapters) == 0){
    chapters <- res %>%
      xml2::xml_find_all(".//TOC/TITLENO/CHAPTI/SUBJECT") %>%
      xml2::xml_text()
  }

  if(verbose) {
    message(sprintf("Pulling the chapter, part, and volume information from:\n%s.\n", file_path))
  }

  return(data.frame(year = as.numeric(gsub(".*CFR-(.*)-title(.*)-vol(.*).xml", "\\1", file_path)),
                    title = as.numeric(gsub(".*CFR-(.*)-title(.*)-vol(.*).xml", "\\2", file_path)),
                    vol = as.numeric(gsub(".*CFR-(.*)-title(.*)-vol(.*).xml", "\\3", file_path)),
                    chapters = chapters,
                    parts = parts,
                    file_path = file_path,
                    stringsAsFactors = FALSE))
}


#' numextract
#'
#' @title Extract the Part Numbers
#' @description \code{numextract} takes the part numbers from \code{cfr_part} output.
#'
#' @details Each CFR chapter has multiple parts that often span volumes. To facilitate targeting a specific part, it's necessary to evaluate which parts are in each volume (e.g., Parts 18 to 199). The CFR sometimes uses terms like "END" or "end" to denote the maximum part in each chapter. \code{numextract} simply returns the max as `Inf` in these situations.
#'
#' @param string ideally \code{cfr_part()}$parts
#' @param return min or max, default is "min"
#'
#' @return numeric value from 1 to `Inf``
#'
#' @keywords internal
#'
numextract <- function(string, return = c("min", "max")[1]){

  if(!grepl("part*", tolower(string))){
    stop("Make sure you are providing a valid 'part'.")
  }

  num_vec <- unlist(regmatches(string,
                               gregexpr("[[:digit:]]+\\.*[[:digit:]]*", string)))

  if(length(num_vec) == 0) {
    stop("Make sure string is a numeric value.")
  }

  if(length(num_vec) == 1){
    num_vec <- c(num_vec, Inf)
  }
  if(return == "min"){
    return(as.numeric(num_vec[1]))
  }
  if(return == "max"){
    return(as.numeric(max(num_vec)))
  }
}

#' cfr_text
#'
#' @title Extract the Text for a Given Year, Title, Chapter, and Part
#' @description \code{cfr_text} returns a tibble of CFR text
#' @details This function is the main function of the \code{fedregs} package. It takes the title, chapter, part, and year and returns a tibble of raw text (\code{return_tidytext = FALSE}) or \href{https://www.tidytextmining.com/tidytext.html}{tidytext} text (\code{return_tidytext = TRUE}). N.b., it has not been extensively tested on titles and chapters other than Title 50 chapter VI and part 648.
#'
#' @param year numeric between 1996 and 2018.
#' @param title_number numeric between 1 and 50.
#' @param chapter numeric or roman numeral.
#' @param part numeric.
#' @param token character. Unit for tokenizing. Currently
#' @param return_tidytext logical. TRUE = tidytext, FALSE = raw data
#' @param verbose logical. Will return "helpful" messages regarding the status of the URL.
#' @param ... Extra arguments passed on to tokenizers, such as n and k for "ngrams" and "skip_ngrams"
#'
#' @return a tibble with year, title_number, chapter, part, and text nested by subpart
#' @export
#' @importFrom magrittr %>%
#' @importFrom utils download.file unzip
#' @importFrom tidyr nest unnest
#'
#' @examples
#' \donttest{regs <- cfr_text(year = 2018,
#' title_number = 50,
#' chapter = 6,
#' part = 648,
#' return_tidytext = TRUE,
#' token = "words",
#' verbose = TRUE)
#' head(regs)}
#'

cfr_text <- function(year, title_number, chapter, part, token = "words", return_tidytext = TRUE,
                     verbose = FALSE, ...) {

  if(!year %in% seq(1996, 2018)){
    stop("Year must be between 1996 and 2018.\n")
  }

  if(!title_number %in% seq(1, 50)){
    stop("Title must be a numeric value between 1 and 50.\n")
  }

  if(!is.numeric(chapter)){
    stop("Chapter must be a numeric value, not a Roman Numeral.\n")
  }
  if(is.numeric(chapter)){
    chapter <- as.character(utils::as.roman(chapter))
  }

  if(!is.numeric(part)){
    stop("Part must be a numeric value.\n")
  }

  if(token == "ngram" & !exists("n", mode = "integer")) {
    stop("For ngram tokens, please include the 'n' argument.")
  }


  url_head <- "https://www.govinfo.gov/bulkdata/CFR"
  url_zip <- sprintf("%s/%s/title-%s/CFR-%s-title-%s.zip", url_head, year, title_number, year, title_number)

  if(httr::http_error(url_zip)){
    stop(sprintf("There aren't any regulations for title %s in %s.\n", title_number, year))
  }

  temp_dir <- tempdir()
  temp <- tempfile(tmpdir = temp_dir)
  download.file(url_zip, temp, quiet = !verbose)

  ## Now check the file to find the right volume
  unzip(temp, exdir = temp_dir)
  paths <- grep("*.xml$", list.files(temp_dir, full.names = TRUE), value = TRUE)
  paths <- grep(sprintf("*CFR-%s-title%s", year, title_number), paths, value = TRUE)

  cfr_part_df <- purrr::map_df(paths, cfr_part, verbose = verbose)

  cfr_select_part <- cfr_part_df %>%
    dplyr::mutate(min_parts = purrr::map(parts, numextract, "min"),
                  max_parts = purrr::map(parts, numextract, "max")) %>%
    dplyr::filter(grepl(sprintf("Chapter %s", chapter), chapters),
                  min_parts <= part,
                  max_parts >= part) %>%
    tidyr::unnest(cols = c("min_parts", "max_parts"))


  if(nrow(cfr_select_part) == 0){
    stop(sprintf("Part not found. Please check that part %s is available on 'https://www.govinfo.gov/bulkdata/CFR'.\n", part))
  }

  max_parts_vector <- unlist(cfr_select_part$max_parts)

  if(nrow(cfr_select_part) > 1 &
    any(!is.infinite(max_parts_vector))) {
    cfr_select_part <- cfr_select_part %>%
      dplyr::filter(!is.infinite(max_parts))
  }

  if(nrow(cfr_select_part) > 1 &
     any(is.infinite(max_parts_vector))) {
    cfr_select_part <- cfr_select_part %>%
      dplyr::filter(min_parts == max(min_parts))
  }


  cfr_xml <- cfr_select_part %>%
    dplyr::select(c(file_path)) %>%
    dplyr::pull() %>%
    xml2::read_xml(as = "text", encoding = "UTF-8")

  subpart_names <- cfr_xml %>%
    xml2::xml_find_all(sprintf(".//PART/HD[contains(text(), '%s')]/following-sibling::CONTENTS/SUBPART/HD",
                               part)) %>%
    xml2::xml_text(.) %>%
    unlist()

  section_all <- dplyr::tibble(subpart_names) %>%
    dplyr::mutate(values = purrr::map(subpart_names, function(x) xml2::xml_find_all(x = cfr_xml,
                                                              sprintf(".//PART/HD[contains(text(), '%s')]/following-sibling::CONTENTS/SUBPART/HD[contains(text(), '%s')]/following-sibling::SECTNO",
                                                                      part,
                                                                      x)) %>%
                                 xml2::xml_text(.))) %>%
    tidyr::unnest(cols = c("values"))

  cfr_subpart <- cfr_xml %>%
    xml2::xml_find_all(sprintf(".//PART/HD[contains(text(), '%s')]/following-sibling::SUBPART", part))

  section_names <- cfr_subpart %>%
    xml2::xml_find_all(".//SECTION/SUBJECT|.//SECTION/RESERVED") %>%
    xml2::xml_text() %>%
    unlist()
  section_names <- section_names[!section_names == ""]

  section_numbers <-  cfr_subpart %>%
    xml2::xml_find_all(".//SECTION/SECTNO") %>%
    xml2::xml_text() %>%
    unlist()

  # section_numbers <- cfr_subpart %>%
  #   purrr::map(~ xml2::xml_find_all(., "//SECTION/SECTNO")) %>%
  #   purrr::map(~ xml2::xml_text(.)) %>%
  #   unlist()
  #
  # section_names <- cfr_subpart %>%
  #   purrr::map(~ xml2::xml_find_all(., "//SECTION/SUBJECT|//SECTION/RESERVED")) %>%
  #   purrr::map(~ xml2::xml_text(.)) %>%
  #   unlist()

  section_text <- dplyr::tibble(SECTION_NAME = section_names,
                                SECTION_NUMBER = section_numbers,
                                values = gsub(".*\u00A7|.*\u2009|.*\\s", "", section_numbers)) %>%
    dplyr::filter(grepl(sprintf("%s[\\.]", part), SECTION_NUMBER)) %>%
    dplyr::left_join(section_all, by = "values") %>%
    dplyr::mutate(TEXT = purrr::map(SECTION_NUMBER, function(x) xml2::xml_find_all(cfr_subpart,
                                                                            sprintf(".//SECTNO[text()='%s']/following-sibling::P",
                                                                                    x)) %>%
                               xml2::xml_text(., trim = TRUE)),
           year = year,
           title_number = title_number,
           chapter = chapter,
           part = part,
           TEXT = tolower(TEXT)) %>%
    dplyr::rename(subpart = subpart_names)


    if(return_tidytext){
    out <- section_text %>%
             tidytext::unnest_tokens(word, TEXT, token, ...) %>%
             dplyr::group_by(year, title_number, chapter, part, subpart) %>%
             tidyr::nest()
  }

  if(!return_tidytext){
    out <- section_text %>%
             dplyr::group_by(year, title_number, chapter, part, subpart) %>%
             tidyr::nest()
  }
  return(out)
}
