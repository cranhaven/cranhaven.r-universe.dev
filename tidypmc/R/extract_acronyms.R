#' Find acronyms in parentheses
#'
#' This function searches for words preceding the acronym that start with the
#' same initial letter and will likely fail in many situations.
#'
#' @param txt A tibble from pmc_text or character vector
#'
#' @return A tibble with acronyms
#'
#' @author Chris Stubben
#'
#' @examples
#' txt <- c(
#'   "An acronym like multinucleated giant cell (MGC)",
#'   "is later mentioned as MGC in the paper.")
#' extract_acronyms(txt)
#' @export

extract_acronyms <- function(txt){
  ## for pmc_text output
  if (is.data.frame(txt)) txt <- txt$text
   if(!is.vector(txt)) stop("txt should be a character vector")
   n <- stringr::str_detect( txt, "\\([A-Z]{2,6}s?\\)")
   if(sum(n) == 0){
      message("No acronyms found")
      z <- NULL
   }else{
     x <- txt[n]
     ## lower case the start of sentence ???
     x <- paste0(tolower(substr(x,1,1)), substring(x,2))
     ## extract all acronyms with 2 to 6 upper case letters, optional s at end
     y <- stringr::str_extract_all(x, "\\([A-Z]{2,6}s?\\)")
     ## remove parentheses
     y <- lapply(y, function(x) gsub(")|\\(", "", x))
     ## repeat text for each acronym in sentence
     z <- tibble::tibble( acronym= unlist(y), text = rep(x, sapply(y, length)))
     # drop duplicates ??
     # z <- filter(z, !duplicated(acronym)) %>% arrange(acronym)
     # add matching pattern for first letter [Aa]
     z <- dplyr::mutate(z, name = NA,
         init = paste0("^[", substr(acronym, 1,1),
                     tolower(substr(acronym, 1,1)), "]"))
     ## Loop through acronyms
     for( i in seq_len(nrow(z))){
        acronym <- z$acronym[i]
        # split into vector of words and find last match to first letter of
        # acronym.  Use space or dash
        y <- stringr::str_split(stringr::str_extract(
               z$text[i], paste0(".*", acronym, "\\)" )), "[ -]")[[1]]
        n <- which( grepl(z$init[i], y))
        if(length(n) == 0){
           message("Cannot parse ", acronym )
        }else{
           # department of defense, start from second to last d?
           n_first <- stringr::str_count(acronym, substr(acronym, 1,1))
           last_n <- length(n)
           ## check if fewer words starting with initial letter
           if(n_first > last_n) n_first <- last_n
           if(n_first > 1 & length(n) > 1) last_n <- last_n - n_first + 1
           # get text from last matching letter to end of string (minus acronym)
           acronym_text <- paste( y[n[ last_n] : (length(y)-1)], collapse= " ")
           ## Plural
           if(grepl("s$", acronym)){
              acronym_text  <- stringr::str_replace(acronym_text , "s$", "")
              acronym <- stringr::str_replace(acronym , "s$", "")
           }
           z$acronym[i] <- acronym
           z$name[i] <- acronym_text
       }
     }
     # include acronyms without names?
     z <- dplyr::filter(z, !is.na(name)) %>%
            dplyr::select(acronym, name)
     z <- dplyr::count(z, acronym, name) %>%
            dplyr::arrange(acronym, dplyr::desc(n), dplyr::desc(name))
  }
  z
}
