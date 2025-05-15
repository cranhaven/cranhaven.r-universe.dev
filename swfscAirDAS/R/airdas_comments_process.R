#' Process comments in AirDAS data
#' 
#' Extract miscellaneous information recorded in AirDAS data comments, i.e. comment-data
#' 
#' @param x \code{airdas_dfr} or \code{airdas_df} object, 
#'  or a data frame that can be coerced to a \code{airdas_dfr} object
#' @param ... ignored
#' @param comment.format list; default is \code{NULL}. 
#'   See the 'Using \code{comment.format}' section
#' 
#' @details Historically, project-specific or miscellaneous data have been 
#'   recorded in AirDAS comments using specific formats and character codes. 
#'   This functions identifies and extracts this data from the comment text strings. 
#'   However, different data types have different comment-data formats. 
#'   Specifically, TURTLE and PHOCOENA comment-data uses identifier codes 
#'   that each signify a certain data pattern, while other comment-data
#'   (usually that of CARETTA) uses data separated by some delimiter. 
#'   
#' @section TURTLE and PHOCOENA comment-data: 
#' 
#'   Current supported data types are: fish balls, molas, jellyfish, and crab pots. 
#'   See any of the AirDAS format PDFs (\code{\link{airdas_format_pdf}}) 
#'   for information about the specific codes and formats used to
#'   record this data. All comments are converted to lower case for processing 
#'   to avoid missing data.
#'   
#'   These different codes contain (at most):
#'   a level one descriptor (e.g. fish ball or crab pot), 
#'   a level two descriptor (e.g. size or jellyfish species), 
#'   and a value (a count or percentage). 
#'   Thus, the extracted data are returned together in this structure. 
#'   The output data frame is long data, i.e. it has one piece of information per line.
#'   For instance, if the comment is "fb1s fb1m", then the output data frame
#'   will have one line for the small fish ball and one for the medium fish ball. 
#'   See Value section for more details.
#'   
#'   Currently this function only recognizes mola data recorded using the 
#'   "m1", "m2", and "m3" codes (small, medium, and large mola, respectively). 
#'   Thus, "mola" is not recognized and processed.
#'   
#'   The following codes are used for the level two descriptors: 
#'   \tabular{lr}{
#'     \emph{Description} \tab \emph{Code}\cr
#'     Small  \tab s\cr
#'     Medium \tab m\cr
#'     Large  \tab l\cr
#'     Unknown  \tab u\cr
#'     Chrysaora  \tab c\cr
#'     Moon jelly \tab m\cr
#'     Egg yolk   \tab e\cr
#'     Other      \tab o\cr
#'   }
#'   
#' @section Using \code{comment.format}: 
#' 
#'  \code{comment.format} is a list that allows the user to specify the comment-data format. 
#'  To use this argument, data must be separated by a delimiter. 
#'  This list must contain three named elements:
#'  \itemize{
#'    \item n: A single number indicating the number of elements of data in each comment. 
#'      Must equal the length of \code{type}. 
#'      A comment must contain exactly this number of \code{sep} to be recognized as comment-data
#'    \item sep: A single string indicating the field separator string (delimiter). 
#'      Values within each comment are separated by this string. 
#'      Currently accepted values are ";" and ","
#'    \item type: A character vector of length \code{n} indicating the data type of 
#'      each data element (column). 
#'      All values must be one of: "character", "numeric", or "integer".
#'  }
#'  
#'  For instance, for most CARETTA data \code{comment.format} should be 
#'  \code{list(n = 5, sep = ";", type = c("character", "character", "numeric", "numeric", "character"))}
#'   
#' @return \code{x}, filtered for comments with recorded data, 
#'   with the following columns added: 
#'   \itemize{
#'     \item comment_str: the full comment string
#'     \item Misc#: Some number of descriptor columns. There should be \code{n} columns, 
#'       although the minimum number will be two columns
#'     \item Value: Associated count or percentage for TURTLE/PHOCOENA data
#'     \item flag_check: logical indicating if the TURTLE/PHOCOENA 
#'       comment string was longer than an expected number of characters, 
#'       and thus should be manually inspected 
#'   }
#'   
#'   See the additional sections for more context. 
#'   If \code{comment.format} is \code{NULL}, 
#'   then the output data frame would two Misc# columns: 
#'   a level one descriptor, e.g. "Fish ball" or "Jellyfish", 
#'   and a level two descriptor, e.g. s, m, or c. 
#'   However, if \code{comment.format$n} is say 4, then the output data frame would have
#'   columns Misc1, Misc2, Misc3, and Misc4.
#'   
#'   Messages are printed if either \code{comment.format} is not \code{NULL}
#'   and not comment-data is identified using \code{comment.format}, or if 
#'   \code{x} has TURTLE/PHOCOENA data but no TURTLE/PHOCOENA comment-data
#' 
#' @examples
#' y <- system.file("airdas_sample.das", package = "swfscAirDAS")
#' y.proc <- airdas_process(y)
#' 
#' airdas_comments_process(y.proc)
#' 
#' @export
airdas_comments_process <- function(x, ...) UseMethod("airdas_comments_process")


#' @name airdas_comments_process
#' @export
airdas_comments_process.data.frame <- function(x, ...) {
  airdas_comments_process(as_airdas_dfr(x, ...))
}


#' @name airdas_comments_process
#' @export
airdas_comments_process.airdas_dfr <- function(x, comment.format = NULL,...) {
  as_airdas_dfr(.airdas_comments_process(x, comment.format, ...))
}


#' @name airdas_comments_process
#' @export
airdas_comments_process.airdas_df <- function(x, comment.format = NULL, ...) {
  as_airdas_df(.airdas_comments_process(x, comment.format, ...))
}



.airdas_comments_process <- function(x, comment.format, ...) {
  #----------------------------------------------------------------------------
  # Input checks
  stopifnot(
    inherits(x, "airdas_df") | inherits(x, "airdas_dfr"), 
    all(x$file_type %in% c("turtle", "caretta", "survey", "phocoena"))
  )
  
  
  # Prep
  x.c.all <- airdas_comments(x) %>% 
    select(.data$file_das, .data$line_num, .data$file_type, .data$comment_str) %>% 
    mutate(str_lower = tolower(.data$comment_str), 
           idx = seq_along(.data$file_das))
  
  df.na <- x.c.all %>% 
    mutate(Misc1 = NA_character_, Misc2 = NA_character_, Value = NA, 
           flag_check = FALSE) %>% 
    slice(0)
  
  
  #----------------------------------------------------------------------------
  ### Extract data based on comment.format
  if (!is.null(comment.format)) {
    # comment.format input checks
    # stopifnot(
    #   "comment.format must be a list" = inherits(comment.format, "list"), 
    #   "comment.format must be of length 3" = length(comment.format) == 3, 
    #   
    #   "The names of comment.format must be 'n', 'sep', and 'type', resepctively" = 
    #     identical(names(comment.format), c("n", "sep", "type")), 
    #   
    #   "n must be a numeric (or integer) of length 1" = 
    #     length(comment.format$n) == 1 & inherits(comment.format$n, c("integer", "numeric")), 
    #   
    #   "n must be a whole number of at least 1" = 
    #     isTRUE(all.equal(comment.format$n, as.integer(comment.format$n))) & comment.format$n >= 1, 
    #   
    #   "sep must be a character of length 1" = 
    #     length(comment.format$sep) == 1 & inherits(comment.format$sep, "character"), 
    #   
    #   "In comment.format, the value of n must be equal to the length of type" = 
    #     isTRUE(all.equal(comment.format$n, length(comment.format$type))),
    #   
    #   "In comment.format, all of type must be one of 'character', 'numeric', or 'integer'" = 
    #     all(comment.format$type %in% c("character", "numeric", "integer")),   
    #   
    #   "sep must be one of ';' or ','" = comment.format$sep %in% c(";", ",")
    # )
    stopifnot(
      inherits(comment.format, "list"), 
      length(comment.format) == 3
    ) 
    
    if (!(identical(names(comment.format), c("n", "sep", "type"))))
      stop("The names of comment.format must be 'n', 'sep', and 'type', resepctively")
    if (!(length(comment.format$n) == 1 & inherits(comment.format$n, c("integer", "numeric"))))
      stop("n must be a numeric (or integer) of length 1")
    if (!(isTRUE(all.equal(comment.format$n, as.integer(comment.format$n))) & comment.format$n >= 1))
      stop("n must be a whole number of at least 1")
    if (!(length(comment.format$sep) == 1 & inherits(comment.format$sep, "character")))
      stop("sep must be a character of length 1")
    if (!(isTRUE(all.equal(comment.format$n, length(comment.format$type)))))
      stop("In comment.format, the value of n must be equal to the length of type")
    if (!(all(comment.format$type %in% c("character", "numeric", "integer"))))
      stop("In comment.format, all of type must be one of 'character', 'numeric', or 'integer'")
    if (!(comment.format$sep %in% c(";", ",")))
      stop("sep must be one of ';' or ','")
    
    
    # Process
    if (any(str_count(x.c.all$str_lower, comment.format$sep) > (comment.format$n - 1)))
      warning("Some comments have more than n of the provided sep character; ", 
              "did you provide the right values to comment.format?")
    
    x.c.ext <- x.c.all %>% 
      filter(str_count(.data$str_lower, comment.format$sep) == (comment.format$n - 1)) %>% 
      mutate(var_extract = str_split(.data$str_lower, comment.format$sep)) 
    
    if (nrow(x.c.ext) > 0) {
      x.c.ext.proc <- data.frame(mapply(function(i, j) {
        if (j == "numeric") {
          as.numeric(i)
        } else if (j == "integer") { 
          as.integer(i)
        } else {
          trimws(as.character(i))
        }
      }, i =  data.frame(t(as.data.frame(x.c.ext$var_extract))), 
      j = comment.format$type, SIMPLIFY = FALSE))
      
      names(x.c.ext.proc) <- paste0("Misc", seq_len(comment.format$n))
      
      x.c.ext <- bind_cols(x.c.ext, x.c.ext.proc) %>% 
        select(-.data$var_extract)
      
    } else {
      message("None of the data contained any comments with the format ", 
              "specified in comment.format")
      x.c.ext <- df.na
    }
    
  } else { #if comment.format is NULL
    x.c.ext <- df.na
  }
  
  
  #----------------------------------------------------------------------------
  ### Extract specific coded data from turtle and phocoena
  if (any(x.c.all$file_type %in% c("turtle", "phocoena"))) {
    # Prep
    x.c <- x.c.all %>% filter(.data$file_type %in% c("turtle", "phocoena"))
    
    #--------------------------------------------
    ### Fish balls
    x.c.fb <- x.c %>% filter(str_detect(.data$str_lower, "fb"))
    
    if (nrow(x.c.fb) > 0) {
      # Get data for 0-9 fbs
      fb.4 <- x.c.fb %>% 
        mutate(var_extract = str_match_all(.data$str_lower, "fb(..)")) %>% 
        unnest(cols = c(.data$var_extract), keep_empty = FALSE) %>% 
        mutate(var_extract = .data$var_extract[, 1], 
               Misc1 = "fish ball", 
               Misc2 = substr(.data$var_extract, 4, 4), 
               Value = suppressWarnings(as.numeric(substr(.data$var_extract, 3, 3))))
      
      # Get data for 10+ fbs
      fb.5 <- x.c.fb %>% 
        mutate(var_extract = str_match_all(.data$str_lower, "fb(...)")) %>% 
        unnest(cols = c(.data$var_extract), keep_empty = FALSE) %>% 
        mutate(var_extract = .data$var_extract[, 1], 
               Misc1 = "fish ball", 
               Misc2 = substr(.data$var_extract, 5, 5), 
               Value = suppressWarnings(as.numeric(substr(.data$var_extract, 3, 4))))
      
      fb.df <- fb.4 %>% 
        bind_rows(fb.5) %>% 
        mutate(flag_check = nchar(.data$str_lower) > 12) %>% 
        filter(!is.na(.data$Value), .data$Misc2 %in% c("s", "m", "l", "u")) %>% 
        arrange(.data$line_num)
      
    } else {
      fb.df <- df.na
    }
    
    
    #--------------------------------------------
    ### Molas
    x.c.mola <- x.c %>% 
      filter(str_detect(.data$str_lower, "m1") | str_detect(.data$str_lower, "m2") | 
               str_detect(.data$str_lower, "m3"))  #str_detect(.data$str_lower, "mola")
    
    mola.df <- if (nrow(x.c.mola) > 0) {
      x.c.mola %>% 
        mutate(str_lower = paste0(" ", .data$str_lower), #in case comment is e.g. "1 m1"
               var_extract = str_match_all(.data$str_lower, "(..) m(.)")) %>% 
        unnest(cols = c(.data$var_extract), keep_empty = FALSE) %>% 
        mutate(var_extract = .data$var_extract[, 1], 
               Misc1 = "mola", 
               Misc2 = substr(.data$var_extract, 5, 5), 
               Value = suppressWarnings(as.numeric(substr(.data$var_extract, 1, 2))), 
               flag_check = nchar(.data$str_lower) > 12) %>% 
        filter(!is.na(.data$Value), .data$Misc2 %in% c(1, 2, 3))
    } else {
      df.na
    }
    
    
    #--------------------------------------------
    ### Jellyfish
    x.c.jelly <- x.c %>% filter(str_detect(.data$str_lower, "jf"))
    
    jelly.df <- if (nrow(x.c.jelly) > 0) {
      x.c.jelly %>% 
        mutate(var_extract = str_match_all(.data$str_lower, "jf(....)")) %>% 
        unnest(cols = c(.data$var_extract), keep_empty = FALSE) %>% 
        mutate(var_extract = .data$var_extract[, 1], 
               Misc1 = "jellyfish", 
               Misc2 = substr(.data$var_extract, 3, 3), 
               Value = suppressWarnings(as.numeric(substr(.data$var_extract, 4, 6))), 
               flag_check = nchar(.data$str_lower) > 15) %>% 
        filter(!is.na(.data$Value), 
               .data$Misc2 %in% c("c", "m", "e", "o", "u"))
    } else {
      df.na
    }
    
    
    #--------------------------------------------
    ### Crab pots
    x.c.cp <- x.c %>% filter(str_detect(.data$str_lower, "cp"))
    
    cp.df <- if (nrow(x.c.cp) > 0) {
      x.c.cp %>% 
        mutate(str_lower = paste0(" ", .data$str_lower), #in case comment is e.g. "1 cp"
               var_extract = str_match_all(.data$str_lower, "(..) cp")) %>% 
        unnest(cols = c(.data$var_extract), keep_empty = FALSE) %>% 
        mutate(var_extract = .data$var_extract[, 1], 
               Misc1 = "crab pot", 
               Misc2 = NA_character_, 
               Value = suppressWarnings(as.numeric(substr(.data$var_extract, 1, 2))), 
               flag_check = nchar(.data$str_lower) > 10) %>% 
        filter(!is.na(.data$Value))
    } else {
      df.na
    }
    
    #--------------------------------------------
    # Concatenate different data type data frames
    df.proc <- bind_rows(fb.df, mola.df, jelly.df, cp.df) 
    if (nrow(df.proc) == 0) {
      message("None of the turtle/phocoena data contained any ", 
              "comment-recorded data")
    } else {
      df.proc <- df.proc %>% select(-.data$var_extract)
    }
    
    
  } else { #if there are no turtle or phocoena data in the data
    df.proc <- df.na
  }
  
  
  #----------------------------------------------------------------------------
  ### Combine, sort, and return
  df.out <- bind_rows(x.c.ext, df.proc) %>% 
    arrange(.data$idx) %>% 
    select(-.data$idx, -.data$str_lower)
  
  right_join(x, df.out, by = c("file_das", "line_num", "file_type"))
}
