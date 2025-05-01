#' Write Many Separated Files into a CSV
#'
#' Given filenames, folder names, or the mixture of the two, the function will read texts 
#' in .txt or other separated files, and then write 
#' them into one .csv file. It helps those who prefer texts in a table format.
#' 
#' Whether a file is taken as \code{NA} is judged by \code{\link{scancn}}. " " (a space) 
#' is also taken as \code{NA}. However, you 
#' can further decide what else is deemed as \code{NA}, e. g., "404 ERROR", if your texts are from 
#' websites. If a file cannot be accessed, the result to be 
#' written in the corresponding cell of csv file will become \code{NA}, and there will be a 
#' message, but no error is raised.
#' In the .csv file, full filenames of txt occupy a column and fulltexts occupy another.
#'
#' @param ... names of folders and files, obtained files may end with ".txt" or not , see below.
#' Encoding for each file is auto-detected.
#' @param csv a .csv file that will contain texts. It must end with ".csv".
#' @param must_txt should be \code{TRUE} or \code{FALSE}. Should all qualified texts 
#' end with ".txt"? If you want to read other types of file, such as .rtf, set 
#' it to \code{FALSE}. Default is \code{TRUE}.
#' @param na_in_txt character vector that specifies what content, when it occupies a single line, 
#' should be treated as \code{NA}. See Details. Length of it can be larger than 1.
#'
#' @export
#' @examples
#' \dontrun{
#' x1 <- file.path(find.package("base"), "CITATION")
#' x2 <- file.path(find.package("base"), "DESCRIPTION")
#' txt2csv(x1, x2, must_txt = FALSE, csv = 'x1x2csv.csv')
#' }
txt2csv <-
function(..., csv, must_txt = TRUE, na_in_txt = NULL) {
  infolocale <- localestart2()
  on.exit(localeend2(infolocale))
  y <- c(...)
  all_file <- dir_or_file_self(y, special = ifelse(must_txt == TRUE, "\\.txt$", ""))
  if (!grepl("\\.csv$|\\.CSV", csv)) 
    stop("csv must be the name of a csv file.")
  if (!is.null(na_in_txt) && !is_character_vector(na_in_txt)) 
    message("na_in_txt must be NULL or a character vector.")
  all_text <- rep("NA", length(all_file))
  for (i in 1:length(all_file)) {
    file_i <- all_file[i]
    text <- tryCatch(expr = {
      text <- scancn(file_i)
      if (is.null(text) || text %in% c(" ", na_in_txt)) {
        text <- "NA"
        message("Content of ", i, " is set to letters NA.")
      }
      text
    }, error = function(e) {
      message("Cannot process ", i, " , so set it to letters NA.")
      return("NA")
    })
    all_text[i] <- text
  }
  utils::write.csv(data.frame(all_file, all_text), csv)
}
