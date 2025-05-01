#' Convenient Tool to Segment Chinese Texts
#'
#' The function first collects filenames or text vectors, then it 
#' calls \code{jiebaR::segment} to segment texts. In 
#' this process, it allows users to do additional modification. 
#' File encoding is detected automatically. 
#' After segmenting, segmented words that belong to a text will be pasted 
#' together into a single character with words split by " ".
#' The segmented result will be returned or written 
#' on the disk.
#'
#' Users should provide their jiebar cutter by \code{mycutter}. Otherwise, the function 
#' uses \code{DEFAULT_cutter} which is created when the package is loaded. 
#' The \code{DEFAULT_cutter} is simply \code{worker(write = FALSE)}. 
#' See \code{jiebaR::worker}. 
#'
#' As long as 
#' you have not manually created another variable called "DEFAULT_cutter", 
#' you can directly use \code{jiebaR::new_user_word(DEFAULT_cutter...)} 
#' to add new words. By the way, whether you manually create an object 
#' called "DEFAULT_cutter", the original loaded DEFAULT_cutter which is 
#' used by default by functions in this package will not be removed by you.
#' So, whenever you want to use this default value, either you do not set 
#' \code{mycutter}, or 
#' set it to \code{mycutter = chinese.misc::DEFAULT_cutter}.
#'
#' The encoding for writing files (if \code{folder} is not NULL) is always "UTF-8".
#'
#' @param ... names of folders, files, or the mixture of the two kinds. It can also be a character 
#' vector of text to be processed when setting \code{from} to "v", see below.
#' @param from should only be "dir" or "v". 
#' If your inputs are filenames, it should be "dir" (default), 
#' If the inputs is a character vector of texts, it should be "v". However, if it is set to "v", 
#' make sure each element of the vector is not identical to filename in your working
#' directory; if they are identical, an error will be raised. 
#' To do this check is because if they are identical, the function 
#' \code{segment} will take the input as a file to read!
#' @param folder a length 1 character indicating the folder to put the segmented text. 
#' Set it to \code{NULL} if you want the result to be a character vector rather than to be written 
#' on your disk. Otherwise, it should be a valid directory path, each segmented 
#' text will be written into a .txt/.rtf file. If the specified folder does not exist, the function 
#' will try to create it.
#'
#' @param mycutter the jiebar cutter to segment text. A default cutter is used. See Details.
#' @param enc the file encoding used to read files. If files have different encodings or you do not 
#' know their encodings, set it to "auto" (default) to let encodings be detected automatically.
#' @param myfun1 a function used to modify each text after being read by \code{scancn} 
#' and before being segmented.
#' @param myfun2 a function used to modify each text after they are segmented.
#' @param special a length 1 character or regular expression to be passed to \code{dir_or_file} 
#' to specify what pattern should be met by filenames. The default is to read all files.
#' @param ext the extension of written files. Should be "txt", "rtf" or "". If it is not one of the 
#' three, it is set to "". This is only used when your input is a text vector rather than 
#' filenames and you want to write the outcome into your disk.
#'
#' @return a character vector, each element is a segmented text, with words split by " ". 
#' If \code{folder} is a folder name, the result will be written into your disk and 
#' nothing returns.
#'
#' @import jiebaR
#' @export
#' @examples
#' require(jiebaR)
#' # No Chinese word is allowed, so we use English here.
#' x <- c("drink a bottle of milk", 
#'   "drink a cup of coffee", 
#'  "DRINK SOME WATER")
#' seg_file(x, from = "v", myfun1 = tolower)
seg_file <-
function(..., from = "dir", folder = NULL, mycutter = DEFAULT_cutter, enc = "auto", myfun1 = NULL, myfun2 = NULL, special = "", ext = "txt") {
  INFOLOCALE <- localestart2()
  on.exit(localeend2(INFOLOCALE))
  message("CHECKING ARGUMENTS")
  if (class(mycutter)[1] != "jiebar") 
    stop("Argument mycutter should be a jiebar cutter.")
  if (!from %in% c("dir", "v")) 
    stop("Argument from should be either dir or v.")
  if (!is.null(myfun1)){
    stopifnot(is.function(myfun1))
	FUN1 <- match.fun(myfun1)
  }
  if (!is.null(myfun2)){
    stopifnot(is.function(myfun2))
	FUN2 <- match.fun(myfun2)
  }
  folder <- make_valid_folder(folder, return_null = TRUE)
  if (identical(ext, "txt")){
    ext2 <- ".txt"
  } else if (identical(ext, "rtf")){
	ext2 <- ".rtf"
  } else if (identical(ext, "")){
	ext2 <- ""
  } else {
    ext2 <- ""
	message ("ext invalid, it has been set to size 0 string.")
  }	
  input <- c(...)
  if (!is_character_vector(input)) 
    stop("The input should be characters.")
  if (from == "dir") {
    message("COLLECTING FILES")
    all_f_input <- dir_or_file_self(input, special = special)
  }
  if (from == "dir" & !is.null(folder)) {
    message("SEG FILES AND WRITE FOLDER")
	all_f_input_len <- length(all_f_input)
	vlen <- nchar(as.character(all_f_input_len))
    for (fi in 1: all_f_input_len) {
	  f <- all_f_input[fi]
      the_enc <- gEtthEEnc(x1 = f, x2 = enc)
      conved <- scancn(f, enc = the_enc)
      if (!is.null(myfun1) && grepl("[^[:space:]]", conved) == TRUE) {
        conved <- FUN1(conved)
		conved <- AftEr_myfUn(conved)
      }
      conved <- paste(jiebaR::segment(conved, mycutter), collapse = " ")
      conved <- gsub("\\s+", " ", conved)
      if (!is.null(myfun2) && grepl("[^[:space:]]", conved) == TRUE) {
        conved <- FUN2(conved)
		conved <- AftEr_myfUn(conved, pa = TRUE)
      }
      basename <- gsub(".*/", "", f)
	  basename <- gsub("\\.([a-zA-Z]{2,3})$", "", basename)
	  zerofull <- paste(folder, "/", zero_paste2(fi, vlen), " ", basename, ext2, sep = "")
      write_code <- "UTF-8"
      utils::write.table(conved, zerofull, row.names = FALSE, col.names = FALSE, quote = FALSE, fileEncoding = write_code)
    }
  }
  if (from == "dir" & is.null(folder)) {
    message("SEG FILES AND RETURN VECTOR")
    returned <- rep(NA, length(all_f_input))
    for (i in 1:length(all_f_input)) {
      f <- all_f_input[i]
      the_enc <- gEtthEEnc(x1 = f, x2 = enc)
      conved <- scancn(f, enc = the_enc)
      if (!is.null(myfun1) && grepl("[^[:space:]]", conved) == TRUE) {
        conved <- FUN1(conved)
		conved <- AftEr_myfUn(conved)
      }
      conved <- paste(jiebaR::segment(conved, mycutter), collapse = " ")
      conved <- gsub("\\s+", " ", conved)
      if (!is.null(myfun2) && grepl("[^[:space:]]", conved) == TRUE) {
        conved <- FUN2(conved)
		conved <- AftEr_myfUn(conved, pa = TRUE)
      }
      returned[i] <- conved
    }
    return(returned)
  }
  if (from == "v") {
    input[is.na(input)] <- ""  
    input <- gsub("\\\\(t|r|n|)", " ", input)
	input <- gsub("\\s+$", "", input)
    if (any(file.exists(input))) 
      stop("You cannot take filename as something to parse.")
    returned <- rep(NA, length(input))
    for (i in 1:length(input)) {
      ii <- input[i]
      if (ii %in% c(NA, "NA", "?") | grepl("[^[:space:]]", ii) == FALSE) {
        message("Element ", i, " is blank or missing.")
        ii <- " "
      }
      if (!is.null(myfun1) && grepl("[^[:space:]]", ii) == TRUE) {
        ii <- FUN1(ii)
		ii <- AftEr_myfUn(ii)
      }
      ii <- paste(jiebaR::segment(ii, mycutter), collapse = " ")
      ii <- gsub("\\s+", " ", ii)
      if (!is.null(myfun2) && grepl("[^[:space:]]", ii) == TRUE) {
        ii <- FUN2(ii)
        ii <- AftEr_myfUn(ii, pa = TRUE)
      }
      returned[i] <- ii
    }
    if (is.null(folder)) {
      message("SEG VECTOR AND RETURN VECTOR")
      return(returned)
    }
    else {
      message("SEG VECTOR AND WRITE FOLDER")
      vlen <- nchar(as.character(length(returned)))
      for (i in 1:length(returned)) {
        zerofull <- paste(folder, "/", zero_paste2(i, vlen), ext2, sep = "")
        utils::write.table(returned[i], zerofull, row.names = FALSE, col.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")
      }
    }
  }
}
