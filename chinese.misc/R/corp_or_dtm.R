#' Create Corpus or Document Term Matrix with 1 Line
#'
#' This function allows you to input a vector of characters, or a mixture of files and folders, it 
#' will automatically detect file encodings, segment Chinese texts, 
#' do specified modification, 
#' remove stop words,  and then generate corpus or dtm (tdm). Since \pkg{tm} 
#' does not support Chinese well, this function manages to solve some problems. See Details.
#'
#' Package \pkg{tm} sometimes
#' tries to segment an already segmented Chinese Corpus and put together terms that 
#' should not be put together. The function is to deal with the problem.
#' It calls \code{\link{scancn}} to read files and 
#' auto-detect file encodings, 
#' and calls \code{jiebaR::segment} to segment Chinese text, and finally 
#' calls \code{tm::Corpus} to generate corpus.
#' When creating DTM/TDM, it 
#' partially depends on \code{tm::DocumentTermMatrix} 
#' and \code{tm::TermDocumentMatrix}, but also has some significant
#' differences in setting control argument. 
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
#' So, whenever you want to use this default value, you do not need to set 
#' \code{mycutter} and keep it as default.
#'
#' The argument \code{control} is very similar to the argument used by 
#' \code{tm::DocumentTermMatrix}, but is quite different and will not be passed
#' to it! The permitted elements are below:
#'
#' \itemize{
#'   \item (1) wordLengths: length 2 positive integer vector. 0 and \code{inf}
#' is not allowed. If you only want words of 4 to 10, then set it to c(4, 10).
#' If you do not want to limit the ceiling value, just choose a large value, 
#' e.g., c(4, 100).
#' In package tm (>= 0.7), 1 Chinese character is roughly
#' of length 2 (but not always computed by multiplying 2), 
#' so if a Chinese words is of 4 characters, the min value 
#' of wordLengths is 8. But here in \code{corp_or_dtm}, word length is exactly
#' the same as what you see on the screen. So, a Chinese word with 4 characters is
#' of length 4 rather than 8.
#'   \item (2) dictionary: a character vetcor of the words which will appear in DTM/TDM 
#' when you do not want a full one. If none of the words in the dictionary appears in 
#' corpus, a blank DTM/TDM will be created. The vector should not contain 
#' \code{NA}, if it does, only non-NA elements will be kept. Make sure at least 1
#' element is not \code{NA}. Note: if both dictionary and wordLengths appear in 
#' your control list, wordLengths will be ignored.
#'   \item (3) bounds: an integer vector of length 2 which limits the term frequency
#' of words. Only words whose total frequencies are in this range will appear in 
#' the DTM/TDM. 0 and \code{inf} is not allowed. Let a large enough value to 
#' indicate the unlimited ceiling.
#'   \item (4) have: an integer vector of length 2 which limits the time a word 
#' appears in the corpus. Suppose a word appears 3 times in the 1st article and 2 
#' times in the 2nd article, and 0 in the 3rd, 
#' then its bounds value = 3 + 2 + 0 = 5; but its have 
#' value = 1 + 1 + 0 = 2.
#'   \item (5) weighting: a function to compute word weights. The default is to 
#' compute term frequency. But you can use other weighting functions, typically
#' \code{tm::weightBin} or \code{tm::weightTfIdf}.
#'   \item (6) tokenizer: this value is temporarily deprecated and  
#' it cannot be modified by users. 
#' }
#' 
#' By default, the argument \code{control} is set 
#' to "auto", "auto1", or \code{DEFAULT_control1}, 
#' which are the same. This control list is created 
#' when the package is loaded. It is simply \code{list(wordLengths = c(1, 25))}, 
#' Alternatively, \code{DEFAULT_control2} (or "auto2") is also created 
#' when loading package, which sets 
#' word length to 2 to 25. 
#'
#' @param ... names of folders, files, or the mixture of the two kinds. It can also be a character 
#' vector of texts to be processed when setting \code{from} to "v", see below.
#' @param from should be "dir" or "v". If your inputs are filenames, it should be "dir" (default), 
#' If the input is a character vector of texts, it should be "v". However, if it is set to "v", 
#' make sure each element is not identical to filename in your working
#' directory; and, if they are identical, the function will raise an error. To do this check is 
#' because if they are identical, \code{jiebaR::segment} 
#' will take the input as a file to read! 
#' @param type what do you want for result. It is case insensitive, thus those start with 
#' "c" or "C" represent a corpus 
#' result; and those start with "d" or "D" for document term matrix, 
#' and those start with "t" or "T" for term document matrix. 
#' Input other than the above represents 
#' a corpus result. The default value is "corpus".
#' @param enc a length 1 character specifying encoding when reading files. If your files 
#' may have different encodings, or you do not know their encodings, 
#' set it to "auto" (default) 
#' to let the function auto-detect encoding for each file.
#' @param mycutter the jiebar cutter to segment text. A default cutter is used. See Details.
#' @param stop_word a character vector to specify stop words that should be removed. 
#' If it is \code{NULL}, nothing is removed. If it is "jiebar", "jiebaR" or "auto", the stop words used by 
#' \pkg{jiebaR} are used, see \code{\link{make_stoplist}}.
#' Please note the default value is \code{NULL}. Texts are transformed to lower case before 
#' removing stop words, so your stop words only need to contain lower case characters.
#' @param stop_pattern vector of regular expressions. These patterns are similar to stop words. 
#' Terms that match the patterns will be removed.
#' Note: the function will automatically adds "^" and "$" to the pattern, which means 
#' first, the pattern you provide should not contain these two; second, the matching
#' is complete matching. That is  to say, if a word is to be removed, it not just
#' contains the pattern (which is to be checked by \code{grepl}, but the whole
#' word match the pattern.
#' @param control a named list similar to that 
#' which is used by \code{DocumentTermMatrix} 
#' or \code{TermDocumentMatrix} to create dtm or tdm. But 
#' there are some significant differences. 
#' Most of the time you do not need to 
#' set this value because a default value is used. When you set the argument to \code{NULL}, 
#' it still points to this default value. See Details.
#' @param myfun1 a function used to modify each text after being read by \code{scancn} 
#' and before being segmented. 
#' @param myfun2 a function used to modify each text after they are segmented.
#' @param special a length 1 character or regular expression to be passed to \code{dir_or_file} 
#' to specify what pattern should be met by filenames. The default is to read all files.
#' See \code{\link{dir_or_file}}.
#' @param use_stri_replace_all default is FALSE. If it is TRUE, 
#' \code{stringi::stri_replace_all} is used to delete stop words, which has 
#' a slightly higher speed. This is still experimental.
#'
#' @return a corpus, or document term matrix, or term document matrix. 
#'
#' @export
#' @import tm
#' @import NLP
#' @examples
#' x <- c(
#'   "Hello, what do you want to drink?", 
#'   "drink a bottle of milk", 
#'   "drink a cup of coffee", 
#'   "drink some water")
#' # The simplest argument setting
#' dtm <- corp_or_dtm(x, from = "v", type = "dtm")
#' # Modify argument control to see what happens
#' dtm <- corp_or_dtm(x, from = "v", type="d", control = list(wordLengths = c(3, 20)))
#' tdm <- corp_or_dtm(x, from = "v", type = "T", stop_word = c("you", "to", "a", "of"))
corp_or_dtm <-
function(..., from = "dir", type = "corpus", enc = "auto", mycutter = DEFAULT_cutter, stop_word = NULL, stop_pattern = NULL, control = "auto",
  myfun1 = NULL, myfun2 = NULL, special = "", use_stri_replace_all=FALSE) {
  INFOLOCALE <- localestart2()
  on.exit(localeend2(INFOLOCALE))
  message("CHECKING ARGUMENTS")
  if (!is.null(stop_word)) {
    stop_word <- as.character2(stop_word)
    stop_word <- stop_word[!is.na(stop_word)]
    if (length(stop_word) == 0) 
      stop_word <- NULL
  }
  if (!is.null(myfun1)){
    stopifnot(is.function(myfun1))
	FUN1 <- match.fun(myfun1)
  }
  if (!is.null(myfun2)){
    stopifnot(is.function(myfun2))
	FUN2 <- match.fun(myfun2)
  }
  if (!is.null(stop_pattern)) 
    stopifnot(all(!is.na(stop_pattern)))
  if (is.null(type)) 
    stop("Argument type should not be NULL.")
  type <- as.character(type[1])
  if (grepl("^d|^D|^t|^T", type)){
    control <- chEck_cOntrOl(control)
  }
  input <- c(...)
  if (!is_character_vector(input))
    stop("Your input should be characters.")
  input[is.na(input)] <- ""
  if (from == "dir") {
    message("PROCESSING FILE NAMES")
    fullname <- dir_or_file_self(input, special = special)
    seged_vec <- rep(NA, length(fullname))
    message("READING AND PROCESSING FILES")
    for (i in 1:length(fullname)) {
      fi <- fullname[i]
      the_enc <- gEtthEEnc(x1 = fi, x2 = enc)
      conved <- scancn(fi, enc = the_enc)
      if (!is.null(myfun1) && grepl("[^[:space:]]", conved) == TRUE) {
        conved <- AftEr_myfUn(FUN1(conved))
      }	  
      if (!is.null(mycutter)) {
        conved <- gsub("\\s+", " ", paste0(jiebaR::segment(conved, jiebar = mycutter), collapse = " "))
      }
      if (!is.null(myfun2) && grepl("[^[:space:]]", conved) == TRUE) {
        conved <- AftEr_myfUn(FUN2(conved), pa = TRUE)
      }
      seged_vec[i] <- conved
    }
	rm(input)
    message("CREATING CORPUS")
    corp <- tm::Corpus(tm::VectorSource(seged_vec))
    rm(seged_vec)
  }
  if (from == "v") {
    input <- gsub("\\\\(t|r|n|)", " ", input)
	input <- gsub("\\s+$", "", input)
    if (any(file.exists(input))) {
      stop("Some strings are identical to filenames in working directory, please make some changes.")
    }
    message("PROCESSING CHARACTER VECTOR")
	length_of_v_input <- length(input)
    seged_vec <- rep(NA, length_of_v_input)
    for (i in 1:length(input)) {
      ii <- input[i]
      if (ii %in% c(NA, "NA", "?") | grepl("[^[:space:]]", ii) == FALSE) {
        message("Element ", i, " may be blank or missing, set it to a space.")
        ii <- " "
      }
      if (is.function(myfun1) && grepl("[^[:space:]]", ii) == TRUE) {
        ii <- AftEr_myfUn(FUN1(ii))
	  }
      if (!is.null(mycutter)) {
        ii <- gsub("\\s+", " ", paste(jiebaR::segment(ii, mycutter), collapse = " "))
      }
      if (is.function(myfun2) && grepl("[^[:space:]]", ii) == TRUE) {
        ii <- AftEr_myfUn(FUN2(ii), pa = TRUE)
      }
      seged_vec[i] <- ii
    }
	rm(input)
    message("GENERATING CORPUS")
    corp <- tm::Corpus(tm::VectorSource(seged_vec))
    rm(seged_vec)
  }
  message("PROCESSING CORPUS")
  corp <- suppressWarnings(tm::tm_map(corp, tm::removePunctuation))
  corp <- suppressWarnings(tm::tm_map(corp, tm::removeNumbers))
  corp <- suppressWarnings(tm::tm_map(corp, tm::content_transformer(tolower)))
  
  # check whether to use stri_replace_all
  REMOVEFUN <- if (use_stri_replace_all==FALSE) tm::removeWords else STRI_REPLACE
   
  if (!is.null(stop_word)) {
    if (stop_word[1] %in% c("jiebar", "auto", "jiebaR")) {
      corp <- suppressWarnings(tm::tm_map(corp, REMOVEFUN, c(find_jiebar_stop())))
    }
    else {
      corp <- suppressWarnings(tm::tm_map(corp, REMOVEFUN, c(stop_word)))
    }
  }
  if (!is.null(stop_pattern)) {
    stop_pattern <- as.character2(stop_pattern)
    stop_pattern <- paste(stop_pattern, collapse = "|")
    corp <- suppressWarnings(tm::tm_map(corp, tm::removeWords, c(stop_pattern)))
  }
  corp <- suppressWarnings(tm::tm_map(corp, tm::stripWhitespace))
  if (grepl("^c|^C", type)) {
    message("DONE")
    return(corp)
  }
  else if (grepl("^d|^D|^t|^T", type)) {
    message("MAKING DTM/TDM")
    todtm=ifelse (grepl("^d|^D", type), TRUE, FALSE)
    DTMname <- rE_dtm(corp, todtm = todtm, re_control = control)
	rm(corp)
	if (from == "dir"){
		DTMname$dimnames$Docs <- fullname
		rm(fullname)
	}
	if (from == "v"){
		DTMname$dimnames$Docs <- as.character(1: length_of_v_input)
	}	
    message("DONE")
	return(DTMname)
  }
  else {
    message("You do not specify a valid type, so return corpus.")
	message("DONE")
    return(corp)
  }
}

corp_or_dtm_new <-
function(..., from = "dir", type = "corpus", enc = "auto", mycutter = DEFAULT_cutter, stop_word = NULL, stop_pattern = NULL, control = "auto",
  myfun1 = NULL, myfun2 = NULL, special = "") {
  INFOLOCALE <- localestart2()
  on.exit(localeend2(INFOLOCALE))
  message("CHECKING ARGUMENTS")
  if (!is.null(stop_word)) {
    stop_word <- as.character2(stop_word)
    stop_word <- stop_word[!is.na(stop_word)]
    if (length(stop_word) == 0) 
      stop_word <- NULL
  }
  if (!is.null(myfun1)){
    stopifnot(is.function(myfun1))
	FUN1 <- match.fun(myfun1)
  }
  if (!is.null(myfun2)){
    stopifnot(is.function(myfun2))
	FUN2 <- match.fun(myfun2)
  }
  if (!is.null(stop_pattern)) 
    stopifnot(all(!is.na(stop_pattern)))
  if (is.null(type)) 
    stop("Argument type should not be NULL.")
  type <- as.character(type[1])
  if (grepl("^d|^D|^t|^T", type)){
    control <- chEck_cOntrOl(control)
  }
  input <- c(...)
  if (!is_character_vector(input))
    stop("Your input should be characters.")
  input[is.na(input)] <- ""
  if (from == "dir") {
    message("PROCESSING FILE NAMES")
    fullname <- dir_or_file_self(input, special = special)
    seged_vec <- rep(NA, length(fullname))
    message("READING AND PROCESSING FILES")
    for (i in 1:length(fullname)) {
      fi <- fullname[i]
      the_enc <- gEtthEEnc(x1 = fi, x2 = enc)
      conved <- scancn(fi, enc = the_enc)
      if (!is.null(myfun1) && grepl("[^[:space:]]", conved) == TRUE) {
        conved <- AftEr_myfUn(FUN1(conved))
      }	  
      if (!is.null(mycutter)) {
        conved <- gsub("\\s+", " ", paste0(jiebaR::segment(conved, jiebar = mycutter), collapse = " "))
      }
      if (!is.null(myfun2) && grepl("[^[:space:]]", conved) == TRUE) {
        conved <- AftEr_myfUn(FUN2(conved), pa = TRUE)
      }
      seged_vec[i] <- conved
    }
	rm(input)
    message("CREATING CORPUS")
    corp <- tm::Corpus(tm::VectorSource(seged_vec))
    rm(seged_vec)
  }
  if (from == "v") {
    input <- gsub("\\\\(t|r|n|)", " ", input)
	input <- gsub("\\s+$", "", input)
    if (any(file.exists(input))) {
      stop("Some strings are identical to filenames in working directory, please make some changes.")
    }
    message("PROCESSING CHARACTER VECTOR")
	length_of_v_input <- length(input)
    seged_vec <- rep(NA, length_of_v_input)
    for (i in 1:length(input)) {
      ii <- input[i]
      if (ii %in% c(NA, "NA", "?") | grepl("[^[:space:]]", ii) == FALSE) {
        message("Element ", i, " may be blank or missing, set it to a space.")
        ii <- " "
      }
      if (is.function(myfun1) && grepl("[^[:space:]]", ii) == TRUE) {
        ii <- AftEr_myfUn(FUN1(ii))
	  }
      if (!is.null(mycutter)) {
        ii <- gsub("\\s+", " ", paste(jiebaR::segment(ii, mycutter), collapse = " "))
      }
      if (is.function(myfun2) && grepl("[^[:space:]]", ii) == TRUE) {
        ii <- AftEr_myfUn(FUN2(ii), pa = TRUE)
      }
      seged_vec[i] <- ii
    }
	rm(input)
    message("GENERATING CORPUS")
    corp <- tm::Corpus(tm::VectorSource(seged_vec))
    rm(seged_vec)
  }
  message("PROCESSING CORPUS")
  corp <- suppressWarnings(tm::tm_map(corp, tm::removePunctuation))
  corp <- suppressWarnings(tm::tm_map(corp, tm::removeNumbers))
  corp <- suppressWarnings(tm::tm_map(corp, tm::content_transformer(tolower)))
  if (!is.null(stop_word) & ! grepl("^d|^D|^t|^T", type)) {
    if (stop_word[1] %in% c("jiebar", "auto", "jiebaR")) {
      corp <- suppressWarnings(tm::tm_map(corp, tm::removeWords, c(find_jiebar_stop())))
    }
    else {
      corp <- suppressWarnings(tm::tm_map(corp, tm::removeWords, c(stop_word)))
    }
  }
  if (!is.null(stop_pattern)) {
    stop_pattern <- as.character2(stop_pattern)
    stop_pattern <- paste(stop_pattern, collapse = "|")
    corp <- suppressWarnings(tm::tm_map(corp, tm::removeWords, c(stop_pattern)))
  }
  corp <- suppressWarnings(tm::tm_map(corp, tm::stripWhitespace))
  if (grepl("^c|^C", type)) {
    message("DONE")
    return(corp)
  }
  else if (grepl("^d|^D|^t|^T", type)) {
    message("MAKING DTM/TDM")
    todtm=ifelse (grepl("^d|^D", type), TRUE, FALSE)
    DTMname <- rE_dtm_new(corp, todtm = todtm, re_control = control, MYSTOP=stop_word)
	rm(corp)
	if (from == "dir"){
		DTMname$dimnames$Docs <- fullname
		rm(fullname)
	}
	if (from == "v"){
		DTMname$dimnames$Docs <- as.character(1: length_of_v_input)
	}	
    message("DONE")
	return(DTMname)
  }
  else {
    message("You do not specify a valid type, so return corpus.")
	message("DONE")
    return(corp)
  }
}

STRI_REPLACE=function(X, MYSTOP){
	stringi::stri_replace_all(
		X, 
		replacement="", 
		regex=paste("\\b", MYSTOP, "\\b", sep=""), 
		vectorize_all=FALSE
	)
}
