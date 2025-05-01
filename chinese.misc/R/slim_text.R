#' Remove Words through Speech Tagging
#'
#' The function calls \code{jiebaR::tagging} to do speech tagging on a Chinese text, and then 
#' removes words that have certain tags.
#'
#' Stop words are often removed from texts. But a stop word list hardly includes all words that need 
#' to be removed. So, before removing stop words, we can remove a lot of insignificant 
#' words by tagging and make the texts "slim".
#' The webpage 
#' http://www.docin.com/p-341417726.html?_t_t_t=0.3930890985844252 
#' provides details about Chinese word tags.
#'
#' Only words with the following tags are to be preserved: 
#' \itemize{
#'   \item (1) "n": nouns;
#'   \item (2) "t": time related words;
#'   \item (3) "s": space related words;
#'   \item (4) "v": verbs;
#'   \item (5) "a": adjectives;
#'   \item (6) "b": words only used as attributes in Chinese;
#'   \item (7) "x": strings;
#'   \item (8) "j", "l", "i", "z": some specific Chinese letters and phrases;
#'   \item (9) "unknown": words of unknown type;
#'   \item (10) "eng": English words. 
#' }
#'
#' Optionally, words related to a specified place ("ns"), time related words ("t") and 
#' english words ("eng") can be removed.
#'
#' By default, a \code{DEFAULT_cutter} is used by the \code{mycutter} argument, which is 
#' assigned as \code{worker(write = FALSE)} when loading the package. 
#' As long as 
#' you have not manually created another variable called "DEFAULT_cutter", 
#' you can directly use \code{jiebaR::new_user_word(DEFAULT_cutter...)} 
#' to add new words. By the way, whether you manually create an object 
#' called "DEFAULT_cutter", the original loaded DEFAULT_cutter which is 
#' used by default by functions in this package will not be removed by you.
#' So, whenever you want to use this default value, you just do not set 
#' \code{mycutter}.
#'
#' @param x a length 1 character of Chinese text to be tagged
#' @param mycutter a jiebar cutter provided by users to tag text. It has a default value, see Details.
#' @param rm_place \code{TRUE} or \code{FALSE}. if \code{TRUE} (default), words related 
#' to a specified place ("ns") are removed.
#' @param rm_time \code{TRUE} or \code{FALSE}. if \code{TRUE} (default), time related 
#' words ("t") are removed. 
#' @param rm_eng \code{TRUE} or \code{FALSE}. if \code{TRUE},  English words are 
#' removed. The default is \code{FALSE}.
#' @param rm_alpha should be "any", \code{TRUE} or \code{FALSE} (default). Some English words
#' are tagged as "x", so cannot be remove by setting \code{rm_eng}. But when
#' \code{rm_alpha} is \code{TRUE}, any word that contains only a-zA-Z 
#' will be removed. If it is "any", then words that are mixtures of a-zA-Z and Chinese/digits 
#' will be removed.
#' @param paste \code{TRUE} or \code{FALSE}, whether to paste the segmented words
#' together into a length 1 character. The default is \code{TRUE}.
#'
#' @return a length 1 character of segmented text, or a character vector, each element of which 
#' is a word.
#'
#' @export
#' @examples
#' \donttest{
#' require(jiebaR)
#' cutter <- jiebaR::worker()
#' # Give some English words a new tag.
#' new_user_word(cutter, c("aaa", "bbb", "ccc"),  rep("x", 3))
#' x <- "we have new words: aaa, bbb, ccc."
#' # The default is to keep English words.
#' slim_text(x, mycutter = cutter)
#' # Remove words tagged as "eng" but others are kept.
#' slim_text(x, mycutter = cutter, rm_eng = TRUE)
#' # Remove any word that only has a-zA-Z, 
#' # even when rm_eng = FALSE.
#' slim_text(x, mycutter = cutter, rm_eng = TRUE, rm_alpha = TRUE)
#' slim_text(x, mycutter = cutter, rm_eng = FALSE, rm_alpha = TRUE)
#' }
slim_text <-
function(x, mycutter = DEFAULT_cutter, rm_place = TRUE, rm_time = TRUE, rm_eng = FALSE, rm_alpha = FALSE, paste = TRUE) {
  stopifnot(class(mycutter)[1] == "jiebar")
  stopifnot(all(c(rm_place, rm_time, rm_eng, paste) %in% c(TRUE, FALSE)))
  stopifnot(identical(rm_alpha, TRUE) | identical(rm_alpha, FALSE) | identical(rm_alpha, "any"))
  if (length(x) > 1) {
    x <- x[1]
    message("Argument x has length larger than 1, only the 1st is used.")
  }
  x <- whetherencode(x)
  ta <- jiebaR::tagging(x, jiebar = mycutter)
  pat <- "^n|^s|^v|^a|^b|^z|^x|^j|^l|^i|unknown"
  if (!rm_time){
    pat <- paste(pat, "|^t", sep = "")
  }
  if (!rm_eng){
    pat <- paste(pat, "|eng", sep = "")
  }  
  tal <- grepl(pat, names(ta))
  ta <- ta[tal]
  if (rm_place) {
    ta <- ta[names(ta) != "ns"]
  }
  if (rm_alpha == TRUE){
    ta <- ta[!grepl("^[a-zA-Z]{1,50}$", ta)]
  }
  if (rm_alpha == "any"){
    ta <- ta[!grepl("[a-zA-Z]", ta)]
  } 
  if (paste) {
    ta <- paste0(ta, collapse = " ")
  }
  return(ta)
}
