#' @title Miscellaneous Tools for Chinese Text Mining and More
#'
#' @description
#' This package aims to help accomplish the basic tasks of Chinese text mining 
#' in a more efficient way. The manual in Chinese is 
#' in \url{https://github.com/githubwwwjjj/chinese.misc}.
#' Compared with other packages and functions, the package puts more weight 
#' on the following three points:
#' (1) It helps save users' time.
#' (2) It helps decrease errors (it tolerates and corrects input errors, if it can;  
#' and if it cannot, it gives meaningful error messages).
#' (3) Although the functions in this package depend on \pkg{tm} and 
#' \pkg{stringi}, several steps and the values of arguments have been 
#' specially set to facilitate processing Chinese text.
#' For example, \code{corp_or_dtm} creates corpus or 
#' document term matrix, users only need to input folder names or file names, and the function 
#' will automatically detect file encoding, segment terms, modify texts, 
#' remove stop words. 
#' \code{txt2csv} and \code{csv2txt} help convert the format of texts and do some data 
#' cleaning. And there are some functions for object class assertion and coercion. 
#'
#' @docType package
#' @name chinese.misc-package
#' @aliases chinese.misc
#' @rdname chinese.misc-package
#' @author Jiang Wu
#' @family chinese.misc_general_topics
#' @examples 
#' require(tm)
#' # Since no Chinese character is allowed, here we 
#' # use English instead.
#' # Make a document term matrix in 1 step, few arguments have 
#' # to be modified by the user.
#' x <- c(
#'   "Hello, what do you want to drink?", 
#'   "drink a bottle of milk", 
#'   "drink a cup of coffee", 
#'   "drink some water", 
#'   "hello, drink a cup of coffee")
#' dtm <- corp_or_dtm(x, from = "v", type = "dtm")
#' # Coerce list containing data frames and other lists
#' df <- data.frame(matrix(c(66, 77, NA, 99), nr = 2))
#' l <- list(a = 1:4, b = factor(c(10, 20, NA, 30)), c = c('x', 'y', NA, 'z'), d = df)
#' l2 <- list(l, l, cha = c('a', 'b', 'c'))
#' as.character2(l2)
NULL