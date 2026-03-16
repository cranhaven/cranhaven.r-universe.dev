#' Internal vegdata functions
#' @name vegdata-internal
#' @aliases reShape.veg bin2word word2bin
#' @noRd
#' @description  Internal vegdata functions.
#' @details These are not intended to be called directly by the user.
#'  tv.home tries to guess the default tv_home directory (\code{'C:\Turbowin'} or \code{'C:\Programme\Turbowin'} or \code{'O:\Turbowin'} on Windows systems and \code{'~/.wine/drive_c/Turbowin'} on Unix systems.
#' @keywords internal


# #'  As dBase is an old DOS format, Umlaute have been stored in Turboveg using the CP437 code table. Change options('tv.iconv') if you run into problems
# gracefully_fail <- function(remote_file) {
#   try_GET <- function(x, ...) {
#     tryCatch(
#       GET(url = x, timeout(1), ...),
#       error = function(e) conditionMessage(e),
#       warning = function(w) conditionMessage(w)
#     )
#   }
#   is_response <- function(x) {
#     class(x) == "response"
#   }
#   # First check internet connection
#   if (!curl::has_internet()) {
#     message("No internet connection.")
#     return(invisible(NULL))
#   }
#   # Then try for timeout problems
#   resp <- try_GET(remote_file)
#   if (!is_response(resp)) {
#     message(resp)
#     return(invisible(NULL))
#   }
#   # Then stop if status > 400
#   if (httr::http_error(resp)) {
#     message_for_status(resp)
#     return(invisible(NULL))
#   }
#
#   # # If you are using rvest as I do you can easily read_html in the response
#   # xml2::read_html(resp)
# }

# gracefully_fail("http://httpbin.org/status/404") # http >400
# #> Not Found (HTTP 404).
# gracefully_fail("http://httpbin.org/delay/2") # Timeout
# #> Timeout was reached: [httpbin.org] Operation timed out after 1000 milliseconds with 0 bytes received
# gracefully_fail("http://httpbin.org") #OK
# #> {html_document}
# #> <html lang="en">
# #> [1] <head>\n<meta http-equiv="Content-Type" content="text/html; charset=UTF-8 ...
# #> [2] <body>\n    <a href="https://github.com/requests/httpbin" class="github-c ...
# gracefully_fail("http://httpbin.org") #OK
#
# remote <- "https://germansl.infinitenature.org/GermanSL/1.5/GermanSL.zip"
# remote2 <- "https://german.infinitenature.org/GermanSL/1.5/GermanSL.zip"
# remote3 <- "https://germansl.infinitenature.org/GermanSL/1.6/GermanSL.zip"
#
# # gracefully_fail(remote) #OK
#
# status <- tryCatch(
#   RCurl::getURL(url, ssl.verifypeer=FALSE, useragent="R"),
#   error = function(e) e
# )
# # inherits(status,  "error")
#
#
# f <- function(url) {
#   if (!curl::has_internet()) {
#     message("No internet connection")
#     return(NULL)
#   }
#   if (httr::http_error(url)) {
#     message("Data source broken.")
#     return(NULL)
#   }
#   url(remote2)
#   if(as.integer(tmp) == 5)
#   tryCatch(http_error(GET(url)),
#            http_404 = function(c) "That url doesn't exist",
#            http_403 = function(c) "You need to authenticate!",
#            http_400 = function(c) "You made a mistake!",
#            http_500 = function(c) "The server screwed up"
#   )
# }

# f(remote2)

asc <- function(char) sapply(char, function(x) strtoi(charToRaw(x), 16L), simplify = TRUE, USE.NAMES = FALSE)
chr <- function(ascii) sapply(ascii, function(x) rawToChar(as.raw(x)), USE.NAMES = FALSE)

bin2word <- function(x) {
  c1 <- substr(x, 1,1)
  c2 <- substr(x, 2,2)
  return(255 * (asc(c1) -1) + asc(c2) - 1)
}
word2bin <- function(x) {
  c1 <- floor(x/255+1)
  c2 <- x - (c1-1)*255
  paste (chr(c1), chr(c2 + 1), sep='')
}

#' @noRd
"[.veg" <- function(x, s,...) {
  taxref <- attr(veg, 'taxreflist')
  out <- NextMethod("[,", drop=TRUE)
  class(out) <- c('veg', 'data.frame')
  attr(veg, 'taxreflist') <- taxref
  return(out)
}


#
# first.word <- function (x, i = 1, expr = substitute(x), add.legal=NULL) {
#   words <- if(!missing(x)) as.character(x)[1] else as.character(unlist(expr))[1]
#   if (i > 2) stop("only first and second word implemented")
#   chars <- substring(words, 1:nchar(words, keepNA = FALSE), 1:nchar(words, keepNA = FALSE))
#   legal.chars <- c(letters, LETTERS, '\u00fc','\u00e4','\u00f6','\u00df','\u00d7', "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", add.legal)
#   non.legal.chars <- (1:length(chars))[!chars %in% legal.chars]
#   # length(non.legal.chars) > 0
#   if (i==1 & is.na(non.legal.chars[1])) return(words)
#   if (i==1 & !is.na(non.legal.chars[1])) return(substring(words, 1, non.legal.chars[1] - 1))
#   if (i==2 & is.na(non.legal.chars[2])) return(substring(words, non.legal.chars[1], nchar(words, keepNA = FALSE)))
#   if (i==2 & !is.na(non.legal.chars[2])) return(substring(words, non.legal.chars[1]+1, non.legal.chars[2]-1)) else return(character(0))
# }
#
# word <- function (string, start = 1L, end = start, sep = fixed(" ")) {
#   n <- max(length(string), length(start), length(end))
#   string <- rep(string, length.out = n)
#   start <- rep(start, length.out = n)
#   end <- rep(end, length.out = n)
#   breaks <- str_locate_all(string, sep)
#   words <- lapply(breaks, invert_match)
#   len <- vapply(words, nrow, integer(1))
#   neg_start <- !is.na(start) & start < 0L
#   start[neg_start] <- start[neg_start] + len[neg_start] + 1L
#   neg_end <- !is.na(end) & end < 0L
#   end[neg_end] <- end[neg_end] + len[neg_end] + 1L
#   start[start > len] <- NA
#   end[end > len] <- NA
#   starts <- mapply(function(word, loc) word[loc, "start"], words, start)
#   ends <- mapply(function(word, loc) word[loc, "end"], words, end)
#   str_sub(string, starts, ends)
# }
# x <-  c('Tortula acaulon (With.) R. H.Zander var. acaulon', 'Phascum cuspidatum Hedw. v. cuspidatum', 'Tortula acaulon var. papillosa (Lindb.) R. H. Zander', 'Phascum cuspidatum subsp. papillosum (Lindb.) J. Guerra & Ros', 'Tortula SP.')

# @noRd
# rbind.df <- function(df1, df2) {
#   cols1 <- names(df1); cols2 <- names(df2)
#   All <- union(cols1, cols2)
#   miss1 <- setdiff(All, cols1)
#   miss2 <- setdiff(All, cols2)
#   df1[, c(as.character(miss1))] <- NA
#   df2[,c(as.character(miss2))] <- NA
#   out <- rbind(df1, df2)
#   return(out)
# }

# @noRd
# cbind.df <- function(df1, df2, by) {
#   cols1 <- names(df1); cols2 <- names(df2)
#   inters <- intersect(cols1, cols2)
#   df.m <- df2[match(df1[,by], df2[,by]), ]
#   for(i in inters) {
#     df1[,i][is.na(df1[,i])] <- df.m[,i][is.na(df1[,i])]
#   }
#   return(df1)
# }

# as.data.frame.list
# Convert a list of vectors to a data frame.
# @noRd
# as.data.frame.list <- function(x, row.names=NULL, optional=FALSE, ...) {
#   if(!all(unlist(lapply(x, class)) %in%
#           c('raw','character','complex','numeric','integer','logical'))) {
#     warning('All elements of the list must be a vector.')
#     NextMethod(x, row.names=row.names, optional=optional, ...)
#   }
#   allequal <- all(unlist(lapply(x, length)) == length(x[[1]]))
#   havenames <- all(unlist(lapply(x, FUN=function(x) !is.null(names(x)))))
#   if(havenames) { #All the vectors in the list have names we can use
#     colnames <- unique(unlist(lapply(x, names)))
#     df <- data.frame(matrix(
#       unlist(lapply(x, FUN=function(x) { x[colnames] })),
#       nrow=length(x), byrow=TRUE), stringsAsFactors = FALSE)
#     names(df) <- colnames
#   } else if(allequal) { #No names, but are of the same length
#     df <- data.frame(matrix(unlist(x), nrow=length(x), byrow=TRUE), stringsAsFactors = FALSE, ...)
#     hasnames <- which(unlist(lapply(x, FUN=function(x) !is.null(names(x)))))
#     if(length(hasnames) > 0) { #We'll use the first element that has names
#       names(df) <- names(x[[ hasnames[1] ]])
#     }
#   } else { #No names and different lengths, we'll make our best guess here!
#     warning(paste("The length of vectors are not the same and do not ",
#                   "are not named, the results may not be correct.", sep=''))
#     #Find the largest
#     lsizes <- unlist(lapply(x, length))
#     start <- which(lsizes == max(lsizes))[1]
#     df <- x[[start]]
#     for(i in (1:length(x))[-start]) {
#       y <- x[[i]]
#       if(length(y) < length(x[[start]])) {
#         y <- c(y, rep(NA, length(x[[start]]) - length(y)))
#       }
#       if(i < start) {
#         df <- rbind(y, df)
#       } else {
#         df <- rbind(df, y)
#       }
#     }
#     df <- as.data.frame(df, row.names=1:length(x))
#     names(df) <- paste('Col', 1:ncol(df), sep='')
#   }
#   if(missing(row.names)) {
#     row.names(df) <- names(x)
#   } else {
#     row.names(df) <- row.names
#   }
#   return(df)
# }
#
