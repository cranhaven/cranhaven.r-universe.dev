#' Split Comma
#'
#' This function takes a character string in the form of Last Name (Surname),
#' First Name or Second String, First String and transposes that string to
#' First Name Last Name (Surname) or First String Second String while removing
#' the comma.
#'
#'
#' @param string character vector that contains a phrase separated by a comma
#'    or not. If not, there is no change.
#'
#'
#'
#'
#' @return the character \code{\link[base]{vector}} in the form of First Name Last Name
#'
#'
#'
#'
#'
#' @source
#' \enumerate{
#'    \item regex - Split on first comma in string - Stack Overflow answered by flodel on Apr 25 2012. See \url{https://stackoverflow.com/questions/10309122/split-on-first-comma-in-string}.
#'    \item regex - r regexp - replace title and suffix in any part of string with nothing in large file (&gt; 2 million rows) - Stack Overflow answered by Molx on Apr 16 2015. See \url{https://stackoverflow.com/questions/29680131/r-regexp-replace-title-and-suffix-in-any-part-of-string-with-nothing-in-large}.
#' }
#'
#'
#'
#'
#'
#'
#' @author Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#'
#' @examples
#'
#' # Example 1
#' 
#' install.load::load_package("iemisc", "data.table")
#' 
#' dtxx <- data.table(Names = c("Cooler, Wine", "Juice, Fruit", "Hard Water",
#' "Hot Bath", "Set, Data"))
#' 
#' dtxx[, Corrected_Names := splitcomma(dtxx$Names)]
#' 
#' dtxx
#' 
#'
#'
#' # Example 2
#' 
#' xtrax <- "FALSER, BRATTIE & SIMX, AGONY"
#' 
#' splitcomma(xtrax)
#' 
#' 
#' 
#' @importFrom stringi stri_split_fixed stri_detect_fixed
#' @importFrom assertthat assert_that
#' @importFrom checkmate testCharacter
#' @importFrom data.table %like%
#'
#' @export
splitcomma <- function(string) {

# Check string
assert_that(!any(testCharacter(string, min.chars = 1) == FALSE), msg = "string is a numeric vector. string should be a character vector that contains at least 1 character. Please try again.")
# only process with string values and provide an error message if the check fails

# Look for a comma in the name column and for those rows create a new column called names that splits the original name in 2 strings and reorder the strings staring with the 2nd string then the 1st string

# If there is " & ", assume that it's more than 2 sets of names and split those character vectors first

if (any(string %like% " & ")) {

string <- unlist(stri_split_fixed(string, pattern = " & ", n = 2))

ifelse (stri_detect_fixed(string, pattern = ", "), sapply(stri_split_fixed(string, pattern = ", ", n = 2), function(st) paste(st[2], st[1], collapse = " "), USE.NAMES = FALSE), string)

} else {

ifelse (stri_detect_fixed(string, pattern = ", "), sapply(stri_split_fixed(string, pattern = ", ", n = 2), function(st) paste(st[2], st[1], collapse = " "), USE.NAMES = FALSE), string)

}
}








#' Split Remove
#'
#' This function removes characters from a string based on a character vector
#' named remove. This function can be used to remove prefixes, suffixes,
#' titles, etc. from a given character vector. The function splits the string
#' by empty spaces, dots, commas, and parentheses first & then it removes the
#' items that are in the remove vector.
#'
#'
#'
#' @param string character vector that contains the text to keep and to remove
#' @param remove character vector that contains the characters to remove from
#'    the string
#'
#'
#'
#' @return the revised character \code{\link[base]{vector}} with the contents of
#'    remove removed from the string 
#'
#'
#'
#'
#'
#' @source
#' regex - r regexp - replace title and suffix in any part of string with nothing in large file (&gt; 2 million rows) - Stack Overflow answered by Molx on Apr 16 2015. See \url{https://stackoverflow.com/questions/29680131/r-regexp-replace-title-and-suffix-in-any-part-of-string-with-nothing-in-large}.
#'
#'
#'
#'
#'
#'
#' @author Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#'
#' @examples
#'
#' # Example
#' 
#' install.load::load_package("iemisc", "data.table")
#'
#' # create the list of items to remove from the text
#' remove <- c("mister", "sir", "mr", "madam", "mrs", "miss", "ms", "iv",
#' "iii", "ii", "jr", "sr", "md", "phd", "mba", "pe", "mrcp", "and", "&", "prof",
#' "professor", "esquire", "esq", "dr", "doctor")
#' 
#' names <- data.table(Named = c("Alfredy 'Chipp' Kahner IV",
#' "Denis G. Barnekdt III", "JERUEG, RICHARDS Z. MR.", "EDWARDST, HOWARDD K. JR."))
#' 
#' # first use split comma
#' names[, Corrected_Named := splitcomma(names$Named)]
#' 
#' names
#' 
#' names[, Corrected_Named := splitremove(names$Corrected_Named, remove)]
#' 
#' names
#' 
#'
#' 
#' 
#' 
#' 
#' @importFrom stringi stri_detect_regex stri_split_regex
#' @importFrom assertthat assert_that
#' @importFrom checkmate testCharacter
#' @importFrom mgsub mgsub
#'
#' @export
splitremove <- function(string, remove) {

remove <- remove

remover <- paste(remove, collapse = "|")


# Check string
assert_that(!any(testCharacter(string, min.chars = 1) == FALSE), msg = "string is a numeric vector. string should be a character vector that contains at least 1 character. Please try again.")
# only process with string values and provide an error message if the check fails


# Split by empty spaces, dots, commas and parenthesis and then remove the items from remove
stringer <- ifelse (stri_detect_regex(string, remover, case_insensitive = TRUE), sapply(stri_split_regex(string, "\\s|\\.|\\,|\\(|\\)"), function(st) paste(st[!(st %qsin% remove)], collapse = " "), USE.NAMES = FALSE), string)

stringers <- rm_white(stringer)

stringerst <- mgsub(stringers, "( \\w{1}) ", " \\1. ")

stringersts <- rm_white(stringerst)

return(stringersts)
}













#' Quick Search
#'
#' This function performs a quick, case insensitive search of strings
#'
#'
#' @param string character vector that contains the values to match
#' @param vector character vector that has the values to be matched against
#'
#' @details 'Utilizes chin from data.table to quickly complete a case
#'    insensitive search through a character vector to return a logical vector
#'    of string detections. Will always return TRUE or FALSE for each position
#'    of string regardless of NA missing values in either provided vector. NA
#'    in string will never match an NA value in the vector.'
#'
#' @return logical vector the length of the original string
#'
#'
#' @source
#' data.table - R case-insensitive \%in\% - Stack Overflow answered and edited by Dylan Russell on Sep 13, 2020. See \url{https://stackoverflow.com/questions/63874824/r-case-insensitive-in}.
#'
#'
#'
#'
#'
#'
#' @author Dylan Russell (stackoverflow code, examples, and function details, function description), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#' @examples
#'
#' # Examples
#'
#' x <- c("apple", "banana", "cherry", NA)
#'
#' "apple" %qsin% x
#'
#' c("APPLE", "BANANA", "coconut", NA) %qsin% x
#'
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate testCharacter
#' @importFrom data.table %chin%
#' @importFrom stats na.omit
#'
#' @export
`%qsin%` <- function(string, vector) {

# The checks fail when qsin is contained within splitremove therefore the following are left commented out

# Check string
# assert_that(!any(testCharacter(string, min.chars = 1) == FALSE), msg = "string is a numeric vector. string should be a character vector that contains at least 1 character. Please try again.")
# only process with string values and provide an error message if the check fails

# Check vector
# assert_that(!any(testCharacter(vector, min.chars = 1) == FALSE), msg = "string is a numeric vector. string should be a character vector that contains at least 1 character. Please try again.")
# only process with string values and provide an error message if the check fails


tolower(string) %chin% na.omit(tolower(vector))

}






#' Not CHIN or IN for Character and Numeric Vectors
#'
#' This function performs a quick, case sensitive search of character vectors
#' that are not in a set of character vectors or a quick, search of numeric
#' vectors that are not in a set of numeric vectors using \code{\link[base]{Negate}} chin
#' for character vectors and \code{\link[base]{Negate}} \code{\link[base]{in}} for numeric vectors
#'
#'
#' @param x character or numeric vector that contains the values to not be matched
#' @param y character or numeric vector that has the values to be checked within
#'
#' @details 'Utilizes chin from data.table to quickly complete a case
#'    insensitive search through a character vector to return a logical vector
#'    of string detections. Will always return TRUE or FALSE for each position
#'    of string regardless of NA missing values in either provided vector. NA
#'    in string will never match an NA value in the vector.'
#'
#' @return logical vector the length of the original string (x). TRUE means that
#'    x is not in y and FALSE means that x is in y
#'
#'
#' @source
#' The %notin% operator Posted on July 8, 2018 by kaijagahm in R bloggers. See \url{https://www.r-bloggers.com/2018/07/the-notin-operator/}.
#'
#'
#'
#'
#'
#'
#' @author kaijagahm (R code), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#' @examples
#'
#' # Examples
#'
#' x <- c("apple", "banana", "cherry", NA)
#'
#' "apple" %notchin% x
#'
#' c("apple", "BANANA", "coconut", NA) %notchin% x
#'
#' x
#'
#'
#' "a" %notchin% letters[5:20]
#' letters[5:20] %notchin% "a"
#'
#'
#' "a" %notchin% LETTERS
#' LETTERS %notchin% "a"
#'
#'
#' 1 %notchin% -12:20
#' -12:20 %notchin% 1
#'
#'
#'
#'
#' @importFrom data.table %chin%
#'
#' @export
`%notchin%` <- function(x, y) {

`%notchin%` <- Negate(`%chin%`)

`%notin%` <- Negate(`%in%`)


if (all(class(x) == "character" & class(y) == "character")) {

x %notchin% y


} else {

x %notin% y

}
}








#' IN ORDER for Character and Numeric Vectors
#'
#' This function preserves the original order (sequence) of character vectors.
#'
#'
#' @param y numeric vector that contains the sequence to return
#' @param table character vector, data.table, and/or tibble that has the
#'     character values to be checked within
#'
#'
#' @return character vector with the characters in the original sequence
#'
#' @source
#' R - preserve order when using matching operators (%in%) - Stack Overflow answered by John Wallace on Nov 3, 2017. See \url{https://stackoverflow.com/questions/10586652/r-preserve-order-when-using-matching-operators-in}.
#'
#'
#'
#'
#'
#'
#' @author John Wallace (Stack Overflow R code), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#' @examples
#'
#' # Examples (from the Source)
#' 
#' LETTERS[1:26 %in% 4:1]
#' 
#' LETTERS[1:26 %inorder% 4:1]
#' 
#' 
#' 
#' LETTERS[1:26 %in% 3:-5]
#' 
#' LETTERS[1:26 %inorder% 3:-5]
#' 
#' 
#' data.frame(letters, LETTERS)[1:5 %in% 3:-5, ] 
#' 
#' data.frame(letters, LETTERS)[1:5 %inorder% 3:-5, ]
#' 
#' 
#' library(data.table)
#'
#' data.table(letters, LETTERS)[1:5 %inorder% 3:-5, ] 
#'
#'
#' library(tibble)
#' 
#' tibble(letters, LETTERS)[1:5 %inorder% 3:-5, ]
#' 
#'
#'
#'
#'
#' @export
`%inorder%` <- function(y, table) {

    ySeq <- seq(along = y)
    
    names(ySeq) <- y
    
    Result <- ySeq[as.character(table)]
    
    Result[!is.na(Result)]
}
