#' Fraction (or Mixed number) to a Decimal (Numeric Vector)
#'
#' Converts a fraction or a mixed number to a decimal
#'
#'
#' @param n character vector that contains the fraction or mixed number (can
#'     also include text, ex. inch, inches, etc. that will be removed from
#'     the vector)
#'
#'
#'
#'
#' @return the numeric \code{\link[base]{vector}} as a decimal
#'
#'
#'
#'
#'
#' @source
#' removing all non-numeric characters from a string, but not "." - R help on nabble.com answered by David Winsemius on Jul 26, 2016. See \url{https://web.archive.org/web/20190730141421/http://r.789695.n4.nabble.com/removing-all-non-numeric-characters-from-a-string-but-not-quot-quot-td4723146.html}. Retrieved thanks to the Internet Archive: Wayback Machine.
#'
#'
#'
#'
#' @references
#' \enumerate{
#'    \item Bill Venables, 2016-02-10, "Vulgar Fractions in R", fractional vignette, \url{https://CRAN.R-project.org/package=fractional/vignettes/Vulgar_Fractions_in_R.html}.
#'    \item The Home Depot, 9 December 2022, "How to Read a Tape Measure", \url{https://archive.vn/fhBmg}. Provided the archive.today webpage capture for The Home Depot URL for acceptance into CRAN. 
#'    \item Wikimedia Foundation, Inc. Wikipedia, 29 December 2021, "Pi", \url{https://en.wikipedia.org/wiki/Pi}.
#' }
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
#' @note
#' If you have a measurement in feet + inches, then use \code{\link{construction_fraction}}
#' instead.
#'
#'
#'
#' @examples
#' 
#' # Please refer to the iemisc: Construction Measurement Examples vignette for
#' # additional examples
#'  
#' # Example 1 -- Reference 1
#' 
#' library(iemisc)
#'
#' xx <- as.character(fractional::fractional(1:9 / 12))
#' 
#' try(frac_to_numeric(xx))
#' # Please note that there will be an error because this function is designed to
#' # only process one fraction at a time.
#' 
#' lapply(xx, frac_to_numeric)
#' # Please note that this is the correct way to work with several fractions at once.
#'
#'
#' 
#' 
#' # Example 2
#' 
#' library(iemisc)
#'
#' xi <- fracture::fracture((50:65) / 12)
#'
#' try(frac_to_numeric(xi))
#' # Please note that there will be an error because this function is designed to
#' # only process one fraction at a time.
#' 
#' lapply(xi, frac_to_numeric)
#' # Please note that this is the correct way to work with several fractions at once.
#'
#'
#' 
#' 
#' # Example 3
#' 
#' library(iemisc)
#'
#' xyy <- fracture::fracture((1:11) / 12)
#'
#' try(frac_to_numeric(xyy))
#' # Please note that there will be an error because this function is designed to
#' # only process one fraction at a time.
#' 
#' lapply(xyy, frac_to_numeric)
#' # Please note that this is the correct way to work with several fractions at once.
#'
#' 
#' 
#'  
#' # Example 4
#' 
#' library(iemisc)
#'
#' xft <- as.character(MASS::fractions((1:70) / 12))
#'
#' try(frac_to_numeric(xft))
#' # Please note that there will be an error because this function is designed to
#' # only process one fraction at a time.
#' 
#' lapply(xft, frac_to_numeric)
#' # Please note that this is the correct way to work with several fractions at once.
#'
#' 
#' 
#'  
#' # Example 5
#' 
#' library(iemisc)
#'
#' pix <- "270/11"
#'
#' pi1 <- "22/7" # Reference 3
#'
#' pi2 <- "355/113" # Reference 3
#' 
#' frac_to_numeric(pix)
#' 
#' frac_to_numeric(pi1)
#' 
#' frac_to_numeric(pi2)
#'
#' 
#' 
#'  
#' # Example 6
#' 
#' # If you have a construction measurement that includes a dimension in feet,
#' # such as 49 ft 7 5/8 in, don't use the frac_to_numeric function, instead
#' # use the construction_fraction function.
#' 
#' library(iemisc)
#'
#' xxift <- "49 ft 7 5/8 in"
#' 
#' construction_decimal(xxift, result = "traditional", output = "vector")
#'
#' 
#' 
#' 
#' # Example 7 -- Reference 2
#' 
#' truss_marks <- "19 3/16 inches"
#' 
#' frac_to_numeric(truss_marks)
#'
#' 
#' 
#' 
#' @importFrom stringi stri_detect_regex stri_trim_both stri_replace_all_regex stri_detect_fixed
#' @importFrom assertthat assert_that
#' @importFrom checkmate testCharacter
#'
#' @export
frac_to_numeric <- function(n) {

# Check n
assert_that(!any(length(n) > 1), msg = "The length of n is greater than 1. n should only be 1 numeric vector. Please try again.")
# only process with string values with numbers and provide an error message if the check fails

assert_that(!any(testCharacter(n, min.chars = 1, pattern = "[0-9]") == FALSE), msg = "n is a numeric vector or a character vector without any numeric values. n should be a character vector that contains numeric values only. Please try again.")
# only process with string values with numbers and provide an error message if the check fails

ifelse(stri_detect_regex(n, "[A-Za-z]"), n <- stri_trim_both(stri_replace_all_regex(n, "[A-Za-z]", "")), n <- n)


if(!stri_detect_fixed(n, " ")) {


if (!stri_detect_fixed(n, "/")) {

change <- as.numeric(n)

return(change)


} else if (stri_detect_fixed(n, "/")) {


change1 <- unlist(stri_split_fixed(n, "/", n = 2))

assert_that(all(stri_detect_regex(change1, "[0-9]")), msg = "n does not include numeric values in both parts. Please try again.")
# only process with a numeric value present in both parts and provide a stop warning if change1 does not include a numeric value in both parts (n in error message since only that parameters has been defined)


change1_part1 <- as.numeric(change1[1])

change1_part2 <- as.numeric(change1[2])


change <- change1_part1 / change1_part2

return(change)

}

} else if (stri_detect_fixed(n, " ")) {

change1 <- unlist(stri_split_fixed(n, " ", n = 2))

assert_that(all(stri_detect_regex(change1, "[0-9]")), msg = "n does not include numeric values in both parts. Please try again.")
# only process with a numeric value present in both parts and provide a stop warning if change1 does not include a numeric value in both parts (n in error message since only that parameters has been defined)

change1_part1 <- as.numeric(change1[1])

change1_part2 <- stri_replace_all_regex(change1[2], "[^0-9]", " ") # Source 1

change2 <- unlist(stri_split_fixed(change1_part2, " ", n = 2))

change2a <- as.numeric(change2[[1]]) / as.numeric(change2[[2]]) 

change <- sum(change1_part1, change2a)

return(change)

}
}
