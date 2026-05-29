.onAttach <- function(lib,pkg) {
  local_version <- utils::packageVersion("grand")
  packageStartupMessage("+-------+  grand v",local_version)
  packageStartupMessage("| GRAND |  Cite: Neal, Z. P., (2023). grand: An R package for using the Guidelines for")
  packageStartupMessage("| ~~~~~ |        Reporting About Network Data. CRAN. https://doi.org/10.32614/CRAN.package.grand")
  packageStartupMessage("| ~~~~~ |")
  packageStartupMessage("| ~~~~~ |  Help: type vignette(\"grand\"); email zpneal@msu.edu; github zpneal/grand")
  packageStartupMessage("+-------+  Beta: type devtools::install_github(\"zpneal/grand\", ref = \"devel\")")
}

#' Restricts `scan()` input to a specified format
#'
#' @param prompt string: prompt for user input
#' @param what string: required format for input
#'
#' @return user input in specified format
#' @noRd
.scan <- function(prompt, what) {

  cat(prompt)  #Display prompt

  answer <- scan(nmax = 1, what = "character", quiet = TRUE, sep = "@")  #Get preliminary answer

  #### Check answer for correct format ####
  #Any input
  if ("anything" %in% what) {
    if (length(answer)==0) {return(answer)}  #Skipped
    if (is.na(answer)) {return(NA)}  #NA
    return(answer)  #Answer
    }

  #Numeric input
  if ("numeric" %in% what) {
    if (length(answer)==0) {return(answer)}  #Skipped
    if (is.na(answer)) {return(NA)}  #NA
    while (!suppressWarnings(!is.na(as.numeric(answer)))) { #Not a number
      cat(prompt)  #Repeat instructions
      answer <- scan(nmax = 1, what = "character", quiet = TRUE, sep = "@")  #Ask again
      if (length(answer)==0) {return(answer)}  #Return if skipped
      if (is.na(answer)) {return(NA)}  #Return if NA
    }
    return(as.numeric(answer))  #Answer
  }

  #Integer input
  if ("integer" %in% what) {
    if (length(answer)==0) {return(answer)}  #Skipped
    if (is.na(answer)) {return(NA)}  #NA
    while (!suppressWarnings(!is.na(as.numeric(answer))) |  #Not a number
           suppressWarnings(as.numeric(answer))%%1!=0) {    #Not an integer
      cat(prompt)  #Repeat instructions
      answer <- scan(nmax = 1, what = "character", quiet = TRUE, sep = "@")  #Ask again
      if (length(answer)==0) {return(answer)}  #Return if skipped
      if (is.na(answer)) {return(NA)}  #Return if NA
    }
    return(as.numeric(answer))  #Answer
  }

  #Custom input
  if (length(what)>1) {
    if (length(answer)==0) {return(answer)}  #Skipped
    if (is.na(answer)) {return(NA)}  #NA
    while (!(answer %in% what)) {    #Not an allowable response
      cat(prompt)  #Repeat instructions
      answer <- scan(nmax = 1, what = "character", quiet = TRUE, sep = "@")  #Ask again
      if (length(answer)==0) {return(answer)}  #Return if skipped
      if (is.na(answer)) {return(NA)}  #Return if NA
    }
    return(answer)  #Answer
  }
}
