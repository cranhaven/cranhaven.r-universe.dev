#' @title printTable
#'
#' @description Returns Latex formatted table from dataframe or ezANOVA ANOVA table.
#' Uses xtable latex package with some basic defaults.
#' For more examples, see R package xtable
#' @param obj Dataframe/ezANOVA object to print
#' @param caption Title of the dataframe
#' @param digits Number of digits to round to NB. length can be 1, or vector with
#'  length equal to the number of numeric columns
#' @param onlyContents TRUE/FALSE
#'
#' @return character
#'
#' @examples
#' # Example 1:
#' library(ez)
#' # create dataframe
#' dat <- createDF(nVP = 6, nTrl = 1,
#'                 design = list("Comp" = c("comp", "incomp", "neutral")))
#'
#' dat <- addDataDF(dat, RT = list("Comp comp"    = c(500, 150, 100),
#'                                 "Comp incomp"  = c(520, 150, 100),
#'                                 "Comp neutral" = c(510, 150, 100)))
#' printTable(dat, digits = c(0, 2)) # latex formatted
#' printTable(dat, digits = 0)       # latex formatted
#'
#' dat$VP <- as.factor(dat$VP)
#' aovRT <- ezANOVA(dat, dv=.(RT), wid = .(VP), within = .(Comp),
#'                  return_aov = TRUE, detailed = TRUE)
#' aovRT <- aovTable(aovRT)
#' printTable(aovRT$ANOVA) # latex formatted
#' printTable(aovRT$ANOVA, digits = c(0,2,2,2)) # latex formatted
#'
#'
#' @export
printTable <- function(obj, caption = "DF", digits=3, onlyContents=FALSE) {

    # TO DO: xtable.sanitize?
    caption    <- gsub("_", "\\\\_", caption)
    names(obj) <- lapply(names(obj), function(x) gsub("_", "\\\\_", x))

    # rows
    nncs <- which(!unlist(lapply(obj, is.numeric)))
    obj[, nncs] <- sapply(obj[, nncs], function(x) gsub("_", "\\\\_", x))

    # remove leading 0 in p column
    pcol <- which(names(obj) == "p")
    if (length(pcol) == 1) {
        obj[, pcol] <- sub("^0.", ".", obj[, pcol])
    }

    # typical symbols in ANOVA table
    names(obj) <- gsub("<.05",     "$<.05$",          names(obj))
    names(obj) <- sub("\\<p\\>",   "$\\\\textit{p}$", names(obj))
    names(obj) <- sub("\\<F\\>",   "$\\\\textit{F}$", names(obj))
    names(obj) <- sub("\\<pes\\>", "$\\\\eta_{p}^2$", names(obj))
    names(obj) <- sub("\\<ges\\>", "$\\\\eta_{G}^2$", names(obj))
    names(obj) <- sub("\\<eps\\>", "$\\\\epsilon$",   names(obj))
    names(obj) <- sub("\\\\_p",    "$\\\\textit{p}$", names(obj))

    if (length(digits) != 1) {
        if (length(digits) != ncol(obj)) {
            # find numeric columns
            numeric_cols <- as.vector(which(unlist(lapply(obj, is.numeric))))
            if (length(numeric_cols) == 0) {
                digits = 0
                message("obj does not contain numeric columns! No additional formatting applied.", immediate = TRUE)
            } else if (length(digits) != length(numeric_cols)) {
                stop("Number of digits does not equal number of numeric columns!")
            } else {
                # make digits length required by xtable command
                tmp <- rep(0, ncol(obj))
                tmp[numeric_cols] <- digits
                digits <- c(0, tmp)
            }
        }
    }
    tab <- xtable::xtable(obj, caption = caption, digits = digits)

    print(tab,
          table.placement = "H",
          caption.placement = "top",
          include.rownames = FALSE,
          floating = FALSE,
          tabular.environment = "longtable",
          only.contents = onlyContents,
          hline.after = c(-1, 0),
          sanitize.text.function = function(x){x})

}



#' @title mathString
#'
#' @description Returns formatted string following addition/subtraction.
#'
#' @param str1 string
#' @param str2 string
#' @param operation "+", "-", "*", "/"
#' @param numDigits number 0 (default)
#' @param unit "ms" , "mV" , "mv", or "\%"
#'
#' @return NULL
#'
#' @examples
#' # Example 1:
#' string <- mathString("550 ms", "480 ms", "-")
#'
#' # Example 2:
#' string <- mathString("2.34", "1.65", "+", numDigits = 2, unit = "mV")
#'
#' @export
mathString <- function(str1, str2, operation = "-",
                       numDigits = 0, unit = "ms") {

  extractNum <- function(x){
    return (as.numeric(regmatches(x, gregexpr("-*\\d+\\.*\\d*", x))))
  }

  nums <- lapply(list(str1, str2), extractNum)
  result <- do.call(operation, nums)

  return(numValueString(result, numDigits, unit))

}



#' @title numValueString
#'
#' @description Returns numerical value with requested unit in Latex format with numDigits
#' number of decimal places and unit symbol.
#'
#' @param value number
#' @param numDigits number 2 (default)
#' @param unit "ms", "mv", "mV", or "\%" or "" (default)
#'
#' @return character
#'
#' @examples
#' # Example 1:
#' string <- numValueString(100.341, 0, "ms")
#'
#' # Example 2:
#' string <- numValueString(2.3412, 2, "mv")
#'
#' # Example 3:
#' string <- numValueString(63.9812, 2, "")
#'
#' @export
numValueString <- function(value, numDigits = 2, unit = "") {

    value <- format(round(value, numDigits), nsmall = numDigits)
    if (unit %in% c("mv", "mV")) {
        return(paste0(value, " $\\mu$V"))
    } else if (unit == "ms") {
        return(paste0(value, " ms"))
    } else if (unit == "%") {
        return(paste0(value, " \\%"))
    } else if (unit == "") {
        return(paste0(value))
    } else {
        stop("Unit not recognized! Unit should be \"mv\", \"mV\", \"ms\" or \"%\".")
    }

}



#' @title pValueString
#'
#' @description Returns Latex formatted string from a p-value required for R/knitr integration.
#' For example, \emph{p} = 0.11 or \emph{p} < 0.01
#' Returns values to 3 sig decimal places or < .001
#'
#' @param pVal p-value between 0 and 1
#'
#' @return character
#'
#' @examples
#' # Example 1:
#' pString <- pValueString(0.670)
#'
#' # Example 2:
#' pString <- pValueString(0.1234)
#'
#' # Example 3:
#' pString <- pValueString("0.03")
#'
#' @export
pValueString <- function(pVal){

    if (is.character(pVal)) {
        pVal <- as.numeric(pVal)
        if (is.na(pVal)) {
            stop("Can't convert string to number!")
        }
    }

    if (pVal >= 0.001) {
        string <- paste0("\\emph{p} = ", sprintf("%.3f", pVal))
        string <- gsub("0\\.", ".", string)
    } else if (pVal < 0.001) {
        string <- paste0("\\emph{p}", " < .001")
    }

    return(string)

}



#' @title pValueSummary
#'
#' @description Returns p-values summarized using ***, **, *, or exact value
#' when \emph{p} > .05 (default 2 significant decimal places).
#'
#' @param pVal vector with p-value between 0 and 1
#'
#' @return character
#'
#' @examples
#' # Examples:
#' psum <- pValueSummary(0.0067)
#' psum <- pValueSummary(c(0.0001, 0.002, 0.02, 0.1))
#'
#' @export
pValueSummary <- function(pVal) {

    if (!is.numeric(pVal)) {
        stop("Input contains a non-number!")
    }

    psum <- ifelse(pVal < 0.001, "***",
                   ifelse(pVal < 0.01, "**",
                          ifelse(pVal <= 0.05, "*", "")))

    return(psum)

}



#' @title requiredPackages
#'
#' @description Installs (default if required) and loads specified packages.
#'
#' @param packages A list of packages
#' @param installPackages TRUE/FALSE Install package if not installed
#' @param lib character vector giving the library directories where to install the packages. Recycled as needed. If missing, defaults to the first element of .libPaths()
#' @param repos character vector, the base URL(s) of the repositories to use, e.g., the URL of a CRAN mirror such as "https://cloud.r-project.org". For more details on supported URL schemes see url. Can be NULL to install from local files, directories or URLs: this will be inferred by extension from pkgs if of length one.
#'
#' @return NULL
#'
#' @export
requiredPackages <- function(packages,
                             installPackages=FALSE,
                             lib = .libPaths()[1],
                             repos = "http://cran.us.r-project.org"){

    isPackageInstalled <- packages %in% rownames(utils::installed.packages())

    if (any(!isPackageInstalled) & installPackages) {
        isPackageAvailable <- packages %in% rownames(utils::available.packages(repos = repos))
        if (any(!isPackageAvailable)) {
            stop(paste0("Package ", packages[!isPackageAvailable], " not available!"))
        }
        utils::install.packages(packages[!isPackageInstalled],
                                lib = lib,
                                repos = repos,
                                dependencies = TRUE)
        isPackageInstalled <- packages %in% rownames(utils::installed.packages())
    } else if (any(!isPackageInstalled)) {
        stop(paste0("Package ", packages[!isPackageInstalled], " not installed!"))
    }

    lapply(packages[isPackageInstalled], library, character.only = TRUE)

}
