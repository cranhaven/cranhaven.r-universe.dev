
## 
## INTERNAL FUNCTION cs() for case sensitive text in cln()
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## version 0.0.2 (14-06-2022)
##
## PARAMETERS
## xz       (list, vector, or data frame with characters)
## level    (clean level, 0 for no-clean, default 1 or 2 with 'what')
## case     (1 for 1st uppercase, 2 lower, 3 upper)
## flgdf    (logical, dataframe?)
## na.rm    (logical, remove NAs?)


cs <-
function (xz, level = 1, case = 1, flgdf = FALSE, na.rm = FALSE) 
{
    if (isTRUE(flgdf == TRUE) == TRUE) {
        xzl <- xz
        if (isTRUE(case == 1L) == TRUE) {
            xzl[] <- lapply(xz, function(z) {
                gsub("(^[[:alpha:]])", "\\U\\1", z, perl = TRUE)
            })
            ifelse(isTRUE(level > 1) == TRUE, xzl[] <- lapply(xz, 
                function(z) {
                  gsub("\\b([[:lower:]])([[:lower:]]+)", "\\U\\1\\L\\2", 
                    z, perl = TRUE)
                }), NA)
        }
        else if (isTRUE(case == 2L) == TRUE) {
            xzl[] <- lapply(xz, tolower)
        }
        else if (isTRUE(case == 3L) == TRUE) {
            xzl[] <- lapply(xz, toupper)
        }
        xz <- as.data.frame(matrix(unlist(xzl), ncol = ncol(xz), 
            byrow = FALSE, dimnames = list(rownames(xz), colnames(xz))))
    }
    else {
        if (isTRUE(case == 1L) == TRUE) {
            xz <- lapply(xz, function(z) {
                gsub("(^[[:alpha:]])", "\\U\\1", z, perl = TRUE)
            })
        }
        else if (isTRUE(case == 2L) == TRUE) {
            xz <- lapply(xz, tolower)
        }
        else if (isTRUE(case == 3L) == TRUE) {
            xz <- lapply(xz, toupper)
        }
        xz <- unlist(xz, use.names = FALSE)
    }
    if (isTRUE(na.rm == TRUE) == TRUE) {
        ifelse(isTRUE(level > 1) == TRUE, xz[xz == "NA"] <- NA, 
            invisible(NA))
        ifelse(isTRUE(flgdf == TRUE) == TRUE, xz <- xz[!apply(xz, 
            1, function(z) all(is.na(z))), ], xz <- Filter(function(y) !all(is.na(y)), 
            xz))
    }
    else {
        ifelse(isTRUE(level > 0) == TRUE, xz[xz == ""] <- NA, 
            invisible(NA))
    }
    return(xz)
}
