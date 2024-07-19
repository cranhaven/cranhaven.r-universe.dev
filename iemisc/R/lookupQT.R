#' Hash Table/Dictionary Lookup
#'  
#' These functions were originally contained in "qdapTools" version 1.3.4
#' \code{lookup} - \href{http://datatable.r-forge.r-project.org/}{\pkg{data.table}} 
#' based hash table useful for large vector lookups.
#' 
#' @param terms A vector of terms to undergo a lookup.
#' @param key.match Takes one of the following: (1) a two column data.frame of a 
#' match key and reassignment column, (2) a named list of vectors (Note: if 
#' data.frame or named list supplied no key reassign needed) or (3) a single 
#' vector match key.
#' @param key.reassign A single reassignment vector supplied if key.match is 
#' not a two column data.frame/named list.
#' @param missing Value to assign to terms not matching the key.match.  If set 
#' to \code{NULL} the original values in \code{terms} corresponding to the 
#' missing elements are retained.
#' @return Outputs A new vector with reassigned values.
#' @author Tyler Rinker ('qdapTools' package version 1.3.4)
#' @export

# modified from lookup from qdapTools by Tyler Rinker
lookupQT <- function (terms, key.match, key.reassign = NULL, missing = NA) {
	
    key.match <- data.frame(x=key.match[, 1], y=key.match[,2])

    if (is.factor(key.match[, 2])) {
        key.match[, 2] <- as.character(key.match[, 2])
        FUN <- as.factor
    } else {
        FUN <- match.fun(paste0("as.", mode(key.match[, 2])))
    }

    output <- lookup_helperQT(terms, key.match, missing)

    if(attributes(output)[["missing"]]) return(FUN(output))

    out_warn <- tryCatch({
        FUN(output)
    }, warning = function(w) {
        TRUE
    }, finally = {
        FALSE
    })
	
    if(length(out_warn) == 1 && !isTRUE(out_warn)) return(FUN(output))
	
    attributes(output) <- NULL
    output

}


#' @importFrom data.table setkey setDT
lookup_helperQT <- function(terms, key, missing = NA) {

	x <- i.y <- NULL
	
    terms <- data.frame(x=terms)
    key <- data.table(key[c("x", "y")])
    setDT(terms)
 
    setkey(key, x)
    out <- key[terms][[2]]
    attributes(out) <- list(missing = TRUE)
	
    if (!is.null(missing) && is.na(missing)) return(out)
    if (!is.null(missing) && !is.na(missing)) {
        hits <- which(is.na(out))
        out[hits] <- missing
        return(out)
    }

    if (is.null(missing)) {
        hits <- which(is.na(out))
        out[hits] <- terms[[1]][hits]
        attributes(out) <- list(missing = FALSE)
        return(out)
    }

}
