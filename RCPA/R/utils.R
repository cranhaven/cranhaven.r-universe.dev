#' @title Check if a package is installed and install it if not
#' @description This function checks if a package is installed and install it if not.
#' This function is internally used by the package.
#' @param pkg The name of the package to be loaded.
#' @return TRUE if the package is installed, FALSE otherwise.
#' @importFrom BiocManager install
#' @importFrom rlang is_interactive interrupt
#' @noRd
.requirePackage <- function(pkg) {
    pkgEnv$isMissingDependency <- FALSE
    if (!(pkg %in% .packages(all.available = TRUE)))
    {
        # Try to install the package
        if (rlang::is_interactive()){
            BiocManager::install(pkg, update = FALSE, ask = TRUE)
            if (pkg %in% .packages(all.available = TRUE)) {
                message("Package ", pkg, " is installed.")
            } else {
                warning("Package ", pkg, " is not installed. Please install it first.\n")
                pkgEnv$isMissingDependency <- TRUE
                rlang::interrupt()
                return(FALSE)
            }
        } else {
            warning("Package ", pkg, " is not installed. Please install it first.\n")
            pkgEnv$isMissingDependency <- TRUE
            return(FALSE)
        }
    }

    require(pkg, character.only = TRUE, quietly = TRUE)
    return(TRUE)
}

#' @title extract grouping information from design matrix and contrast matrix
#' @description This function extracts grouping information from design matrix and contrast matrix.
#' This function is used internally.
#' @param design The design matrix.
#' @param contrast The contrast matrix.
#' @return A list with two elements: group and pair.
#' group is a vector of group information.
#' pair is a vector of pair information.
#' @importFrom dplyr %>%
#' @noRd
.extractPairInfo <- function(design, contrast) {
    groupMat <- design[, names(contrast[contrast[, 1] != 0,]), drop = FALSE]
    group <- rep(NA, nrow(groupMat))
    names(group) <- rownames(groupMat)
    for (i in seq_len(ncol(groupMat))) {
        group[groupMat[, i] == 1] <- colnames(groupMat)[i]
    }
    group <- group[!is.na(group)]
    pairMat <- design[names(group), names(contrast[contrast[, 1] == 0,]), drop = FALSE]
    vars <- names(attr(design, "contrasts"))

    candidatePairs <- lapply(vars, function(v) {
        pMat <- pairMat[, colnames(pairMat) %>% str_starts(v), drop = FALSE]

        if (ncol(pMat) != length(group)/2 - 1) {
            return(NULL)
        }

        if (any(colSums(pMat) != length(unique(group)))) {
            return(NULL)
        }

        pairs <- rep("pair1", nrow(pMat))
        for (i in seq_len(ncol(pMat))) {
            pairs[pMat[, i] == 1] <- colnames(pMat)[i]
        }

        pairs
    }) %>% `[`(!sapply(., is.null))

    if (length(candidatePairs) > 1) {
        warning("More than one pair information is found in the model matrix.")
    }

    list(
        group = as.numeric(as.factor(group)) %>% setNames(names(group)),
        pair = (if (length(candidatePairs) == 0) {
            NULL
        } else {
            as.numeric(as.factor(candidatePairs[[1]])) %>% setNames(names(group))
        })
    )
}

#' @title Check URL is ok
#' @description This function checks whether the URL is available
#' @param url The URL to the internet source
#' @return TRUE is the URL is available, FALSE otherwise
#' @importFrom httr http_error
#' @importFrom rlang interrupt
#' @noRd

.checkURLAvailable <- function(url) {
  toCheck <- try({httr::http_error(url)}, silent = TRUE)
  if (inherits(toCheck, "try-error") || toCheck) {
    warning("The data source is temporarily unvailable. Please try it again later or contact the maintainer(s) to solve this issue.")
    rlang::interrupt()
  }
}

#' @title Load data from GitHub
#' @description This function loads data from GitHub.
#' @param name The name of the data.
#' @return Load the data with the specified name.
#' @examples
#' 
#' library(RCPA)
#' RNASeqDataset <- loadData("RNASeqDataset")
#' 
#' 
#' @export
loadData <- function(name){
      
     oldTimeout <- options("timeout")
     on.exit({options(timeout = oldTimeout)})
     options(timeout = 3600)
     
     # data <- load(gzcon(url(paste0("https://raw.githubusercontent.com/tinnlab/RCPA/main/.data/", name, ".rda"))))
     # get(data)
     
     toUseURL <- paste0("https://raw.githubusercontent.com/tinnlab/RCPA/main/.data/", name, ".rda")
     
     # toCheck <- try({httr::http_error(toUseURL)}, silent = TRUE)
     # toCheck <- crul::ok(toUseURL)
     .checkURLAvailable(toUseURL)
     
     data <- load(gzcon(url(toUseURL)))
     get(data)
     
     
     
}
