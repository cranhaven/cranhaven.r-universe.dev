#' Downloads distance matrices
#' 28 animals' data including 24 normal and 4 noisy are retrieved from figshare.
#' @param distance "mSBD" or "Euclid" can be specified
#' @param qc "PASS" or "WARN" or "FAIL" can be specified. "PASS" downloads
#' 24 data except 4 noisy data. "WARN" downloads 27 data except 1 noisy data.
#' "FAIL" downloads all 28 data.
#' @return A List of containing distance matrices. The list also includes
#' metadata for each animals.
#' @examples
#' \donttest{
#'     Ds_Euclid <- worm_download("Euclid", qc = "WARN")
#'     Ds_mSBD <- worm_download("mSBD", qc = "PASS")
#' }
#' @importFrom usedist dist_subset
#' @importFrom utils download.file
#' @importFrom utils read.csv
#' @export
worm_download <- function(distance = c("mSBD", "Euclid"),
                          qc = c("PASS", "WARN", "FAIL")) {
    # Argument Check
    distance <- match.arg(distance)
    qc <- match.arg(qc)
    # Distance matrices
    Ds <- NULL
    temp_dl_path <- tempdir()
    tempfile1 <- file.path(temp_dl_path, "Ds.RData")
    if (distance == "mSBD") {
        if(.Platform$OS.type == "windows") {
            # download for windows
            download.file("https://ndownloader.figshare.com/files/35963780",
                          tempfile1,
                          mode="wb")
        } else {
            # download for unix
            download.file("https://ndownloader.figshare.com/files/35963780",
                          tempfile1)
        }
    } else if (distance == "Euclid") {
        if(.Platform$OS.type == "windows") {
            # download for windows
            download.file("https://ndownloader.figshare.com/files/35963777",
                          tempfile1,
                          mode="wb")
        } else {
            # download for unix
            download.file("https://ndownloader.figshare.com/files/35963777",
                          tempfile1)
        }
    } else {
        stop("Please specify distance as 'mSBD' or 'Euclid'!")
    }
    load(tempfile1)
    if (qc == "PASS") {
        idx <- which(.qcresult %in% c("PASS"))
    }
    if (qc == "WARN") {
        idx <- which(.qcresult %in% c("PASS", "WARN"))
    }
    if (qc == "FAIL") {
        idx <- seq(28)
    }
    # Labels
    tempfile2 <- file.path(temp_dl_path, "labels.csv")
    if(.Platform$OS.type == "windows") {
        # download for windows
        download.file("https://ndownloader.figshare.com/files/36186483",
                      tempfile2,
                      mode="wb")
    } else {
        # download for unix
        download.file("https://ndownloader.figshare.com/files/36186483",
                      tempfile2)
    }
    labels <- read.csv(tempfile2)
    # Output
    Ds_f <- lapply(Ds, .filter_cellnames)
    list(Ds = Ds_f[idx], labels = labels)
}

.qcresult <- rep("", 28)
.qcresult[c(1:2, 4:7, 9:19, 21:24, 26:28)] <- "PASS"
.qcresult[c(3, 8, 25)] <- "WARN"
.qcresult[c(20)] <- "FAIL"
.filter_cellnames <- function(X) {
    D <- X
    D_cell <- attr(D, "Labels")
    D_cell_f <- D_cell[grep("^[0-9]", D_cell, invert = TRUE)]
    D_f <- dist_subset(D, D_cell_f)
    D_f
}
