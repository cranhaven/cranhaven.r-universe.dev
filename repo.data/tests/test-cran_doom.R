library("repo.data")
cd <- cran_doom()
if (length(cd) == 1L && is.na(cd)) {
    q("no")
}
stopifnot(is.list(cd))
stopifnot(names(cd) == c("time_till_last", "last_archived", "npackages", "details"))
stopifnot(is.data.frame(cd$details))
stopifnot(names(cd$details) == c("Package", "Deadline", "type", "repo", "n_affected"))
wo_bioc <- nrow(cd$details)
stopifnot(as.logical(wo_bioc))
cd <- cran_doom(bioc = TRUE)
stopifnot(nrow(cd$details) >= wo_bioc)
