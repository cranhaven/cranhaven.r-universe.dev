library("repo.data")
pkg <- "ggeasy"
pd <- package_dependencies(pkg)
if (length(pd) == 1L && is.na(pd)) {
    q("no")
}
ud <- update_dependencies(pkg)
stopifnot(as.logical(length(ud)))
stopifnot(NROW(ud) <= NROW(pd))

pkg <- "teal"
rd <- repos_dependencies(pkg)
pd <- package_dependencies(pkg)
diff <- merge(pd, rd, by = "Name")

ud <- suppressWarnings(update_dependencies(pkg))
m <- merge(ud, pd, all = FALSE)
stopifnot("Packages that don't need updating show up on update_dependencies" = NROW(m) <= NROW(diff))
