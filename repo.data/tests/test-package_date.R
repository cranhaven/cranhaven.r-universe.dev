library("repo.data")

# Test deps without version number
pd <- package_date("ggeasy")
if (length(pd) == 1L && is.na(pd)) {
    q("no")
}
stopifnot(length(pd) == 2L)
stopifnot(names(pd) == c("Published", "deps_available"))
# Test with a mix of version numbers and no versions
ud <- package_date("usethis")
stopifnot(as.logical(length(ud)))
