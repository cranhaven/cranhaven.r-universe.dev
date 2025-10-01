# Raw testing without using any package (testthat or tinytest) as described on WRE

library("repo.data")
opts <- options(repos = "https://CRAN.R-project.org")
on.exit(options(opts), add = TRUE)
# cran archive ####
rtweet <- cran_archive("rtweet")
if (length(rtweet) == 1L && is.na(rtweet)) {
    q("no")
}
stopifnot(nrow(rtweet) >= 15L)
# Repeat search for a different package to test for accessing the cache with a package currently on CRAN
BaseSet <- cran_archive("BaseSet")
    stopifnot(nrow(BaseSet) > 1L, nrow(BaseSet) < 200L)
# Test on a package without archive
ABACUS <- cran_archive("ABACUS")
    stopifnot(nrow(ABACUS) == 1L)

pkges <- c("BaseSet", "dplyr")
cpk <- cran_archive(pkges)
pkgs_out <- setdiff(pkges, cpk$package)
stopifnot(identical(pkgs_out, pkges))
cpk2 <- cran_archive(pkges)
stopifnot(identical(cpk, cpk2))

# cran alias ####
pkges <- c("BaseSet", "dplyr")
cpk <- cran_alias(pkges)
pkgs_out <- setdiff(pkges, cpk$package)
stopifnot(identical(pkgs_out, pkges))
cpk2 <- cran_alias(pkges)
stopifnot(identical(cpk, cpk2))

# cran links ####
pkges <- c("BaseSet", "dplyr")
cpk <- cran_links(pkges)
pkgs_out <- setdiff(pkges, cpk$package)
stopifnot(identical(pkgs_out, pkges))
cpk2 <- cran_links(pkges)
stopifnot(identical(cpk, cpk2))


# dependencies ####
pkges <- "BaseSet"
cpk <- package_dependencies(pkges)
stopifnot(ncol(cpk) == 5L)
pkgs_out <- setdiff(pkges, cpk$package)
stopifnot(identical(pkgs_out, pkges))
cpk2 <- package_dependencies(pkges)
stopifnot(identical(cpk, cpk2))

pd <- package_dependencies(c("ggeasy", "BaseSet"))
stopifnot(ncol(pd) == 5L)

# Snapshot ####
suppressWarnings(cs <- cran_snapshot(Sys.Date() -2 ))
stopifnot(NROW(cs) > 1000)

# check_packages
stopifnot(isTRUE((tryCatch(suppressWarnings(package_dependencies(character())), error = function(e){TRUE}))))

# Repos_dependencies
rd <- repos_dependencies(c("BaseSet", "dplyr", "rlang", "cli", "generics", "glue", "lifecycle",
"magrittr", "pillar", "R6", "tibble", "tidyselect", "vctrs",
"utf8", "pkgconfig", "withr"))
stopifnot(NROW(rd) > 1)

# Repos_dependencies
pd <- package_dependencies(c("BaseSet", "dplyr", "rlang", "cli", "generics", "glue", "lifecycle",
"magrittr", "pillar", "R6", "tibble", "tidyselect", "vctrs",
"utf8", "pkgconfig", "withr"))
stopifnot(NROW(rd) > 1)
