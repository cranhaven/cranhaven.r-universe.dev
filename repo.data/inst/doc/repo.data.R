## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(repos = c("@CRAN@" = "https://CRAN.R-project.org"))
with_internet <- as.logical(NROW(suppressWarnings(available.packages())))
library(repo.data)

## ----package_dependencies-----------------------------------------------------
pd <- package_dependencies("ggeasy")
head(pd)

## ----update_dependencies------------------------------------------------------
# Discover the requirements that can be upgraded
update_dependencies("ggeasy")

## ----package_date-------------------------------------------------------------
package_date("ggeasy")

## ----dup_alias, eval = !is.na(cran_alias(c("fect", "gsynth")))----------------
alias <- cran_alias(c("fect", "gsynth"))
dup_alias <- duplicated_alias(alias)
head(dup_alias)

## ----cran_help_pages----------------------------------------------------------
pkg <- "BaseSet"
head(cran_help_pages_wo_links(pkg))
head(cran_help_pages_not_linked(pkg))

## ----cran_help_cliques, eval=requireNamespace("igraph", quietly = TRUE) && with_internet----
cliques <- cran_help_cliques(pkg)
# Number of help pages connected
if (length(cliques) != 1) {
    table(cliques$n) 
}

## ----cran_help_pages_links_wo_deps, eval=with_internet------------------------
cran_help_pages_links_wo_deps(pkg)

## ----cran_snapshot, eval=with_internet----------------------------------------
cs <- cran_snapshot(as.Date("2020-01-31"))
nrow(cs)

## ----cran_sessions, eval=with_internet----------------------------------------
cran_session()

## ----cran_date, eval=with_internet--------------------------------------------
versions <- data.frame(Package = c("dplyr", "Rcpp", "rlang"),
                       Version = c("1.1.4", "0.8.9", NA))
cran_date(versions)

## ----cran_installed, eval=FALSE-----------------------------------------------
# cran_date(installed.packages())

## ----doom, eval=with_internet-------------------------------------------------
cd <- cran_doom(bioc = TRUE)
if (length(cd) != 1) {
    cd[c("time_till_last", "last_archived", "npackages")]
    knitr::kable(head(cd$details))
}

## ----sessions-----------------------------------------------------------------
sessionInfo()

