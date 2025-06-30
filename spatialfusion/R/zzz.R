.onAttach <- function(...) {
  #  pkgdesc <- utils::packageDescription("eggCounts", lib.loc = eggCountsLib)
  #  builddate <- gsub(';.*$', '', pkgdesc$Packaged)
  #  packageStartupMessage(paste("eggCounts (Version ", pkgdesc$Version, ")", sep = ""))
  packageStartupMessage(paste0("Loading spatialfusion (version ", utils::packageVersion("spatialfusion"),"):
- The compilation time for a Stan model can be up to 20s.
- INLA method is recommended for larger datasets (> 1000 observations).
- It is good practice to test the model specification on sub-sampled dataset first before running it on the full dataset."))
}
