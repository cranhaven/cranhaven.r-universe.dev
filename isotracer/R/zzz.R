### * .onAttach()

# Inspired by rstan code from
# https://github.com/stan-dev/rstan/blob/develop/rstan/rstan/R/zzz.R

.onAttach <- function(...) {
    packageStartupMessage("To automatically run isotracer in parallel ",
                          "on a multicore CPU, you can call:\n",
                          "  options(mc.cores = parallel::detectCores())\n")
}

### * .onUnload()

# Following recommendations from https://r-pkgs.org/src.html

.onUnload <- function(libpath) {
    library.dynam.unload("isotracer", libpath)
}

### * release_questions()

# Cf. https://r-pkgs.org/release.html#release-submission
release_questions <- function() {
  c("Have you rebuilt the precompiled vignettes manually recently?",
    "Have you updated the CRAN badge by running `.download-cran-version-badge.R`?")
}
