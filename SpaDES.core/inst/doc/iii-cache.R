## ----setup, include=FALSE-----------------------------------------------------
# Packages used in this vignette
vignette_pkgs <- c("knitr", "RColorBrewer", "SpaDES.tools")

# Check availability without attaching
has_pkgs <- function(pkgs) all(vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1)))

# CRAN sets NOT_CRAN = "false"; devtools sets it to "true" locally
not_cran <- identical(Sys.getenv("NOT_CRAN"), "true")

# Evaluate chunks only if NOT_CRAN and all vignette packages are available
knitr::opts_chunk$set(
  eval = not_cran && has_pkgs(vignette_pkgs),
  message = FALSE,
  warning = FALSE
)

options(
  spades.moduleCodeChecks = FALSE,
  spades.useRequire = FALSE
)


## ----examples, echo=TRUE, message=FALSE---------------------------------------
# library(terra)
# library(reproducible)
# library(SpaDES.core)
# 
# mySim <- simInit(
#   times = list(start = 0.0, end = 3.0),
#   params = list(
#     .globals = list(stackName = "landscape", burnStats = "testStats"),
#     randomLandscapes = list(.plotInitialTime = NA),
#     fireSpread = list(.plotInitialTime = NA)
#   ),
#   modules = list("randomLandscapes", "fireSpread"),
#   paths = list(modulePath = getSampleModules(tempdir()))
# )

## ----spades-------------------------------------------------------------------
# # compare caching ... run once to create cache
# system.time({
#   outSim <- spades(Copy(mySim), cache = TRUE, notOlderThan = Sys.time())
# })

## ----spades-cached------------------------------------------------------------
# # faster 2nd time
# system.time({
#   outSimCached <- spades(Copy(mySim), cache = TRUE)
# })
# all.equal(outSim, outSimCached)

## ----module-level, echo=TRUE--------------------------------------------------
# # Module-level
# params(mySim)$randomLandscapes$.useCache <- TRUE
# system.time({
#   randomSim <- spades(Copy(mySim), .plotInitialTime = NA,
#                       notOlderThan = Sys.time(), debug = TRUE)
# })
# 
# # faster the second time
# system.time({
#   randomSimCached <- spades(Copy(mySim), .plotInitialTime = NA, debug = TRUE)
# })

## ----test-module-level--------------------------------------------------------
# layers <- list("DEM", "forestAge", "habitatQuality", "percentPine", "Fires")
# same <- lapply(layers, function(l) {
#   identical(randomSim$landscape[[l]], randomSimCached$landscape[[l]])
# })
# names(same) <- layers
# print(same) # Fires is not same because all non-init events in fireSpread are not cached

## ----event-level, echo=TRUE---------------------------------------------------
# params(mySim)$fireSpread$.useCache <- "init"
# system.time({
#   randomSim <- spades(Copy(mySim), .plotInitialTime = NA,
#                       notOlderThan = Sys.time(), debug = TRUE)
# })
# 
# # faster the second time
# system.time({
#   randomSimCached <- spades(Copy(mySim), .plotInitialTime = NA, debug = TRUE)
# })

## ----function-level, echo=TRUE------------------------------------------------
# obj <- system.time({rnorm(3e6) |> mean()} |> Cache())

## ----manual-cache-------------------------------------------------------------
# ## cache something, then retrieve it manually rather than re-running Cache()
# Cache(rnorm(10), cachePath = cachePath(mySim), userTags = "manualExample")
# cacheDB <- showCache(mySim, userTags = "manualExample")
# loadFromCache(cacheId = cacheDB$cacheId, cachePath = cachePath(mySim))

## ----eval=FALSE, echo=TRUE----------------------------------------------------
# simInit() --> many .inputObjects calls
# 
# spades() call --> many module calls --> many event calls --> many function calls

## ----eval=FALSE, echo=TRUE----------------------------------------------------
# parameters = list(
#   FireModule = list(.useCache = TRUE)
# )
# mySim <- simInit(..., params = parameters)
# mySimOut <- spades(mySim)

