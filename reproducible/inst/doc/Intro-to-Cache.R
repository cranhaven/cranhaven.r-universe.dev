## ----setup, include = FALSE---------------------------------------------------
SuggestedPkgsNeeded <- c("terra")
hasSuggests <- all(sapply(SuggestedPkgsNeeded, require, character.only = TRUE, quietly = TRUE))
useSuggests <- !(tolower(Sys.getenv("_R_CHECK_DEPENDS_ONLY_")) == "true")

knitr::opts_chunk$set(eval = hasSuggests && useSuggests)

## ----function-level, echo=TRUE------------------------------------------------
library(reproducible)
library(data.table)

tmpDir <- file.path(tempfile(), "reproducible_examples", "Cache")
dir.create(tmpDir, recursive = TRUE)

# Source raster with a complete LCC definition
ras <- terra::rast(terra::ext(0, 300, 0, 300), vals = 1:9e4, res = 1)
terra::crs(ras) <- "+proj=lcc +lat_1=60 +lat_2=70 +lat_0=50 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Target CRS in PROJ form (no EPSG lookup)
newCRS <- "+proj=longlat +datum=WGS84 +no_defs"

# Derive target extent from source extent (no registry lookup)
target_ext <- terra::project(terra::ext(ras), from = terra::crs(ras), to = newCRS)

# Build template with chosen resolution; assign CRS
tmplate <- terra::rast(target_ext, resolution = 0.00001)
terra::crs(tmplate) <- newCRS

# No Cache
system.time(map1 <- terra::project(ras, tmplate, method = "near"))

# Try with memoise for this example -- for many simple cases, memoising will not be faster
opts <- options("reproducible.useMemoise" = TRUE)
# With Cache -- a little slower the first time because saving to disk
system.time({
  suppressWarnings({
    map1 <- terra::project(ras, tmplate, method = "near") |> 
      Cache(cachePath = tmpDir)
  })
})

# faster the second time; improvement depends on size of object and time to run function
system.time({
  map2 <- terra::project(ras, tmplate, method = "near") |> 
    Cache(cachePath = tmpDir)
})

options(opts)

all.equal(map1, map2, check.attributes = FALSE) # TRUE

## -----------------------------------------------------------------------------
try(clearCache(tmpDir, ask = FALSE), silent = TRUE) # just to make sure it is clear

ranNumsA <- rnorm(10, 16) |> Cache(cachePath = tmpDir)

# All same
ranNumsB <- rnorm(10, 16) |> Cache(cachePath = tmpDir) # recovers cached copy
ranNumsD1 <- Cache(quote(rnorm(n = 10, 16))) |> Cache(cachePath = tmpDir) # recovers cached copy
ranNumsD2 <- Cache(rnorm(n = 10, 16)) |> Cache(cachePath = tmpDir) # recovers cached copy
# pipe
ranNumsD3 <- rnorm(n = 10, 16) |> Cache(cachePath = tmpDir) # recovers cached copy

# Any minor change makes it different
ranNumsE <- rnorm(10, 6) |> Cache(cachePath = tmpDir) # different

## ----tags---------------------------------------------------------------------
ranNumsA <- rnorm(4) |> Cache(cachePath = tmpDir, userTags = "objectName:a")
ranNumsB <- runif(4) |> Cache(cachePath = tmpDir, userTags = "objectName:b")

showCache(tmpDir, userTags = c("objectName"))
showCache(tmpDir, userTags = c("^a$")) # regular expression ... "a" exactly
showCache(tmpDir, userTags = c("runif")) # show only cached objects made during runif call

clearCache(tmpDir, userTags = c("runif"), ask = FALSE) # remove only cached objects made during runif call
showCache(tmpDir) # all

clearCache(tmpDir, ask = FALSE)

## ----accessed-tag-------------------------------------------------------------
ranNumsA <- rnorm(4) |> Cache(cachePath = tmpDir, userTags = "objectName:a")
ranNumsB <- runif(4) |> Cache(cachePath = tmpDir, userTags = "objectName:b")

# access it again, from Cache
Sys.sleep(1)
ranNumsA <- rnorm(4) |> Cache(cachePath = tmpDir, userTags = "objectName:a")
wholeCache <- showCache(tmpDir)

# keep only items accessed "recently" (i.e., only objectName:a)
onlyRecentlyAccessed <- showCache(tmpDir, userTags = max(wholeCache[tagKey == "accessed"]$tagValue))

# inverse join with 2 data.tables ... using: a[!b]
# i.e., return all of wholeCache that was not recently accessed
#   Note: the two different ways to access -- old way with "artifact" will be deprecated
toRemove <- unique(wholeCache[!onlyRecentlyAccessed, on = "cacheId"], by = "cacheId")$cacheId
clearCache(tmpDir, toRemove, ask = FALSE) # remove ones not recently accessed
showCache(tmpDir) # still has more recently accessed

## ----keepCache----------------------------------------------------------------
ranNumsA <- rnorm(4) |> Cache(cachePath = tmpDir, userTags = "objectName:a")
ranNumsB <- Cache(runif(4)) |> Cache(cachePath = tmpDir, userTags = "objectName:b")

# keep only those cached items from the last 24 hours
oneDay <- 60 * 60 * 24
keepCache(tmpDir, after = Sys.time() - oneDay, ask = FALSE)

# Keep all Cache items created with an rnorm() call
keepCache(tmpDir, userTags = "rnorm", ask = FALSE)
showCache(tmpDir)

# Remove all Cache items that happened within a rnorm() call
clearCache(tmpDir, userTags = "rnorm", ask = FALSE)
showCache(tmpDir) ## empty

# Also, can set a time before caching happens and remove based on this
#  --> a useful, simple way to control Cache
ranNumsA <- rnorm(4) |> Cache(cachePath = tmpDir, userTags = "objectName:a")
startTime <- Sys.time()
Sys.sleep(1)
ranNumsB <- rnorm(5) |> Cache(cachePath = tmpDir, userTags = "objectName:b")
keepCache(tmpDir, after = startTime, ask = FALSE) # keep only those newer than startTime

clearCache(tmpDir, ask = FALSE)

## ----searching-within-cache---------------------------------------------------
# default userTags is "and" matching; for "or" matching use |
ranNumsA <- runif(4) |> Cache(cachePath = tmpDir, userTags = "objectName:a")
ranNumsB <- rnorm(4) |> Cache(cachePath = tmpDir, userTags = "objectName:b")

# show all objects (runif and rnorm in this case)
showCache(tmpDir)

# show objects that are both runif and rnorm
# (i.e., none in this case, because objecs are either or, not both)
showCache(tmpDir, userTags = c("runif", "rnorm")) ## empty

# show objects that are either runif or rnorm ("or" search)
showCache(tmpDir, userTags = "runif|rnorm")

# keep only objects that are either runif or rnorm ("or" search)
keepCache(tmpDir, userTags = "runif|rnorm", ask = FALSE)

clearCache(tmpDir, ask = FALSE)

## ----expensive-computations---------------------------------------------------
ras <- terra::rast(terra::ext(0, 5, 0, 5),
  res = 1,
  vals = sample(1:5, replace = TRUE, size = 25),
  crs = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
)

rasCRS <- terra::crs(ras)
# Build an explicit target raster (same CRS, coarser resolution).
# Avoids the `terra::project(x, char_crs, res = N)` shorthand which
# recurses internally and forwards an unrecognized `wopt` set on
# recent terra versions, producing
# `[write] unknown option(s): xscale,yscale`.
rasTarget <- terra::rast(terra::ext(ras), crs = rasCRS, resolution = 5)

# A slow operation, like GIS operation
notCached <- suppressWarnings(
  # project raster generates warnings when run non-interactively
  terra::project(ras, rasTarget)
)

cached <- suppressWarnings(
  # project raster generates warnings when run non-interactively
  # using quote works also
  terra::project(ras, rasTarget) |> Cache(cachePath = tmpDir)
)

# second time is much faster
reRun <- suppressWarnings(
  # project raster generates warnings when run non-interactively
  terra::project(ras, rasTarget) |> Cache(cachePath = tmpDir)
)

# recovered cached version is same as non-cached version
all.equal(notCached, reRun, check.attributes = FALSE) ## TRUE

## ----nested-------------------------------------------------------------------
##########################
## Nested Caching
# Make 2 functions
inner <- function(mean) {
  d <- 1
  rnorm(n = 3, mean = mean)
}
outer <- function(n) {
  inner(0.1) |> Cache(cachePath = tmpdir2)
}

# make 2 different cache paths
tmpdir1 <- file.path(tempfile(), "first")
tmpdir2 <- file.path(tempfile(), "second")

# Run the Cache ... notOlderThan propagates to all 3 Cache calls,
#   but cachePath is tmpdir1 in top level Cache and all nested
#   Cache calls, unless individually overridden ... here inner
#   uses tmpdir2 repository
outer(n = 2) |> Cache(cachePath = tmpdir1)

showCache(tmpdir1) # 2 function calls
showCache(tmpdir2) # 1 function call

# userTags get appended
# all items have the outer tag propagate, plus inner ones only have inner ones
clearCache(tmpdir1, ask = FALSE)
outerTag <- "outerTag"
innerTag <- "innerTag"
inner <- function(mean) {
  d <- 1
  rnorm(n = 3, mean = mean) |> Cache(notOlderThan = Sys.time() - 1e5, userTags = innerTag)
}
outer <- function(n) {
  inner(0.1) |> Cache()
}
aa <- Cache(outer, n = 2) |> Cache(cachePath = tmpdir1, userTags = outerTag)
showCache(tmpdir1) # rnorm function has outerTag and innerTag, inner and outer only have outerTag

## ----selective-cacheId--------------------------------------------------------
### cacheId
set.seed(1)
rnorm(1) |> Cache(cachePath = tmpdir1)
# manually look at output attribute which shows cacheId: 422bae4ed2f770cc
rnorm(1) |> Cache(cachePath = tmpdir1, cacheId = "422bae4ed2f770cc") # same value
# override even with different inputs:
rnorm(2) |> Cache(cachePath = tmpdir1, cacheId = "422bae4ed2f770cc")

## ----manual-cache-------------------------------------------------------------
# As of reproducible version 1.0, there is a new backend directly using DBI
mapHash <- unique(showCache(tmpDir, userTags = "project")$cacheId)
map <- loadFromCache(mapHash[1], cachePath = tmpDir)
terra::plot(map)

## ----cleanup------------------------------------------------------------------
## cleanup
unlink(dirname(tmpDir), recursive = TRUE)

