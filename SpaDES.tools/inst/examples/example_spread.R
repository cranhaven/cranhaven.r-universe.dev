library(terra)
origDTThreads <- data.table::setDTthreads(2L)
origNcpus <- options(Ncpus = 2L)

# Make random forest cover map
set.seed(123)
emptyRas <- rast(ext(0, 1e2, 0, 1e2), res = 1)
hab <- randomPolygons(emptyRas, numTypes = 40)
names(hab) <- "hab"
mask <- rast(emptyRas)
values(mask) <- 0
mask[1:5000] <- 1
numCol <- ncol(emptyRas)
numCell <- ncell(emptyRas)
directions <- 8

# Can use transparent as a colour
coltab(hab) <- paste(c("transparent", grey(0:40/40)))

terra::plot(hab)

# initiate 10 fires
startCells <- as.integer(sample(1:ncell(emptyRas), 100))
fires <- spread(hab, loci = startCells, 0.235, persistence = 0, numNeighs = 2,
                mask = NULL, maxSize = 1e8, directions = 8, iterations = 1e6, id = TRUE)

terra::plot(hab, type = "classes", legend = FALSE)
fires[fires == 0] <- NA
terra::plot(fires, add = TRUE, col = "red", type = "continuous", legend = FALSE)

# Instead, to give a colour to the zero values, use \code{zero.color=}
coltab(fires) <- NULL
# need to specify "type" to get correct legend
terra::plot(fires,  col = c(colorRampPalette(c("blue", "green"))(100)),
            type = "continuous")

##------------------------------------------------------------------------------
## Continue event by passing interrupted object into spreadState
##------------------------------------------------------------------------------

## Interrupt a spread event using iterations - need `returnIndices = TRUE` to
##  use outputs as new inputs in next iteration
fires <- spread(hab, loci = as.integer(sample(1:ncell(hab), 10)),
                returnIndices = TRUE, 0.235, 0, NULL, 1e8, 8, iterations = 3, id = TRUE)
fires[, list(size = length(initialLocus)), by = id]  # See sizes of fires

fires2 <- spread(hab, loci = NA_real_, returnIndices = TRUE, 0.235, 0, NULL,
                 1e8, 8, iterations = 2, id = TRUE, spreadState = fires)
# NOTE events are assigned arbitrary IDs, starting at 1


## Use data.table and loci...
fires <- spread(hab, loci = as.integer(sample(1:ncell(hab), 10)),
                returnIndices = TRUE, 0.235, 0, NULL, 1e8, 8,
                iterations = 2, id = TRUE)
fullRas <- rast(hab)
fullRas[] <- 1:ncell(hab)
burned <- fires[active == FALSE]
burnedMap <- rasterizeReduced(burned, fullRas, "id", "indices")

terra::plot(burnedMap, type = "classes")

####################
## stopRule examples
####################

# examples with stopRule, which means that the eventual size is driven by the values on the raster
#  passed in to the landscape argument. It won't be exact because the pixel values
#  will likely not allow it
stopRule22 <- function(landscape) sum(landscape) > 100

set.seed(1234)
stopRule1 <- function(landscape) sum(landscape) > 50
stopRuleA <- spread(hab, loci = as.integer(sample(1:ncell(hab), 10)), 1, 0, NULL,
                    maxSize = 1e6, 8, 1e6, id = TRUE, circle = TRUE, stopRule = stopRule1,
                    stopRuleBehavior = "excludePixel")
tapply(hab[], stopRuleA[], sum) # all below 50

set.seed(1234)
# using stopRuleBehavior = "excludePixel"
stopRuleB <- spread(hab, loci = as.integer(sample(1:ncell(hab), 10)), 1, 0, NULL,
                    maxSize = 1e6, 8, 1e6, id = TRUE, circle = TRUE, stopRule = stopRule22,
                    stopRuleBehavior = "excludePixel")
tapply(hab[], stopRuleB[], sum) # all below 100

if (interactive())
  terra::plot(c(stopRuleA, stopRuleB))
# Cellular automata shapes
# Diamonds - can make them with: a boolean raster, directions = 4,
#    stopRule in place, spreadProb = 1
diamonds <- spread(hab > 0, spreadProb = 1, directions = 4, id = TRUE, stopRule = stopRule22)

terra::plot(diamonds)

# Squares - can make them with: a boolean raster, directions = 8,
#    stopRule in place, spreadProb = 1
squares <- spread(hab > 0, spreadProb = 1, directions = 8, id = TRUE, stopRule = stopRule22)
terra::plot(squares)

# Interference shapes - can make them with: a boolean raster, directions = 8,
#    stopRule in place, spreadProb = 1
stopRule2 <- function(landscape) sum(landscape) > 200
squashedDiamonds <- spread(hab > 0, spreadProb = 1,
                           loci = (ncell(hab) - ncol(hab)) / 2 + c(4, -4),
                           directions = 4, id = TRUE, stopRule = stopRule2)
terra::plot(squashedDiamonds)

# Circles with spreadProb < 1 will give "more" circular shapes, but definitely not circles
stopRule2 <- function(landscape) sum(landscape) > 200
seed <- sample(1e4, 1)
set.seed(seed)

circlish <- spread(hab > 0, spreadProb = 1, iterations = 10,
                   loci = (ncell(hab) - ncol(hab)) / 2 + c(4, -4),
                   directions = 8, id = TRUE, circle = TRUE)#, stopRule = stopRule2)
if (interactive())
  terra::plot(c(circlish))

set.seed(seed)
regularCA <- spread(hab > 0, spreadProb = 1, iterations = 10,
                    loci = (ncell(hab) - ncol(hab)) / 2 + c(4, -4),
                    directions = 8, id = TRUE)#, stopRule = stopRule2)

if (interactive()) # compare to circlish
  terra::plot(regularCA)

####################
# complex stopRule
####################

initialLoci <- sample(seq_len(ncell(hab)), 2)
endSizes <- seq_along(initialLoci) * 200

# Can be a function of landscape, id, and/or any other named
#   variable passed into spread
stopRule3 <- function(landscape, id, endSizes) sum(landscape) > endSizes[id]

set.seed(1)
twoCirclesDiffSize <- spread(hab, spreadProb = 1, loci = initialLoci,
                             circle = TRUE, directions = 8, id = TRUE,
                             stopRule = stopRule3, endSizes = endSizes,
                             stopRuleBehavior = "excludePixel")

# or using named list of named elements:
set.seed(1)
twoCirclesDiffSize2 <- spread(hab, spreadProb = 1, loci = initialLoci,
                              circle = TRUE, directions = 8, id = TRUE,
                              stopRule = stopRule3,
                              vars = list(endSizes = endSizes),
                              stopRuleBehavior = "excludePixel")

compareGeom(twoCirclesDiffSize, twoCirclesDiffSize2, res = TRUE,
            stopOnError = FALSE)

terra::plot(c(twoCirclesDiffSize , twoCirclesDiffSize2))

cirs <- values(twoCirclesDiffSize)
vals <- tapply(hab[][cirs > 0], cirs[cirs > 0], sum) # one is <200, other is <400 as per endSizes

# Stop if sum of landscape is big or mean of quality is too small
quality <- rast(hab)
quality[] <- runif(ncell(quality), 0, 1)
stopRule4 <- function(landscape, quality, cells) {
  (sum(landscape) > 20) | (mean(values(quality)[cells]) < 0.3)
}

twoCirclesDiffSize <- spread(hab, spreadProb = 1, loci = initialLoci, circle = TRUE,
                             directions = 8, id = TRUE, stopRule = stopRule4,
                             quality = quality, stopRuleBehavior = "excludePixel")


## Using alternative algorithm, not probabilistic diffusion
## Will give exactly correct sizes, yet still with variability
## within the spreading (i.e., cells with and without successes)
seed <- sample(1e6, 1)
set.seed(seed)
startCells <- startCells[1:4]
maxSizes <- rexp(length(startCells), rate = 1 / 500)
fires <- spread(hab, loci = startCells, 1, persistence = 0,
                neighProbs = c(0.5, 0.5, 0.5) / 1.5,
                mask = NULL, maxSize = maxSizes, directions = 8,
                iterations = 1e6, id = TRUE, plot.it = FALSE, exactSizes = TRUE)
all(table(fires[fires > 0][]) == floor(maxSizes))

terra::plot(fires)
hist(fires[][fires[] > 0], main = "fire size distribution")

## Example with relativeSpreadProb ... i.e., a relative probability spreadProb
##  (shown here because because spreadProb raster is not a probability).
##  Here, we force the events to grow, choosing always 2 neighbours,
##  according to the relative probabilities contained on hab layer.
##
## Note: `neighProbs = c(0,1)` forces each active pixel to move to 2 new pixels
## (`prob = 0` for 1 neighbour, `prob = 1` for 2 neighbours)
##
## Note: set hab3 to be very distinct probability differences, to detect spread
##  differences
hab3 <- (hab < 20) * 200 + 1
seed <- 643503
set.seed(seed)
sam <- sample(which(hab3[] == 1), 1)
set.seed(seed)
events1 <- spread(hab3, spreadProb = hab3, loci = sam, directions = 8,
                  neighProbs = c(0, 1), maxSize = c(70), exactSizes = TRUE)

# Compare to absolute probability version
set.seed(seed)
events2 <- spread(hab3, id = TRUE, loci = sam, directions = 8,
                  neighProbs = c(0, 1), maxSize = c(70), exactSizes = TRUE)

terra::plot(events1)

terra::plot(events2, col = c("white", "red", "red"))

hist(events1[], breaks = 30, main = "Event size distribution") ## TODO: fix this plot
# Compare outputs -- should be more high value hab pixels spread to in event1
#  (randomness may prevent this in all cases)
sum(hab3[events1[] > 0]) >= sum(hab3[events2[] > 0]) ## should be usually TRUE

# clean up
data.table::setDTthreads(origDTThreads)
options(Ncpus = origNcpus)
