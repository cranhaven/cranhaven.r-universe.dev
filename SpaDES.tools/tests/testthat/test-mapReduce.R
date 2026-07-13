test_that("mapReduce: file does not work correctly 1", {
  testInit("terra")
  rastDF <- needTerraAndRaster()

  for (ii in seq_len(NROW(rastDF))) {
    pkg <- rastDF$pkg[ii]
    cls <- rastDF$class[ii]
    read <- eval(parse(text = rastDF$read[ii]))
    extFun <- eval(parse(text = rastDF$ext[ii]))

    ras <- read(extFun(0, 15, 0, 15), res = 1)
    ras[] <- NA

    set.seed(123)
    fullRas <- randomPolygons(ras, numTypes = 2)
    names(fullRas) <- "mapcodeAll"
    uniqueComms <- as.vector(unique(fullRas[]))
    reducedDT <- data.table(
      mapcodeAll = as.integer(uniqueComms),
      communities = sample(1:1000, length(uniqueComms)),
      biomass = as.integer(rnbinom(length(uniqueComms), mu = 4000, 0.4))
    )

    biomass <- rasterizeReduced(reducedDT, fullRas, "biomass")
    expect_equal(sort(as.vector(unique(biomass[]))), sort(reducedDT$biomass))

    communities <- rasterizeReduced(reducedDT, fullRas, "communities")
    expect_equal(sort(as.vector(unique(communities[]))), sort(reducedDT$communities))

    expect_true(sum(table(sort(fullRas[])) * reducedDT$communities) == sum(communities[]))

    ## test factor raster
    cls <- data.frame(id = sort(unique(as.vector(fullRas[]))))
    cls$Bclass <- LETTERS[cls$id]
    clsDT <- as.data.table(cls)

    if (is(fullRas, "SpatRaster")) {
      levels(fullRas) <- cls
    } else {
      fullRas <- raster::as.factor(fullRas)
      levs <- levels(fullRas)[[1]]
      levs$Bclass <- clsDT[id %in% unique(fullRas[]), "Bclass"]

      levels(fullRas) <- levs
    }
    reducedDT <- reducedDT[clsDT, on = "mapcodeAll==id"]
    reducedDT[, mapcodeAll := Bclass]

    biomass2 <- rasterizeReduced(reducedDT, fullRas, "biomass")
    if (is(biomass, "Raster")) {
      raster::compareRaster(biomass, biomass2, extent = TRUE, rowcol = TRUE, crs = TRUE, res = TRUE,
                            orig = TRUE, rotation = TRUE, values = TRUE, stopiffalse = FALSE)
    } else {
      terra::compareGeom(biomass, biomass2, crs = TRUE, ext = TRUE, rowcol = TRUE, res = TRUE,
                         stopOnError = FALSE)
    }
  }
})

test_that("mapReduce: file does not work correctly 2", {
  testInit("terra")
  rastDF <- needTerraAndRaster()
  rasOrig <- terra::rast(terra::ext(0, 15, 0, 15), resolution = 1)
  set.seed(321) # random fails here ... trying set.seed as a solution

  for (ii in seq_len(NROW(rastDF))) {
    pkg <- rastDF$pkg[ii]
    cls <- rastDF$class[ii]
    read <- eval(parse(text = rastDF$read[ii]))
    extFun <- eval(parse(text = rastDF$extent[ii]))

    ras <- read(rasOrig)
    fullRas <- randomPolygons(ras, numTypes = 5)
    names(fullRas) <- "mapcodeAll"
    uniqueComms <- as.numeric(unique(fullRas[]))
    reducedDT <- data.table(
      mapcodeAll = uniqueComms,
      communities = sample(1:1000, length(uniqueComms)),
      biomass = rnbinom(length(uniqueComms), mu = 4000, 0.4)
    )
    biomass <- rasterizeReduced(reducedDT, fullRas, "biomass")

    expect_equal(sort(unique(as.numeric(terra::values(biomass)))), sort(reducedDT$biomass))
    expect_equal(length(unique(as.numeric(terra::values(biomass)))),
                 length(unique(as.numeric(terra::values(fullRas)))))

    setkey(reducedDT, biomass)
    communities <- rasterizeReduced(reducedDT, fullRas, "communities")
    expect_equal(sort(unique(as.numeric(terra::values(communities)))), sort(reducedDT$communities))
    expect_equal(length(unique(as.numeric(terra::values(communities)))),
                 length(unique(as.numeric(terra::values(fullRas)))))
  }
})
