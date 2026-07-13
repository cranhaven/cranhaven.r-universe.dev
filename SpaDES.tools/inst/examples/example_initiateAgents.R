if (require("sf", quietly = TRUE)) {
  library(data.table)
  library(terra)

  origDTThreads <- data.table::setDTthreads(2L)
  origNcpus <- options(Ncpus = 2L)

  map <- rast(system.file("extdata", "map.tif", package = "SpaDES.tools"))
  names(map) <- "layer"
  pr <- probInit(map, p = (map[] / terra::minmax(map)[2])^2)
  agents <- initiateAgents(map, 100, pr, asSpatialPoints = "sf")
  if (interactive()) {
    terra::plot(map)
    terra::plot(agents, add = TRUE)
  }
  # Test that they are indeed selecting according to probabilities in pr
  dt1 <- data.table(table(round(extract(map, agents), 0)[, "layer"]))
  setnames(dt1, old = "N", new = "count")
  dt2 <- data.table(table(round(map[], 0)))
  setnames(dt2, old = "N", new = "available")
  dt <- dt1[dt2, on = "V1"]  # join the counts and available data.tables
  setnames(dt, old = "V1", new = "mapValue")
  dt[, selection := count / available]
  dt[is.na(selection), selection := 0]
  if (interactive()) {
    with(dt, plot(mapValue, selection))
  }
  #'
  # Note, can also produce a Raster representing agents,
  # then the number of points produced can't be more than
  # the number of pixels:
  agentsRas <- initiateAgents(map, 30, pr, asSpatialPoints = FALSE)
  if (interactive()) {
    terra::plot(agentsRas)
  }
  #'
  # clean up
  data.table::setDTthreads(origDTThreads)
  options(Ncpus = origNcpus)
}
