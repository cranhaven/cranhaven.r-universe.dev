# modified based on original function from package SDraw (archived package)

voronoiPolygons <- function (x, bounding.polygon = NULL){
  if (!inherits(x, "SpatialPoints")) {
    stop("Must pass a SpatialPoints* object to voronoi.polygons.")
  }
  crds = coordinates(x)
  # if (is.null(bounding.polygon)) {
  #
  #   dxy <- diff(c(t(sp::bbox(x))))[c(1, 3)]
  #   bb <- sp::bbox(x) + (matrix(dxy, nrow = 2, ncol = 1) %*%
  #                          matrix(c(-1, 1), nrow = 1, ncol = 2)) * abs(c(0.1, 0.1))
  #   bb <- c(t(bb))
  # }
  # else {
    bb = c(t(sp::bbox(bounding.polygon)))
  # }
  z = deldir::deldir(crds[, 1], crds[, 2], rw = bb)
  w = deldir::tile.list(z)
  polys = vector(mode = "list", length = length(w))
  for (i in seq(along = polys)) {
    pcrds = cbind(w[[i]]$x, w[[i]]$y)
    pcrds = rbind(pcrds, pcrds[1, ])
    polys[[i]] = Polygons(list(Polygon(pcrds)), ID = as.character(i))
  }
  SP = SpatialPolygons(polys, proj4string = CRS(proj4string(x)))
  voronoi = SpatialPolygonsDataFrame(SP, data = data.frame(x = crds[,
                                                                    1], y = crds[, 2], area = sapply(slot(SP, "polygons"),
                                                                                                     slot, "area"), row.names = sapply(slot(SP, "polygons"),
                                                                                                                                       slot, "ID")))
  # if (!is.null(bounding.polygon)) {
    bounding.polygon <- st_union(st_as_sf(bounding.polygon))
    voronoi.clipped <- as_Spatial(st_intersection(st_as_sf(voronoi), bounding.polygon,
                                     by_feature = TRUE))

    df <- data.frame(voronoi)
    df$area <- sapply(slot(voronoi.clipped, "polygons"),
                      slot, "area")
    voronoi <- SpatialPolygonsDataFrame(voronoi.clipped, df)
  # }
  voronoi
}
