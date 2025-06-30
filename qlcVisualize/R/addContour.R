addContour <- function(heights, points, window, crs,
                       levels = c(.4, .45, .5), grid = 5e4, ...) {

  # prepare data
  data <- sf::st_sf(geometry = sf::st_geometry(points), feature = heights, crs = crs)
  w <- sf::as_Spatial(st_geometry(window))

  # make grid inside polygons
  grd <- as.data.frame(sp::spsample(w, "regular", n = grid))
  names(grd) <- c("X", "Y")
  grd <- sf::st_as_sf(grd, coords = c("X", "Y"), crs = crs)
  grd <- stars::st_rasterize(grd)

  # single polygon gets zero outside instead of NA
  grd$ID[grd$ID == 0] <- NA

  # ordinary kriging
  m <- automap::autofitVariogram(feature ~ 1, data)
  k <- gstat::krige(feature ~ 1, data, newdata = grd, model = m$var_model)
  z <- k["var1.pred",,]

  # draw contour with default lwd
  if (is.null(list(...)$lwd)) {
    lwd <- rev(seq(2, 0.5, length.out = length(levels)))
    contour(z, add = TRUE, drawlabels = FALSE,
            levels = levels, lwd = lwd, ...)
  } else {
    contour(z, add = TRUE, drawlabels = FALSE,
            levels = levels, ...)
  }
}
