# ====================
# make a dirichlet/voronoi tessellation of points in a window
# this is just a convenience wrapper around "dirichlet" from spatstat
# ====================

voronoi <- function(points, window) {

  .Deprecated("weightedMap")

  p <- spatstat.geom::ppp(points[,1], points[,2], window = window)
  v <- spatstat.geom::dirichlet(p)

  if (!is.null(attr(p, "rejects"))) {
    rejected <- cbind(attr(p, "rejects")$x, attr(p, "rejects")$y)
    index <- apply(rejected, 1, function(x) {
                which(points[,1] == x[1] & points[,2] == x[2])
             })
    attr(v, "rejects") <- unlist(index)
  }

	return(v)
}

# turn into sf object
# sfGeom <- sf::st_as_sfc(tessellation)

# add features
# n <- rep(1, times = length(sfGeom))
# sfDataFrame <- sf::st_sf(geometry = sfGeom, n = n)

# make cartogram
# carto <- cartogramR::cartogramR(sfDataFrame, count = "n")$cartogram

# easy to colour, also set "border = NA" to remove borders
# problem: very difficult to make outer border

# sfDataFrame <- sf::st_sf(geometry = carto, n = n)
# singel <- dyplr::summarise(sfDataFrame)
# plot(carto)
# plot(tmp, add = T, border = "red")

# also : sf::st_union()
# both work only with rather smooth boundaries after cartogram

# ====================
# plotting of a voronoi-map (v-map)
# default plotting of tessellations in spatstat is not easy to use with colour filling
# ====================

vmap <- function(tessellation, col = NULL, add = FALSE, outer.border = "black", border = "grey", lwd = 1, ...) {

  .Deprecated("weightedMap")

	if (!add) {
		plot(0,0
			, xlim = tessellation$window$xrange
			, ylim = tessellation$window$yrange
			, type = "n"
			, axes = FALSE
			, ann = FALSE
		)
	}

	tiles <- spatstat.geom::tiles(tessellation)

	# repeat colors if necessary
	if (is.null(col)) {
	  cols <- col
	} else {
  	cols <- rep(col, length.out = length(tiles))
	}

	# plot all tiles individually, to allow for separate colors
	# vectorize the plotting using polygon()

	poly <- function(tile) {
	  if (tile$type == "rectangle") {
	    x <- tile$xrange
	    y <- tile$yrange
	    coor <- cbind( x = c(x, rev(x)), y = rep(y, each = 2))
	    return(rbind(coor, c(NA, NA)))
	  } else {
  	  parts <- sapply(tile$bdry, function(poly) {
  	              coor <- cbind( x = poly$x, y = poly$y)
  	              rbind(coor, c(NA, NA))
  	               }, simplify = FALSE)
  	  return(do.call(rbind, parts))
	  }
	}

	coor <- sapply(tiles, poly)
	coor <- do.call(rbind, coor)
	coor <- head(coor, -1)

	nr_polys <- sapply(tiles, function(tile){ length(tile$bdry) } )
  nr_rect <- sapply(tiles, function(tile){ tile$type=="rectangle" } )
  nr <- nr_polys + nr_rect

	polygon(coor
	        , col = rep(cols, times = nr)
	        , border = border
	        , lwd = lwd
	        , ...
	        )

	# add outer border
	spatstat.geom::plot.owin(tessellation$window
						, add = TRUE
						, border = outer.border
						, lwd = lwd
						)
}
