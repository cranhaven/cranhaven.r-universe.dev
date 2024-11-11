# Draws a scatter plot with a point for each row of slice
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

draw.density = function(
		slice,
		start,
		end,
		column = "value",
		cex.lab = 1,
		bty = "o",
		fg = "#000000",
		pal = grDevices::grey,
		border = NA,
		depth = 8,
		dpi = 7,
		bw.x = 0.005,
		bw.y = 0.2,
		precision = 1,
		skewing = 1.75,
		...
	) {
	# Coercions
	if(is.numeric(start)) start <- as.integer(start)
	if(is.numeric(end))   end <- as.integer(end)
	
	# Generic checks
	if(!is.integer(start))         stop("'start' must be integer or numeric")
	if(!is.integer(end))           stop("'end' must be integer or numeric")
	if(!is.data.frame(slice))      stop("'slice' must be a data.frame")
	if(!"start" %in% names(slice)) stop("'slice' needs a 'start' column")
	if(!"end" %in% names(slice))   stop("'slice' needs a 'end' column")
	
	# Specific checks
	if(!column %in% names(slice))                                     stop("'column' can not be found in 'slice'")
	if(!is.function(pal))                                             stop("'pal' must be a function")
	if(!is.numeric(precision) || is.na(precision) || precision <= 0L) stop("'precision' must be a single non-NA positive number")
	
	
	# Background
	draw.bg(
		start = start,
		end = end,
		cex.lab = cex.lab,
		bty = bty,
		fg = fg,
		...
	)
	
	# Points to use for density
	x <- (slice$start + slice$end) / 2
	y <- slice[[column]]
	x <- x[ !is.na(y) ]
	y <- y[ !is.na(y) ]
	
	if(length(x) > 0L) {
		# Precision is more intuitive with a minimum at 0 and the default at 1
		precision <- precision/2 + 0.5
		
		# Grid dimension (depends on figure size)
		grid <- round(graphics::par("din")*dpi)
		
		# X bandwidth should evolve with the zoom level
		bw.x <- bw.x * (end - start + 1L)/1e6
		
		# Compute Gaussian kernel 2D densities
		d <- MASS::kde2d(x=x/1e6, y=y, n=grid*(2*precision-1), h=c(bw.x, bw.y) / precision, lims=c(graphics::par("usr")[1:2]/1e6, graphics::par("usr")[3:4]))
		
		# Density levels to show
		colors <- pal(seq(from=1, to=0, length=depth*(0.5+precision/2)+1))
		levels <- seq(from=0, to=max(d$z)^(1/skewing), length=depth+1)^skewing
		
		# Plot density (pilled polygons are more efficient than graphics::.filled.contour's tilling)
		poly <- grDevices::contourLines(x=d$x*1e6, y=d$y, z=d$z, levels=levels)
		for(i in 1:length(poly)) graphics::polygon(x=poly[[i]]$x, y=poly[[i]]$y, col=colors[ match(poly[[i]]$level, levels) ], border=border)
	} else {
		# No box (not enough)
		graphics::text(
			x = mean(graphics::par("usr")[1:2]),
			y = mean(graphics::par("usr")[3:4]),
			label = paste(length(x), "element(s) in this range"),
			col = fg,
			adj = c(0.5, 0.5),
			cex = cex.lab
		)
	}
	
	# Surrounding box
	graphics::box(
		which = "plot",
		col = fg,
		bty = bty
	)
}
