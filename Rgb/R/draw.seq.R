# Draws base barplot from BAM slice
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

draw.seq = function(
		slice = NULL,
		start,
		end,
		bty = "o",
		labelCex = 0.75,
		bases = c(A="#44CC44", C="#4444CC", G="#FFCC00", T="#CC4444"),
		maxRange = 500,
		cex.lab = 1,
		fg = "#000000",
		...
	) {
	# Coercions
	if(is.numeric(start)) start <- as.integer(start)
	if(is.numeric(end))   end <- as.integer(end)
	
	# Checks
	if(!is.integer(start)) stop("'start' must be integer or numeric")
	if(!is.integer(end))   stop("'end' must be integer or numeric")
	
	errorMessage <- NA
	if(end - start > maxRange) {
		# Range too large (slice should be NULL)
		errorMessage <- sprintf("'maxRange' reached in draw.seq (%i)", end - start)
	} else if(is.null(slice)) {
		# Range too large (slice should be NULL)
		errorMessage <- sprintf("'maxRange' reached in slice (%i)", end - start)
	} else if(length(slice) == 0) {
		# No element
		errorMessage <- "No sequence in the region"
	} else {
		# Checks
		if(!is.character(slice))   stop("'slice' must be a character vector")
		if(any(nchar(slice) != 1)) stop("'slice' must contain single characters")
	}
	
	# Background
	draw.bg(
		start = start,
		end = end,
		cex.lab = cex.lab,
		bty = bty,
		fg = fg,
		...
	)
	
	if(is.na(errorMessage)) {
		# Positions
		x <- start:end
		col <- bases[ toupper(slice) ]
		
		# Colored boxes
		graphics::rect(xleft=x-0.5, xright=x+0.5, ybottom=0, ytop=1, col=col, border=col)
		
		# Label only if wide enough
		charWidth <- round(graphics::xinch(graphics::par("cin")[1]) * labelCex)
		if(charWidth <= 1) graphics::text(x=x, y=0.5, labels=slice, cex=labelCex)
	} else {
		# Plot only a message
		graphics::text(
			x = mean(graphics::par("usr")[1:2]),
			y = mean(graphics::par("usr")[3:4]),
			label = errorMessage,
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

