# Draws base barplot from BAM slice
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

draw.pileup = function(
		slice = NULL,
		start,
		end,
		ylim = NA,
		bty = "o",
		label = TRUE,
		labelCex = 0.75,
		bases = c(A="#44CC44", C="#4444CC", G="#FFCC00", T="#CC4444"),
		maxRange = 500,
		cex.lab = 1,
		alphaOrder = 3,
		alphaMin = 0.1,
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
		errorMessage <- sprintf("'maxRange' reached in draw.pileup (%i)", end - start)
	} else if(is.null(slice)) {
		# Range too large (slice should be NULL)
		errorMessage <- sprintf("'maxRange' reached in slice (%i)", end - start)
	} else {
		# FIXME
		slice <- t(slice)
		
		# Checks
		if(!is.matrix(slice) || !is.integer(slice))         stop("'slice' must be an integer matrix")
		if(!identical(rownames(slice), c("A","C","G","T"))) stop("'slice' row names must be A, C, G and T")
		if(ncol(slice) > 0 && is.null(colnames(slice)))     stop("'slice' must have column names (positions)")
		
		if(ncol(slice) == 0) {
			# No element
			errorMessage <- "No feature in the region"
		} else {
			# Bar heights and coordinates (bases ordered by count)
			sorted.base <- rownames(slice)[ apply(slice, 2, order, decreasing=TRUE) ]
			sorted.count <- apply(slice, 2, sort, decreasing=TRUE)
			sorted.y <- rbind(0, apply(sorted.count, 2, cumsum))
			ybot <- as.vector(sorted.y[ -nrow(sorted.y) ,])
			ytop <- as.vector(sorted.y[ -1 ,])
			
			# Position variability as a ratio
			variation <- apply(sorted.count, 2, function(x){x[2]/x[1]})
			variation[ is.na(variation) ] <- 0 # x[1] == 0
			
			# Turn ratio into transparency
			alphaBase <- 10^alphaOrder
			alphaMax <- 1 - (alphaBase^(1-1)) / alphaBase
			alpha <- 1 - (alphaBase^(1-variation)) / alphaBase   # Customisable log slope
			alpha <- alpha / alphaMax                            # Adjust theoretical max to 1
			alpha <- alpha * (1-alphaMin) + alphaMin             # Adjust theoretical min to alphaMin
			
			# Apply transparency (not using alpha mecanism of rgb() because of non-compatible devices)
			mtx <- grDevices::col2rgb(bases)[, sorted.base ]
			col <- grDevices::rgb(t(mtx-255) * rep(alpha, each=4) + 255, maxColorValue=255)
			
			# Base positions (as empty positions are not in the slice)
			positions <- as.integer(colnames(slice))
			x <- rep(positions, each=nrow(slice))
			
			# Filter out bases not shown
			filter <- which(ytop != ybot)
			ybot <- ybot[ filter ]
			ytop <- ytop[ filter ]
			x <- x[ filter ]
			col <- col[ filter ]
			
			# ylim
			if(any(is.na(ylim))) ylim <- c(0, max(sorted.y))
		}
	}
	
	# ylim (error case)
	if(any(is.na(ylim))) ylim <- c(0, 1)
	
	# Background
	draw.bg(
		start = start,
		end = end,
		ylim = ylim,
		cex.lab = cex.lab,
		bty = bty,
		fg = fg,
		...
	)
	
	if(is.na(errorMessage)) {
		# Plot
		graphics::rect(xleft=x-0.5, xright=x+0.5, ybottom=ybot, ytop=ytop, col=col, border=col)
		
		# Depth level
		graphics::lines(
			x = rep(positions, each=2L) + c(-0.5, 0.5),
			y = rep(colSums(slice), each=2L)
		)
		
		if(isTRUE(label)) {
			# Label only bars wide enough
			charWidth <- round(graphics::xinch(graphics::par("cin")[1]) * labelCex)
			if(charWidth <= 1) {
				# Label only bars high enough
				charHeight <- round(graphics::yinch(graphics::par("cin")[2]) * labelCex)
				labelable <- sorted.count[ filter ] >= charHeight
		
				# Nucleotides for significant bars
				graphics::text(
					x = x[ labelable ],
					y = ybot[ labelable ],
					labels = sorted.base[ filter ][ labelable ],
					adj = c(0.5, -0.3),
					cex = labelCex
				)
			}
		}
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

