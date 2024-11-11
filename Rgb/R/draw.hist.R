# Draws an histogram with a bar for each row of slice
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

draw.hist = function(
		slice,
		start,
		end,
		column = "value",
		fillColor = "#666666",
		border = "#666666",
		cex.lab = 1,
		origin = 0,
		bty = "o",
		fg = "#000000",
		ylim = NA,
		...
	) {
	# Coercions
	if(is.numeric(start)) start <- as.integer(start)
	if(is.numeric(end))   end <- as.integer(end)
	
	# Checks
	if(!is.integer(start))         stop("'start' must be integer or numeric")
	if(!is.integer(end))           stop("'end' must be integer or numeric")
	if(!is.data.frame(slice))      stop("'slice' must be a data.frame")
	if(!"start" %in% names(slice)) stop("'slice' needs a 'start' column")
	if(!"end" %in% names(slice))   stop("'slice' needs a 'end' column")
	if(!column %in% names(slice))  stop("'column' can not be found in 'slice'")
	
	# Automatic ylim
	if(!is.numeric(ylim) || length(ylim) != 2L || all(is.na(ylim))) {
		# Both boundaries to be guessed
		if(any(!is.na(slice[[column]]))) { ylim <- range(slice[[column]], na.rm=TRUE)
		} else                           { ylim <- c(0L, 1L)
		}
	} else if(is.na(ylim[1])) {
		# Bottom only
		if(any(!is.na(slice[[column]]))) { ylim[1] <- min(slice[[column]], na.rm=TRUE)
		} else                           { ylim[1] <- 0L
		}
	} else if(is.na(ylim[2])) {
		# Top only
		if(any(!is.na(slice[[column]]))) { ylim[2] <- max(slice[[column]], na.rm=TRUE)
		} else                           { ylim[2] <- 0L
		}
	}
	
	# Background
	draw.bg(
		start = start,
		end = end,
		cex.lab = cex.lab,
		bty = bty,
		fg = fg,
		ylim = ylim,
		...
	)
	
	if(nrow(slice) > 0) {
		# Draw high boxes behind
		slice <- slice[ order(slice[[column]], decreasing=TRUE) ,]
		
		# Box filling
		if(is.function(fillColor)) {
			environment(fillColor) <- environment()
			fillColor <- fillColor()
		}
		
		# Box border
		if(is.function(border)) {
			environment(border) <- environment()
			border <- border()
		} else if(identical(border, "fillColor")) {
			border <- fillColor
		}
		
		# Boxes
		graphics::rect(
			xleft = slice$start,
			xright = slice$end,
			ytop = slice[[column]],
			ybottom = if(is.numeric(origin)) { origin } else { slice[[origin]] },
			col = fillColor,
			border = border
		)
	} else {
		# No box (not enough)
		graphics::text(
			x = mean(graphics::par("usr")[1:2]),
			y = mean(graphics::par("usr")[3:4]),
			label = paste(nrow(slice), "element(s) in this range"),
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
