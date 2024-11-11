# Draws boxes for each row of slice
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

draw.steps = function(
		slice,
		start,
		end,
		startColumns = "start",
		endColumns = "end",
		maxDepth = 100,
		label = TRUE,
		labelStrand = FALSE,
		labelCex = 1,
		labelSrt = 0,
		labelAdj = "center",
		labelOverflow = TRUE,
		labelFamily = "sans",
		labelColor = "#000000",
		fillColor = "#BBBBBB",
		border = "#666666",
		cex.lab = 1,
		spacing = 0.1,
		bty = "o",
		fg = "#000000",
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
	
	# Background
	draw.bg(
		start = start,
		end = end,
		cex.lab = cex.lab,
		bty = bty,
		fg = fg,
		...	
	)
	
	errorMessage <- NA
	if(nrow(slice) == 0) {
		# No element
		errorMessage <- "No feature in the region"
	} else {
		# Collision boxes
		boxes <- data.frame(
			start.plot = as.integer(slice[[ startColumns[1] ]]),
			end.plot   = as.integer(slice[[ endColumns[1] ]]),
			label      = slice$name,
			stringsAsFactors = FALSE
		)
		
		# Compute collision
		boxes <- yline(
			boxes = boxes,
			start = start,
			end = end,
			label = label,
			labelStrand = labelStrand,
			labelCex = labelCex,
			labelSrt = labelSrt,
			labelAdj = labelAdj,
			labelOverflow = labelOverflow,
			maxDepth = maxDepth
		)
		
		# Break if an error occured
		if(is(boxes, "error")) {
			# Pass error
			errorMessage <- boxes
		} else {
			# boxes = slice
			slice$plotLine <- boxes$yline
			
			# Maximal depth used
			maxLine <- max(boxes$yline) + 1L
			
			
			
			## FEATURE PLOTING ##
			
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
			
			# X coordinates
			x <- NULL
			for(i in 1:length(startColumns)) x <- cbind(x, slice[[ startColumns[i] ]], slice[[ startColumns[i] ]])
			for(i in length(endColumns):1)   x <- cbind(x, slice[[ endColumns[i] ]], slice[[ endColumns[i] ]])
			
			# Y coordinates
			y <- NULL
			step <- 1 / length(startColumns)
			for(i in 1:length(startColumns)) y <- c(y, step*(i-1), step*i)
			step <- 1 / length(endColumns)
			for(i in length(endColumns):1)   y <- c(y, step*i, step*(i-1))
			
			# Plot polygons
			for(i in 1:nrow(slice)) {
				graphics::polygon(
					x = x[i,],
					y = (y * (1 - spacing) + spacing / 2 + slice[i,"plotLine"]) / maxLine,
					border = rep(border, length=nrow(slice))[i],
					col = rep(fillColor, length=nrow(slice))[i]
				)
			}
			
			# Box labels
			if(isTRUE(label)) {
				# Background
				charHeight <- graphics::yinch(graphics::par("cin")[2]) * labelCex
				graphics::rect(
					xleft = boxes$start.lab,
					xright = boxes$end.lab,
					ybottom = (boxes$yline + 0.1) / maxLine,
					ytop = (boxes$yline + 0.1) / maxLine + charHeight,
					col = "#FFFFFF",
					border = border
				)		
				
				# Add strand to labels (collision already accounts for it)
				if(isTRUE(labelStrand)) {
					boxes[ boxes$strand == "-" , "label" ] <- sprintf("< %s", boxes[ boxes$strand == "-" , "label" ])
					boxes[ boxes$strand == "+" , "label" ] <- sprintf("%s >", boxes[ boxes$strand == "+" , "label" ])
				}
				
				# Label color
				if(is.function(labelColor)) {
					environment(labelColor) <- environment()
					labelColor <- labelColor()
				}
				
				# Plotting arguments
				args <- with(
					boxes[ (isTRUE(label) && isTRUE(labelOverflow)) | !boxes$overflow ,],
					list(
						x = (boxes$start.lab + boxes$end.lab) / 2,
						y = (boxes$yline + 0.1) / maxLine + charHeight/2,
						label = label,
						col = labelColor,
						adj = c(0.5, 0.5),
						cex = labelCex,
						srt = labelSrt
					)
				)
				
				# Font family
				if(labelFamily == "Hershey") { args$vfont <- c("sans serif", "bold")
				} else                       { args$family <- labelFamily
				}
				
				# Execute plotting
				do.call(graphics::text, args)
			}
		}
	}
	
	# Plot only a message
	if(!is.na(errorMessage)) {
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

