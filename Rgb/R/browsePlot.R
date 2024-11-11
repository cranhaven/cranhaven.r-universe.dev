# Non interactive genome browsing
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

browsePlot = function(
		drawables,
		chrom = NA,
		start = NA,
		end = NA,
		customLayout = FALSE,
		xaxt = "s",
		xaxm = 1.5,
		panelWidth = "5 cm",
		panelSide = "left",
		panel = NA,
		...
		)
	{
	# Check tracks
	if(!is(drawables, "drawable.list")) stop("'drawables' must be a 'drawable.list' object")
	drawables$check(warn=FALSE)
	
	# Break if no track
	if(drawables$count > 0) {
		# Ignore hidden objects (showing hidden=NA)
		toProcess <- which(!sapply(drawables$hidden, isTRUE))
		if(length(toProcess) > 0) {
			# Checks
			assemblies <- rep(NA_character_, drawables$count)
			organisms <- rep(NA_character_, drawables$count)
			for(i in toProcess) {
				if("assembly" %in% names(drawables$get(i)$getRefClass()$fields())) {
					assemblies[i] <- drawables$get(i)$assembly
				}
				if("organism" %in% names(drawables$get(i)$getRefClass()$fields())) {
					organisms[i] <- drawables$get(i)$organism
				}
			}
			
			# Warnings
			if(length(unique(stats::na.omit(assemblies))) > 1) warning("'drawables' contains objects from distinct assemblies")
			if(length(unique(stats::na.omit(organisms))) > 1)  warning("'drawables' contains objects from distinct organisms")
			
			# Default values
			if(is.na(chrom)) stop("'chrom' must be provided")
			if(is.na(start)) start <- 0L
			if(is.na(end))   end <- drawables$getChromEnd(chrom)
			
			# Forces integer coordinates (for 'sliceable')
			if(abs(start) > .Machine$integer.max) stop("'start' is too large (integer limit reached)")
			if(abs(end) > .Machine$integer.max)   stop("'end' is too large (integer limit reached)")
			start <- as.integer(floor(start))
			end <- as.integer(ceiling(end))
			if(end == start) end <- end - 1L
			
			if(!isTRUE(customLayout)) {
				# Compute layout
				layout <- drawables$getLayout(check=TRUE, panel=panel, panelWidth=panelWidth, panelSide=panelSide)
				
				# Extract panel definitive value
				panel <- layout$panel
				layout$panel <- NULL
				
				# Apply layout
				do.call(what=graphics::layout, args=layout)
				on.exit(graphics::layout(1), add=FALSE)
			} else {
				# Panel display
				if(is.na(panel)) {
					# Let tracks decide
					panel <- FALSE
					for(i in toProcess) panel <- panel || drawables$get(i)$getParam("panel")
				} else {
					# Manually decided
					panel <- as.logical(panel)
				}
			}
			
			# X axis for last track
			if(xaxt != "n") {
				# Focus on last track
				lastTrack <- toProcess[ length(toProcess) ]
				
				# Get last track's 'mar'
				mar <- drawables$get(lastTrack)$getParam("mar")
				arguments <- list(...)
				if("mar" %in% names(arguments)) mar <- arguments$mar
			
				# Update lower margin
				mar[1] <- max(mar[1], xaxm)
			}
			
			# Panels
			if(panel) {
				for(i in toProcess) {
					# new=TRUE must apply to both tracks and panels
					if(isTRUE(drawables$get(i)$getParam("new"))) graphics::par(new=new)
					
					# drawPanel() (drawables produce an empty plot as default)
					if(xaxt != "n" && i == lastTrack) { drawables$get(i)$drawPanel(chrom=chrom, start=start, end=end, xaxt=xaxt, mar=mar, ...)
					} else                            { drawables$get(i)$drawPanel(chrom=chrom, start=start, end=end, xaxt="n", ...)
					}
				}
			}
			
			# Tracks
			for(i in toProcess) {
				if(xaxt != "n" && i == lastTrack) { drawables$get(i)$draw(chrom=chrom, start=start, end=end, xaxt=xaxt, mar=mar, ...)
				} else                            { drawables$get(i)$draw(chrom=chrom, start=start, end=end, xaxt="n", ...)
				}
			}
			
			# Track graphical parameters to return (last track)
			outPar <- graphics::par()
			outPar$chrom <- chrom
			outPar$panel <- panel
			
			invisible(outPar)
		}
	}
}

