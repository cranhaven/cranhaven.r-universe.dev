# Compute Y from collision boxes
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

yline <- function(
		boxes,
		start,
		end,
		label,
		labelStrand,
		labelCex,
		labelSrt,
		labelAdj,
		labelOverflow,
		maxDepth	
	) {
	# Check collision boxes
	if(!is.data.frame(boxes)) stop("'boxes' must be a data.frame")
	if(!"start.plot" %in% colnames(boxes) || !is.integer(boxes$start.plot)) stop("boxes$start.plot must be an integer")
	if(!"end.plot" %in% colnames(boxes)   || !is.integer(boxes$end.plot))   stop("boxes$end.plot must be an integer")
	if(!"label" %in% colnames(boxes)      || !is.character(boxes$label))    stop("boxes$label must be a character")
	
	# Label widths
	if(labelSrt == 0)         { labelWidths <- round(graphics::xinch(graphics::par("cin")[1]) * (nchar(boxes$label) + ifelse(labelStrand, 3, 1)) * labelCex) * 0.66
	} else if(labelSrt == 90) { labelWidths <- rep(round(graphics::xinch(graphics::par("cin")[2]) * 1 * labelCex) * 0.66, nrow(boxes))
	} else                    { labelWidths <- rep(0, nrow(boxes)) #TODO
	}
	
	# Label box position
	if(labelAdj == "left") {
		# May overflow by the right
		boxes$start.lab <- as.integer(boxes$start.plot)
		boxes$end.lab <- as.integer(boxes$start.plot + labelWidths)
	} else if(labelAdj == "right") {
		# May overflow by the left
		boxes$start.lab <- as.integer(boxes$end.plot - labelWidths)
		boxes$end.lab <- as.integer(boxes$end.plot)
	} else if(labelAdj == "center") {
		# May overflow by both sides
		boxes$start.lab <- as.integer((boxes$start.plot + boxes$end.plot) / 2 - labelWidths / 2)
		boxes$end.lab <- as.integer((boxes$start.plot + boxes$end.plot) / 2 + labelWidths / 2)
	} else {
		stop("Invalid 'labelAdj'")
	}
	
	# Label out of sight (left)
	outLeft <- boxes$start.lab < start
	if(any(outLeft)) {
		boxes[ outLeft , "start.lab" ] <- as.integer(start)
		boxes[ outLeft , "end.lab" ] <- as.integer(start + labelWidths[ outLeft ])
	}
	
	# Label out of sight (right)
	outRight <- boxes$end.lab > end
	if(any(outRight)) {
		boxes[ outRight , "start.lab" ] <- as.integer(end - labelWidths[ outRight ])
		boxes[ outRight , "end.lab" ] <- as.integer(end)
	}
	
	# Final collision box boundaries
	boxes$start <- boxes$start.plot
	boxes$end <- boxes$end.plot
	boxes$overflow <- boxes$start.lab < boxes$start.plot | boxes$end.lab > boxes$end.plot
	if(isTRUE(label) && isTRUE(labelOverflow)) {
		# Allow overflow
		boxes[ boxes$overflow , "start" ] <- boxes[ boxes$overflow , "start.lab" ]
		boxes[ boxes$overflow , "end" ] <- boxes[ boxes$overflow , "end.lab" ]
	}
	
	# 'boxes' must be ordered to use subtrack
	boxes$.order <- 1:nrow(boxes)
	boxes <- boxes[ order(boxes$start) ,]
	
	# To bypass subtrack() indexing during next step, as working on a single chromosome subtrack
	boxes$chrom <- 1L
	boxIndex <- length(boxes$start)
	
	# Looking for non overlaping boxLines (attribution from the widest box to the narrowest)
	errorMessage <- NA
	boxLines <- rep(-1L, nrow(boxes))
	for(l in order(boxes$end - boxes$start, decreasing=TRUE)) {
		overlaps <- subtrack(1L, boxes$start[l]+1L, boxes$end[l]-1L, boxIndex, boxes, boxLines=boxLines)
		i <- 0L; while(any(overlaps$boxLines == i)) i <- i + 1L
		if(i > maxDepth) {
			errorMessage <- "'maxDepth' reached"
			break
		}
		boxLines[l] <- i
	}
	boxes$yline <- boxLines
	
	# Get original order back
	boxes <- boxes[ order(boxes$.order) ,]
	boxes$.order <- NULL
	
	# Break if maxDepth has been reached
	if(is.na(errorMessage)) { return(boxes)
	} else                  { return(simpleError(errorMessage))
	}
}

