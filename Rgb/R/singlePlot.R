# Plots all chromosomes on a single plot
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

singlePlot <- function(
		drawables,
		columns = 4,
		exclude = c("X", "Y"),
		add = c(5e6, 15e6),
		vertical = FALSE,
		capWidth = "1 cm",
		spacer = "1 cm",
		finalize = TRUE,
		cap.border = "black",
		cap.font.col = "black",
		cap.bg.col = NA,
		cap.adj = c(0.5, 0.5),
		cap.cex = 2,
		cap.font = 2,
		mar = c(0,0,0,0),
		bty = "n",
		xaxt = "n",
		xgrid = FALSE,
		yaxt = "n",
		ylab = "",
		ysub = "",
		...
	) {
	# Check tracks
	if(!is(drawables, "drawable.list")) stop("'drawables' must be a 'drawable.list' object")
	drawables$check(warn=FALSE)
	
	# Chromosome list
	first <- TRUE
	chromosomes <- NULL
	for(object in drawables$objects) {
		tmp <- object$chromosomes()
		if(!is.null(tmp)) {
			if(first) {
				chromosomes <- tmp
				first <- FALSE
			} else if(!setequal(chromosomes, tmp)) {
				stop("Selected objects have chromosome lists that do not totally overlap.")
			}
		}
	}
	if(length(chromosomes) == 0) stop("Unable to get chromosome list from 'drawables' objects")
	chromosomes <- setdiff(chromosomes, exclude)
	
	# Chromosome lengths
	chromLengths <- integer(0)
	for(chrom in chromosomes) chromLengths[ chrom ] <- drawables$getChromEnd(chrom)
	
	# Layout matrix (chromosomes)
	n <- length(chromLengths)
	if(n %% columns == 0) { lay.chrom <- n
	} else                { lay.chrom <- n + (columns - (n %% columns))
	}
	lay.chrom <- matrix(1:lay.chrom, ncol=columns)
	if(isTRUE(vertical)) lay.chrom <- lay.chrom[ nrow(lay.chrom):1 ,]
	
	# Mask empty cells
	lay.chrom[ !lay.chrom %in% 1:length(chromosomes) ] <- NA
	
	# Layout column widths (chromosomes)
	widths.chrom <- matrix(chromLengths[ chromosomes[ lay.chrom ] ], nrow=nrow(lay.chrom), ncol=ncol(lay.chrom))
	widths.chrom <- apply(widths.chrom, 2, max, na.rm=TRUE) + sum(add)
	
	# Count layable tracks
	isHidden <- sapply(drawables$hidden, isTRUE)
	isNew <- rep(as.logical(NA), drawables$count)
	for(i in 1:drawables$count) isNew[i] <- drawables$objects[[i]]$getParam("new")
	toLay <- which(!isHidden & !isNew)
	trackCount <- length(toLay)
	
	# Expand chromosomes into tracks
	lay <- matrix(as.integer(NA), nrow=nrow(lay.chrom)*(trackCount+1L), ncol=0)
	widths <- NULL
	for(k in 1:ncol(lay.chrom)) {
		# Chromosome cap column
		capColumn <- rep(lay.chrom[,k], each=trackCount+1L)
		capColumn[ c(rep(FALSE, trackCount), TRUE) ] <- NaN
		
		# Plot column
		plotColumn <- rep((lay.chrom[,k]-1L)*trackCount, each=trackCount+1L) + 1:(trackCount+1L) + max(lay.chrom, na.rm=TRUE)
		plotColumn[ c(rep(FALSE, trackCount), TRUE) ] <- NaN
		
		# Add both to layout matrix
		lay <- cbind(lay, capColumn, plotColumn)
		
		# Column widths
		widths <- c(widths, capWidth, widths.chrom[k]/1e6)
	}
	
	# Layout row heights (plots)
	trackHeights <- NULL
	for(i in toLay) trackHeights <- c(trackHeights, drawables$objects[[i]]$getParam("height"))
	heights <- rep(c(trackHeights, spacer), nrow(lay.chrom))
	
	# Spacer in excess
	isSpacer <- apply(is.na(lay), 1, all)
	if(isSpacer[ 1L ]) remove <- 1L
	if(isSpacer[ nrow(lay) ]) remove <- nrow(lay)
	lay <- lay[ -remove ,]
	heights <- heights[ -remove ]
	
	# Mask empty cells
	emptySpace <- any(is.na(lay))
	lay[ is.na(lay) & !is.nan(lay) ] <- max(lay, na.rm=TRUE) + 1L
	lay[ is.nan(lay) ] <- max(lay, na.rm=TRUE) + 1L
	
	# Apply layout
	graphics::layout(lay, widths=widths, heights=heights)
	if(isTRUE(finalize)) on.exit(graphics::layout(1))
	
	# Draw caps
	for(chrom in chromosomes) {
		graphics::par(mar=c(0,0,0,0))
		graphics::plot(x=NA, y=NA, xlim=0:1, ylim=0:1, xlab="", ylab="", xaxt="n", yaxt="n", bty="o", xaxs="i", yaxs="i")
		graphics::rect(xleft=0, xright=1, ybottom=0, ytop=1, col=cap.bg.col, border=cap.border)
		graphics::text(x=cap.adj[1], y=cap.adj[2], labels=chrom, cex=cap.cex, font=cap.font, col=cap.font.col, srt=if(isTRUE(vertical)){ 90 } else { 0 })
	}
	
	# Draw chromosomes
	for(i in 1:length(chromosomes)) {
		# Window coordinates
		chrom <- chromosomes[i]
		start <- 0L - add[1]
		end <- widths.chrom[ col(lay.chrom)[i] ] - add[1]
		
		# Argument to browsePlot()
		args <- list(
			drawables = drawables,
			chrom = chrom,
			start = start,
			end = end,
			customLayout = TRUE,
			...
		)
		
		# Suggested arguments
		if(!is.null(mar))   args$mar <- c(0,0,0,0)
		if(!is.null(bty))   args$bty <- "n"
		if(!is.null(xaxt))  args$xaxt <- "n"
		if(!is.null(xgrid)) args$xgrid <- FALSE
		if(!is.null(yaxt))  args$yaxt <- "n"
		if(!is.null(ylab))  args$ylab <- ""
		if(!is.null(ysub))  args$ysub <- ""
		
		# Plot the whole chromosome
		do.call(what=browsePlot, args=args)
	}
	
	# Fill empty space
	if(isTRUE(finalize) && isTRUE(emptySpace)) {
		graphics::par(mar=c(0,0,0,0))
		graphics::plot(x=NA, y=NA, xlim=0:1, ylim=0:1, xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
	}
}

