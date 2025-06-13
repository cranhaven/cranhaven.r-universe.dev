# Transform a continuous vector into shades of grey
colorize <- function(x) grey(1L - (x - min(x)) / (max(x) - min(x)))

# Mix image() and heatmap(), with multiple row annotation and distinct default values
# Author : Sylvain Mareschal <maressyl@gmail.com>
heat.map <- function(
	expr,
	side = NULL,
	cex.col = NA,
	cex.row = NA,
	mai.left = NA,
	mai.bottom = NA,
	mai.right = 0.1,
	mai.top = 0.1,
	side.height = 1,
	side.col = NULL,
	side.srt = 0,
	side.cex = 1,
	col.heatmap = heat(),
	zlim = "0 centered",   # or "range", or numeric(2)
	zlim.trim = 0.02,
	norm = c("rows", "columns", "none"),
	norm.robust = FALSE,
	customLayout = FALSE,
	getLayout = FALSE,
	font = c(1, 3),
	xaxt = "s",
	yaxt = "s"
	) {
	# Arg check
	norm <- match.arg(norm)
	
	# Layout parameters
	if(!isTRUE(customLayout)) {
		if(!is.null(side) && ncol(side) > 0) {
			mat <- matrix(c(1:2), ncol=1)
			heights <- c(lcm(ncol(side)*side.height), 1)
			widths <- 1L
		} else {
			mat <- 1L
			heights <- 1L
			widths <- 1L
		}
	} else {
		# No layout call
		mat <- as.integer(NA)
		heights <- as.integer(NA)
		widths <- as.integer(NA)
	}
	
	# Stop returning layout
	if(isTRUE(getLayout)) {
		return(list(mat=mat, heights=heights, widths=widths))
	}
	
	# Side color initialization
	if(is.null(side)) {
		# No side color
		side <- matrix(character(0), ncol=0, nrow=0)
		pal.side <- character(0)
	} else {
		# Check
		if(any(! rownames(expr) %in% rownames(side))) stop("All 'expr' row names must be in 'side' row names")
		
		# To reverted matrix
		side <- side[ rownames(expr) , ncol(side):1 , drop=FALSE ]
		
		# Attribute colors column-wise
		pal.side <- list()
		for(k in colnames(side)) {
			if(is.numeric(side[[k]])) {
				# Add extremes to legend
				pal.side[[k]] <- c("#FFFFFF", "#000000")
				names(pal.side[[k]]) <- sprintf("%s = %g", c("min", "max"), signif(range(side[[k]], na.rm=TRUE), 3))
				
				# Add NAs if any
				if(any(is.na(side[[k]]))) {
					pal.side[[k]] <- c(pal.side[[k]], "NA"="#880000")
				}
			} else if(is.factor(side[,k])) {
				# Unique values (ignore custom colors and numeric columns)
				val <- levels(side[,k])
				
				# Do not attribute colors to interpretable color names or codes
				val <- val[ !is.na(val) ]
				val <- grep("^#([0-9A-Fa-f]{2}){3,4}$", val, value=TRUE, invert=TRUE)
				val <- setdiff(val, colors())
				
				# Attribute colors to values
				if(length(val) > 0) {
					if(is.null(side.col)) {
						# Default palettes
						if(length(val) > 8L) { pal.side[[k]] <- rainbow(n=length(val), v=0.8)
						} else               { pal.side[[k]] <- c("#2a80b9", "#c1392b", "#f39c11", "#52be80", "#c185db", "#fee203", "#bec3c7", "#333333")[1:length(val)]
						}
					} else {
						# Custom function
						pal.side[[k]] <- side.col(length(val))
					}
					
					# Use value as name
					names(pal.side[[k]]) <- val
				} else {
					# Empty legend (if only custom colors are provided)
					pal.side[[k]] <- character(0)
				}
			} else if(is.character(side[,k])) {
				# Character will be printed as is
				pal.side[[k]] <- character(0)
			} else if(is.logical(side[,k])) {
				# Logical will be printed with fixed colors
				pal.side[[k]] <- character(0)
			}else warning("Column \"", k, "\" type is not handled, ignored")
		}
	}

	# Layout call
	if(!isTRUE(customLayout)) {
		layout(mat=mat, heights=heights, widths=widths)
		on.exit(layout(1))
	}
	
	# Centering and scaling output heatmap (heatmap() uses norm.robust=FALSE)
	if(isTRUE(norm.robust)) {
		center <- median
		scale <- mad
	} else {
		center <- mean
		scale <- sd
	}
	if(norm == "columns")     { expr <- (expr - apply(expr, 1, center, na.rm=TRUE)) / apply(expr, 1, scale, na.rm=TRUE)         # samples
	} else if(norm == "rows") { expr <- t((t(expr) - apply(expr, 2, center, na.rm=TRUE)) / apply(expr, 2, scale, na.rm=TRUE))   # genes
	}
	
	# Symmetrical palette around 0
	if(identical(zlim, "0 centered")) {
		zlim <- max(abs(quantile(expr, probs=c(zlim.trim/2L, 1L-zlim.trim/2L), na.rm=TRUE)))
		zlim <- c(-zlim, zlim)
	} else if(identical(zlim, "range")) {
		zlim <- quantile(expr, probs=c(zlim.trim/2L, 1L-zlim.trim/2L), na.rm=TRUE)
	}
	
	# Apply ceiling
	expr[ expr < zlim[1] ] <- zlim[1]
	expr[ expr > zlim[2] ] <- zlim[2]
	
	# Evolutive cex (from heatmap())
	if(is.na(cex.col)) cex.col <- ifelse(nrow(expr) == 1L, 5, 0.2 + 1 / log10(nrow(expr)))
	if(is.na(cex.row)) cex.row <- ifelse(ncol(expr) == 1L, 5, 0.2 + 1 / log10(ncol(expr)))
	
	# Evolutive margins
	if(is.na(mai.left))   mai.left   <- max(strwidth(colnames(expr), units="inches", cex=cex.row)) + par("cin")[2]
	if(is.na(mai.bottom)) mai.bottom <- max(strwidth(rownames(expr), units="inches", cex=cex.col)) + par("cin")[2]
	
	# Side color plot (top)
	if(ncol(side) > 0) {
		par(mai=c(0, mai.left, mai.top, mai.right))
		plot(x=NA, y=NA, xlim=c(0.5, nrow(expr)+0.5), ylim=c(0, ncol(side)), xaxs="i", yaxs="i", xaxt="n", yaxt="n", xlab="", ylab="")
		for(k in 1:ncol(side)) {
			# Annotation colors
			lab <- col <- side[[k]]
			if(is.numeric(side[[k]])) {
				# Represent numeric columns as grey shades
				tmp <- rep(NA, length(col))
				tmp[  is.na(col) ] <- "#880000"
				tmp[ !is.na(col) ] <- colorize(col[ !is.na(col) ])
				col <- tmp
			} else if(is.factor(side[[k]])) {
				# Use palette, or hexadecimal codes / color name as is
				isCustom <- grepl("^#([0-9A-Fa-f]{2}){3,4}$", col) | col %in% colors()
				col <- as.character(col)
				col[!isCustom] <- pal.side[[k]][ as.character(col[!isCustom]) ]
			} else if(is.character(side[[k]])) {
				# Print text, or use hexadecimal codes / color name as is
				isCustom <- grepl("^#([0-9A-Fa-f]{2}){3,4}$", col) | col %in% colors()
				col[!isCustom] <- as.character(NA)
				lab[ isCustom] <- as.character(NA)
			} else if(is.logical(side[[k]])) {
				# Logical
				col <- ifelse(side[[k]], "#333333", "#BBBBBB")
			}
			
			# Draws annotation boxes
			rect(xleft=(1:nrow(expr))-0.5, xright=(1:nrow(expr))+0.5, ybottom=k-1L, ytop=k, col=col)
			
			# Character
			if(is.character(side[[k]])) text(x=1:nrow(expr), y=k-0.5, labels=lab, srt=side.srt, cex=side.cex, adj=c(0.5, 0.5), xpd=NA)
			
			# Add annotation title
			if(!is.null(colnames(side))) mtext(side=2, at=k-0.5, text=colnames(side)[k], las=2, line=1)
		}
	}
	
	# Heatmap (bottom)
	par(mai=c(mai.bottom, mai.left, mai.top, mai.right))
	image(expr, xaxt="n", yaxt="n", col=col.heatmap, zlim=zlim)
	if(xaxt == "s" && !is.null(rownames(expr))) axis(side=1, at=seq(from=0L, to=1L, length=nrow(expr)), labels=rownames(expr), las=2, cex.axis=cex.col, tick=FALSE, line=-0.5, font=font[1])
	if(yaxt == "s" && !is.null(colnames(expr))) axis(side=2, at=seq(from=0L, to=1L, length=ncol(expr)), labels=colnames(expr), las=2, cex.axis=cex.row, tick=FALSE, line=-0.5, font=font[2])
	box()
	
	# Invisibly return parameters for heatScale()
	invisible(
		list(
			zlim = zlim,
			col.heatmap = col.heatmap,
			legend = pal.side,
			cex.col = cex.col,
			cex.row = cex.row,
			mai.left = mai.left,
			mai.bottom = mai.bottom
		)
	)
}

