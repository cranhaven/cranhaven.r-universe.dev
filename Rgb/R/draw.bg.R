# Draws the common background of track plots
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

draw.bg = function(
		start,
		end,
		ylab = "",
		ylab.horiz = FALSE,
		ysub = as.character(NA),
		mar = c(0.2, 5, 0.2, 1),
		xaxt = "s",
		yaxt = "n",
		yaxs = "r",
		ylim = c(0, 1),
		cex.lab = 1,
		cex.axis = 1,
		mgp = c(3, 1, 0),
		tck = NA,
		tcl = -0.5,
		xaxp = as.numeric(NA),
		yaxp = as.numeric(NA),
		bty = "o",
		las = 0,
		xgrid = TRUE,
		new = FALSE,
		bg = NA,
		bg.inner = NA,
		fg = "#000000",
		...
	) {
	# Coercions
	if(is.numeric(start))  start <- as.integer(start)
	if(is.numeric(end))    end <- as.integer(end)
	
	# Checks
	if(!is.integer(start)) stop("'start' must be integer or numeric")
	if(!is.integer(end))   stop("'end' must be integer or numeric")
	
	# pretty() acuraccy workaround
	if(start == -1) start <- 0L
	
	# xaxp / yaxp default to NULL
	if(any(is.na(xaxp))) xaxp <- NULL
	if(any(is.na(yaxp))) yaxp <- NULL
	
	# Background
	graphics::par(cex=1, mar=mar, new=new)
	graphics::plot(
		x=NA, y=NA,
		xlim = c(start, end),
		ylim = ylim,
		xlab = "",
		xaxt = "n",
		xaxs = "i",
		ylab = "",
		yaxt = "n",
		yaxs = yaxs,
		bty = "n",
		las = las,
		cex.lab = cex.lab,
		cex.axis = cex.axis,
		tck = tck,
		tcl = tcl,
		col = fg,
		col.axis = fg,
		col.lab = fg,
		col.main = fg,
		col.sub = fg
	)
	
	# Inner background
	usr <- graphics::par("usr")
	if(!is.na(bg.inner)) graphics::rect(xleft=usr[1], xright=usr[2], ybottom=usr[3], ytop=usr[4], border=bg.inner, col=bg.inner)
	
	# Outer background
	if(!is.na(bg)) {
		graphics::rect(
			xleft = usr[1] - diff(usr[1:2])*100,
			xright = usr[2] + diff(usr[1:2])*100,
			ybottom = usr[3] - diff(usr[3:4])*100,
			ytop = usr[4] + diff(usr[3:4])*100,
			border = bg,
			col = bg,
			xpd = TRUE
		)
	}
	
	# Main ylab
	graphics::mtext(side=2, text=ylab, col=fg, las=ifelse(isTRUE(ylab.horiz), 1, 0), line=mgp[1])
	
	# Secondary ylab (assembly)
	if(yaxt == "n" && !is.na(ysub)) {
		graphics::mtext(
			side = 2,
			text = ysub,
			line = 1,
			adj = 0.5,
			cex = cex.lab,
			col = fg
		)
	}
	
	# Y axis (to set 'fg')
	if(yaxt != "n") {
		if(length(yaxp) != 3L) { at <- pretty(c(ylim[1], ylim[2]), n=2)
		} else                 { at <- pretty(c(yaxp[1], yaxp[2]), n=yaxp[3])
		}
		graphics::axis(side=2, las=las, at=at, cex.axis=cex.axis, col.axis=fg, col.ticks=fg, mgp=mgp)
	}
	
	# X grid and axis (Mb)
	if(length(xaxp) != 3L) { at <- pretty(c(start, end), n=12)
	} else                 { at <- pretty(c(xaxp[1], xaxp[2]), n=xaxp[3])
	}
	gridCol <- grDevices::col2rgb(fg)[,1]
	gridCol <- grDevices::rgb(gridCol[1], gridCol[2], gridCol[3], alpha=100, maxColorValue=255)
	if(xaxt != "n") {
		# With axis labels
		if(isTRUE(xgrid)) { graphics::axis(side=1, at=at, las=las, tck=1, col=gridCol, lty="dotted", cex.axis=cex.axis, labels=at/1e6, padj=-1)
		} else            { graphics::axis(side=1, at=at, las=las, cex.axis=cex.axis, labels=at/1e6, padj=-1, col=fg)
		}
	} else {
		# Without axis labels
		if(isTRUE(xgrid)) { graphics::axis(side=1, at=at, las=las, tck=1, col=gridCol, lty="dotted", cex.axis=cex.axis, labels=FALSE, padj=-1)
		}
	}
	
	# Proper box
	graphics::box(
		which = "plot",
		col = fg,
		bty = bty
	)
}

