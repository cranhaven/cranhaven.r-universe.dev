factorMap <- function( x
                  , order = NULL
                  , col = rainbow(4)
                  , show.remaining = FALSE
                  , col.remaining = "grey"
                  , pch.na = 20
                  , col.na = "lightgrey"
          				, legend = length(col)
          				, labels.x = rownames(x)
          				, labels.y = colnames(x)
          				, cex.axis = 1
          				, cex.legend = 1
          				, cex.remaining = 1
          				, font = ""
          				, asp = nrow(x)/ncol(x)
                  , method = "hamming"
                  , control = NULL
          				, plot = TRUE
			          	) {

  # === reordering data ===

  x <- as.matrix(x)
  order.rows <- 1:nrow(x)
  order.cols <- 1:ncol(x)

  if (!is.null(order)) {
    if(nrow(x) > 1 & ncol(x) > 1) {
      sim.cols <- qlcMatrix::sim.obs(t(x), method = method)
      sim.rows <- qlcMatrix::sim.obs(x, method = method)

      makeDist <- function(sim) {
        sim <- as.matrix(sim)
        max(sim, na.rm = TRUE) - sim
      }

      if (order == "eig") {

        # use eigenvalues as very efficiently calculated in RSPectra
        # use first dimension for ordering
        order.cols <- order(RSpectra::eigs(sim.cols,2)$vectors[,1])
        order.rows <- order(RSpectra::eigs(sim.rows,2)$vectors[,1])

      } else if (order == "pca") {

        # PCA on similarities (aka "correspondence analysis")
        # use second dimension for ordering
        order.cols <- order(prcomp(as.matrix(sim.cols))$x[,2])
        order.rows <- order(prcomp(as.matrix(sim.rows))$x[,2])

      } else if (order == "varimax") {

        # varimax rotations on PCA for ordering of correspondences
        # use second dimension
        order.cols <- order(varimax(prcomp(as.matrix(sim.cols))$x)$loadings[,2])
        order.rows <- order(varimax(prcomp(as.matrix(sim.rows))$x)$loadings[,2])

      } else if (order == "mds") {

        # classic MDS
        # use first dimension for ordering
        order.cols <- order(cmdscale(makeDist(sim.cols))[,1])
        order.rows <- order(cmdscale(makeDist(sim.rows))[,1])

      } else {

        # use library seriation for ordering
        # option "R2E" works nice for getting groups of data
        order.cols <- seriation::get_order(seriation::seriate(
                                as.dist(makeDist(sim.cols))
                                , method = order
                                , control =  control
                              ))
        order.rows <- seriation::get_order(seriation::seriate(
                                as.dist(makeDist(sim.rows))
                                , method = order
                                , control = control
                              ))
      }
    } else {
      if (nrow(x) == 1) {
        order.cols <- order(x[1,])
      }
      if (ncol(x) == 1) {
        order.rows <- order(x[,1])
      }
    }
    x <- x[order.rows, order.cols, drop = FALSE]
  }

  if (plot) {

	# === plotting windows ===

	plot.new()
  oldpar <- par(family = font, mar = c(5,4,4,4) + 0.1)
  on.exit(par(oldpar))
	plot.window(xlim = c(0, dim(x)[1])
			       , ylim = c(0, dim(x)[2])
			       , asp = asp
			      	)

	# === axes ===

	rect(-0.1, -0.1, nrow(x)+0.1, ncol(x)+0.1)
	axis(1
		, at = c(1:dim(x)[1]) - 0.5
		, labels = labels.x
		, tick = FALSE
		, las = 2
		, cex.axis = cex.axis
		, mgp = c(3,0.3,0)
		, pos = - 0.5
		)
	axis(2
		, at = c(1:dim(x)[2]) - 0.5
		, labels = labels.y
		, tick = FALSE
		, las = 2
		, cex.axis = cex.axis
		, mgp = c(3,0.3,0)
		, pos = - 0.5
		)

	# === selecting levels to plot ===

	if (is.list(col)) {
	  col <- unlist(col)
	}
  if (!is.null(names(col))) {
	  levs <- names(col)
	} else {
		levs <- names(sort(table(as.vector(x)), decreasing = T))
		if (length(levs) > length(col)) {
			levs <- levs[1:length(col)]
		} else {
			col <- col[1:length(levs)]
		}
		names(col) <- levs
	}

	# === show missing data	===

	if (is.numeric(pch.na)) {
	  points(which(is.na(x), arr.ind = T) - 0.5
	         , pch = pch.na
	         , col = col.na
	         , cex = cex.remaining
	  )
	} else if (is.null(pch.na)) {
    x[is.na(x)] <- "NA"
    levs <- c(levs, "NA")
    col <- c(col, col.na)
    names(col) <- levs
	}

	# === plot boxes ===
	# trick from https://stackoverflow.com/questions/15627674/

	cuts <- function(x) {
	  n <- length(x) %/% 4
	  map <- rep(c(rep(TRUE,4),FALSE), n)
	  result <- rep(NA, n*5)
	  result[map] <- x
	  result
	}

	for (i in levs) {

    # undocumented special coloring
    # should be removed at some point
		# if (i == "-"){
		# 	col[i] = "lightgrey"
		# }

		todo <- which(x == i, arr.ind=T)
		todoX <- cbind(todo[,1]-1, todo[,1]-1, todo[,1], todo[,1])
		todoY <- cbind(todo[,2]-1, todo[,2], todo[,2], todo[,2]-1)
		polygon(  cuts(t(todoX))
				, cuts(t(todoY))
				, col = col[i]
				, border = NA
				)
	}

	# === add names of rare levels ===

	if (show.remaining) {
	  all <- names(table(x))
		remaining <- all[is.na(match(all,levs))]
		for (i in remaining) {
			todo <- which(x == i, arr.ind = T)
			text(todo - 0.5
				, labels = i
				, cex = cex.remaining
				, col = "grey"
				)
		}
		pch.remaining = 4
	} else {
		pch.remaining = 0
	}

	# === add legend ===

	if (!is.null(legend)) {

	  if (is.numeric(legend)) {
	    shown <- c(levs[1:legend])
	  } else {
	    shown <- levs[legend]
	  }

  	legend(x = dim(x)[1] + 0.2
  		 , y = dim(x)[2] + 0.2
  		 , legend = shown
  		 , xpd = TRUE
  		 , pch = c(rep(15, times = length(shown)))
  		 , col = c(col)
  		 , bty = "n"
  		 , cex = cex.legend
  		 , ncol = 1
  		 )
	}

	# === return to default par settings ===

	par(oldpar)

	# === return ordering invisibly ===

	return(invisible(list(rows = order.rows, cols = order.cols)))

  } else {

  # === only return ordering ===

    return(list(rows = order.rows, cols = order.cols))

  }
}

# abbreviated function call
fmap <- factorMap

# old name deprecated
limage <- function() {
  .Deprecated("factorMap")
}

