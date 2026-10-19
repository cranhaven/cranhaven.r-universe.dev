#' plot function for experiments
#'
#' This function uses plot.errors and the base plot functions like
#' [matplot].
#'
#' @param x experiment setup (a list)
#' @param y simulation results (a list)
#' @param ... forwarded to the more specific plot function [plot.errors]
#' @return plot object
#' @export
#' @examples
#' m <- model_from_tsv(uqsa_example("AKAR4"))
#' o <- write_and_compile(as_ode(m))
#' ex <- experiments(m,o)
#' s <- simulator.c(ex,o)
#' p0 <- values(m$Parameter)
#' y <- s(p0)
#' plot(ex,y)
plot.experiments <- function(x, y, ...){
	experiments <- x
	if (missing(y)) {
		simulations <- NULL
	} else {
		stopifnot(length(x)==length(y))
		simulations <- y
		nf <- dim(simulations[[1]]$func)[1]
	}
	for (i in seq_along(experiments)){
		time <- experiments[[i]]$outputTimes
		if (!is.null(simulations)){
			n <- dim(simulations[[i]]$func)
		} else {
			n <- c(dim(experiments[[i]]$data),1)
		}
		oNames <- rownames(experiments[[i]]$data)
		for (j in seq(n[1])){
			a <- ceiling(255*exp(-0.01*n[3]))
			## only plot if the output was actually measured
			if (oNames[j] %in% colnames(experiments[[i]]$measurements)){
				d <- experiments[[i]]$data[j,]
				p <- plot(
					errors::as.errors(time),
					d,
					main=names(experiments)[i],
					xlab="time",
					ylab=oNames[j],
					lty=1,
					...
				)
				if (!is.null(simulations)){
					matplot(
						time,
						simulations[[i]]$func[j,,],
						col=rgb(0,0,255,a,maxColorValue=255),
						lwd=2,
						add=TRUE,
						type="l",
						lty=1
					)
				}
			}
		}
	}
	return(p)
}

