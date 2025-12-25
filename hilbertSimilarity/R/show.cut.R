#' Plot the cuts generated through make.cut
#'
#' Visualize the cuts in relation with the distribution of the data for each dimension
#' in the original matrix
#'
#' @param cuts the output of the \code{\link{make.cut}}.
#' @param type which cuts to show. This must be one of "all", "fixed" or "combined".
#'              Any unambiguous substring can be given.
#' @param local defaults to \code{FALSE}; if \code{TRUE}, shows the local minima and maxima as a rug plot.
#'
#' @details "fixed" will show \code{n} equally spaced cuts (see \code{\link{make.cut}} for the definition of \code{n}).
#'            "combined" will show the cuts after adjustment for local minima and maxima.
#'            "all" will show both. Setting \code{local} to \code{TRUE} will enable the visualization of
#'            local minima and maxima detected by the algorithm in each dimension.
#'
#' @return the function returns an invisible `NULL`.
#'
#' @example examples/example.cut.R
#'
#' @author Yann Abraham
#'
#' @importFrom graphics plot plot.new abline rug legend par
#' @importFrom grDevices n2mfrow
#'
#' @export
show.cut <- function(cuts, type = 'all', local = FALSE) {
  types <- lapply(cuts,function(x) names(x[['cuts']]))
  types <- unlist(types)
  types <- unique(types)
  types <- c('all',types)
  if (is.na(pmatch(type,types))) {
    warning(type,'was not recognized, showing all cuts\n')
    type <- 'all'
  } else {
    type <- types[pmatch(type,types)]
  }
  old.par <- par(no.readonly =TRUE)
  on.exit(par(old.par))
  par(mfrow=n2mfrow(length(cuts)+1),
      mar=c(1,1,3,1))
  ksink <- lapply(names(cuts),function(cur.ch) {
    plot(cuts[[cur.ch]]$dens,main=paste(type,'cuts for',cur.ch))
    if (type=='all') {
      lapply(types[types!='all'],function(tp) {
        abline(v=cuts[[cur.ch]][['cuts']][[tp]],lty=2,col=match(tp,types))
      })
    } else {
      abline(v=cuts[[cur.ch]][['cuts']][[type]],lty=2,col=match(type,types))
    }
    if (local) {
      rug(unlist(cuts[[cur.ch]][c('minima','maxima')]),col=4,lwd=3)
    }
  })
  if (type=='all') {
    plot.new()
    legend('center',legend=types[types!='all'],col=seq(2,length(types)),
                     pch=16,bty='n')
  } else {
    plot.new()
    legend('center',legend=type,col=match(type,types),
                     pch=16,bty='n')
  }
  return(invisible(NULL))
}
