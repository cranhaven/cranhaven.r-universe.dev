mycolours=c('#000000', '#e6194B', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#42d4f4', '#f032e6', '#bfef45', '#fabed4', '#469990', '#dcbeff', '#9A6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#a9a9a9')

#' @rdname glinv
#' @param  x               An object of class \code{glinv}.
#' @param  internal_nodes  Boolean, whether to plot the internal nodes's numbers
#' @param  ...  Not used.
#' @export
plot.glinv = function (x, internal_nodes=TRUE, ...) {
  ## x$regtags contains an NA, really; and I can't plot the original tree without
  ## a mapping because the region is in a new tree's edge ordering.
  rt = x$parfntags
  rt[which(is.na(rt))] = 1L
  surt = sort(unique(rt))
  if (length(surt)>1L) {
    graphics::par(mar=c((graphics::par("mar")->oldmar)[1:3], 6.1))
    colours = mycolours[-1L]
  } else {
    colours = mycolours
  }
  plot(x$rawmod$origtree,
            edge.color=colours[ rt[x$rawmod$origtree$edge[,2L]]  ], edge.width=1.5)
  if (internal_nodes)
    ape::nodelabels(bg='#F6F8FA', cex=0.75, frame='none')
  if (length(surt)>1L) {
    graphics::legend('topleft',legend=sprintf('#%d',surt), lty=1L, col=colours[surt],
           xpd=T, inset=c(1,0), bty='n')
    graphics::par(mar=oldmar)
  }
  invisible(NULL)
}
