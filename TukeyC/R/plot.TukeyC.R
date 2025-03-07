##
## S3 method to plot 'TukeyC' object
##

plot.TukeyC <- function(x,
                        result     = TRUE,
                        replicates = TRUE, 
                        pch        = 19,
                        col        = NULL,
                        xlab       = NULL,
                        ylab       = NULL,
                        xlim       = NULL,
                        ylim       = NULL,
                        axisx      = TRUE,
                        axisy      = TRUE,
                        id.lab     = NULL,
                        id.las     = 1,
                        yl         = TRUE,
                        yl.lty     = 3,
                        yl.col     = 'gray',
                        dispersion = c('none',
                                       'mm',
                                       'sd',
                                       'ci',
                                       'cip'),
                        d.lty      = 1,
                        d.col      = 'black',
                        title      = '', ...)
{
  fun <- function(m) {
    a <- rep('\n', length(m))
    a[which(m != '')[1]] <- ''
    return(paste(a, m, sep=''))
  }

  if(!inherits(x,
               'TukeyC'))
    stop("Use only with 'TukeyC' objects!")

  if(is.null(xlab)) xlab <- 'Levels'

  if(is.null(ylab)) ylab <- 'Means'

  means <- x$info$Means[['means']]

  if(replicates)
    r <- x$out$Replicates

  groups <- 1:length(means)

  m.res <- t(x$out$Result[, 2:ncol(x$out$Result)])

  if(dim(m.res)[1] != 1) {
    m.res <- apply(m.res, 2, fun)
    id.groups <- c(apply(m.res,
                         2,
                         paste,
                         collapse=''))
  } else {
    id.groups <- m.res 
  }

  minmax1 <- x$info$mm[['min']]
  minmax2 <- x$info$mm[['max']]
  minmax <- data.frame(minmax1,
                       minmax2)

  sd1 <- x$info$sd[['linf_sd']]
  sd2 <- x$info$sd[['lsup_sd']]
  sdd <- data.frame(sd1,
                    sd2)

  ic1 <- x$info$ic[['linf_se']]
  ic2 <- x$info$ic[['lsup_se']]
  ic <- data.frame(ic1,
                   ic2)

  icp1 <- x$info$icpool[['linf_sepool']]
  icp2 <- x$info$icpool[['lsup_sepool']]
  icp <- data.frame(icp1,
                    icp2) 

  if(is.null(col))
    col <- 'black'

  if(is.null(xlim))
    xlim <- c(1,
              max(groups))

  if(is.null(ylim))
    if(replicates)
      ylim <- c(.90 * min(minmax[, 1]),
                max(minmax[, 2]))
    else
      ylim <- c(min(minmax[, 1]),
                max(minmax[, 2]))

  # By J.C.Faria
  ngroups <- dim(x$out$Result)[2] - 1
  if(ngroups > 3){
    op <- par('mar')       # Original par('mar')
    np <- op               # A copy
    np[3] <- ngroups + 1   # Changing top to show all letters
    par(mar=np)            # Setting new par('mar')
  }
 
  plot(groups,
       means,
       pch  = pch,
       col  = col,
       xlab = xlab,
       ylab = ylab,
       xlim = xlim,
       ylim = ylim,
       axes = FALSE, ...)

  if(yl == TRUE)
    segments(rep(-0.5,
                 length(means)),
             means,
             groups,
             means,
             lty = yl.lty,
             col = yl.col, ...)

  switch(match.arg(dispersion),
         mm = {
           segments(groups,
                    minmax[,1],
                    groups,
                    minmax[,2],
                    lty = d.lty,
                    col = d.col, ...)
         },
         sd = {
           segments(groups,
                    sdd[,1],
                    groups,
                    sdd[,2],
                    lty = d.lty,
                    col = d.col, ...)
         },
         ci = {
           segments(groups,
                    ic[,1],
                    groups,
                    ic[,2],
                    lty = d.lty,
                    col = d.col, ...)
         },
         cip = {
           segments(groups,
                    icp[,1],
                    groups,
                    icp[,2],
                    lty = d.lty,
                    col = d.col, ...)
         },
         none = NULL)

  if(axisy){
    axis(2,
         at = round(seq(ylim[1],
                        ylim[2],
                        length.out = 5),
                    1))
  }

  if(is.null(id.lab))
    #id.lab <- names(x$out$Result[,1])
    id.lab <- row.names(x$out$Result)

  if(axisx){
    axis(1,
         at       = 1:length(means),
         labels   = id.lab,
         las      = id.las,
         col.axis = FALSE, ...) 
  }   

  if(result) 
    axis(3,
         at     = 1:length(means),
         labels = id.groups, ...)

  if(replicates)
    text(x      = 1:length(means),
         y      = min(ylim),
         labels = r,
         pos    = 3, ...)

  mtext(text = id.lab,
        side = 1,
        line = 1,
        at   = 1:length(means),
        las  = id.las, ...)

  title(title, ...)

  # By J.C.Faria
  if(ngroups > 3){
    par(mar=op)  # Restoring the original par('mar')
  }
}
