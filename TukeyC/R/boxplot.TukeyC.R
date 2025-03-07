boxplot.TukeyC <- function(x,
                           mean.type = c('line',
                                         'point',
                                         'none'),
                           xlab = NULL,
                           mean.col = 'gray',
                           mean.pch = 1,
                           mean.lwd = 1,
                           mean.lty = 1,
                           args.legend = NULL,
                           ...){
  # x is a object of Tukeyc class
  fun <- function(m) {
    a <- rep('\n',
             length(m))
    a[which(m != '')[1]] <- ''
    return(paste(a,
                 m,
                 sep=''))
  }

  if(!inherits(x,
               'TukeyC'))
    stop("Use only with 'TukeyC' objects!") 

  treat <- eval(getCall(x)$which) 

  if(is.null(xlab)) xlab <- 'Levels' 

  if(inherits(x,'TukeyC.formula')){
    aux2 <- eval(getCall(x)$formula)
    aux3 <- eval(getCall(x)$data)
    response <- as.character(formula(aux2)[[2]])    
  } else if(inherits(x,'TukeyC.aovlist')){
    aux <- eval(getCall(x)$x)
    aux3 <- model.frame(aux)
    response <- as.character(attr(aux,
                                  'terms')[[2]]) 
  } else{ 
    aux <- eval(getCall(x)$x)
    aux2 <- eval(getCall(aux)$formula)
    aux3 <- eval(getCall(aux)$data)
    response <- as.character(formula(aux2)[[2]]) 
   } 

  ltreat <- rownames(x$out$Result)
  means <- x$info$Means[['means']] 

  auxinter <- unlist(strsplit(treat,
                              ':')) # objeto criado para auxliar nos casos que envolve interações.

  if(length(auxinter)>1){
    aux3$treat <- with(aux3,
                        interaction(eval(parse(text=treat))))
    aux3$treat <- gsub(':',
                       '/',
                       aux3$treat)
    aux3 <- subset(aux3,
                   treat%in%ltreat)

    treat <- 'treat'
   
  }

 aux3[[treat]] <- factor(aux3[[treat]],
                          levels = ltreat)   

 m.res <- t(x$out$Result[, 2:ncol(x$out$Result)])

  if(dim(m.res)[1] != 1) {
    m.res <- apply(m.res,
                   2,
                   fun)
    id.groups <- c(apply(m.res,
                         2,
                         paste,
                         collapse=''))
  }
  else{
    id.groups <- m.res 
  }

  aux22 <- as.formula(paste(response,
                            '~',
                            treat))

  ngroups <- dim(x$out$Result)[2] - 1
  if(ngroups > 3){
    op <- par('mar')       # Original par('mar')
    np <- op               # A copy
    np[3] <- ngroups + 1   # Changing top to show all letters
    par(mar=np)            # Setting new par('mar')
  }

  gr <- boxplot(aux22,
                data=aux3,
                xlab = xlab,
                ...)# OK lm class!!! 
  axis(3,
       at     = 1:length(ltreat),
       labels = id.groups, ...)

  #gr$stats[3, ] <- unclass(with(aux3,
  #                              by(aux3[[response]],
  #                                 aux3[[treat]], 
  #                                 function(x) mean(x,na.rm=TRUE)))) 
  gr$stats[3,] <- means

  switch(match.arg(mean.type),
         line = {
           bxp(gr,
               add = TRUE,
               frame.plot = FALSE,
               medcol = mean.col,
               lty = mean.lty,
               lwd = mean.lwd,
               boxlty = 'blank',
               whisklty="blank",
               outlty="blank",
               outpch = NA,
               staplelty="blank",
               show.names=FALSE,
               ...)

           auxlty <- c(1,
                       mean.lty)
           auxpch <- NULL
         },
         point = {
           points(means,
                  col = mean.col,
                  lwd = mean.lwd,
                  pch = mean.pch,
                  ...)

           auxlty <- c(1, NA)
           auxpch <- c(NA, mean.pch)
         },
         none = invisible(NULL))

  if(is.null(args.legend)){

    args.2Kl <- list(x      = 'topleft',
                     legend = c('Median',
                                'Mean'),
                     col    = c('black',
                                mean.col),
                     lwd    = c(1,
                                mean.lwd),
                     bty    = 'n',
                     cex    = 0.8,
                     lty    = auxlty,
                     pch    = auxpch)

    do.call('legend',
            args.2Kl)

  } else {

    args.2Kl <- list(x      = 'topleft',
                     legend = c('Median',
                                'Mean'),
                     col    = c('black',
                                 mean.col),
                     lwd    = c(1,
                                mean.lwd),
                     bty    = 'n',
                     cex    = 0.8,
                     lty    = auxlty,
                     pch    = auxpch)

    args.2Kl[names(args.legend)] <- args.legend     

    do.call('legend',
            args.2Kl) 

  }       

  if(ngroups > 3){
    par(mar=op)  # Restoring the original par('mar') 
  }
}
