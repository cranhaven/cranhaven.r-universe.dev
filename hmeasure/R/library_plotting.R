
#' @export
#' @importFrom graphics legend lines plot
plotROC <- function(
  results,
  which=1,
  bw='nrd0',
  cols = c('red','blue','green','magenta','yellow','forestgreen'),
  greyscale = FALSE,
  lty = c(1)
){
  
  if (class(results)!='hmeasure'){
    stop('The first argument of plotROC() must be of class \'hmeasure\'')
  }
  data <- attr(results,'data')
  pi1 <- data$pi1
  pi0 <- data$pi0
  no.of <- length(data)
  classifier.names <- rownames(results$metrics)
  
  # ROC CURVES
  plot.types <- c(1:4)
  if (!any(which %in% plot.types)){
    stop('Type of plot not recognised. Use a numeric value between 1 and 4.')
  }
  
  if (!greyscale){
    colorlist <-  cols
    ltys <- rep(1, length(cols))
  } else {
    colorlist <- rep('black', length(cols))
    ltys <- c(1:length(cols))
  }
  
  
  if (which == 1){ # ROC
    
    legend.colors <- array(data=NA, no.of)
    legend.lty <- array(data=NA, dim=no.of)
    legend.names <- array(data=NA, dim=no.of)
    
    # Plot the diagonal
    plot(c(0,1), c(0,1), type='l', lty=4, col='grey',
         xlab='FPR (1-F0)', ylab='TPR (1-F1)',
         main= 'ROC (continuous) and ROCH (dotted)')
    
    # Define legend entry for diagonal
    legend.lty[1] <- 4
    legend.colors[1] <- 'grey'
    legend.names[1] <- 'trivial'
    
    for (count in (1:no.of)){
      data.now <- data[[count]]
      
      # Plot the ROC
      lines(
        1-data.now$F0,1-data.now$F1, type='l', lty=ltys[count], col=colorlist[count])
      
      # Plot the convex hull
      if (!greyscale){
        lines(
          data.now$G0,data.now$G1, type='l', lty=3, col=colorlist[count])
      }
      
      
      # Define legend entry
      legend.lty[count+1] <- ltys[count]
      legend.colors[count+1] <- colorlist[count]
      legend.names[count+1] <- classifier.names[count]
      
    }
    # put in legend
    legend('bottomright', legend=legend.names,
           lty=legend.lty, col=legend.colors)
  }
  
  if (which==2){ # H cost weights
    
    legend.colors <- array(data=NA, no.of)
    legend.lty <- array(data=NA, dim=no.of)
    legend.names <- array(data=NA, dim=no.of)
    
    # prior parameters are the same for all classifiers
    severity.ratio <- data[[1]]$severity.ratio
    
    if (is.na(severity.ratio)){
      severity.ratio <- pi1/pi0
    }
    if (severity.ratio > 0){
      shape1 <- 2
      shape2 <- 1+(shape1-1)*1/severity.ratio
    }
    if (severity.ratio < 0){
      shape1 <- pi1+1
      shape2 <- pi0+1
    }
    cost.parameter <- severity.ratio*((1+severity.ratio)^(-1))
    
    b <- c(1:100)/100
    y <- dbeta(b,shape1,shape2)
    plot(b, y, type='l', xlab= 'Normalised Cost = SR/(1+SR)', ylab='w(c) ',
         main='H measure w(c)'
    )
    
    lines(rep(cost.parameter,100),c(1:100*max(y))/100,lty=3)
    # put in legend
    legend('topright', lty=c(1,3), col=c('black','black'),
           legend=c('All classifiers',
                    paste('SR = ', format(severity.ratio,digits=3), sep=' ')
           )
    )
    
  }
  
  if (which==3){ # AUC cost weights
    
    # initialise legend specifications
    legend.colors <- array(data=NA, no.of)
    legend.lty <- array(data=NA, dim=no.of)
    legend.names <- array(data=NA, dim=no.of)
    
    # iterate over classifiers
    for (count in (1:no.of)){
      temp <- data[[count]]
      
      aucd <- c((temp$n0*temp$G0 + temp$n1*temp$G1),1)
      aucd2 <- c(1, (temp$n0*temp$G0 + temp$n1*temp$G1))
      aucf <- (aucd-aucd2)/temp$n
      
      if (count == 1){
        plot(temp$cost[2:temp$hc], aucf[2:temp$hc], type='h',
             xlim=c(0,1), ylim=c(0,1), lwd=2,
             main='AUC w(c)',
             xlab='Cost', ylab='w(c)', col=colorlist[count])
      } else {
        lines(temp$cost[2:temp$hc], aucf[2:temp$hc], type='h',
              xlim=c(0,1), ylim=c(0,1), lwd=2,
              main='AUC measure weight function of c',
              xlab='Cost', ylab='w(c)', col=colorlist[count])
      }
      
      # Define legend entry
      legend.lty[count] <- 1
      legend.colors[count] <- colorlist[count]
      legend.names[count] <- classifier.names[count]
    }
    # put in legend
    legend('top', legend=legend.names,
           lty=legend.lty, col=legend.colors)
  }
  
  if (which == 4){ # scoring densities
    
    legend.colors <- array(data=NA, no.of)
    legend.lty <- array(data=NA, dim=no.of)
    legend.names <- array(data=NA, dim=no.of)
    
    
    
    # First compute densities
    ymax <- 0
    to.plot0 <- list()
    to.plot1 <- list()
    for (count in (1:no.of)){
      data.now <- data[[count]]
      s.min <- min(data.now$s.class0, data.now$s.class1)
      s.max <- max(data.now$s.class0, data.now$s.class1)
      
      to.plot0[[count]] <- density(data.now$s.class0,bw=bw)
      to.plot1[[count]] <- density(data.now$s.class1,bw=bw)
      
      # compute maximum density value for plotting purposes
      ymax <- max(to.plot0[[count]]$y, to.plot1[[count]]$y, ymax)
    }
    
    # Now plot
    for (count in (1:no.of)){
      
      if (count == 1){
        plot(to.plot0[[count]], xlim=c(s.min,s.max), col=colorlist[count], lty=4,
             main='Smoothed score distributions \n (class 0: dash-dotted, class 1: dashed) ',
             xlab= 'Score ', ylim=c(0,ymax))
      } else {
        lines(to.plot0[[count]], xlim=c(s.min,s.max), col=colorlist[count], lty=4, ylim=c(0,ymax))
      }
      lines(to.plot1[[count]], lty=2, col=colorlist[count], ylim=c(0,ymax))
      
      
      # Define legend entry
      legend.lty[count] <- 1
      legend.colors[count] <- colorlist[count]
      legend.names[count] <- classifier.names[count]
      
    }
    # put in legend
    legend('top', legend=legend.names,
           lty=legend.lty, col=legend.colors)
    
  }
  
}

