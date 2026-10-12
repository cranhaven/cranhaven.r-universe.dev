ztobins <- function(zmat, n.association.status = 3, n.bins = 120, type = 0, df = 7,
                    central.prop = 0.5,
                    pi0=NULL,plot.diagnostics = FALSE,
                    trim.z=FALSE,trim.z.upper = 8,trim.z.lower = -8,
                    force.bin.number = FALSE,
                    pi.using.plugin = FALSE, pi.plugin.lambda = 0.05){
  
  null.CDF.two.sided.pvalues = function(x){
    ret = pnorm(abs(x)) - pnorm(-1*abs(x))
    return(ret)
  }
  
  return(
    backend.ztobins(zmat,n.association.status,n.bins,type,df,
            central.prop,pi0,plot.diagnostics,
            trim.z,trim.z.upper,trim.z.lower,force.bin.number,
            null.CDF = null.CDF.two.sided.pvalues, one.sided.plot=FALSE,
            pi.using.plugin, pi.plugin.lambda)
  )  
}

twosided.PValues.tobins <- function(pval.mat, n.bins = 120, type = 0, df = 7,
                                   central.prop = 0.5,
                                   pi0=NULL,plot.diagnostics = FALSE,
                                   trim.z=FALSE,trim.z.upper = 8,trim.z.lower = -8, force.bin.number = FALSE,
                                   pi.plugin.lambda = 0.05){
  
  zmat = qnorm(1- 0.5 * pval.mat)
  
  null.CDF.two.sided.pvalues = function(x){
    ret = pnorm(abs(x)) - pnorm(-1*abs(x))
    return(ret)
  }
  
  return(
    backend.ztobins(zmat, n.association.status = 2, n.bins, type, df,
                    central.prop,
                    pi0,plot.diagnostics,
                    trim.z,trim.z.upper,trim.z.lower, force.bin.number,
                    null.CDF = null.CDF.two.sided.pvalues, one.sided.plot=TRUE,
                    pi.using.plugin = TRUE, pi.plugin.lambda)
  )

}

backend.ztobins  <- function(zmat, n.association.status = 3, n.bins = 120, type = 0, df = 7,
                     central.prop = 0.5,
                     pi0=NULL,plot.diagnostics = FALSE,
                     trim.z=F,trim.z.upper = 8,trim.z.lower = -8, force.bin.number = F,
                     null.CDF = NULL, one.sided.plot=FALSE,
                     pi.using.plugin = FALSE, pi.plugin.lambda = 0.05) 
{
  if(is.null(null.CDF)){
    null.CDF = function(x){
      ret = pnorm(abs(x)) - pnorm(-1*abs(x))
      return(ret)
    }
  }
  if (type != 0 & type != 1)
    stop("type must equal 0 or 1")
  if (central.prop <= 0 | central.prop >= 1)
    stop("central.prop must take value between 0 and 1")
  #checking validity of pi0
  if(!is.null(pi0)){
    if(length(pi0) != ncol(zmat)){
      stop('pi0 should have length similar to the number of columns of zmat - the Z score matrix')
    }
    for(i in 1:length(pi0)){
      if(!is.na(pi0[i]) & !is.numeric(pi0[i])){
        stop(paste0('argument ',i,' in pi0 vector is not NA or numeric'))
      }
      
      if(is.numeric(pi0[i])){
        if(pi0[i]<= 0 | pi0[i] >1){
          stop('pi0 must be between 0 and 1. use the default value (NULL) for estimation.')  
        }  
      }
    }
  }else{
    # else pi is null, we want estimation on everything
    pi0 = rep(NA,ncol(zmat))
  }
  
  if(!is.logical(one.sided.plot)){
    stop('one.sided.plot must be TRUE or FALSE ')  
  }
  if(!is.logical(pi.using.plugin)){
    stop('pi.using.plugin must be TRUE or FALSE ')  
  }

  PlotWarnings = rep(NA,ncol(zmat))
  #convert z scores bigger than Z.MAX, or a smaller than Z.MIN
  Z.MAX = trim.z.upper
  Z.MIN = trim.z.lower
  if(trim.z){
    for(i in 1:ncol(zmat)){
      ind = which(zmat[,i]>Z.MAX)
      if(length(ind)>0){
        zmat[ind,i] = Z.MAX
        warning(paste0('Study ',i, ' had Z scores larger than Z.MAX = ',Z.MAX,'. Trimmed to Z.MAX.'))
      }
      ind = which(zmat[,i]<Z.MIN)
      if(length(ind)>0){
        zmat[ind,i] = Z.MIN
        warning(paste0('Study ',i, ' had Z scores smaller than Z.MIN = ',Z.MIN,'. Trimmed to Z.MIN.'))
      }
    }
  }
  
  
  #done checking validity of pi0
  M <- dim(zmat)[1]
  #print(paste0("n.bins: ",n.bins))
  #print(paste0("M < n.bins^2 & missing(n.bins): ",M < n.bins^2 & missing(n.bins)))
  nbins.override.flag = F
  if((is.na(n.bins) | is.null(n.bins))){
    nbins.override.flag = T;warning('nbins is not available')
  }
  if(! nbins.override.flag & !force.bin.number){
    if(n.bins< 5 | M < n.bins^2){
      nbins.override.flag = T;
      warning(paste0('Nr. bins must be between 5 and  sqrt(nr.SNPS), use force.bin.number = T to disable this condition.'))
    }
  }
  if(nbins.override.flag){
    n.bins <- floor(sqrt(M));
    warning(paste0('overriding n.bins to ',n.bins))}
  else{
    #nothing to do
  }
  
  #print(paste0("M: ",M))
  #print(paste0("missing(n.bins): ",missing(n.bins)))
  #print(paste0("n.bins: ",n.bins))
  n.studies <- dim(zmat)[2]
  binned.z.mat <- array(dim = c(M, n.studies))
  dimnames(binned.z.mat) <- dimnames(zmat)
  pdf.binned.z <- array(0, dim = c(n.studies, n.bins - 1, n.association.status))
  dimnames(pdf.binned.z) <- list(sprintf("Study %i", 1:n.studies), 
                                 sprintf("bin %i", 1:(n.bins - 1)), if (n.association.status == 2) c("H:0", "H:1") else c("H:-1", "H:0", "H:1"))
  
  breaks.matrix = matrix(NA,nrow = n.bins,ncol = ncol(zmat))
  proportions = matrix(NA,nrow = n.association.status,ncol = ncol(zmat))
  skip_study = F
  for (i in 1:n.studies)
    {
    skip_study = F
    ### This portion is adapted from the locfdr function in the locfdr package.
    
    ## density's estimation
    breaks <- seq(min(zmat[,i]), max(zmat[,i]), length = n.bins)
    if(one.sided.plot){
      breaks <- seq(0, max(zmat[,i]), length = n.bins)
    }
    breaks.matrix[,i] = breaks
    x <- (breaks[-1] + breaks[-length(breaks)])/2
    y <- hist(zmat[,i], breaks = breaks, plot = F)$counts
    K <- length(x)      #  K = n.bins-1
    
    if (type == 0) {               # spline(default)
      f <- glm(y ~ splines::ns(x, df = df), poisson)$fit
    }
    if (type == 1) {               # polynomial
      f <- glm(y ~ poly(x, df = df), poisson)$fit
    }
    D <- (y - f)/(f + 1)^0.5
    D <- sum(D[2:(K - 1)]^2)/(K - 2 - df)       # deviance 
    if (D > 1.5 ){                  # Efron's criterion
      w  = paste("In column ",i," ",colnames(zmat)[i],
                 ",f(z) misfit = ", round(D, 1), ". Rerun with increased df")
      PlotWarnings[i]=w
      warning(w)      
    } 

    
    
    ## theoretical null 
    f0 <- exp(-x^2/2)
    f0 <- (f0 * sum(f))/sum(f0)     
    # (is identical to locfdr(zmat[,i],plot=0,nulltype=0)$mat[,7] )
    
    
    bins <- ceiling((zmat[, i] - min(zmat[, i]))/(x[2] - 
                                                    x[1]))
    bins[bins == 0] <- 1
    bins[bins == n.bins] <- n.bins - 1
    binned.z.mat[, i] <- bins
    
    ## pi0's estimation
    if(is.na(pi0[i])){
      p0theo<- 1
      
      #handle the two sided z score setting
      if(!pi.using.plugin){
        lowCentral  <- quantile(zmat[,i], (1-central.prop)/2)
        highCentral <- quantile(zmat[,i], 1-(1-central.prop)/2)
        central <- (1:K)[x > lowCentral & x < highCentral]
        p0theo <- sum(f[central])/sum(f0[central])  
      }else{
        two.sided.pvalues = 1 - null.CDF(zmat[,i]) #we assume this function to be a real density, with zero mass everywhere
        p0theo = (sum(two.sided.pvalues >= pi.plugin.lambda) + 1)/(length(two.sided.pvalues) * ( 1 - pi.plugin.lambda
))
      }
      if (p0theo>=1){
        skip_study = T
      }
    }else{
      p0theo = pi0[i]
      if(p0theo>1){
        skip_study = T
      }
    }
    
    if(skip_study){
      s=paste0("In column ",i," ",colnames(zmat)[i]," the estimated fraction of nulls is 1.")
      warning(s)
      # (is identical to locfdr(zmat[,i],plot=0,nulltype=0)$fp0[1,3] )
      temp_breaks = breaks
      temp_breaks[1] = -Inf
      if(one.sided.plot){
        temp_breaks[1] = 0  
      }
      temp_breaks[length(temp_breaks)] = Inf
      
      pdf.binned.z[i, , 1:(dim(pdf.binned.z)[3])] = NA
      ind_to_write = 1
      if(n.association.status == 3){ind_to_write = 2}
      pdf.binned.z[i, , ind_to_write] = null.CDF(temp_breaks[-c(1)]) - null.CDF(temp_breaks[-c(length(temp_breaks))])
      
      proportions[,i] = c(1,rep(0,length(proportions[,i])-1))
      next #going on to next study
    }
    
    
    ## fdr's first estimation
    fdr0 <- pmin((p0theo * f0)/f, 1)
    # fdr's adjustement from Efron (equals to one in central position)
    l <- log(f)                     # log-likelihood
    imax <- seq(l)[l == max(l)][1]   
    xmax <- x[imax]                 # "bin" of the max loglik
    if (sum(x <= xmax & fdr0 == 1) > 0) 
      xxlo <- min(x[x <= xmax & fdr0 == 1]) else xxlo = xmax
      if (sum(x >= xmax & fdr0 == 1) > 0) 
        xxhi <- max(x[x >= xmax & fdr0 == 1]) else xxhi = xmax
        if (sum(x >= xxlo & x <= xxhi) > 0) 
          fdr0[x >= xxlo & x <= xxhi] <- 1
        fdr0 <- as.numeric(fdr0)
        # (is identical to locfdr(zmat[,i],plot=0,nulltype=0)$mat[,8] )
        
        ## pi1*f1(alternative) estimation
        p1f1 <- (1 - fdr0) * f
        p1f1 <- as.numeric(p1f1)
        # (is identical to locfdr(zmat[,i],plot=0,nulltype=0)$mat[,11] )
        
        ### code from repfdr
        
        
        
        
        if (n.association.status == 2){
          proportions[1,i] = 1-sum(p1f1)/sum(f)
          proportions[2,i] = sum(p1f1)/sum(f)
          #proportions[,i] = proportions[,i]/sum(proportions[,i])
           
          pdf.binned.z[i, , 1] <- f0/sum(f0)
          pdf.binned.z[i, , 2] <- p1f1/sum(p1f1)
        }
        if (n.association.status == 3){
          
          proportions[2,i] = sum(f0)
          
          
          pdf.binned.z[i, , 2] <- f0/sum(f0)
          pdf.binned.z[i, , 1] <- ifelse(x <= 0, p1f1, rep(0, 
                                                          n.bins - 1))
          pdf.binned.z[i, , 3] <- ifelse(x > 0, p1f1, rep(0, 
                                                          n.bins - 1))
          proportions[1,i] = sum(p1f1[x <= 0]/sum(f))
          proportions[3,i] = sum(p1f1[x > 0]/sum(f))
          proportions[2,i] = 1- sum(p1f1)/sum(f)
          #proportions[,i] = proportions[,i]/sum(proportions[,i])
          
          pdf.binned.z[i, , 1] <- pdf.binned.z[i, , 1]/sum(pdf.binned.z[i, 
                                                                        , 1])
          pdf.binned.z[i, , 3] <- pdf.binned.z[i, , 3]/sum(pdf.binned.z[i, 
                                                                        , 3])
        }
        if (n.association.status != 2 & n.association.status != 3)
          stop("Invalide number of hypothesis states.")
  }
  
  ret = list(pdf.binned.z = pdf.binned.z,
             binned.z.mat = binned.z.mat,
             breaks.matrix = breaks.matrix,
             df = df,
             proportions = proportions,
             PlotWarnings = PlotWarnings,
             trim.z.lower = trim.z.lower,
             trim.z.upper = trim.z.upper
             )
  if(plot.diagnostics){
    diagnostics.plot()
  }
  return(ret)
}


diagnostics.plot=function(env = parent.frame()){
  ztobins.res = env$ret
  theo.CDF = env$null.CDF
  breaks.mat = ztobins.res$breaks.matrix
  pdf.binned.z = ztobins.res$pdf.binned.z
  ind_to_zero = which(is.nan(pdf.binned.z))
  pdf.binned.z[ind_to_zero] = 0
  binned.z.mat.to.plot = ztobins.res$binned.z.mat
  proportions = ztobins.res$proportions
  trim.z.lower = ztobins.res$trim.z.lower
  trim.z.upper = ztobins.res$trim.z.upper
  
  par(mfrow=c(1,1))
  for(i in 1:ncol(binned.z.mat.to.plot)){
    current_breaks = 0.5*(breaks.mat[-c(1),i] + breaks.mat[-c(nrow(breaks.mat)),i])
    lengths = (breaks.mat[-c(1),i] - breaks.mat[-c(nrow(breaks.mat)),i])
    binned.z.mat.to.plot[,i] = current_breaks[binned.z.mat.to.plot[,i]]
    
    counts_table = table(env$binned.z.mat[,i])
    
    counts_data = rep(0,length(current_breaks))
    counts_data[as.numeric(names(counts_table))] = as.numeric(counts_table)
    total_counts = sum(counts_data)
   
    plot_y = rep(NA,nrow(pdf.binned.z[i,,]))
    for(j in 1:length(plot_y)){
      temp = pdf.binned.z[i,j,] * proportions[,i]
      temp = temp[is.finite(temp)] # the z-to-bins procedure can divide by zero for n.assoc =3, if H1 is for example N(5,1)
      plot_y[j] = total_counts * sum(temp)
    }
    #plot spline for total distribution
    line_w = 3
    max_y = 1.1* max(max(plot_y),max(counts_data))
    plot_max = 1.1 * max(current_breaks)
    plot_min = 1.1 * min(current_breaks)
    if(min(current_breaks)>0){
      plot_min = 0
    }
    if(plot_min< trim.z.lower){plot_min = trim.z.lower}
    if(plot_max> trim.z.upper){plot_max = trim.z.upper}
    plot(current_breaks,plot_y, main = paste0('RepFdr Diagnostics, Study ',i,', df=',ztobins.res$df),
         xlab = 'Z. Scores',ylab='Counts',lty=1,ylim = c(0,max_y),type='l',col = 'green',lwd = 3,xlim = c(plot_min,plot_max))
    
    #sticks for real data and residuals (pink)
    ind_of_null = 1
    if(dim(pdf.binned.z[i,,])[2] == 3){
      ind_of_null = 2  
    }
    alternative = counts_data - pdf.binned.z[i,,ind_of_null] * proportions[ind_of_null,i] * total_counts
    alternative = alternative * (alternative>0)
    delta = (current_breaks[2] - current_breaks[1])
    move_step = 0.5*delta
    for(j in 1:length(current_breaks)){
      rect(current_breaks[j] - move_step,0, current_breaks[j] + move_step,counts_data[j],lwd = 1,col = "#a7abb2")
      rect(current_breaks[j] - move_step,0, current_breaks[j] + move_step,alternative[j],lwd = 1,col = "pink")
    }
    lines(current_breaks,plot_y, main = paste0('RepFdr Diagnostics, Study ',i,', df=',ztobins.res$df),
         xlab = 'Z. Scores',ylab='Counts',lty=1,ylim = c(0,max_y),type='l',col = 'green',lwd = 3)
    
    line_w = 3
    x_limits = par("xaxp")[1:2]
    y_limits = par("yaxp")[1:2]
    if(dim(pdf.binned.z[i,,])[2] == 3){
      lines(current_breaks,(pdf.binned.z[i,,1]) * proportions[1,i] * total_counts,col='red',lwd = line_w,lty=2)
      lines(current_breaks,(pdf.binned.z[i,,2]) * proportions[2,i] * total_counts ,col='blue',lwd = 2,lty=3)  
      lines(current_breaks,(pdf.binned.z[i,,3]) * proportions[3,i] * total_counts ,col='#e86609',lwd = line_w,lty=2)
      legend( plot_max - 0.35*(plot_max-plot_min), max(counts_data) + 0.05*(max(counts_data)),
              legend = c('Standard Normal','Mixture Density Est.','Left Cond. Density Est.','Right Cond. Density Est.','Obs. - # Null Exp.'),
              col = c('blue','green','red','#e86609','pink'),
              lwd = c(2,3,line_w,line_w,NA),
              lty=c(3,1,2,2,NA),
              pt.cex = c(1,1,1,1,2),
              pch = c(NA,NA,NA,NA,15),
              cex = 0.8)
    }else{
      lines(current_breaks,(pdf.binned.z[i,,1]) * proportions[1,i] * total_counts ,col='blue',lwd = 2,lty=3)
      lines(current_breaks,(pdf.binned.z[i,,2]) * proportions[2,i] * total_counts ,col='red',lwd = line_w,lty=2)
      legend( plot_max - 0.35*(plot_max - plot_min), max(counts_data) + 0.05*(max(counts_data)),
              legend = c('Standard Normal','Mixture Density Est.','Alternative Cond. Density Est.','Obs. - # Null Exp.'),
              col = c('blue','green','red','pink'),
              lwd = c(2,3,line_w,NA),
              lty=c(3,1,2,NA),
              pt.cex = c(1,1,1,2),
              pch = c(NA,NA,NA,15),
              cex = 0.8)
    }
    w=ztobins.res$PlotWarnings[i]
    if(!is.na(w)){
      
      #text(current_breaks[floor(length(current_breaks)/4)],0.8*max(plot_y),
           #wrap.it(w,30),col='red')
      text(mean(par("xaxp")[1:2]),0.8*max(par("yaxp")[1:2]),
      wrap.it(w,20),col='red')
    }
    
    #plot qqnorm plot
    x = env$zmat[,i]
    if(env$one.sided.plot) #for turning 2-sided pvalues in Z scores on a monotone scale
      x = qnorm(theo.CDF(x))
    #x_theory = qnorm((rank(x)-0.5)/length(x))  
    plot_max = 1.1 * max(x)
    plot_min = 1.1 * min(x)
    if(min(x)>0){
      plot_min = 0
    }
    if(plot_min< trim.z.lower){plot_min = trim.z.lower}
    if(plot_max> trim.z.upper){plot_max = trim.z.upper}
    qqnorm(sort(x),type='l',lwd = 2,main = paste0('Q-Q plot, Study ',i),xlim = c(-2.1,2.1) , ylim = c(plot_min,plot_max))
    qqline(x,col='red')
    abline(0,1,col = 'black',lty=2,lwd=2)
    points(0,0,pch=20,cex=2,col='black')
    
    legend(x = -2,y = plot_max - 0.1* (plot_max - plot_min),legend = c('smoothed fit','normal QQline','diagonal line'),lwd = c(2,1,2),col = c('black','red','black'),lty = c(1,1,2))
    
    #x_p = (rank(x) - 0.5)/length(x)
    #plot(sort(x_p),sort(pnorm(x)),xlab = 'Theoretical Uniform Quantiles',ylab = "Sample Quantiles",main = paste0('P-P plot, Study ',i),xlim = c(0,1),ylim = c(0,1),type='l',lwd = 1)
    #abline(a=0,b = 1,col='red',lty=2)
  }#end of for loop over studues
  par(mfrow=c(1,1))
}

#function for wrappign text, taken from:
#http://stackoverflow.com/questions/20241065/r-barplot-wrapping-long-text-labels
wrap.it <- function(x, len)
{ 
  sapply(x, function(y) paste(strwrap(y, len), 
                              collapse = "\n"), 
         USE.NAMES = FALSE)
}
