
LoadToEnvironment <- function(RData, env = new.env()){
  load(RData, env)
  return(env) 
}

get_smoothing <- function(m, method=c("OLS", "WLS"), numFreq=3, delta=0, 
                          sigma=NULL){
  
  method <- match.arg(method)
  
  if(method=="OLS"){
    # method <- "ols_harmR"
    method <- "harmR"
  } else {
    method <- "wls_harmR"
    if(is.null(sigma)){
      stop("sigma must be provided when WLS method is used")
    }
  }
  
  smooth_m <- matrix(nrow = nrow(m), ncol = ncol(m))
  
  for(i in 1:nrow(m)){
    smooth_m[i,] <- haRmonics(y=m[i,], method=method, sigma=sigma, 
                              numFreq=numFreq, delta=delta)$fitted
  }
  
  smooth_m
}

tsclusters_centroid <- function(m, type='h', seed, distance="L2",
                                trace){
  
  clust <- tsclust(series=m, type = type, seed = seed, distance = distance, 
                   centroid = dtwclust::shape_extraction, trace = trace, 
                   control = dtwclust::hierarchical_control(method = "average"))
  clust
}

fitHarmonic <- function(amp,pha,L,t){
  sum(sapply( 0:(length(amp)-1),
              function(k) amp[k+1] * cos( 2 * pi * k * t / L - pha[k+1]/(180/pi) )  
  )
  )
}

harmonicFit <- function(amp,pha,L,t){
  sapply(1:length(t), function(s) fitHarmonic(amp=amp, pha=pha, L=L, t=s) )
}

get_harmonicFit <- function(samples=100, m, method=c("OLS", "WLS"), sigma=NULL, 
                            numFreq=4, delta=0){
  
  method <- match.arg(method)
  
  if(method == "OLS"){
    # method <- "ols_harmR"
    method <- "harmR"
  } else {
    method <- "wls_harmR"
    if(is.null(sigma)){
      stop("sigma must be provided when WLS method is used")
    }
  }
  
  smooth_m_aug <- matrix(nrow=samples, ncol=nrow(m))
  for(i in 1:ncol(smooth_m_aug)){
    TEMP <- haRmonics(y=m[i,], method=method, sigma=sigma, numFreq=numFreq, 
                      delta=delta)
    
    smooth_m_aug[,i] <- sapply(seq(0, (ncol(m)-1), length=samples),
                               function(s) fitHarmonic(amp=TEMP$amplitude,
                                                       pha=TEMP$phase,
                                                       t=s, L=ncol(m)))
  }
  
  smooth_m_aug
}

addid <- function(name,id){
  zeros <- ifelse(nchar(id)==1, "0000", 
                  ifelse(nchar(id)==2, "000", 
                         ifelse(nchar(id)==3, "00",
                                ifelse(nchar(id)==4, "0", ""))))
  paste0(name, "_", zeros, id)
}

# ---
getAICtable <- function(observed, fitted, freqNum, sigma=NULL,
                        method=c("iid", "heter")){
  
  AICm <- numeric(nrow(fitted))
  
  if(method=="iid"){
    
    for(i in 1:length(AICm)){
      AICm[i] <- 2 * ( 2 * freqNum + 1) - 2 * sum( (observed[i,] - fitted[i,])^2 / (2 * stats::sd(observed[i,])^2) )
    }
    
  } else {
    
    # AICm <- numeric(nrow(mat_smooth_sigma)) # matrix(nrow=2, ncol=22)
    
    if(is.null(sigma)){
      
      stop("sigma must be provided")
      
    } else {
      
      for(i in 1:length(AICm)){
        AICm[i] <- 2 * ( 2 * freqNum + 1) - 2 * sum( (observed[i,] - fitted[i,])^2 / sigma[i]^2 )/2
      }
      
    }
  }
  
  AICm
}

# --- Added on Sept 24, 2022
# --- improvement on getAICtable()
getAICtable_improv <- function(observed, fitted, freqNum, sigma=NULL,
                               method=c("OLS", "WLS")){
  
  method <- match.arg(method)
  
  if(method == "WLS"){
    if(is.null(sigma)){
      stop("sigma must be provided when WLS method is used")
    }
  }
  
  AICm <- numeric(nrow(fitted))
  
  if(method=="OLS"){
    for(i in 1:length(AICm)){
      AICm[i] <- 2 * ( 2 * freqNum + 1) - 2 * sum( (observed[i,] - fitted[i,])^2 / (2 * stats::sd(observed[i,])^2) )
    }
  } else {
    for(i in 1:length(AICm)){
      AICm[i] <- 2 * ( 2 * freqNum + 1) - 2 * sum( (observed[i,] - fitted[i,])^2 / sigma[i]^2 )/2
    }
  }
  
  AICm
}

# ---
.tsPlot <- function(x, startYear, endYear, frequency,
                    sizeLine=1, sizePoint=2, position_legend="none", 
                    title_legend=NULL,
                    xLab="Time", yLab="Index", xLim, ...){
  
  x_axis <- get_metadata_years(x=x, startYear=startYear, 
                               endYear=endYear,
                               frequency=frequency)
  
  df <- data.frame(values=x,
                   years=as.factor(rep(x_axis$xLabels, each=frequency)),
                   x=x_axis$xDates)
  
  if( missing(xLim) ){
    # Dec 10, 2023: Added "..." below
    out <- ggplot(df, aes(x=x, y=values, color=years), ...) + 
      ggplot2::geom_line(linewidth=sizeLine) + geom_point(size=sizePoint) +
      ggplot2::theme(legend.position = position_legend, legend.title = title_legend) +
      ggplot2::labs(x=xLab, y=yLab)
  } else {
    out_aux <- ggplot(data = df, aes(x=x, y=values, color=years)) + 
      ggplot2::geom_line(linewidth=sizeLine) + geom_point(size=sizePoint) +
      ggplot2::theme(legend.position = position_legend) +
      ggplot2::labs(x=xLab, y=yLab)
    
    COLORS <- unique( ggplot_build(out_aux)$data[1][[1]]$colour )
    
    ind_begin <- (1:length(x_axis$xDates))[x_axis$xDates == xLim[1]]
    
    ind_end <- which.min( (as.numeric(x_axis$xDates)-as.numeric(xLim[2]))^2 )
    
    A <- strsplit(as.character(xLim[1]),"-")
    B <- strsplit(as.character(xLim[2]),"-")
    
    LABELS <- (as.numeric(A[[1]][1]) %% 100):(as.numeric(B[[1]][1]) %% 100)
    
    remainder <- length(ind_begin:ind_end) %% frequency
    
    x <- x[ind_begin:(ind_end-remainder)]
    
    df <- data.frame(x=x_axis$xDates[ind_begin:(ind_end-remainder)], 
                     years=as.factor(rep(x_axis$xLabels[LABELS+1], 
                                         each=frequency)),
                     values=x)
    
    # Dec 10, 2023: Added "..." below
    out <- ggplot(data=df, aes(x=x, y=values, colour=years), ...) +
      ggplot2::geom_line(linewidth = sizeLine) + geom_point(size = sizePoint) +
      ggplot2::labs(y=yLab, x=xLab) +
      ggplot2::theme(legend.position = position_legend, #) +
                     legend.title = ggplot2::element_text(title_legend) ) +
      ggplot2::scale_color_manual(values = COLORS[LABELS+1])
  }
  
  out
}

# --- Dec 10, 2023
# --- Added xLim=NULL
.rmPlot <- function(x, startYear, endYear, frequency, 
                    sizeLine=1, sizePoint=2, position_legend="none", 
                    # title_legend=NULL,
                    xLab="Time", yLab="Index", xLim, ...){
  
  out_aux <- .tsPlot(x=x, startYear = startYear, endYear = endYear, 
                     frequency = frequency, xLab=xLab, yLab=xLab)
  # title_legend = NULL)
  COLORS <- unique( ggplot_build(out_aux)$data[1][[1]]$colour )
  
  DdA <- seq(1, 365, by=ceiling(365/frequency))
  
  x_axis <- get_metadata_years(x=x, startYear=startYear, endYear=endYear,
                               frequency=frequency)
  
  if( missing(xLim) ){
    df <- data.frame(DoY=factor(DdA, levels=DdA),
                     years=as.factor(rep(x_axis$xLabels, each=frequency)),
                     values=x)
    
    out <- ggplot(data=df, aes(x=DoY, y=values, group=years, colour=years), ...) +
      ggplot2::geom_line(size = sizeLine) + geom_point(size = sizePoint) +
      ggplot2::labs(y=yLab, x=xLab) +
      ggplot2::theme(legend.position = position_legend)#,
    # legend.title = element_text(title_legend))
  } else {
    ind_begin <- (1:length(x_axis$xDates))[x_axis$xDates == xLim[1]]
    
    ind_end <- which.min( (as.numeric(x_axis$xDates)-as.numeric(xLim[2]))^2 )
    
    A <- strsplit(as.character(xLim[1]),"-")
    B <- strsplit(as.character(xLim[2]),"-")
    
    LABELS <- (as.numeric(A[[1]][1]) %% 100):(as.numeric(B[[1]][1]) %% 100)
    
    remainder <- length(ind_begin:ind_end) %% frequency
    
    x <- x[ind_begin:(ind_end-remainder)]
    
    df <- data.frame(DoY=factor(DdA, levels=DdA),
                     years=as.factor(rep(x_axis$xLabels[LABELS+1], 
                                         each=frequency)),
                     values=x)
    
    out <- ggplot(data=df, aes(x=DoY, y=values, 
                               group=years,
                               colour=years ), ...) +
      ggplot2::geom_line(size = sizeLine) + geom_point(size = sizePoint) +
      ggplot2::labs(y=yLab, x=xLab) +
      ggplot2::theme(legend.position = position_legend) +
      # legend.title = element_text(title_legend)) +
      ggplot2::scale_color_manual(values = COLORS[LABELS+1])
  }
  
  out
}


# Added on Dec 19, 2023
# Commented startYear and endYear
.msPlot <- function(x, #startYear=2000, endYear=2021, 
                    msTitle="Cluster", 
                    pointShape=16, pointSize=2, pointStroke=3, 
                    textFontface=2, textSize=5, text_hjust=0.5, 
                    text_vjust=-0.5, ...){
  
  if(!inherits(x, "sephora")){
    stop("x must be of class 'sephora'")
  }
  
  out_aux <- .tsPlot(x=x$x, startYear=x$startYear, endYear=x$endYear,
                     frequency=x$freq, xLab="", yLab="")
  
  COLORS <- unique( ggplot_build(out_aux)$data[1][[1]]$colour )
  
  cmd_distmat <- cmdscale(x$clustering@distmat)
  cmd_distmat <- as.data.frame(cmd_distmat)
  
  cmd_distmat$cluster <- as.factor(x$clustering@cluster)
  
  TEMP <- get_metadata_years(x=x$x, startYear = x$startYear, 
                             endYear = x$endYear, frequency = x$freq)
  AUX <- paste0("'", TEMP$xLabels)
  
  # cmd_distmat$years <- c("'00","'01","'02","'03","'04","'05","'06",
  #                        "'07","'08","'09","'10","'11","'12","'13",
  #                        "'14","'15","'16", "'17", "'18", "'19", "'20",
  #                        "'21")
  
  cmd_distmat$years <- AUX
  
  names(cmd_distmat) <- c('x','y','cluster','years')
  
  df <- data.frame(x=cmd_distmat$x, y=cmd_distmat$y,
                   cluster=cmd_distmat$cluster,
                   years=cmd_distmat$years)
  
  ggplot(data=df,
         aes(x=x,y=y, group=years)) +
    geom_point(aes(colour=cluster), shape=pointShape, size=pointSize,
               stroke=pointStroke) +
    ggplot2::xlab("x") +
    ggplot2::ylab("y") +
    ggplot2::labs(color=msTitle) +
    new_scale_color() +
    ggplot2::geom_text(aes(label=years, col=years),
                       fontface=textFontface, size=textSize,
                       hjust=text_hjust, vjust=text_vjust,
                       show.legend=FALSE) +
    ggplot2::scale_color_manual(values=COLORS)
  
}

.derPlot <- function(x, yLab="NDVI", ...){
  
  oldpar <- par(no.readonly = TRUE)
  # on.exit(par(oldpar), add=TRUE)
  
  if(!inherits(x, "sephora")){
    stop("x must be of class 'sephora'")
  }
  
  fec_tipo <- cbind(as.vector(x$phenoparams), names(x$phenoparams))%>%as.data.frame()
  names(fec_tipo) <- c("Date", "Type")
  param_list <- list(params=fec_tipo, number_params=x$phenoparams)
  
  fechas <- c()
  tipos <- c()
  if(is.factor(param_list$params$Date)) {
    fechas <- as.vector(param_list$params$Date)
    tipos <- as.vector(param_list$params$Type)
  } else {
    fechas <- param_list$params$Date
    tipos <- param_list$params$Type
  }
  
  graphics::par(mfrow=c(5,1))
  on.exit(par(oldpar), add=TRUE)
  yRan <- range(x$m_aug_smooth)
  
  graphics::par(mar=c(1.5,4.5,0.5,1))
  on.exit(par(oldpar), add=TRUE)
  base::plot(x$fpca_fun_0der(seq(1,365,length=365)), col="red", type="l", lwd=4,
       ylab=yLab, ylim=yRan, xaxt="n", cex.lab=1.25, font.lab=4)
  # axis(side=1, at=DAYS_AT_YEAR, labels=c(MONTHS, ""))
  graphics::abline(h=0, lty=3, col="blue")
  graphics::abline(v=fechas, col=colores, lwd=4)
  
  yRan <- range(x$fpca_fun_1der(seq(1,365,length=365)))
  yRan[1] <- yRan[1]-0.00025
  yRan[2] <- yRan[2]+0.00025
  graphics::par(mar=c(1.5,4.5,0,1))
  on.exit(par(oldpar), add=TRUE)
  base::plot(x$fpca_fun_1der(seq(1, 365, length=365)), ylim=yRan,
       # ylab="NDVI'",
       # ylab=expression(bold(yLab^'(1)')),
       ylab=bquote(.(yLab)^'(1)'),
       xaxt="n", lwd=4, col="blue", cex.lab=1.25, font.lab=4)
  graphics::abline(h=0, lty=3, col="blue")
  graphics::abline(v=fechas, col=colores, lwd=4)
  
  yRan <- range(x$fpca_fun_2der(seq(1,365,length=365)))
  yRan[1] <- yRan[1]-0.0005
  yRan[2] <- yRan[2]+0.0005
  graphics::par(mar=c(1.5,4.5,0,1))
  on.exit(par(oldpar), add=TRUE)
  base::plot(x$fpca_fun_2der(seq(1, 365, length=365)), ylim=yRan,
       # ylab="NDVI''",
       # ylab=expression(bold(yLab^'(2)')),
       ylab=bquote(.(yLab)^'(2)'),
       xaxt="n", lwd=4, col="magenta", cex.lab=1.25, font.lab=4)
  graphics::abline(h=0, lty=3, col="blue")
  graphics::abline(v=fechas, col=colores, lwd=4)
  
  yRan <- range(x$fpca_fun_3der(t=seq(1,365,length=365)))
  graphics::par(mar=c(1.5,4.5,0.5,1))
  on.exit(par(oldpar), add=TRUE)
  base::plot(x$fpca_fun_3der(t=seq(1, 365, length=365)), ylim=yRan,
       # ylab=expression(bold('NDVI'^'(3)')),
       ylab=bquote(.(yLab)^'(3)'),
       xaxt="n", lwd=4, col="#872657", cex.lab=1.25, cex.axis=1.25, font.lab=4)
  graphics::abline(h=0, lty=3, col="blue")
  graphics::abline(v=fechas, col=colores, lwd=4)
  
  yRan <- range(x$fpca_fun_4der(t=seq(1,365,length=365)))
  graphics::par(mar=c(2,4.5,0,1))
  on.exit(par(oldpar), add=TRUE)
  base::plot(x$fpca_fun_4der(t=seq(1, 365, length=365)), ylim=yRan,
       # ylab=expression(bold(yLab^'(4)')),
       ylab=bquote(.(yLab)^'(4)'),
       xaxt="n", lwd=4, col="#DC143C", cex.lab=1.25, cex.axis=1.25, font.lab=4)
  graphics::axis(side=1, at=DAYS_AT_YEAR, labels=c(MONTHS, ""))
  graphics::abline(h=0, lty=3, col="blue")
  graphics::abline(v=fechas, col=colores, lwd=4)
  invisible()
}

# --- Added on July 13, 2022
remove_outliers <- function(x){
  TEMP <- graphics::boxplot(x, plot=FALSE)
  
  AUX <- x
  
  index <- unlist(sapply(1:length(TEMP$out), function(s) which(x==TEMP$out[s]) ))
  
  AUX[index] <- NA
  
  list(x_clean=AUX, loc_outliers=as.numeric(index))
}

# --- Added on Sept 6, 2022
# --- Comment on Sept 29, 2022: Perhaps a better name is phenodatesToDoY
getTheDates <- function(x, names){
  
  DATES <- datesToDoY(phenodates = x)
  
  df_text <- c()
  for(i in 1:nrow(DATES)){
    df_text[i] <- paste("<b> <font color=\"", colores[i], "\">", names[i], "</font>",
                        DATES$month[i], ", ", DATES$day[i], "</b>")
  }  
  
  df_text
}

# --- added on Sept 30, 2022 during check of sephora package
dbind <- function (M1, M2) {
  if (is.null(M1)) {
    p <- M2
  }
  else {
    if (is.null(M2)) {
      p <- M1
    }
    else {
      r1 <- dim(M1)[1]
      r2 <- dim(M2)[1]
      c1 <- dim(M1)[2]
      c2 <- dim(M2)[2]
      r <- r1 + r2
      c <- c1 + c2
      p <- matrix(rep(0, r * c), ncol = c)
      p[(1:r1), (1:c1)] <- M1
      p[((r1 + 1):r), ((c1 + 1):c)] <- M2
    }
  }
  
  return(p)
}

spectralnorm <- function (M) {
  result <- sqrt(max(svd(t(M) %*% M)$d))
  return(result)
}

# ---
get_phenopar_tryCatch <- function(x, frequency=23, method=c("OLS", "WLS"),
                                  sigma,
                                  numFreq, delta=0, distance, samples, basis, 
                                  corr, k, trace){
  
  # basis <- drbasis(n=184, q=2)
  # 
  # x=temp
  # sigma=NULL
  # numFreq = 3
  # distance = "dtw2"
  # samples = 184
  # basis = basis
  # frequency = 12
  # k=3 
  # trace = TRUE
  
  method <- match.arg(method)
  
  status <- "Failure"
  
  xMat <- vecToMatrix(x=x, lenPeriod=frequency)
  
  if(method == "OLS"){
    Sigma <- sigma
  } else {
    Sigma <- hetervar(m=xMat, lenPeriod = frequency, method = "standard")
  }
  
  # mat_smooth_sigma <- get_smoothing_sigma(m=xMat, sigma=Sigma,
  #                                         numFreq=numFreq, delta=delta)
  
  mat_smooth_sigma <- get_smoothing(m=xMat, sigma=Sigma, numFreq=numFreq, delta=delta,
                                    method=method)
  
  clust_distance <- tsclusters_centroid(m=mat_smooth_sigma, seed=11,
                                        distance=distance, trace=trace)
  
  # mat_augSmooth <- get_harmonicFit_sigma(samples=samples, m=mat_smooth_sigma,
  #                                        sigma=Sigma, numFreq=numFreq,
  #                                        delta=delta)
  
  mat_augSmooth <- get_harmonicFit(samples=samples, m=mat_smooth_sigma,
                                   sigma=Sigma, numFreq=numFreq,
                                   delta=delta, method=method)
  
  x_fpca <- getFPCA(seriesByCluster=clust_distance@cluster, k=k,
                    mAug=mat_augSmooth, basis=basis, corr=corr)
  
  if(x_fpca$status=="Failure"){
    fit <- list()
    fit$a.coef <- NA
    fit$b.coef <- NA
    fit$amplitude <- NA
    fit$phase <- NA
    
    phenoDates <- list()
    phenoDates$zder <- NA
    phenoDates$fder <- NA
    phenoDates$sder <- NA
    phenoDates$thrder <- NA
    phenoDates$fthder <- NA
    
    phenoDates$status_phenodates <- status #"Failure"
    
    message("FPCA model non inverted, try increasing 'samples'")
  } else {
    fit <- haRmonics(y=x_fpca$fpca, numFreq=numFreq, delta=delta)
    
    # phenoDates <- getPhenoDates(fit=fit, numFreq=numFreq)
    phenoDates <- getPhenoDatesUpdate(fit=fit, numFreq=numFreq)
    
    # fit_curvature <- ndvi_curvature(amp=fit$amplitude, pha=fit$phase, L=L)
    
  }
  
  if(phenoDates$status_phenodates == "Full"){
    status <- "Success"
  }
  
  # if(phenoDates$status_phenodates == "Partial"){
  #   status <- "Partial"
  # }
  
  if(unlist(strsplit(phenoDates$status_phenodates, "-"))[1] == "Partial"){
    status <- "Partial"
  }
  
  
  list(x=x,
       freq=frequency,
       sigma=Sigma, 
       m_aug_smooth=mat_augSmooth,
       clustering=clust_distance,
       fpca=x_fpca$fpca, 
       fpca_harmfit_params=list(a.coef=fit$a.coef, b.coef=fit$b.coef, 
                                amplitude=fit$amplitude, phase=fit$phase),
       fpca_fun_0der=phenoDates$zder,
       fpca_fun_1der=phenoDates$fder,
       fpca_fun_2der=phenoDates$sder,
       fpca_fun_3der=phenoDates$thrder,
       fpca_fun_4der=phenoDates$fthder,
       # fpca_curvature=phenoDates$zcurvature,
       # fpca_curvature_1der=phenoDates$zcurvature_prime,
       phenoparams=phenoDates$dates,
       status=status)
}
# ---

show_condition <- function(code) {
  tryCatch(code, error = function(c) "ERROR")
}

# ---

getFPCA <- function(seriesByCluster, k, mAug, basis, corr){
  # seriesByCluster=clust_distance@cluster
  # mAug=mat_augSmooth; basis=basis
  
  status <- "Success"
  
  # --- 0.75 * length(seriesByCluster) was added on Jan 15, 2024
  # --- this is less restrictive than a given number (e.g. 10) as it was the
  # --- case until today
  
  flag <- FALSE # se usaron menos de 17 series 
  seriesInCluster <- lapply(1:length(unique(seriesByCluster)), 
                            function(s) which(seriesByCluster == s))
  
  if( length(seriesInCluster[[1]]) >= 0.75*length(seriesByCluster) ) {
    SERIES <- seriesInCluster[[1]]
  }
  
  if(length(seriesInCluster[[2]]) >= 0.75*length(seriesByCluster)) {
    SERIES <- seriesInCluster[[2]]
  }
  
  AUX <- ifelse(any(table(seriesByCluster) >= 0.75*length(seriesByCluster)), TRUE, FALSE)
  if( AUX == FALSE ) {
    # SERIES <- 1:22
    SERIES <- 1:length(seriesByCluster)
    flag <- TRUE
  }
  
  maug <- mAug[,SERIES]
  output <- FPCA_tryCatch(DATA=maug, k=k, basis=basis, corr=corr)
  
  if(output$status=="Failure"){
    output <- list()
    output$f <- NA
    flag <- NA
    status <- "Failure"
  }
  
  # list(fpca=output$f, usedTotal=flag, status=status)
  list(fpca=output$f, usedTotal=flag, status=status)
}
# ---

FPCA_tryCatch <- function(DATA, k, corr = NULL, basis) {
  
  # DATA=maug; k=3; corr=NULL; basis=basis
  
  if( missing(k) ){
    stop("k must be an integer between 1 and the sample size")
  }
  
  df_DATA <- data.frame(DATA)
  
  if( ncol(df_DATA) < 2 ){
    stop("Data matrix must contain at least 2 columns")
  }
  
  if( missing(basis) ){
    basis <- drbasis(nn = nrow(df_DATA), qq = 2)
    message("Running STEP 0 * Computing DR Basis")
  } else {
    message("Running STEP 0 * Loading DR Basis")
  }
  
  auxiliary <- show_condition(core_FPCA_tryCatch(DATA=DATA, basis=basis,
                                                 corr=corr, k=k,
                                                 num_times=nrow(DATA)))
  
  # auxiliary <- core_FPCA_tryCatch(DATA=DATA, k=k, basis=basis, corr=corr,
  #                                 num_times=nrow(DATA))
  
  output <- list()
  if(inherits(auxiliary, "character")){
    # output <- list()
    output$status <- "Failure"
    output$est <- NA
    output$f <- NA
  } else {
    # output <- list()
    output$status <- "Success"
    output$est <- auxiliary$est
    output$f <- auxiliary$f
  }
  
  list(est = output$est, f = output$f, status=output$status)
}


# ---
check_phenoCond <- function(GU_Mat, SoS_EoS, Sen, Dor, f2der){
  
  x_GU <- GU_Mat$max
  
  x_SoS <- SoS_EoS$max
  
  x_Mat <- GU_Mat$min
  
  x_Sen <- Sen$x_opt # Sen$mins[ which.max(f2der(Sen$mins)) ]
  
  x_EoS <- SoS_EoS$min
  
  x_Dorm <- Dor$x_opt # Dorm$maxs[ which.min(f2der(Dorm$maxs)) ]
  
  params <- list(GU= ifelse(x_GU%%1 < 0.5, floor(x_GU), ceiling(x_GU)), 
                 SoS=ifelse(x_SoS%%1 < 0.5, floor(x_SoS), ceiling(x_SoS)), 
                 Mat=ifelse(x_Mat%%1 < 0.5, floor(x_Mat), ceiling(x_Mat)), 
                 Sen=ifelse(x_Sen%%1 < 0.5, floor(x_Sen), ceiling(x_Sen)), 
                 EoS=ifelse(x_EoS%%1 < 0.5, floor(x_EoS), ceiling(x_EoS)), 
                 Dor=ifelse(x_Dorm%%1 < 0.5, floor(x_Dorm), ceiling(x_Dorm)))
  
  num_of_na <- sum(is.na(unlist(params)))
  
  flag <- ifelse( num_of_na==0, "Full", 
                  ifelse(num_of_na==1, "Partial-5", 
                         ifelse(num_of_na==2, "Partial-4",
                                ifelse(num_of_na==3, "Partial-3",
                                       ifelse(num_of_na==2, "Partial-4",
                                              ifelse(num_of_na==1, "Partial-5", "Failure")
                                       )
                                ) 
                         ) 
                  ) 
  )
  
  list(params=params, flag=flag)
}

getPhenoDatesUpdate <- function(fit, numFreq=3, delta=0, L=365, 
                                interval=seq(1,365,length=365)){

  fzero <- ndvi_derivatives(amp=fit$amplitude, pha=fit$phase, degree=0, L=L)
  
  fprime <- ndvi_derivatives(amp=fit$amplitude, pha=fit$phase, degree=1, L=L)
  
  fbiprime <- ndvi_derivatives(amp=fit$amplitude, pha=fit$phase, degree=2, L=L)
  
  fthrprime <- ndvi_derivatives(amp=fit$amplitude, pha=fit$phase, degree=3, L=L)
  
  ffthprime <- ndvi_derivatives(amp=fit$amplitude, pha=fit$phase, degree=4, L=L)
  
  GU_Mat <- global_min_max(f=fbiprime, f1der=fthrprime, f2der=ffthprime, D=interval)
  
  # Sen <- get_xsen(f=fbiprime, f1der=fthrprime, f2der=ffthprime, x0=GU_Mat$min)
  Sen <- local_min_max(f=fbiprime, f1der=fthrprime, f2der=ffthprime, what="min", x0=GU_Mat$min, 
                       D=interval)
  
  SoS_EoS <- global_min_max(f=fprime, f1der=fbiprime, f2der=fthrprime, D=interval)
  
  # Dorm <- get_xdorm(f=fbiprime, f1der=fthrprime, f2der=ffthprime, x0=GU_Mat$max)
  Dorm <- local_min_max(f=fbiprime, f1der=fthrprime, f2der=ffthprime, what="max", x0=GU_Mat$max,
                        D=interval)
  
  parPheno <- check_phenoCond(SoS_EoS=SoS_EoS, GU_Mat=GU_Mat, Sen=Sen, Dor=Dorm,
                              f2der=fbiprime)
  
  # --- Added on June 14, 2023
  # --- Removed on Sep 25, 2023
  # fit_curvature <- ndvi_curvature(amp=fit$amplitude, pha=fit$phase, L=L)
  
  list(dates=unlist(parPheno$params),
       status_phenodates=parPheno$flag, #status_phenodates,
       zder=fzero, fder=fprime, sder=fbiprime, 
       thrder=fthrprime, fthder=ffthprime)
  # zcurvature=fit_curvature$curvature,
  # zcurvature_prime=fit_curvature$dcurvature)
}
# ---

# ---
core_FPCA_tryCatch <- function(DATA, basis, k, corr, num_times){
  
  q <- c(2, 2)
  q.trend <- q[1]
  q.ef <- q[2]
  iterations <- ncol(DATA) * 10 # num.cases * 10
  convergence <- NULL
  MoD.all <- NULL
  ti <- NULL
  di <- NULL
  x <- seq(0, 1, length = nrow(DATA))
  nd <- floor(nrow(DATA)/8)  # floor(num_times/8)
  f.d1 <- matrix(nrow=nrow(DATA), ncol=ncol(DATA))
  f.d2 <- matrix(nrow=nrow(DATA), ncol=ncol(DATA))
  
  n <- num_times # nrow(DATA)
  message("Running STEP 1 * Computing Initial Values")
  q.trend1 <- 2 # why using 2?
  
  f.X <- basis$eigenvectorsQR[, 1:q.trend1]
  f.Z <- basis$eigenvectorsQR[, (q.trend1 + 1):nrow(DATA)] %*% 
    diag(1/sqrt(basis$eigenvalues[(q.trend1 + 1):nrow(DATA)]))
  f.D <- diag(basis$eigenvalues)
  f.N <- basis$eigenvectorsQR
  
  One.Vec.All <- rep(1, nrow(DATA) * ncol(DATA))
  X.pop <- kronecker(matrix(1, nrow = ncol(DATA)), f.X)
  Z.pop <- kronecker(matrix(1, nrow = ncol(DATA)), f.Z)
  
  y <- c(as.matrix(DATA))
  
  est <- nlme::lme(y ~ -1 + X.pop,
                   random = list(One.Vec.All = nlme::pdIdent(~Z.pop - 1)),
                   correlation = NULL)
  
  sigma2.e <- est$sigma^2
  sigma2.b.pop <- (est$sigma^2) * exp(2 * unlist(est$modelStruct))
  lambda <- sigma2.e/sigma2.b.pop
  
  f <- matrix(rep(est$fitted[1:nrow(DATA), 2], ncol(DATA)), 
              nrow=nrow(DATA), ncol=ncol(DATA))
  di <- DATA - f
  
  message("Running STEP 2 * Selecting k (Omited)")
  svd <- svd(t(di), nu = ncol(DATA), nv = nrow(DATA))
  u <- svd$u
  v <- svd$v
  d <- svd$d
  y <- t(di) %*% v
  ef <- v[, 1:k]
  if (k == 1 & (min(ef) < 0)) {
    ef <- -ef
  }
  
  message("Running STEP 3 * Updating Components")
  One.Vec.All <- rep(1, nrow(DATA) * ncol(DATA))
  time <- rep(x, ncol(DATA)) # num.cases)
  casesId <- rep(1:ncol(DATA), each = nrow(DATA))
  y <- c(as.matrix(DATA))
  
  # basis <- basis
  # f.X <- basis$eigenvectorsQR[, 1:q.trend]
  # f.Z <- basis$eigenvectorsQR[, (q.trend + 1):nrow(DATA)] %*% 
  #   diag(1/sqrt(basis$eigenvalues[(q.trend + 1):nrow(DATA)]))
  # f.D <- diag(basis$eigenvalues)
  # f.N <- basis$eigenvectorsQR
  
  
  f.X <- basis$eigenvectorsQR[, 1:q.trend]
  f.Z <- basis$eigenvectorsQR[, (q.trend + 1):nrow(DATA)] %*% 
    diag(1/sqrt(basis$eigenvalues[(q.trend + 1):nrow(DATA)]))
  f.D <- diag(basis$eigenvalues)
  f.N <- basis$eigenvectorsQR
  
  X.pop <- kronecker(matrix(1, nrow = ncol(DATA)), f.X)
  Z.pop <- kronecker(matrix(1, nrow = ncol(DATA)), f.Z)
  
  for (ITER in 1:iterations) {
    message(paste("Iteration ", ITER))
    f.Z.ef <- ef
    Z.cases.ef <- kronecker(matrix(1, nrow = ncol(DATA)), f.Z.ef)
    
    est <- nlme::lme(y ~ -1 + X.pop, 
                     random = list(One.Vec.All = nlme::pdIdent(~Z.pop - 1), 
                                   casesId = nlme::pdDiag(~Z.cases.ef - 1)), 
                     correlation = NULL)
    
    sigma2.e <- est$sigma^2
    sigma2.b.pop <- (est$sigma^2) * 
      exp(2 * unlist(est$modelStruct)[k + 1])
    lambda <- sigma2.e/sigma2.b.pop
    alpha <- t(est$coefficients$random$casesId)
    fi <- matrix(est$fitted[, 3], nrow=nrow(DATA), ncol=ncol(DATA))
    di <- ef %*% alpha
    f <- fi - di
    si <- NULL
    ei <- DATA - f
    ef.c <- NULL
    ef.d1.c <- NULL
    ef.d2.c <- NULL
    thetaf.c <- NULL
    if (k == 1) {
      alpha <- matrix(alpha, ncol = 1)
    } else {
      alpha <- matrix(alpha, ncol = ncol(alpha))
      alpha <- t(alpha)
    }
    
    for (i in 1:k) {
      if (k == 1) {
        di.m <- 0
      } else {
        di.m <- ef[, -i] %*% t(alpha[, -i])
      }
      r <- DATA - f - di.m
      r <- c(as.matrix(r))
      X.ef <- NULL
      Z.ef <- NULL
      N.ef <- NULL
      D.ef <- NULL
      
      for (j in 1:ncol(DATA)) {
        # basisd <- basis
        basisd <- basis
        alphamat <- diag(rep(alpha[j, i], nrow(DATA)))
        X.ef.c <- alphamat %*% basisd$eigenvectorsQR[, 1:q.ef]
        X.ef <- dbind(X.ef, X.ef.c)
        Z.ef.c <- alphamat %*% basisd$eigenvectorsQR[, (q.ef + 1):nd] %*% 
          diag(1/sqrt(basisd$eigenvalues[(q.ef + 1):nd]))
        
        Z.ef <- dbind(Z.ef, Z.ef.c)
        D.ef.c <- diag(basisd$eigenvalues[1:nd])
        D.ef <- dbind(D.ef, D.ef.c)
        N.ef.c <- alphamat %*% basisd$eigenvectorsQR[, 1:nd]
        N.ef <- dbind(N.ef, N.ef.c)
      }
      
      One.Vec.All <- rep(1, nrow(DATA) * ncol(DATA))
      if (is.null(corr)) {
        est.r <- nlme::lme(r ~ -1 + X.ef, 
                           random = list(One.Vec.All = nlme::pdIdent(~Z.ef - 1)), 
                           correlation = NULL)
      } else {
        est.r <- nlme::lme(r ~ -1 + X.ef, 
                           random = list(One.Vec.All = nlme::pdIdent(~Z.ef - 1)), 
                           correlation = NULL) # correlation=corr?
      }
      sigma2.e <- est.r$sigma^2
      sigma2.b <- (est.r$sigma^2) * exp(2 * unlist(est.r$modelStruct))
      lambda <- sigma2.e/sigma2.b
      add1 <- 0
      add2 <- 0
      X.ef <- basisd$eigenvectorsQR[, 1:q.ef]
      Z.ef <- basisd$eigenvectorsQR[, (q.ef + 1):nd] %*% 
        diag(1/sqrt(basisd$eigenvalues[(q.ef + 1):nd]))
      N.ef <- basisd$eigenvectorsQR[, 1:nd]
      D.ef <- diag(basisd$eigenvalues[1:nd])
      
      for (l in 1:ncol(DATA)) {
        r.c <- matrix(r, ncol = ncol(DATA))[, l]
        add1 <- add1 + (alpha[l, i]^2) * 
          diag(ncol(N.ef)) + (lambda/ncol(DATA)) * D.ef
        add2 <- add2 + (alpha[l, i]) * (t(N.ef) %*% r.c)
      }
      
      ef.c <- cbind(ef.c, N.ef %*% (solve(add1) %*% add2))
      thetaf.c <- cbind(thetaf.c, solve(add1) %*% add2)
    }
    
    convergence.c <- spectralnorm(ef - ef.c)
    convergence <- c(convergence, convergence.c)
    threshold <- 5/100
    
    if( convergence[length(convergence)] < (threshold) ){
      message("Convergence achieved")
      if (ITER == 1) {
        ef <- ef.c
        ef.d1 <- ef.d1.c
        ef.d2 <- ef.d2.c
        thetaf <- thetaf.c
        if (k > 1) {
          ef <- ef.c
          ef.d1 <- ef.d1.c
          ef.d2 <- ef.d2.c
          thetaf <- thetaf.c
        }
      }
      break
    } else {
      if (ITER == iterations) {
        stop("No convergence achieved")
      }
    }
    if (k == 1) {
      ef.c <- ef.c/sqrt(sum(ef.c^2))
    } else {
      thetaf.c <- qr(thetaf.c)
      thetaf.c <- qr.Q(thetaf.c)
      ef.c <- N.ef %*% thetaf.c
    }
    ef <- ef.c
    thetaf <- thetaf.c
  }
  
  message("Running STEP 4 * Getting Estimators for Components")
  if (is.null(corr)) {
    corcoefs <- "Model fitted with white noise remainder"
    est <- nlme::lme(y ~ -1 + X.pop, 
                     random = list(One.Vec.All = nlme::pdIdent(~Z.pop - 1), 
                                   casesId = nlme::pdDiag(~Z.cases.ef - 1)), 
                     correlation = NULL)
  } else {
    est <- nlme::lme(y ~ -1 + X.pop, 
                     random = list(One.Vec.All = nlme::pdIdent(~Z.pop - 1), 
                                   casesId = nlme::pdDiag(~Z.cases.ef - 1)), 
                     correlation = corr)
  }
  
  sigma2.e <- est$sigma^2
  sigma2.b.pop <- (est$sigma^2) * exp(2 * unlist(est$modelStruct)[k + 1])
  lambda <- sigma2.e/sigma2.b.pop
  cr.Zef <- est$coefficients$random$casesId
  alpha <- cr.Zef
  
  fi <- matrix(est$fitted[, 3], nrow=nrow(DATA), ncol=ncol(DATA))
  di <- ef %*% t(alpha)
  f <- fi - di
  error <- DATA - fi
  nf <- ncol(X.pop)
  nr <- ncol(Z.pop)
  beta <- est$coefficients$fixed[1:nf]
  mu <- est$coefficients$random$One.Vec.All[1:nr]
  samples <- t(MASS::mvrnorm(n = 10000, f.X %*% beta, sigma2.b * 
                               f.Z %*% t(f.Z)))
  upp <- apply(samples, 1, max)
  low <- apply(samples, 1, min)
  
  list(est = est, f = f[, 1])
}


# --- test on globalVariables

globalVariables(c('MONTHS', 'i', 'DoY', 'cluster', 
                  'values', 'y', 'years'), 
                package = "sephora")

# ---

# --- Colors used in spiral and phenoParam plots
cgu <- rgb(173/255,221/255,142/255)
csos <- rgb(120/255,198/255,121/255)
cmat <- rgb(49/255, 163/255,84/255)
csen <- rgb(217/255, 95/255, 14/255)
ceos <- rgb(254/255, 153/255, 41/255)
cdor <- rgb(208/255, 209/255, 230/255)

colores <- c(cgu,csos,cmat,csen,ceos,cdor)

# meses <- c('Ene','Feb','Mar','Abr','May','Jun','Jul','Ago','Sep','Oct','Nov', 'Dic')

# --- to get x-axis for phenoParams plot 

DAYS <- c(31,28,31,30,31,30,31,31,30,31,30,31)

DAYS_AT_YEAR <- c(1,cumsum(DAYS))

middleMonth <- sapply(1:length(DAYS_AT_YEAR), 
                      function(s) median(c(DAYS_AT_YEAR[s],DAYS_AT_YEAR[s+1])) )

middleMonth <- ceiling(middleMonth[!is.na(middleMonth)])

MONTHS <- c("Jan", "Feb", "March", "Abr", "May", "Jun", "Jul",
            "Aug", "Sep", "Oct", "Nov", "Dec")

# LABELS_21 <-  c("'00","'01","'02","'03","'04","'05","'06", "'07","'08","'09",
#                 "'10","'11","'12","'13", "'14","'15","'16","'17","'18","'19","'20",
#                 "'21")

# --- Added on May 19, 2023

phenopar_polygon_auxiliar <- function(data, product, frequency, method, sigma,
                                      numFreq, delta, distance, samples,
                                      basis, corr, k, numCores, dirToSave,
                                      reportFileBaseName, trace) {
  
  reportFileName <- paste0( dirToSave, "/", reportFileBaseName, ".txt" )
  file.create(path = reportFileName, showWarnings = FALSE)
  write( "--- SEPHORA began at ---", file = reportFileName,
         append = TRUE )
  write( as.character(Sys.time()[1]), file = reportFileName,
         append = TRUE )
  
  dataPOLYGON <- data 
  
  kluster <- parallel::makeCluster(spec=numCores, outfile="")
  registerDoParallel(kluster)
  
  output <- foreach(i=1:nrow(dataPOLYGON), .combine="rbind",
                    .export=c("vecFromData", "vecToMatrix", "fill_initialgap_MOD13Q1",
                              "get_phenopar_tryCatch",
                              "hetervar", "get_smoothing", "tsclusters_centroid",
                              "get_harmonicFit",
                              "getFPCA", "FPCA_tryCatch", "core_FPCA_tryCatch",
                              "show_condition", "getPhenoDatesUpdate",
                              "harmonicFit", "fitHarmonic",
                              "spectralnorm", "dbind",
                              "global_min_max", "check_phenoCond", "local_min_max",
                              "ndvi_derivatives"),
                    .packages=c("dtwclust", "TSclust", "geoTS", "bigmemory",
                                "rootSolve")) %dopar% {
                                  
                                  x <- vecFromData(data = dataPOLYGON, numRow = i, 
                                                   product = product, lenPeriod=frequency)
                                  
                                  get_phenoparams <- get_phenopar_tryCatch(x=x$vec, frequency=frequency, method=method,
                                                                           sigma=sigma, trace=trace,
                                                                           numFreq=numFreq, delta=delta, distance=distance,
                                                                           samples=samples, basis=basis, k=k, corr=corr)
                                  
                                  s <- c(get_phenoparams$phenoparams[1], get_phenoparams$phenoparams[2],
                                         get_phenoparams$phenoparams[3], get_phenoparams$phenoparams[4],
                                         get_phenoparams$phenoparams[5], get_phenoparams$phenoparams[6])
                                  
                                  if( i %% 50 == 0 ){
                                    texto <- paste0("Working on ROW: ", i)
                                    write(texto, file = reportFileName, append = TRUE)
                                  }
                                  
                                  return(s)
                                }
  stopCluster(kluster)
  
  write( as.character(Sys.time()[1]), file = reportFileName, append = TRUE)
  write(paste("OUTPUT will be saved at ", dirToSave,  " shortly"),
        file = reportFileName, append = TRUE)
  write( "--- SEPHORA ended at ---", file = reportFileName, append = TRUE)
  write( as.character(Sys.time()[1]), file = reportFileName, append = TRUE)
  
  # save(output, file = paste0(dirToSave, "/",
  #                            tools::file_path_sans_ext(basename(path)),
  #"_phenoparams.RData"))
  
  output
}

# --- Added on Dec 21, 2023

toSephoraClass <- function(x, startYear, endYear){
  out <- list()
  out$x <- x$x
  out$startYear <- startYear
  out$endYear <- endYear
  out$freq <- x$freq
  out$sigma <- x$sigma
  out$m_aug_smooth <- x$m_aug_smooth
  out$clustering <- x$clustering
  out$fpca <- x$fpca
  out$fpca_harmfit_params <- x$fpca_harmfit_params
  out$fpca_fun_0der <- x$fpca_fun_0der
  out$fpca_fun_1der <- x$fpca_fun_1der
  out$fpca_fun_2der <- x$fpca_fun_2der
  out$fpca_fun_3der <- x$fpca_fun_3der
  out$fpca_fun_4der <- x$fpca_fun_4der
  out$phenoparams <- x$phenoparams
  out$status <- x$status

out  
}


