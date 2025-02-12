#====================================================================
#====================================================================
# x.stat=label="nsup"; y.stat="accuracy"; nbreaks.x = 7; line.color="orange"; point.size = 1.2
multitrait.plot <- function(object, trait_names = NULL,
                            x.stat = c("nsup","lambda"),
                            y.stat = c("accuracy","MSE"), label = x.stat,
                            line.color = "orange", point.color = line.color,
                            point.size = 1.2, nbreaks.x = 6, ...)
{
    x.stat <- match.arg(x.stat)
    y.stat <- match.arg(y.stat)
    if(!is.null(label)){
      label <- match.arg(label, choices=c("nsup","lambda"))
    }
    x <- nsupmin <- nsupmax <- nsup_trait <- x1 <- NULL
    args0 <- list(...)

    if(!inherits(object, "SGP")){
      stop("Input 'object' is not of the class 'SGP'")
    }
    if(length(args0) > 0L){
      tmp <- unlist(lapply(args0, function(x)inherits(x, "SGP")))
      if(sum(tmp)>0){
        message("More than one 'SGP' class objects were provided. Only the first one is plotted")
      }
    }

    xlab <- ifelse(x.stat=="nsup",expression("Support set size ("*n[sup]*")"),
                   expression("-log("*lambda*")"))
    ylab <- capitalize(y.stat)
    ylab2 <- "Proportion of support set"
    alpha <- ifelse("alpha" %in% names(args0), args0$alpha, 0.25)
    lwd <- ifelse("lwd" %in% names(args0), args0$lwd, 0.65)
    if("xlab" %in% names(args0)) xlab <- args0$xlab
    if("ylab" %in% names(args0)) ylab <- args0$ylab
    if("ylab2" %in% names(args0)) ylab2 <- args0$ylab2
    pch <- c(19,19)
    if("pch" %in% names(args0)) pch <- args0$pch
    if(length(pch)==1) pch <- c(pch[1],pch[1])
    pch <- lapply(1:length(pch),function(k){
      suppressWarnings(ifelse(is.na(as.numeric(pch[k])),pch[k],as.numeric(pch[k])))
    })
    if(length(point.size)==1) point.size <- c(point.size[1],point.size[1])
    if(length(point.color)==1) point.color <- c(point.color[1],point.color[1])

    isCV <- as.logical("SGP.CV" %in% attr(object,"type"))
    if("y" %in% names(args0)){
      y <- as.vector(args0$y)
    }else{
      y <- NULL
    }
    if(!"summary" %in% attr(object,"type")){
      if(isCV){
        object <- summary.SGP(object)
      }else{
        if(is.null(object$yHat)){
          object <- summary.SGP(object, y=y)
        }else{
          object <- summary.SGP(object)
        }
      }
    }

    if(is.null(trait_names)){
      if(is.null(object$trait_names)){
        trait_names <- paste0("Trait ",seq(object$ntraits))
      }else{
        trait_names <- object$trait_names
      }
    }else{
      stopifnot(length(trait_names) == object$ntraits)
    }
    names(trait_names) <- seq(object$ntraits)
    if(object$ntraits == 1L){
      stop("Input 'object' is not from a multi-trait SGP")
    }
    nTRN <- as.vector(object$nTRN[1]) #length(object$trn)
    nTST <- as.vector(object$nTST[1]) #length(object$tst)

    if(isCV){
      main <- bquote("SGP CV ("*n[TRN]==.(nTRN[1])*")")
    }else{
      main <- bquote("SGP ("*n[TST]==.(nTST[1])*")")
    }
    if("main" %in% names(args0)) main <- args0$main

    legend.justification <- c(1,ifelse(tolower(y.stat)=="mse",1,0))
    legend.position <- c(0.99,ifelse(tolower(y.stat)=="mse",0.99,0.01))
    if("legend.justification" %in% names(args0)){
      legend.justification <- args0[['legend.justification']]
    }
    if("legend.position" %in% names(args0)){
      legend.position <- args0[['legend.position']]
    }

    theme0 <- mytheme() + ggplot2::theme(legend.justification = legend.justification,
                                         legend.position = legend.position,
                                         legend.box.margin = ggplot2::margin(l=-10))

    facet_lab <- paste0(" * ' (' * n[",ifelse(isCV,'TRN','TST'),"] * ' = ' * ")
    eps <- .Machine$double.eps^0.5
    nlambda <- object$nlambda

    # Get count of support set by trait
    names_nsup <- paste0("nsup_",seq(object$ntraits))
    dat0 <- object$nsup_trait
    dat0$nsup <- apply(dat0[,names_nsup],1,sum)
    dat0[,names_nsup] <- dat0[,names_nsup]/dat0$nsup
    if(isCV){
      dat0 <- dat0[as.character(dat0$trait) %in% names(object$nTRN[object$nTRN>0]),]
    }else{
      dat0 <- dat0[as.character(dat0$trait) %in% names(object$nTST[object$nTST>0]),]
    }

    # Data to plot
    dat <- dat_across <- data.frame(matrix(nrow=0,ncol=7))
    if(nlambda > 1L & !is.null(object$accuracy) & !is.null(object$MSE))
    {
      traits0 <- colnames(object$lambda)
      tt <- reshape2::melt(object$lambda)
      colnames(tt) <- c("SSI","trait","lambda")
      tt$x <- -log(tt$lambda)
      tmp <- object[[which(tolower(names(object)) == tolower(y.stat))]]
      tt$y <- reshape2::melt(tmp)$value
      tt$nsup <- reshape2::melt(object$nsup)$value
      if(isCV){
        tt <- tt[as.character(tt$trait) %in% names(object$nTRN[object$nTRN>0]),]
        tt$n <- object$nTRN[as.character(tt$trait)]
      }else{
        tt <- tt[as.character(tt$trait) %in% names(object$nTST[object$nTST>0]),]
        tt$n <- object$nTST[as.character(tt$trait)]
      }

      if(length(which(tt$lambda < eps))>0){
        tmp <- tt$lambda[which(tt$lambda >= eps)]
        tt[which(tt$lambda < eps),'lambda'] <- ifelse(length(tmp)>0,min(tmp),eps)
      }

      tt0 <- tt[as.character(tt$trait) == "overall",]
      tt <- tt[as.character(tt$trait) != "overall",]

      dat_across <- data.frame(tt0,stringsAsFactors=FALSE)
      dat <- data.frame(tt,stringsAsFactors=FALSE)
    }
    stopifnot(paste(dat0$SSI,dat0$trait) == paste(dat$SSI,dat$trait))
    dat <- data.frame(dat, dat0[,names_nsup])
    dat$trait <- factor(as.character(dat$trait))

    if(nrow(dat) == 0){
       stop("The plot can not be generated with the provided data")
    }
    dat <- dat[!is.na(dat$y),]   # Remove NA values

    n0 <- as.vector(unlist(lapply(split(dat,dat$trait),function(x)x$n[1])))
    levels_trait <- paste0("'",trait_names[levels(dat$trait)],"'", facet_lab, n0,"L * ')'")
    dat$trait2 <- paste0("'",trait_names[as.character(dat$trait)],"'", facet_lab, dat$n,"L * ')'")
    dat$trait2 <- factor(as.character(dat$trait2), levels=levels_trait)

    if("ylim" %in% names(args0)){
       ylim <- args0$ylim
    }else{
       ylim <- c(NA, NA)
    }

    dd <- rep(NA,length(levels_trait))
    if(x.stat == "nsup")
    {
      xd <- 5
      breaksx <- labelsx <- index <- x2 <- c()
      maxx <- 0
      if("xlim" %in% names(args0)){
         xlim <- args0$xlim
      }else{
         xlim <- c(1,max(dat$nsup, na.rm=TRUE))
      }
      for(k in 1:length(levels_trait))
      {
        index0 <- which(as.character(dat$trait2) == levels_trait[k])
        dt <- dat[index0,]

        tmp <- dt$nsup >= xlim[1] & dt$nsup <= xlim[2]
        index0 <- index0[tmp]
        dt <- dt[tmp,]

        # Labels and breaks for the nsup axis
        breaks0 <- get_breaks(x=dt$x, y=dt$nsup, nbreaks.x=nbreaks.x, ymin=xlim[1])

        dd[k] <- maxx + (k-1)*xd
        maxx <- maxx + max(dt$x)
        x2 <- c(x2, dd[k] + dt$x)
        breaksx <- c(breaksx, dd[k] + breaks0$breaks.x)
        labelsx <- c(labelsx, breaks0$breaks.y)
        index <- c(index, index0)
      }
      dat <- dat[index,]
      dat$x <- x2
    }else{
      if("xlim" %in% names(args0)){
        xlim <- args0$xlim
      }else{
        tmp <- dat[dat$nsup>=1,]
        tmp <- tmp[abs(tmp$nsup-min(tmp$nsup))<1E-8,,drop=F]
        xlim <- c(mean(tmp$x, na.rm=T), max(dat$x))
      }

      dat <- dat[dat$x >= xlim[1] & dat$x <= xlim[2],]
    }

    # New data sets by trait
    dat_split <- split(dat, dat$trait2)

    # Data for optimum SSI and smaller lambda (maybe GBLUP)
    dat1 <- do.call(rbind,lapply(dat_split,function(dt){
      dt[ifelse(tolower(y.stat)=="mse",which.min(dt$y),which.max(dt$y)),]
    }))
    dat2 <- do.call(rbind,lapply(dat_split,function(dt){
      dt[which.min(dt$lambda),]
    }))

    ylim2 <- c(0,1)
    # Data for areas
    names0 <- colnames(dat)[!colnames(dat)%in%names_nsup]
    dat3 <- do.call(rbind,lapply(dat_split,function(dt){
      ylim1 <- range(dt$y)
      b <- diff(ylim1)/diff(ylim2)
      a <- ylim1[1] - b*ylim2[1]
      tt <- b*as.matrix(dt[,names_nsup])
      dt2 <- c()
      for(i in 1:nrow(dt)){
        cc0 <- a + c(0,cumsum(tt[i,]))
        cc1 <- cbind(nsupmin=cc0[1:length(names_nsup)],nsupmax=cc0[-1])
        dt2 <- rbind(dt2, data.frame(dt[rep(i,length(names_nsup)),names0],
                                     nsup_trait=names_nsup,cc1))
      }
      rownames(dt2) <- NULL
      dt2
    }))
    dat3$nsup_trait <- as.vector(trait_names[gsub("nsup_","",dat3$nsup_trait)])

    expand.x <- c(0.02,0.015)
    breaksy2 <- seq(0,1,length=object$ntraits+1)
    # Data for 'prop of support set'
    dat4 <- do.call(rbind,lapply(dat_split,function(dt){
        xlim <- range(dt$x)
        ylim1 <- range(dt$y)
        b <- diff(ylim1)/diff(ylim2)
        a <- ylim1[1] - b*ylim2[1]
        data.frame(dt[1,c("trait","trait2"),drop=T],x=xlim[2],
                   x1=xlim[2] + 0*expand.x[2]*diff(xlim),
                   x2=xlim[2] + 1*expand.x[2]*diff(xlim),
                   y=a + b*breaksy2, label=sprintf('%.2f',breaksy2))
    }))
    rownames(dat1)<-rownames(dat2)<-rownames(dat3)<-rownames(dat4) <- NULL
    scaleFUN <- function(x) sprintf('%.2f', x)
    thrcol <- "gray50"
    pp <- ggplot2::ggplot(dat, ggplot2::aes(x=x,y=y)) +
          ggplot2::geom_ribbon(data=dat3, ggplot2::aes(ymin=nsupmin,ymax=nsupmax,fill=nsup_trait),
                               color="white", linewidth=0.2, alpha=alpha) +
          ggplot2::geom_hline(data=dat2, ggplot2::aes(yintercept=y),
                              linewidth=0.5, linetype="dotted", color=thrcol) +
          ggplot2::geom_line(linewidth=lwd, color=line.color) +
          ggplot2::geom_point(data=dat1, ggplot2::aes(x=x), shape=pch[[1]],
                              color=point.color[1], size=point.size[1]) +
          ggplot2::geom_vline(data=dat1, ggplot2::aes(xintercept=x),
                              linewidth=0.5, linetype="dotted", color=thrcol) +
          ggplot2::geom_point(data=dat2, ggplot2::aes(x=x), shape=pch[[2]],
                              color=point.color[2], size=point.size[2]) +
          ggplot2::geom_text(data=dat4, ggplot2::aes(label=label), size=2.6,
                             hjust=1.2, color="gray20") +
          ggplot2::geom_rect(data=dat4, ggplot2::aes(xmin=x1, xmax=Inf, ymin=y, ymax=y),
                             fill=NA, color="gray20") +
          #coord_cartesian(clip = 'off') +
          ggplot2::labs(title=main, x=xlab, y=ylab) + ggplot2::theme_bw() + theme0 +
          ggplot2::facet_wrap(~trait2, scales="free", labeller=ggplot2::label_parsed) +
          ggplot2::scale_y_continuous(labels=scaleFUN, limits=ylim,
                             expand=ggplot2::expansion(mult=c(0.02)),
                             sec.axis=ggplot2::sec_axis(~., ylab2, breaks=NULL)) +
          ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(alpha=0.5)))

    if(!is.null(label)){
      dat1$label <- unlist(lapply(1:nrow(dat1),function(i){
        if(label == "nsup"){
          bquote(n[sup]==.(round(dat1$nsup[i])))
        }else{
          bquote(lambda==.(sprintf('%.6f',dat1$lambda[i])))
        }
      }))

      pp <- pp + ggplot2::geom_text(data=dat1, ggplot2::aes(x=x,y=-Inf,label=label), angle=90, parse=TRUE,
                           vjust=-0.25, hjust=-0.25, size=2.6, color="gray20")
    }

    if(x.stat == "nsup"){
       pp <- pp +
             ggplot2::scale_x_continuous(breaks=breaksx, labels=round(labelsx),
                                         expand=ggplot2::expansion(mult=expand.x))
    }else{
       pp <- pp +
             ggplot2::scale_x_continuous(breaks=scales::extended_breaks(n=nbreaks.x),
                                         limits=xlim,expand=ggplot2::expansion(mult=expand.x))
    }

    return(pp)
}
