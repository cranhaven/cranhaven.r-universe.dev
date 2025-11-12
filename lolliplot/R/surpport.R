############### handle legend ####################
## set the legend as a list,
## if all the legend for different tracks is same
## set draw legend for last track later
handleLegend <- function(legend, len, dat){
  if(length(legend)>0){
    if(is.character(legend)){
      if(!missing(dat)){
        if(is.list(dat)){
          if(length(legend)<length(dat)){
            legend <- rep(legend, length(dat))[seq_along(dat)]
          }
        }else{
          dat <- list(dat)
          legend <- legend[1]
        }
        para <- c("shape", "color", "border", "alpha")
        preset <- list("circle", "white", "black", 1)
        shapeMap <- c("circle"=21, "square"=22, "diamond"=23,
                      "triangle_point_up"=24, "triangle_point_down"=25)
        names(preset) <- para
        legend <- mapply(function(.legend, .dat){
          coln <- colnames(mcols(.dat))
          if(.legend %in% coln){
            labels <- mcols(.dat)[, .legend]
            gp <- lapply(para, function(.ele){
              if(.ele %in% coln){
                mcols(.dat)[, .ele]
              }else{
                rep(preset[[.ele]], length(.dat))
              }
            })
            names(gp) <- para
            names(gp)[names(gp)=="color"] <- "fill"
            gp <- as.data.frame(gp, stringsAsFactors=FALSE)
            gp <- cbind(labels=labels, gp)
            gp[, "shape"] <- shapeMap[gp[, "shape"]]
            names(gp)[names(gp)=="shape"] <- "pch"
            gp <- gp[!duplicated(gp[, "labels"]), ]
            gp <- gp[order(gp[, "labels"]), ]
            gp <- as.list(gp)
          }
        }, legend, dat, SIMPLIFY = FALSE)
      }
    }
    if(!is.list(legend)){
      tmp <- legend
      legend <- vector(mode = "list", length = len)
      legend[[len]] <- tmp
      rm(tmp)
    }else{
      if(length(legend)==1){
        tmp <- legend[[1]]
        legend <- vector(mode = "list", length = len)
        legend[[len]] <- tmp
        rm(tmp)
      }else{
        if("labels" %in% names(legend)){
          tmp <- legend
          legend <- vector(mode = "list", length = len)
          legend[[len]] <- tmp
          rm(tmp)
        }else{
          if(length(legend)<len){
            length(legend) <- len
          }
        }
      }
    }
  }
  return(legend)
}


################ handle ranges #####################
## if !missing(ranges) set ranges as feature ranges
handleRanges <- function(ranges, SNP.gr, features, len){
  if(length(ranges)>0){
    stopifnot(inherits(ranges, c("GRanges", "GRangesList", "list")))
    if(is(ranges, "GRanges")){
      if(length(ranges)==1){
        ranges <- split(rep(ranges, len)[seq.int(len)],
                        seq.int(len))
      }else{
        ranges <- split(rep(ranges, len),
                        rep(seq.int(len), each=len))[seq.int(len)]
      }
    }else{## GRangesList
      if(length(ranges)!=len){
        ranges <- rep(ranges, seq.int(len))[seq.int(len)]
      }
    }
    stopifnot(length(ranges)==len)
  }else{
    if(is(features, "GRanges")){
      ranges <- split(range(unname(features), ignore.strand=TRUE)[rep(1, len)],
                      seq.int(len))
    }else{
      if(length(features)!=len){
        stop("if both SNP.gr and features is GRangesList,",
             " the lengthes of them should be identical.")
      }
      ranges <- GRangesList(lapply(features, function(.ele){
        range(unname(.ele), ignore.strand=TRUE)}))
    }
  }
  return(ranges)
}


##cut all SNP.gr by the range
cutSNP <- function(SNP.gr, ranges, len){
  if(is(ranges, "GRanges")){
    for(i in seq.int(len)){
      range <- ranges[i]
      stopifnot(all(width(SNP.gr[[i]])==1))
      SNP.gr[[i]] <- subsetByOverlaps(SNP.gr[[i]], range, ignore.strand=FALSE)
    }
  }else{
    if(inherits(ranges, c("GRangesList", "list"))){
      for(i in seq.int(len)){
        range <- ranges[[i]]
        stopifnot(all(width(SNP.gr[[i]])==1))
        SNP.gr[[i]] <- subsetByOverlaps(SNP.gr[[i]], range, ignore.strand=FALSE)
      }
    }
  }
  return(SNP.gr)
}


convertHeight2NPCnum <- function(.ele){
  if(is(.ele, "unit")){
    return(convertHeight(.ele, unitTo="npc", valueOnly=TRUE))
  }else{
    if(is.list(.ele)){
      .ele <- sapply(.ele, function(.e){
        if(is(.e, "unit")){
          .e <- convertHeight(.e, unitTo="npc", valueOnly=TRUE)
        }
        .e[1]
      })
      return(unlist(.ele))
    }else{
      if(is.numeric(.ele)){
        return(.ele)
      }else{
        if(is.integer(.ele)){
          return(.ele)
        }else{
          return(.ele)
        }
      }
    }
  }
}


## multiple transcripts in one gene could be separated by featureLayerID
setFeatureLayerID <- function(feature, range){
  feature <- feature[end(feature)>=start(range) &
                       start(feature)<=end(range)]
  if(length(feature$featureLayerID)!=length(feature)){
    feature$featureLayerID <- rep("1", length(feature))
    feature$featureLayerID <- as.character(feature$featureLayerID)
    start(feature)[start(feature)<start(range)] <- start(range)
    end(feature)[end(feature)>end(range)] <- end(range)
  }
  return(feature)
}


## bottomblank, the transcripts legend height
plotFeatureLegend <- function(feature, LINEH, range, xaxis, xaxis.gp, label_on_feature=FALSE){
  if(label_on_feature) return(0)
  if(length(xaxis)>1 || as.logical(xaxis[1])){
    xaxisSpace <- 2
    if(is.numeric(xaxis.gp$cex)) xaxisSpace <- 2*xaxis.gp$cex
  }else{
    xaxisSpace <- 0
  }
  if(length(names(feature))>0){ ## features legend
    feature.s <- feature[!duplicated(names(feature))]
    cex <- if(length(unlist(feature.s$cex))==length(feature.s))
      unlist(feature.s$cex) else 1
    ncol <- getColNum(names(feature.s), cex=cex)
    featureLegendSpace <- max(ceiling(length(names(feature.s)) / ncol) * cex + 1 )
    pushViewport(viewport(x=.5, y=featureLegendSpace*LINEH/2,
                          width=1,
                          height=featureLegendSpace*LINEH,
                          xscale=c(start(range), end(range))))
    color <- if(length(unlist(feature.s$color))==length(feature.s))
      unlist(feature.s$color) else "black"
    fill <- if(length(unlist(feature.s$fill))==length(feature.s))
      unlist(feature.s$fill) else "black"
    pch <- if(length(unlist(feature.s$pch))==length(feature.s))
      unlist(feature.s$pch) else 22
    grid.legend(label=names(feature.s), ncol=ncol,
                byrow=TRUE, vgap=unit(.2, "lines"),
                hgap=unit(.5, "lines"),
                pch=pch,
                gp=gpar(col=color, fill=fill, cex=cex))
    popViewport()
  }else{
    featureLegendSpace <- 0
  }
  bottomblank <- (xaxisSpace + featureLegendSpace) * LINEH
  return(bottomblank)
}


getColNum <- function(labels, spaces="WW", cex){
  ncol <- floor(as.numeric(convertX(unit(1, "npc"), "line")) /
                  maxStringWidth(labels, spaces=spaces, cex) /
                  as.numeric(convertX(stringWidth("W"), "line")))
  nrow <- ceiling(length(labels) / ncol)
  ncol <- ceiling(length(labels) / nrow)
  ncol
}


maxStringWidth <- function(labels, spaces="WW", cex){
  max(as.numeric(convertX(stringWidth(paste0(labels, spaces)), "line"))*cex)
}


plot.grid.xaxis <- function(xaxis, gp=gpar(col="black")){
  ## axis, should be in the bottom of transcripts
  if(length(xaxis)==1 && as.logical(xaxis)) {
    grid.xaxis(gp=gp)
  }
  if(length(xaxis)>1 && is.numeric(xaxis)){
    xaxisLabel <- names(xaxis)
    if(length(xaxisLabel)!=length(xaxis)) xaxisLabel <- TRUE
    grid.xaxis(at=xaxis, label=xaxisLabel, gp=gp)
  }
}


plotFeatures <- function(feature.splited, LINEH, bottomHeight,
                         label_on_feature=FALSE){
  feature.height <- 0
  for(n in seq_along(feature.splited)){
    this.feature.height <-
      max(c(feature.splited[[n]]$height/2,
            .0001)) + 0.2 * LINEH
    feature.height <- feature.height + this.feature.height
    ##baseline
    grid.lines(x=c(0, 1), y=c(bottomHeight+feature.height,
                              bottomHeight+feature.height))
    for(m in seq_along(feature.splited[[n]])){
      this.dat <- feature.splited[[n]][m]
      color <- if(is.list(this.dat$color)) this.dat$color[[1]] else
        this.dat$color
      if(length(color)==0) color <- "black"
      fill <- if(is.list(this.dat$fill)) this.dat$fill[[1]] else
        this.dat$fill
      if(length(fill)==0) fill <- "white"
      this.cex <- if(length(this.dat$cex)>0) this.dat$cex[[1]][1] else 1
      lwd <- if(length(this.dat$lwd)>0) this.dat$lwd[[1]][1] else 1
      this.feature.height.m <-
        if(length(this.dat$height)>0)
          this.dat$height[[1]][1] else
            2*this.feature.height
      grid.rect(x=start(this.dat)-.1, y=bottomHeight+feature.height,
                width=width(this.dat)-.8,
                height=this.feature.height.m,
                just="left", gp=gpar(col=color, fill=fill, lwd=lwd),
                default.units = "native")
      if(label_on_feature & !is.na(names(this.dat)[1])){
        grid.text(x=(start(this.dat)+end(this.dat))/2,
                  y=bottomHeight+feature.height,
                  just = "centre",
                  label = names(this.dat)[1],
                  gp= gpar(list(cex=this.cex *
                                  this.feature.height.m/
                                  this.feature.height,
                                color=color)),
                  default.units = "native")
      }
    }
    feature.height <- feature.height + this.feature.height
  }
  feature.height
}


plotLollipops <- function(SNPs, feature.height, bottomHeight, baseline,
                          type, ranges, yaxis, yaxis.gp, scoreMax, scoreMax0, scoreType,
                          LINEW, cex, ratio.yx, GAP, pin, dashline.col,
                          side=c("top", "bottom"), jitter=c("node", "label"),
                          main=TRUE){
  side <- match.arg(side)
  jitter <- match.arg(jitter)
  if(side=="top"){
    pushViewport(viewport(y=bottomHeight,
                          height=1,
                          just="bottom",
                          xscale=c(start(ranges),
                                   end(ranges)),
                          clip="off"))
  }else{
    pushViewport(viewport(y=bottomHeight+feature.height,
                          height=1,
                          just="top",
                          xscale=c(start(ranges),
                                   end(ranges)),
                          yscale=c(1, 0),
                          clip="off"))
  }
  if(type=="pie.stack" && length(SNPs$stack.factor)>0){
    stopifnot(is.vector(SNPs$stack.factor, mode="character"))
    if(length(SNPs$stack.factor.order)>0 ||
       length(SNPs$stack.factor.first)>0){
      warning("stack.factor.order and stack.factor.first are used by this function!",
              "The values in these column will be removed.")
    }
    ## condense the SNPs
    stack.factors <- unique(as.character(SNPs$stack.factor))
    stack.factors <- sort(stack.factors)
    stack.factors.order <- seq_along(stack.factors)
    names(stack.factors.order) <- stack.factors
    SNPs <- SNPs[order(as.character(seqnames(SNPs)), start(SNPs),
                       as.character(SNPs$stack.factor))]
    SNPs$stack.factor.order <- stack.factors.order[SNPs$stack.factor]
    SNPs$stack.factor.first <- !duplicated(SNPs)
    SNPs.condense <- SNPs
    SNPs.condense$oid <- seq_along(SNPs)
    SNPs.condense$factor <- paste(as.character(seqnames(SNPs)), start(SNPs), end(SNPs))
    SNPs.condense <- split(SNPs.condense, SNPs.condense$factor)
    SNPs.condense <- lapply(SNPs.condense, function(.ele){
      .oid <- .ele$oid
      .gr <- .ele[1]
      mcols(.gr) <- NULL
      .gr$oid <- NumericList(.oid)
      .gr
    })
    SNPs.condense <- unlist(GRangesList(SNPs.condense), use.names = FALSE)
    SNPs.condense <- sort(SNPs.condense)
    lab.pos.condense <- jitterLables(start(SNPs.condense),
                                     xscale=c(start(ranges), end(ranges)),
                                     lineW=LINEW*cex)
    lab.pos.condense <- reAdjustLabels(lab.pos.condense,
                                       lineW=LINEW*cex)
    condense.ids <- SNPs.condense$oid
    lab.pos <- rep(lab.pos.condense, elementNROWS(condense.ids))
    lab.pos <- lab.pos[order(unlist(condense.ids))]
  }else{
    lab.pos <- jitterLables(start(SNPs),
                            xscale=c(start(ranges), end(ranges)),
                            lineW=LINEW*cex)
    lab.pos <- reAdjustLabels(lab.pos,
                              lineW=LINEW*cex)
  }

  if(length(SNPs)>0){
    yaxisat <- NULL
    yaxisLabel <- TRUE
    if(length(yaxis)>1 && is.numeric(yaxis)){
      yaxisat <- yaxis
      if(length(names(yaxis))==length(yaxis)) yaxisLabel <- names(yaxis)
      yaxis <- TRUE
    }
    if(yaxis && scoreMax>1 && !type %in% c("pie", "pie.stack")){
      if(side=="top"){
        grid.yaxis(at=yaxisat,
                   label=yaxisLabel,
                   main = main,
                   gp=yaxis.gp,
                   vp=viewport(x=.5+ifelse(main, -1, 1) *LINEW,
                               y=feature.height+5.25*GAP*cex+
                                 scoreMax*LINEW*ratio.yx/2*cex,
                               width=1,
                               height=scoreMax*LINEW*ratio.yx*cex,
                               yscale=c(0, scoreMax0+.5)))
      }else{
        grid.yaxis(at=yaxisat,
                   label=yaxisLabel,
                   main = main,
                   gp=yaxis.gp,
                   vp=viewport(x=.5+ifelse(main, -1, 1) *LINEW,
                               y=1-(feature.height+5.25*GAP*cex+
                                      scoreMax*LINEW*ratio.yx/2*cex),
                               width=1,
                               height=scoreMax*LINEW*ratio.yx*cex,
                               yscale=c(scoreMax0+.5, 0)))
      }
    }
    if(length(SNPs$alpha)==length(SNPs)){
      SNPs$alpha[is.na(SNPs$alpha)] <- 0
      if(all(is.numeric(SNPs$alpha))){
        if(any(SNPs$alpha>1)){## convert to 0-1
          SNPs$alpha <- SNPs$alpha/max(SNPs$alpha)
        }
      }else{ ## not correct format.
        SNPs$alpha <- as.numeric(factor(as.character(SNPs$alpha)))
        SNPs$alpha <- (SNPs$alpha+max(SNPs$alpha))/max(SNPs$alpha)/2
      }
    }else{
      SNPs$alpha <- NULL
    }
    if(type=="circle"){
      if(length(SNPs$shape)==length(SNPs)){
        ## shape could only be "circle", "square", "diamond", "triangle_point_up", "triangle_point_down"
        if(!all(SNPs$shape %in% c("circle", "square", "diamond", "triangle_point_up", "triangle_point_down"))){
          message('shape must be "circle", "square", "diamond", "triangle_point_up", or "triangle_point_down"')
          SNPs$shape <- as.numeric(factor(SNPs$shape))
          SNPs$shape <- rep(c("circle", "square", "diamond", "triangle_point_up", "triangle_point_down"),
                            max(SNPs$shape))[SNPs$shape]
        }
      }else{
        SNPs$shape <- NULL
      }
    }
    for(m in seq_along(SNPs)){
      this.dat <- SNPs[m]
      color <- if(is.list(this.dat$color)) this.dat$color[[1]] else this.dat$color
      border <-
        if(is.list(this.dat$border)) this.dat$border[[1]] else this.dat$border
      fill <- if(is.list(this.dat$fill)) this.dat$fill[[1]] else this.dat$fill
      alpha <- if(length(this.dat$alpha)>0) this.dat$alpha[[1]] else 1
      lwd <- if(is.list(this.dat$lwd)) this.dat$lwd[[1]] else this.dat$lwd
      id <- if(is.character(this.dat$label)) this.dat$label else NA
      id.col <- if(length(this.dat$label.col)>0) this.dat$label.col else "black"
      shape <- if(length(this.dat$shape)>0) this.dat$shape[[1]] else "circle"
      rot <- if(length(this.dat$label.rot)>0) this.dat$label.rot else 15
      this.cex <- if(length(this.dat$cex)>0) this.dat$cex[[1]][1] else 1
      this.dashline.col <-
        if(length(this.dat$dashline.col)>0) this.dat$dashline.col[[1]][1] else dashline.col
      if(length(names(this.dat))<1) this.dashline.col <- NA
      this.dat.mcols <- mcols(this.dat)
      this.dat.mcols <- cleanDataMcols(this.dat.mcols, type)

      grid.lollipop(x1=convertX(unit(start(this.dat), "native"), "npc",
                                valueOnly=TRUE),
                    y1=baseline,
                    x2=convertX(unit(ifelse(jitter=="node",
                                            lab.pos[m],
                                            start(this.dat)),
                                     "native"), "npc", valueOnly=TRUE),
                    y2=feature.height,
                    y3=4*GAP*cex, y4=2.5*GAP*cex,
                    radius=LINEW*cex/2,
                    col=color,
                    border=border,
                    percent=this.dat.mcols,
                    edges=100,
                    type=type,
                    ratio.yx=ratio.yx,
                    pin=pin,
                    scoreMax=scoreMax * LINEW * cex,
                    scoreType=scoreType,
                    id=id, id.col=id.col,
                    cex=this.cex, lwd=lwd, dashline.col=this.dashline.col,
                    side=side, rot=rot, alpha=alpha, shape=shape)

    }
    this.height <- getHeight(SNPs,
                             ratio.yx, LINEW, GAP, cex, type,
                             scoreMax=scoreMax,
                             level="data")
    labels.rot <- 90
    if(length(names(SNPs))>0){
      if(type=="pie.stack"){
        ## unique lab.pos and SNPs
        idx <- !duplicated(names(SNPs))
        lab.pos <- lab.pos[idx]
        SNPs <- SNPs[idx]
      }
      labels.x <- lab.pos
      labels.text <- names(SNPs)
      labels.just <- ifelse(side=="top", "left", "right")
      labels.hjust <- NULL
      labels.vjust <- NULL
      labels.check.overlap <- FALSE
      labels.default.units <- "native"
      labels.gp <- gpar(cex=cex)

      ## change the parameter by use definations.
      for(label.parameter in c("x", "y", "just", "hjust", "vjust",
                               "rot", "check.overlap", "default.units",
                               "gp")){
        label.para <- paste0("label.parameter.", label.parameter)
        if(label.para %in% colnames(mcols(SNPs))){
          assign(paste0("labels.", label.parameter),
                 mcols(SNPs)[, label.para])
        }
      }
      if(!"cex" %in% names(labels.gp)){
        labels.gp <- c(labels.gp, cex=cex)
      }
      mergeList <- function(.ele){
        .n <- unique(unlist(lapply(.ele, names)))
        .out <- list()
        if(length(.n)>0){
          for(.name in .n){
            .out[[.name]] <- sapply(.ele, function(.e){
              if(.name %in% names(.e)){
                .e[[.name]][1]
              }else{
                NA
              }
            })
          }
        }else{
          .n <- unique(names(.ele))
          for(.name in .n){
            .out[[.name]] <- unlist(.ele[names(.ele) %in% .name])
          }
        }
        .out
      }
      labels.gp <- mergeList(labels.gp)
      labels.gp[duplicated(names(labels.gp))] <- NULL
      labels.gp <- do.call(gpar, labels.gp)
      if(jitter=="label"){
        ## add guide lines
        rased.height <- 4*GAP*cex
        guide.height <- 2.5*GAP*cex
        for(i in seq_along(SNPs)){
          this.dashline.col <-
            if(length(SNPs[i]$dashline.col)>0)
              SNPs[i]$dashline.col[[1]][1] else
                dashline.col
          if(length(names(SNPs[i]))<1) this.dashline.col <- NA
          grid.lines(x=c(start(SNPs[i]), labels.x[i]),
                     y=c(this.height+feature.height-cex*LINEW,
                         this.height+feature.height+rased.height),
                     default.units = labels.default.units,
                     gp=gpar(col=this.dashline.col, lty=3))
          grid.lines(x=c(labels.x[i], labels.x[i]),
                     y=c(this.height+rased.height+feature.height,
                         this.height+rased.height+
                           guide.height+feature.height),
                     default.units = labels.default.units,
                     gp=gpar(col=this.dashline.col, lty=3))
        }
        ## add this height
        this.height <- this.height + rased.height + guide.height
      }
      grid.text(x=labels.x, y=this.height + feature.height,
                label = labels.text,
                just = labels.just,
                hjust = labels.hjust,
                vjust = labels.vjust,
                rot=labels.rot,
                check.overlap = labels.check.overlap,
                default.units = labels.default.units,
                gp=labels.gp)
    }
  }
  popViewport()
}


jitterLables <- function(coor, xscale, lineW, weight=1.2){
  if(weight==1.2) {
    stopifnot("Please sort your inputs by start position"=
                order(coor)==1:length(coor))
  }
  if(weight<0.5) return(coor)
  stopifnot(length(xscale)==2)
  pos <- convertX(unit(coor, "native"), "npc", valueOnly=TRUE)
  pos.diff <- diff(c(0, pos, 1))
  idx <- which(pos.diff < weight*lineW)
  if(length(idx)<1){
    return(coor)
  }
  if(all(idx %in% c(1, length(pos)+1))){
    return(coor)
  }
  idx.diff <- diff(c(-1, idx))
  idx.grp <- rle(idx.diff)
  idx.grp$values[idx.grp$values==1] <- length(pos) + 1:sum(idx.grp$values==1)
  idx.grp <- inverse.rle(idx.grp)
  idx.grp.w <- which(idx.grp>length(pos))-1
  idx.grp.w <- idx.grp.w[idx.grp.w>0]
  idx.grp[idx.grp.w] <- idx.grp[idx.grp.w+1]
  idx.grp <- split(idx, idx.grp)
  flag <- as.numeric(names(idx.grp))>length(pos)
  idx.grp.mul <- lapply(idx.grp[flag], function(.ele){
    c(.ele[1]-1, .ele)
  })
  idx.grp.sin <- lapply(idx.grp[!flag], function(.ele){
    lapply(as.list(.ele), function(.ele){c(.ele-1, .ele)})
  })
  idx.grp.sin <- unlist(idx.grp.sin, recursive = FALSE)
  idx.grp <- c(idx.grp.mul, idx.grp.sin)

  adj.pos <- lapply(idx.grp, function(.ele){
    .ele <- .ele[.ele>0 & .ele<=length(pos)]
    this.pos <- pos[.ele]
    names(this.pos) <- .ele
    if(length(this.pos)%%2==1){
      center <- ceiling(length(this.pos)/2)
    }else{
      center <- length(this.pos)/2 + .5
    }
    if(length(this.pos)>5){ ## too much, how to jitter?
      this.pos <- this.pos +
        ((1:length(this.pos))-center) * (weight-.1) *
        lineW/ceiling(log(length(this.pos), 5))
    }else{
      this.pos <- this.pos +
        ((1:length(this.pos))-center) * (weight-.1) * lineW
    }
    this.pos
  })
  names(adj.pos) <- NULL
  adj.pos <- unlist(adj.pos)
  coor[as.numeric(names(adj.pos))] <- adj.pos*diff(xscale)+xscale[1]

  Recall(coor, xscale=xscale, lineW=lineW, weight=weight-0.2)
}


reAdjustLabels <- function(coor, lineW){
  # resort
  coor <- sort(coor)
  bins <- ceiling(1/lineW)
  pos <- convertX(unit(coor, "native"), "npc", valueOnly=TRUE)
  pos.bin <- cut(pos, c(-Inf, (0:bins)*lineW, Inf), labels=0:(bins+1), right=FALSE)

  ## split the coors by into clusters
  ## give the clusters with more idx more spaces if there are spaces between clusters
  tbl <- table(pos.bin)
  if(all(tbl<2)) return(coor)
  tbl.len <- length(tbl)
  if(tbl.len<3) return(coor)
  loops <- 1000
  loop <- 1
  while(any(tbl==0) && any(tbl>1) && loop < loops){
    tbl.bk <- tbl
    for(i in order(tbl.bk, decreasing=TRUE)){
      if(tbl[i]>1 && tbl.bk[i]==tbl[i]){
        if(i==1){
          if(tbl[2]<tbl[1]){
            half <- sum(tbl[1:2])/2
            tbl[2] <- ceiling(half)
            tbl[1] <- floor(half)
          }
        }else{
          if(i==tbl.len){
            if(tbl[tbl.len]>tbl[tbl.len-1]){
              half <- sum(tbl[(tbl.len-1):tbl.len])/2
              tbl[tbl.len-1] <- ceiling(half)
              tbl[tbl.len] <- floor(half)
            }
          }else{
            if(tbl[i-1]<tbl[i+1]){
              ## i-1 and i should be balanced
              half <- sum(tbl[(i-1):i])/2
              tbl[i-1] <- floor(half)
              tbl[i] <- ceiling(half)
            }else{
              half <- sum(tbl[i:(i+1)])/2
              tbl[i] <- floor(half)
              tbl[i+1] <- ceiling(half)
            }
          }
        }
      }
    }
    loop <- loop + 1
  }
  coef <- unlist(lapply(tbl, function(.ele){
    if(.ele==0) return(0)
    .ele <- seq(from=0, to=1, length.out=.ele+1)
    (.ele[-length(.ele)] + .ele[-1])/2
  }))
  coef <- coef[coef!=0]
  coor <- (rep(as.numeric(names(tbl)), tbl) - 1 + coef) * lineW
  coor <- convertX(unit(coor, "npc"), "native", valueOnly=TRUE)
  coor
}


cleanDataMcols <- function(this.dat.mcols, type){
  this.dat.mcols <-
    this.dat.mcols[,
                   !colnames(this.dat.mcols) %in%
                     c("color", "fill", "lwd", "id",
                       "cex", "dashline.col",
                       "id.col", "stack.factor", "SNPsideID",
                       "shape", "alpha"),
                   drop=FALSE]
  if(type!="pie.stack"){
    this.dat.mcols <-
      this.dat.mcols[, !colnames(this.dat.mcols) %in%
                       c("stack.factor.order",
                         "stack.factor.first"),
                     drop=FALSE]
  }
  this.dat.mcols <-
    this.dat.mcols[, !grepl("^label.parameter",
                            colnames(this.dat.mcols)),
                   drop=FALSE]
  return(this.dat.mcols)
}


grid.lollipop <- function (x1=.5, y1=.5,
                           x2=.5, y2=.75,
                           y3=.04, y4=.02,
                           radius=.8,
                           col=NULL,
                           border=NULL,
                           percent=NULL,
                           edges=100,
                           type=c("circle", "pie", "pin", "pie.stack", "flag"),
                           ratio.yx=1,
                           pin=NULL,
                           scoreMax,
                           scoreType,
                           id=NA, id.col="black",
                           cex=1, lwd=1,
                           dashline.col="gray80",
                           side=c("top", "bottom"),
                           rot=15,
                           alpha=NULL,
                           shape=shape){
  side <- match.arg(side)
  stopifnot(is.numeric(c(x1, x2, y1, y2, y3, y4, radius, edges)))
  type <- match.arg(type)
  side <- side!="top"
  if(!type %in% c("pie", "pie.stack")){
    this.score <- if(length(percent$score)>0) max(percent$score, 1) else 1
    if(type=="circle"){
      y0 <- c(y1, y2, y2+y3, y2+y3+y4+(this.score-1)*2*radius*ratio.yx+(1-cex)*radius*ratio.yx)
      if(scoreType) y0[4] <- y2+y3+y4
      if(side) y0 <- 1 - y0
      grid.lines(x=c(x1, x1, x2, x2), y=y0,
                 gp=gpar(col=border, lwd=lwd))
      y0 <- c(y2+y3+y4+this.score*2*radius*ratio.yx,
              y2+y3+y4+scoreMax*ratio.yx)
      if(scoreType) y0[1] <- y2+y3+y4+this.score*2*radius*ratio.yx*cex
      if(side) y0 <- 1 - y0
      grid.lines(x=c(x2, x2),
                 y=y0,
                 gp=gpar(col=dashline.col, lty=3, lwd=lwd))
    }else{
      y0 <- c(y1, y2, y2+y3, y2+y3+y4+(this.score-.5)*2*radius*ratio.yx)
      if(side) y0 <- 1 - y0
      grid.lines(x=c(x1, x1, x2, x2), y=y0,
                 gp=gpar(col=border, lwd=lwd))
    }

  }else{
    if(type=="pie.stack"){
      if(percent$stack.factor.first){
        y0 <- c(y1, y2, y2+y3, y2+y3+y4)
        if(side) y0 <- 1 - y0
        grid.lines(x=c(x1, x1, x2, x2), y=y0,
                   gp=gpar(col=border, lwd=lwd))
        y0 <- c(y2+y3+y4, y2+y3+y4+scoreMax*ratio.yx)
        if(side) y0 <- 1 - y0
        grid.lines(x=c(x2, x2),
                   y=y0,
                   gp=gpar(col=dashline.col, lty=3, lwd=lwd))
      }
    }else{
      y0 <- c(y1, y2, y2+y3, y2+y3+y4)
      if(side) y0 <- 1 - y0
      grid.lines(x=c(x1, x1, x2, x2), y=y0,
                 gp=gpar(col=border, lwd=lwd))
    }
  }
  if(length(pin)>0){
    if(length(border)>0) pin@paths[[2]]@rgb <- rgb2hex(col2rgb(border[1]))
    if(length(col)>0) pin@paths[[1]]@rgb <- rgb2hex(col2rgb(col[1]))
    if(length(col)>1) pin@paths[[3]]@rgb <- rgb2hex(col2rgb(col[2]))
  }
  switch(type,
         circle={
           if(length(border)==0) border <- "black"
           if(length(col)==0) col <- "white"
           if(scoreType){
             for(i in 1:this.score){
               y0 <- y2+y3+y4+2*radius*ratio.yx*(i-.5)*cex
               if(side) y0 <- 1 - y0
               switch(shape, #"circle", "square", "diamond", "triangle_point_up", "star", or "triangle_point_down"
                      circle=grid.circle1(x=x2, y=y0,
                                          r=radius*ratio.yx*cex,
                                          gp=gpar(col=border, fill=col, lwd=lwd, alpha=alpha)),
                      square=grid.square(x=x2, y=y0,
                                         r=radius*ratio.yx*cex,
                                         gp=gpar(col=border, fill=col, lwd=lwd, alpha=alpha)),
                      diamond=grid.diamond(x=x2, y=y0,
                                           r=radius*ratio.yx*cex,
                                           gp=gpar(col=border, fill=col, lwd=lwd, alpha=alpha)),
                      triangle_point_up=grid.triangle_point_up(x=x2, y=y0,
                                                               r=radius*ratio.yx*cex,
                                                               gp=gpar(col=border, fill=col, lwd=lwd, alpha=alpha)),
                      triangle_point_down=grid.triangle_point_down(x=x2, y=y0,
                                                                   r=radius*ratio.yx*cex,
                                                                   gp=gpar(col=border, fill=col, lwd=lwd, alpha=alpha)),
                      star=grid.star(x=x2, y=y0,
                                     r=radius*ratio.yx*cex,
                                     gp=gpar(col=border, fill=col, lwd=lwd, alpha=alpha)),
                      grid.circle1(x=x2, y=y0,
                                   r=radius*ratio.yx*cex,
                                   gp=gpar(col=border, fill=col, lwd=lwd, alpha=alpha)))

             }
           }else{
             y0 <- y2+y3+(this.score-.5)*2*radius*ratio.yx+y4
             if(side) y0 <- 1 - y0
             switch(shape,
                    circle=grid.circle1(x=x2, y=y0,
                                        r=radius*ratio.yx*cex,
                                        gp=gpar(col=border, fill=col, lwd=lwd, alpha=alpha)),
                    square=grid.square(x=x2, y=y0,
                                       r=radius*ratio.yx*cex,
                                       gp=gpar(col=border, fill=col, lwd=lwd, alpha=alpha)),
                    diamond=grid.diamond(x=x2, y=y0,
                                         r=radius*ratio.yx*cex,
                                         gp=gpar(col=border, fill=col, lwd=lwd, alpha=alpha)),
                    triangle_point_up=grid.triangle_point_up(x=x2, y=y0,
                                                             r=radius*ratio.yx*cex,
                                                             gp=gpar(col=border, fill=col, lwd=lwd, alpha=alpha)),
                    triangle_point_down=grid.triangle_point_down(x=x2, y=y0,
                                                                 r=radius*ratio.yx*cex,
                                                                 gp=gpar(col=border, fill=col, lwd=lwd, alpha=alpha)),
                    star=grid.star(x=x2, y=y0,
                                   r=radius*ratio.yx*cex,
                                   gp=gpar(col=border, fill=col, lwd=lwd, alpha=alpha)),
                    grid.circle1(x=x2, y=y0,
                                 r=radius*ratio.yx*cex,
                                 gp=gpar(col=border, fill=col, lwd=lwd, alpha=alpha)))
             if(!is.na(id)){
               y0 <- y2+y3+(this.score-.5)*2*radius*ratio.yx+y4
               if(side) y0 <- 1 - y0
               grid.text(label=id, x=x2,
                         y=y0,
                         just="centre", gp=gpar(col=id.col, cex=.75*cex))
             }
           }
         },
         pie={
           y0 <- y2+y3+y4+radius*ratio.yx
           if(side) y0 <- 1 - y0
           grid.pie(x=x2, y=y0,
                    radius = radius*cex,
                    col = col,
                    border = border,
                    percent=percent,
                    edges=edges,
                    lwd=lwd, alpha=alpha)
         },
         pie.stack={
           y0 <- y2+y3+y4+(2*percent$stack.factor.order-1)*radius*ratio.yx
           if(side) y0 <- 1 - y0
           grid.pie(x=x2,
                    y=y0,
                    radius = radius*cex,
                    col = col,
                    border = border,
                    percent=percent[, !colnames(percent) %in%
                                      c("stack.factor.order",
                                        "stack.factor.first")],
                    edges=edges,
                    lwd=lwd, alpha=alpha)
         },
         pin={
           y0 <- y2+y3+(this.score-.5)*2*radius*ratio.yx+y4/2
           if(side) y0 <- 1 - y0
           grid.picture(picture=pin, x=x2,
                        y=y0,
                        width=2*radius*ratio.yx*cex,
                        height=3*radius*ratio.yx*cex+y4)
           if(!is.na(id)){
             y0 <- y2+y3+(this.score-.25)*2*radius*ratio.yx+2*y4/3
             grid.text(label=id, x=x2,
                       y=y0,
                       just="centre", gp=gpar(col=id.col, cex=.5*cex))
           }
         },
         flag={
           if(is.na(id)){
             id <- " "
           }
           LINEH <- as.numeric(convertY(unit(1, "line"), "npc"))*cex
           y0 <- y2+y3+(this.score-.5)*2*radius*ratio.yx+y4/2
           if(side) y0 <- 1 - y0
           LINEW <- as.numeric(convertX(stringWidth(paste0("o", id, "u")), "npc"))*cex
           LINEW <- LINEW * sign(cos(pi*rot/180))
           LINEH0 <- LINEW*ratio.yx*tan(pi*rot/180)
           grid.polygon(x=c(x2, x2+LINEW, x2+LINEW, x2),
                        y=c(y0, y0+LINEH0, y0+LINEH0+LINEH*1.25, y0+LINEH*1.25),
                        gp=gpar(fill=col, col=border, alpha=alpha))
           grid.text(label=id, x=x2+LINEW*.5,
                     y=y0 + LINEH*.625+LINEH0*.5,
                     hjust=.5, vjust=.5,
                     gp=gpar(col=id.col, cex=cex),
                     rot=rot)
         },
         grid.pie(x=x2, y=y2+y3+y4+radius*ratio.yx,
                  radius = radius*cex,
                  col = col,
                  border = border,
                  percent=percent,
                  edges=edges,
                  lwd=lwd, alpha=alpha))
}


grid.pie <- function (x=.5, y=.5,
                      radius=.8,
                      col=NULL,
                      border=NULL,
                      percent=NULL,
                      edges=100,
                      lwd=1,
                      alpha=1) {
  if(length(percent)>0) percent <- unlist(percent[, sapply(percent, is.numeric)])
  if(length(percent)<1){
    percent <- 1
  }
  percent <- c(0, cumsum(percent)/sum(percent))
  if(any(is.na(percent))){
    warning("There are events with NA number after calculating the percentage.",
            "Please make sure all the events must contain at least one values greater than 0")
    percent[is.na(percent)] <- 0
  }
  dx <- diff(percent)
  nx <- length(dx)
  if (is.null(col))
    col <- c("white", "lightblue", "mistyrose", "lightcyan",
             "lavender", "cornsilk")
  if (!is.null(col))
    col <- rep_len(col, nx)
  if (!is.null(border))
    border <- rep_len(border, nx)
  twopi <- 2 * pi
  ratio.yx <- 1/as.numeric(convertX(unit(1, "snpc"), "npc"))
  t2xy <- function(t) {
    t2p <- twopi * t + pi/2
    list(x = radius * cos(t2p), y = radius * sin(t2p) * ratio.yx)
  }
  for (i in 1L:nx) {
    n <- max(2, floor(edges * dx[i]))
    P <- t2xy(seq.int(percent[i], percent[i + 1], length.out = n))
    grid.polygon(unit(c(P$x, 0)+x,"npc"), unit(c(P$y, 0)+y, "npc"), gp=gpar(col = border[i], fill = col[i], lwd=lwd, alpha=alpha))
  }
  invisible(NULL)
}


getHeight <- function(SNPs, ratio.yx, LINEW, GAP, cex, type, scoreMax,
                      level=c("data", "data&labels")){
  level=match.arg(level)
  stack.factors <- unique(as.character(SNPs$stack.factor))
  stack.factors <- sort(stack.factors)
  if(level=="data"){
    switch(type,
           circle={
             labels.y <- LINEW + # add gaps for labels
               6.5*GAP*cex +
               scoreMax * LINEW * ratio.yx*cex
           },
           pin={
             if(length(SNPs$score)>0) {
               this.scores <- ceiling(SNPs$score)
             }else {
               this.scores <- .5
             }
             this.scores[is.na(this.scores)] <- .5
             labels.y <- LINEW +
               6.5*GAP*cex +
               (this.scores-0.5) * LINEW * ratio.yx*cex
           },
           pie={
             labels.y <- LINEW*max(ratio.yx, 1.2) +
               6.5*GAP*cex + 0.5 * LINEW * ratio.yx * cex
           },
           pie.stack={
             labels.y <- LINEW +
               6.5*GAP*cex +
               (scoreMax-0.5) * LINEW * ratio.yx*cex
           },
           flag={
             labels.y <- LINEW +
               6.5*GAP*cex +
               scoreMax * LINEW * ratio.yx*cex
           })
    labels.y
  }else{
    if(length(SNPs$label.parameter.rot)>0) {
      labels.rot <- SNPs$label.parameter.rot
    }else{
      labels.rot <- 90
    }
    labels.cex <- 1
    if(length(SNPs$label.parameter.gp)>0){
      if(length(SNPs$label.parameter.gp$cex)>0)
        labels.cex <- SNPs$label.parameter.gp$cex[[1]][1]
    }
    labels.length.rate <- labels.cex * max(cospi((labels.rot-90)/180), 0) * ratio.yx
    stringH <- as.numeric(convertY(stringHeight("W"), "npc"))

    switch(type,
           circle={
             if(length(names(SNPs))>0){
               maxStrHeight <-
                 max(as.numeric(
                   convertX(stringWidth(names(SNPs)), "npc")
                 ))+stringH
             }else{
               maxStrHeight <- 0
             }
             maxStrHeight <- maxStrHeight * labels.length.rate
             ypos <- LINEW + 6.5*GAP*cex +
               scoreMax * LINEW * ratio.yx*cex + maxStrHeight
           },
           pin={
             if(length(names(SNPs))>0){
               thisStrHeight <- max(as.numeric(
                 convertX(stringWidth(names(SNPs)), "npc")) ) +
                 stringH
             }else{
               thisStrHeight <- 0
             }
             thisStrHeight <- thisStrHeight * labels.length.rate
             if(length(SNPs$score)>0){
               ypos <-
                 max(LINEW +
                       6.5*GAP*cex +
                       (SNPs$score-0.5) * LINEW * ratio.yx*cex +
                       thisStrHeight)
             }else{
               ypos <- max(LINEW*max(ratio.yx, 1.2) +
                             6.5*GAP*cex + thisStrHeight)
             }
           },
           pie={
             if(length(names(SNPs))>0){
               maxStrHeight <-
                 max(as.numeric(
                   convertX(stringWidth(names(SNPs)), "npc")
                 ))+stringH
             }else{
               maxStrHeight <- 0
             }
             maxStrHeight <- maxStrHeight * labels.length.rate
             ypos <- LINEW +
               6.5*GAP*cex + maxStrHeight
           },
           pie.stack={
             if(length(names(SNPs))>0){
               maxStrHeight <-
                 max(as.numeric(
                   convertX(stringWidth(names(SNPs)), "npc")
                 ))+stringH
             }else{
               maxStrHeight <- 0
             }
             maxStrHeight <- maxStrHeight * labels.length.rate
             ypos <- LINEW +
               6.5*GAP*cex + maxStrHeight +
               (scoreMax-0.5) * LINEW * ratio.yx*cex
           },
           flag={
             if(length(names(SNPs))>0){
               maxStrHeight <-
                 max(as.numeric(
                   convertX(stringWidth(names(SNPs)), "npc")
                 ))+stringH
             }else{
               maxStrHeight <- 0
             }
             maxStrHeight <- maxStrHeight * labels.length.rate
             ypos <- LINEW + 6.5*GAP*cex +
               scoreMax * LINEW * ratio.yx*cex + maxStrHeight
           }
    )
    ypos
  }
}


plotLegend <- function(legend, this.height, LINEH){
  ypos <- this.height
  pch <- 21
  if(length(legend)>0){
    if(is.list(legend)){
      thisLabels <- legend[["labels"]]
      if("pch" %in% names(legend)) pch <- legend[["pch"]]
      gp <- legend[!names(legend) %in% c("labels", "pch")]
      if(is.null(gp$cex)) gp$cex <- 1
      class(gp) <- "gpar"
    }else{
      thisLabels <- names(legend)
      gp <- gpar(fill=legend, cex=1)
    }
    if(length(thisLabels)>0){
      ncol <- getColNum(thisLabels, cex=gp$cex)
      topblank <- ceiling(length(thisLabels) / ncol) * gp$cex[1]
      pushViewport(viewport(x=.5,
                            y=ypos+(topblank+.2*gp$cex[1])*LINEH/2,
                            width=1,
                            height=topblank*LINEH,
                            just="bottom"))
      this.height <- ypos + (topblank+.2*gp$cex[1])*LINEH
      grid.legend(label=thisLabels, ncol=ncol,
                  byrow=TRUE, vgap=unit(.1*gp$cex[1], "lines"),
                  hgap=unit(.5*gp$cex[1], "lines"),
                  pch=pch,
                  gp=gp)
      popViewport()
    }
  }
  this.height + LINEH
}


#"circle", "square", "diamond", "triangle_point_up", "star", or "triangle point_down"
grid.circle1 <- function(x = 0.5, y = 0.5, r = 0.5,
                         default.units = "npc", name = NULL,
                         gp = gpar(), draw = TRUE, vp = NULL){
  fill <- gp$fill
  col <- gp$col
  lwd <- if(length(gp$lwd)>0) gp$lwd else 1
  alpha <- gp$alpha
  if(is.null(fill)) fill <- "white"
  twopi <- 2 * pi
  ratio.yx <- 1/as.numeric(convertX(unit(1, "snpc"), "npc"))
  t2xy <- function(t) {
    t2p <- twopi * t + pi/2
    list(x = r * cos(t2p)/ratio.yx, y = r * sin(t2p))
  }
  P <- t2xy(seq.int(0, 1, length.out = 100))
  invisible(grid.polygon(unit(P$x+x,"npc"), unit(P$y+y, "npc"),
                         gp=gpar(col = col, fill = fill, lwd=lwd, alpha=alpha)))
}


grid.diamond <- function(x = 0.5, y = 0.5, r = 0.5,
                         default.units = "npc", name = NULL,
                         gp = gpar(), draw = TRUE, vp = NULL){
  fill <- gp$fill
  col <- gp$col
  lwd <- if(length(gp$lwd)>0) gp$lwd else 1
  alpha <- gp$alpha
  if(is.null(fill)) fill <- "white"
  ratio.yx <- 1/as.numeric(convertX(unit(1, "snpc"), "npc"))
  P <-
    list(x = c(0, r/ratio.yx, 0, -r/ratio.yx),
         y = c(-r, 0, r, 0))
  invisible(grid.polygon(unit(P$x+x,"npc"), unit(P$y+y, "npc"),
                         gp=gpar(col = col, fill = fill, lwd=lwd, alpha=alpha)))
}


grid.triangle_point_up <- function(x = 0.5, y = 0.5, r = 0.5,
                                   default.units = "npc", name = NULL,
                                   gp = gpar(), draw = TRUE, vp = NULL){
  fill <- gp$fill
  col <- gp$col
  lwd <- if(length(gp$lwd)>0) gp$lwd else 1
  alpha <- gp$alpha
  if(is.null(fill)) fill <- "white"
  ratio.yx <- 1/as.numeric(convertX(unit(1, "snpc"), "npc"))
  P <-
    list(x = c(-r/ratio.yx, r/ratio.yx, 0, -r/ratio.yx),
         y = c(-r, -r, r, -r))
  invisible(grid.polygon(unit(P$x+x,"npc"), unit(P$y+y, "npc"),
                         gp=gpar(col = col, fill = fill, lwd=lwd, alpha=alpha)))
}


grid.triangle_point_down <- function(x = 0.5, y = 0.5, r = 0.5,
                                     default.units = "npc", name = NULL,
                                     gp = gpar(), draw = TRUE, vp = NULL){
  fill <- gp$fill
  col <- gp$col
  lwd <- if(length(gp$lwd)>0) gp$lwd else 1
  alpha <- gp$alpha
  if(is.null(fill)) fill <- "white"
  ratio.yx <- 1/as.numeric(convertX(unit(1, "snpc"), "npc"))
  P <-
    list(x = c(-r/ratio.yx, r/ratio.yx, 0, -r/ratio.yx),
         y = c(r, r, -r, r))
  invisible(grid.polygon(unit(P$x+x,"npc"), unit(P$y+y, "npc"),
                         gp=gpar(col = col, fill = fill, lwd=lwd, alpha=alpha)))
}


grid.star <- function(x = 0.5, y = 0.5, r = 0.5,
                      default.units = "npc", name = NULL,
                      gp = gpar(), draw = TRUE, vp = NULL){
  fill <- gp$fill
  col <- gp$col
  lwd <- if(length(gp$lwd)>0) gp$lwd else 1
  alpha <- gp$alpha
  if(is.null(fill)) fill <- "white"
  ratio.yx <- 1/as.numeric(convertX(unit(1, "snpc"), "npc"))
  i <- 1:11
  angle <- 180
  alpha <- 2*pi / 10
  r <- r * (i %% 2 + 1)/2
  omega <- alpha * i + angle * pi /180
  invisible(grid.polygon(unit(r*sin(omega)/ratio.yx+x,"npc"),
                         unit(r*cos(omega)+y, "npc"),
                         gp=gpar(col = col, fill = fill, lwd=lwd, alpha=alpha)))
}


rgb2hex <- function(x){
  hex <- function(a) format(as.hexmode(a), width=2, upper.case=TRUE)
  if(length(x==3))
    paste0("#",hex(x[1]),hex(x[2]),hex(x[3]))
  else
    paste0("#",hex(x[1]),hex(x[2]),hex(x[3]),hex(x[4]))
}


grid.square <- function(x = 0.5, y = 0.5, r = 0.5,
                        default.units = "npc", name = NULL,
                        gp = gpar(), draw = TRUE, vp = NULL){
  fill <- gp$fill
  col <- gp$col
  lwd <- if(length(gp$lwd)>0) gp$lwd else 1
  alpha <- gp$alpha
  if(is.null(fill)) fill <- "white"
  ratio.yx <- 1/as.numeric(convertX(unit(1, "snpc"), "npc"))
  invisible(grid.rect(unit(x,"npc"), unit(y, "npc"),
                      width = unit(r*2/ratio.yx, "npc"),
                      height = unit(r*2, "npc"),
                      gp=gpar(col = col, fill = fill, lwd=lwd, alpha=alpha)))
}
