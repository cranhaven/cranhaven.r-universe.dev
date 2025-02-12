#====================================================================
# Plot a undirect graph as per the map function
# All the nodes are shown in the graph and only nodes in modules
# are shown with edges.
#====================================================================

plot.net <- function(x, i = NULL, show.names = FALSE,
                     group = NULL, group.shape = NULL,
                     set.color = NULL, set.size = NULL,
                     axis.labels = TRUE, curve = FALSE,
                     bg.color = "white", unified = TRUE, ni = 36,
                     line.color = "gray70", line.width = 0.3,
                     legend.pos = "right", point.color = "gray20",
                     sets = c("Testing","Supporting","Non-active"),
                     circle = FALSE, ...)
{

  y <- set_name <- label <- x_TRN <- x_TST <- y_TRN <- y_TST <- NULL
  #xxx <- yyy <- namesK <- NULL
  legend.pos <- match.arg(legend.pos, choices=c("right","bottomright","bottomleft",
                                                "topleft","topright","none"))

  args0 <- list(...)

  if(!inherits(x, "net")){
    stop("Input 'x' is not of the class 'net'")
  }

  axis_labels <- x$axis_labels
  isSGP <- x$isSGP
  isEigen <- x$isEigen
  symmetric <- x$symmetric
  modules <- x$modules
  yyy <- x$index_module
  xy <- x$xy
  mid_point <- x$mid_point
  radius <- x$radius

  if(!isSGP & symmetric){
    legend.pos <- "none"
    if(!unified){
      message(" Only an 'unified' plot can be produced with the input object data")
      unified <- TRUE
    }
  }

  if(!is.null(i)){
    if(any(!(i %in% seq_along(yyy)))){
      stop("All elements in 'i' must lie between 0 < i <= ",length(yyy))
    }
    yyy <- yyy[i]
    modules <- modules[i]
  }

  if(!unified & length(yyy) >= ni){
    message("Large number of ",ifelse(isSGP,"testing subjects","modules"),
            ". Only the first ",ni," are shown")
    yyy <- yyy[1:ni]
    modules <- modules[1:ni]
  }

  # Sets: 1 = Main node in a module
  #       2 = Secondary node (connected to the main node) within a module
  #       3 = Node that do not belong to a module
  #       4 = Node that appear as primary or secondary in a module
  set <- rep(3,nrow(xy))
  tmp <- unique(unlist(modules))
  set[tmp[!tmp%in%yyy]] <- 2
  set[yyy] <- 1
  tmp <- intersect(tmp,yyy)  # are both trn and tst
  if(!symmetric & length(tmp) > 0) set[tmp] <- 4

  justx <- ifelse(length(grep("left",legend.pos))>0,0,1)
  justy <- ifelse(length(grep("bottom",legend.pos))>0,0,1)
  if(!legend.pos %in% c("none","right")){
     legend.pos <- c(abs(justx-0.01),abs(justy-0.01))
  }

  flagGp <- !is.null(group)
  if(is.null(group)) group <- data.frame(group=rep(1,nrow(xy)))
  gpName <- colnames(group)

  if(!(inherits(sets, "character") & length(sets) == 3)){
    stop("Parameter 'sets' must be a triplet of 'character' type")
  }
  stopifnot(!("NA" %in% sets))
  index_set <- (!is.na(sets))
  sets[!index_set] <- "NA"

  dat <- data.frame(id=1:nrow(xy), label=x$label, set=set,
                    set_name=sets[set], group=group, xy)
  if(any(set==4)){
     dat$set_name[set==4] <- sets[1]
  }
  dat$set_name <- ifelse(is.na(dat$set_name),"NA",dat$set_name)

  dat$group <- factor(as.character(dat$group))
  dat$set_name <- factor(as.factor(dat$set_name),levels=c(sets))

  if(length(show.names)==1L) show.names <- rep(show.names, 3)

  # Shape and color for the levels of group
  if(!flagGp) dat$group <- dat$set_name
  levelsGp <- levels(dat$group)
  if(length(levelsGp) > 5){
    stop("'group' should be a vector with at most 5 levels")
  }

  if(is.null(group.shape)){
    if(flagGp){
      group.shape <- c(21,22,23,24,25)
    }else{
      group.shape <- c(21,21,21)
    }
  }
  group.shape <- group.shape[1:length(levelsGp)]

  if(is.null(set.color)){
    set.color <- c("#E69F00","#56B4E9","gray80")
  }else{
     if(length(set.color)==1) set.color <- rep(set.color,length(sets))
  }
  set.color <- set.color[1:length(sets)]

  if(is.null(set.size)){
    set.size <- c(3.1, 2.1, 0.8)
    if(any(show.names)) set.size[show.names] <- 3.1
  }else{
    if(length(set.size)==1L) set.size <- rep(set.size,length(sets))
  }
  set.size <- set.size[1:length(sets)]

  if(any(is.na(group.shape))){
    stop("'group.shape' should be a vector with length(group.shape) = ",length(levelsGp))
  }

  if(any(is.na(set.size)) | any(is.na(set.color))){
    stop("'set.size' and 'set.color' should be vectors with length ",length(sets))
  }

  theme0 <- mytheme() + ggplot2::theme(legend.justification = c(justx,justy),
                                       legend.position = legend.pos,
                                       legend.key.height = ggplot2::unit(0.9, "lines"),
                                       legend.key.width = ggplot2::unit(0.9, "lines"),
                                       strip.text.x = ggplot2::element_blank(),
                                       panel.spacing = ggplot2::unit(0.1,"lines"))

  main <- NULL
  if("main" %in% names(args0)) main <- args0$main
  if(is.null(main)){
    theme0 <- theme0 + ggplot2::theme(plot.title = ggplot2::element_blank())
  }

  xlab <- axis_labels[1]
  ylab <- axis_labels[2]
  if("xlab" %in% names(args0)) xlab <- args0$xlab
  if("ylab" %in% names(args0)) ylab <- args0$ylab

  if(is.null(xlab)){
    theme0 <- theme0 + ggplot2::theme(axis.title.x = ggplot2::element_blank())
  }
  if(is.null(ylab)){
    theme0 <- theme0 + ggplot2::theme(axis.title.y = ggplot2::element_blank())
  }

  if(!isEigen | !axis.labels){
    theme0 <- theme0 + ggplot2::theme(axis.text = ggplot2::element_blank(),
                                      axis.ticks = ggplot2::element_blank())
  }

  names(group.shape) <- levelsGp
  names(set.color) <- names(set.size) <- sets

  # If unified plot
  if(unified){
    pp <- ggplot2::ggplot(dat, ggplot2::aes(x=x,y=y))
    if(show.names[3]){
      pp <- pp + ggplot2::geom_label(data=dat[dat$set==3,],
                                     ggplot2::aes(label=label,fill=set_name),
                                     label.padding=ggplot2::unit(0.15,"lines"),
                                     color=point.color, size=set.size[3],
                                     show.legend=FALSE)
    }else{
      pp <- pp + ggplot2::geom_point(data=dat[dat$set==3,],
                                     ggplot2::aes(shape=group,fill=set_name),
                                     color=point.color,size=set.size[3])
    }

    for(k in 1:length(yyy))
    {
      xxx0 <- modules[[k]]
      if(length(xxx0)>0)
      {
        dat1 <- dat[xxx0, c("x","y")]
        dat2 <- dat[yyy[k], c("x","y")]
        colnames(dat1) <- paste0(colnames(dat1),"_TRN")
        colnames(dat2) <- paste0(colnames(dat2),"_TST")
        dat1 <- data.frame(dat2[rep(1,nrow(dat1)),],dat1)
        if(curve){
          pp <- pp +
                ggplot2::geom_curve(ggplot2::aes(x=x_TST,y=y_TST,xend=x_TRN,yend=y_TRN),
                                    data=dat1,alpha=0.4,linewidth=line.width,
                                    color=line.color,curvature=0.4)
        }else{
          pp <- pp +
                ggplot2::geom_segment(ggplot2::aes(x=x_TST,y=y_TST,xend=x_TRN,yend=y_TRN),
                                      data=dat1,alpha=0.4,linewidth=line.width,color=line.color)
        }
      }
    }

    if(show.names[1] & !show.names[2]){
      # Primary nodes in modules
      pp <- pp +
            ggplot2::geom_label(data=dat[dat$set%in%c(1,4),],
                                ggplot2::aes(label=label,fill=set_name),
                                label.padding=unit(0.15,"lines"), color=point.color,
                                size=set.size[1], show.legend=FALSE) +
            ggplot2::geom_point(data=dat[dat$set==2,],
                                ggplot2::aes(shape=group,fill=set_name),
                                color=point.color,size=set.size[2])
    }else{
      # Secondary nodes in modules
      if(show.names[2]){
        pp <- pp + ggplot2::geom_label(data=dat[dat$set==2,],
                                       ggplot2::aes(label=label,fill=set_name),
                                       label.padding=ggplot2::unit(0.15,"lines"),
                                       color=point.color, size=set.size[2], show.legend=FALSE)
      }else{
        pp <- pp + ggplot2::geom_point(data=dat[dat$set==2,],
                                       ggplot2::aes(shape=group,fill=set_name),
                                       color=point.color,size=set.size[2])
      }
      # Primary nodes in modules
      if(show.names[1]){
        pp <- pp +
              ggplot2::geom_label(data=dat[dat$set%in%c(1,4),],
                                  ggplot2::aes(label=label,fill=set_name),
                                  label.padding=ggplot2::unit(0.15,"lines"),
                                  color=point.color, size=set.size[1], show.legend=FALSE)
      }else{
        pp <- pp  + ggplot2::geom_point(data=dat[dat$set%in%c(1,4),],
                                        ggplot2::aes(shape=group,fill=set_name),
                                        color=point.color, size=set.size[1])
      }
    }

    # Nodes that are in both rows and columns (based on row/column names)
    if(any(dat$set==4)){
      if(show.names[1] | show.names[2]){
        pp <- pp +
              ggplot2::geom_label(data=dat[dat$set==4,],label=" ",fill=set.color[sets[2]],
                                  label.padding=ggplot2::unit(0.135,"lines"),
                                  label.r=ggplot2::unit(0.35,"lines"),
                                  color=set.color[sets[1]], size=set.size[2], show.legend=FALSE) +
              ggplot2::geom_text(data=dat[dat$set==4,], ggplot2::aes(label=label),
                                 color=point.color,size=set.size[2])
      }else{
        pp <- pp +
              ggplot2::geom_point(data=dat[dat$set==4,], ggplot2::aes(shape=group),
                                  fill=set.color[sets[2]], color=set.color[sets[1]],
                                  size=set.size[1]*0.55)
      }

    }
    pp <- pp + ggplot2::theme_bw() + theme0

    if(circle){
      q <- length(radius)
      for(k in 1:q){
        tmp <- circleFun(mid_point[k,], radius[k], n=150)
        pp <- pp + ggplot2::geom_path(ggplot2::aes(x,y), data=tmp, size=0.2)
      }
      pp <- pp + ggplot2::theme(panel.border = ggplot2::element_blank())
    }

  }else{
      set.size <- 0.7*set.size
      dat2 <- c()
      for(k in 1:length(yyy)){
        xxx0 <- modules[[k]]
        if(length(xxx0) > 0){
          tmp <- dat[-xxx0,]
          tmp$set <- 3; tmp$set_name <- sets[3]
          tmp2 <- dat[xxx0, ]
          tmp2$set <- 2; tmp2$set_name <- sets[2]
          tmp <- rbind(tmp, tmp2, dat[yyy[k], ])
          dat2 <- rbind(dat2,data.frame(tmp, ind=k))
        }
      }

      pp <- ggplot2::ggplot(dat2, ggplot2::aes(x=x,y=y)) + ggplot2::facet_wrap(~ind) +
            ggplot2::geom_point(data=dat2[dat2$set_name==sets[3],],
                                ggplot2::aes(fill=set_name,shape=group),
                                color=point.color,size=set.size[3]) +
            ggplot2::geom_point(data=dat2[dat2$set_name==sets[2],],
                                ggplot2::aes(fill=set_name,shape=group),
                                color=point.color,size=set.size[2]) +
            ggplot2::geom_point(data=dat2[dat2$set_name==sets[1],],
                                ggplot2::aes(fill=set_name,shape=group),
                                color=point.color,size=set.size[1]) +
            ggplot2::theme_bw() + theme0

  }

  pp <- pp +
        ggplot2::labs(title=main, x=xlab, y=ylab) +
        ggplot2::scale_shape_manual(values=group.shape,
                                    guide=ggplot2::guide_legend(override.aes=list(size=2.7,
                                                                                  fill="white"))) +
        ggplot2::scale_fill_manual(values=set.color, breaks=names(set.color[index_set]),
                                   guide=ggplot2::guide_legend(override.aes=list(shape=22,
                                                                                 size=2.7,
                                                                                 label="")))

  if(!flagGp){
     pp <- pp + ggplot2::guides(shape="none")
  }

  pp
}
