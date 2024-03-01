#'Plotting all connections of one node
#'
#'This function plots all connections (incoming and outgoing) of a specific node in one network or in all network in the discovered model.
#'@param localint an annotated list of interactions obtained by the function \link{annotateEdges}
#'@param node center node name
#'@param p defines a threshold for the posterior probability; edges whose posterior is higher than the threshold will be plotted
#'@param rmult defines the raduis of the circle
#'@param dbcheck logical, defines if interactions absent in the database are denoted with a dashed line
#'@param cex regulates font size
#'@return plots a graph consisting of a specified node and its neighbours in the networks representing clusters identified by 'bnclustOmics'
#'@author Polina Suter
#'@examples
#' bnnames<-bnInfo(simdata,c("b","c"),c("M","T"))
#' allInteractions<-annotateEdges(bnres3,bnnames,sump=1.2,minp=0.5,minkp=0.9,dblist=simint)
#' plotNode(allInteractions,"T43",p=0.5)
#' plotNode(allInteractions,"T43",p=0.5,dbcheck=FALSE)
#'@export
plotNode<-function(localint, node, p=0.3,rmult=7,dbcheck=TRUE,cex=0.5) {
  localint<-localint[localint$from==node | localint$to==node,]
  old.par<-par(no.readonly = TRUE)
  on.exit(par(old.par))
  nint<-nrow(localint)
  bidir<-rep(0,nint)
  typecols<-c("#fbb4ae", "#fed9a6", "#ffffcc", "#b3cde3", "#decbe4")
  names(typecols)<-c("M","CN","T","P","PP")
  pcols<-colnames(localint)[which(grepl("pcl",colnames(localint)))]
  ncl<-length(pcols)
  sumncl<-sum(1:ncl)
  clind<-which(grepl("pcl",colnames(localint)))

  nodeint<-localint[which(localint$from==node | localint$to==node)[1],]
  if(nodeint$from==node) nodetype=nodeint$type1 else nodetype=nodeint$type2

  nodebord<-typecols[nodetype]

  comnodes<-setdiff(intersect(localint$from,localint$to),node)
  deleteind<-c()
  if(length(comnodes)>0) {
    for(i in comnodes) {
      cind<-which(localint$from==i | localint$to==i)
      bidir[cind]<-1
      pclmax<-apply(localint[cind,clind],2,max)
      localint[cind[1],clind]<-pclmax
      localint[cind[2],clind]<-pclmax
      deleteind<-c(deleteind,cind[2])
    }
    localint<-localint[-deleteind,]
    bidir<-bidir[-deleteind]
  }

  nint<-nrow(localint)
  namesint<-c()
  typesint<-c()
  dirint<-c()
  logint<-c()
  loge<-c()

  ws<-c(rep(1,ncl),2,3)
  names(ws)<-c(as.character(1:ncl),as.character(sumncl+1),as.character(sumncl))

  edgecol<-c(brewer.pal(n=ncl, name="Set1")[1:ncl],"#636363","black")
  names(edgecol)<-names(ws)

  for(i in 1:nrow(localint)) {
    if(localint$from[i]==node) {
      namesint<-c(namesint,localint$gene2[i])
      typesint<-c(typesint,localint$type2[i])
      dirint<-c(dirint,"f")
      if(dbcheck) logint<-c(logint,localint$db | (localint$gene1[i]==localint$gene2[i])) else logint<-c(logint,TRUE)
      logicy<-sum(which(localint[i,c(clind)]>p))
      if(logicy<=ncl | logicy==sumncl) {
        logicy<-as.character(logicy)
      } else {
        logicy<-as.character(sumncl+1)
      }
      loge<-c(loge,logicy)
      if(localint$type2[i]=="PP") namesint[length(namesint)]<-paste(localint$gene2[i],sub('.*_',"\n",localint$to[i]),sep="")
    } else {
      namesint<-c(namesint,localint$gene1[i])
      typesint<-c(typesint,localint$type1[i])
      dirint<-c(dirint,"t")
      if(dbcheck) logint<-c(logint,localint$db | (localint$gene1[i]==localint$gene2[i])) else logint<-c(logint,TRUE)
      logicy<-sum(which(localint[i,c(clind)]>p))
      if(logicy<=ncl | logicy==sumncl) {
        logicy<-as.character(logicy)
      } else {
        logicy<-as.character(sumncl+1)
      }
      loge<-c(loge,logicy)
      if(localint$type1[i]=="PP") namesint[length(namesint)]<-paste(localint$gene1[i],sub('.*_',"\n",localint$from[i]),sep="")
    }
  }

  if(localint$from[1]==node) {
    node<-localint$gene1[1]
    if(localint$type1[1]=="PP") node<-paste(node,sub('.*_',"\n",localint$from[1]),sep="")
    nodetype<-localint$type1[1]
  } else {
    node<-localint$gene2[1]
    if(localint$type2[1]=="PP") node<-paste(node,sub('.*_',"\n",localint$to[1]),sep="")
    nodetype<-localint$type2[1]
  }
  par(mar=rep(1,4))

  if(all(unique(c(localint$type1,localint$type2))%in%c("M","CN","T","P","PP"))) {
    typecols<-c("#fbb4ae","#fed9a6","#ffffcc","#b3cde3","#decbe4")
    names(typecols)<-c("M","CN","T","P","PP")
  } else {
    lot<-length(unique(c(localint$type1,localint$type2)))
    typecols<-c("#fbb4ae","#fed9a6","#ffffcc","#b3cde3","#decbe4")[1:lot]
    names(typecols)<-unique(c(localint$type1,localint$type2))
  }
  nint<-nrow(localint)
  r<-1.5
  deltar<-r*0.2
  x<-17.5
  y<-17.5
  if(nint>10) {
    R<-rmult*r
    Ri<-(rmult-1.2*r)*r
  } else {
    R<-rmult*r
    Ri<-rmult*r
  }


  #cex<-0.7
  plot(0,0, type="n",xaxt="n", yaxt="n", xlab="", ylab="",
       xlim=c(0,35), ylim=c(0,35),bty="n")

  draw.circle(x=x, y=y, radius=r,col=typecols[nodetype], border=nodebord)
  text(x=x,y=y,node,col="red",cex=cex)
  centers<-matrix(nrow=nint,ncol=2)
  colnames(centers)<-c("x","y")
  if(nint<10) {
    deltaphi<-629/(nint)
  } else {
    deltaphi<-629/(nint+1)
  }
  fromRx<-x+2*r
  fromRy<-y+2*r
  toRx<-x+R-r
  toRy<-y+R-r
  for(i in 1:nint) {
    if(i%%2==0) {
      centers[i,"x"]<-R*cos(deltaphi*i/100)
      centers[i,"y"]<-R*sin(deltaphi*i/100)
    } else {
      centers[i,"x"]<-Ri*cos(deltaphi*i/100)
      centers[i,"y"]<-Ri*sin(deltaphi*i/100)
    }
    if(1>2){#dirint[i]=="f"
      egint<-localint[i,]
      logicDE<-as.character(egint[1,c(14:16)]<0.05)
      logicDE<-sapply( logicDE, function(x)paste(substr(strsplit(x, " ")[[1]], 1, 1), collapse='') , USE.NAMES=FALSE)
      logicDE<-paste(logicDE,collapse='')
      bordcol<-edgecol[logicDE]
    } else {
      bordcol<-typecols[typesint[i]]
    }

    draw.circle(x=x+centers[i,"x"], y=y+centers[i,"y"], radius=r,col=typecols[typesint[i]], border=bordcol)
    textt(x=x+centers[i,"x"],y=y+centers[i,"y"],namesint[i],F,cex=cex)
    slopey<-(centers[i,2]-y)/(centers[i,1]-x)
    if(dirint[i]=="f") {
      sz<-ifelse(bidir[i]==1,0,0.5)
      wh<-ifelse(bidir[i]==1,0,0.8)
      iArrows(r*cos(deltaphi*i/100)+x,r*sin(deltaphi*i/100)+y,
              -r*cos(deltaphi*i/100)+x+centers[i,"x"],-r*sin(deltaphi*i/100)+y+centers[i,"y"],
              sh.lty=ifelse(logint[i],1,2),
              sh.lwd=ws[loge[i]], sh.col=edgecol[loge[i]], size=sz,width=wh,h.lty=1)
    } else {
      sz<-ifelse(bidir[i]==1,0,0.5)
      wh<-ifelse(bidir[i]==1,0,0.8)
      iArrows(-r*cos(deltaphi*i/100)+x+centers[i,"x"],-r*sin(deltaphi*i/100)+y+centers[i,"y"],
              r*cos(deltaphi*i/100)+x,r*sin(deltaphi*i/100)+y,
              sh.lty=ifelse(logint[i],1,2),
              sh.lwd=ws[loge[i]], sh.col=edgecol[loge[i]], size=sz,width=wh,h.lty=1)
    }
  }


}
textt<-function(x,y,name,logic,cols=c("red","black"),cex=1) {
  if(logic) {
    text(x,y,name,col=cols[1],cex=cex)
  } else {
    text(x=x,y=y,name,col=cols[2],cex=cex)
  }
}

#the was copied from the R package igraph
#Csardi G, Nepusz T (2006). “The igraph software package for complex network research.” InterJournal, Complex Systems, 1695.
#https://igraph.org
iArrows<-function (x1, y1, x2, y2, code = 2, size = 1, width = 1.2/4/cin,
          open = TRUE, sh.adj = 0.1, sh.lwd = 1, sh.col = if (is.R()) par("fg") else 1,
          sh.lty = 1, h.col = sh.col, h.col.bo = sh.col, h.lwd = sh.lwd,
          h.lty = sh.lty, curved = FALSE) {
  cin <- size * par("cin")[2]
  width <- width * (1.2/4/cin)
  uin <- if (is.R())
    1/xyinch()
  else par("uin")
  x <- sqrt(seq(0, cin^2, length = floor(35 * cin) + 2))
  delta <- sqrt(h.lwd) * par("cin")[2] * 0.005
  x.arr <- c(-rev(x), -x)
  wx2 <- width * x^2
  y.arr <- c(-rev(wx2 + delta), wx2 + delta)
  deg.arr <- c(atan2(y.arr, x.arr), NA)
  r.arr <- c(sqrt(x.arr^2 + y.arr^2), NA)
  bx1 <- x1
  bx2 <- x2
  by1 <- y1
  by2 <- y2
  lx <- length(x1)
  r.seg <- rep(cin * sh.adj, lx)
  theta1 <- atan2((y1 - y2) * uin[2], (x1 - x2) * uin[1])
  th.seg1 <- theta1 + rep(atan2(0, -cin), lx)
  theta2 <- atan2((y2 - y1) * uin[2], (x2 - x1) * uin[1])
  th.seg2 <- theta2 + rep(atan2(0, -cin), lx)
  x1d <- y1d <- x2d <- y2d <- 0
  if (code %in% c(1, 3)) {
    x2d <- r.seg * cos(th.seg2)/uin[1]
    y2d <- r.seg * sin(th.seg2)/uin[2]
  }
  if (code %in% c(2, 3)) {
    x1d <- r.seg * cos(th.seg1)/uin[1]
    y1d <- r.seg * sin(th.seg1)/uin[2]
  }
  if (is.logical(curved) && all(!curved) || is.numeric(curved) &&
      all(!curved)) {
    segments(x1 + x1d, y1 + y1d, x2 + x2d, y2 + y2d, lwd = sh.lwd,
             col = sh.col, lty = sh.lty)
    phi <- atan2(y1 - y2, x1 - x2)
    r <- sqrt((x1 - x2)^2 + (y1 - y2)^2)
    lc.x <- x2 + 2/3 * r * cos(phi)
    lc.y <- y2 + 2/3 * r * sin(phi)
  }
  else {
    if (is.numeric(curved)) {
      lambda <- curved
    }
    else {
      lambda <- as.logical(curved) * 0.5
    }
    lambda <- rep(lambda, length.out = length(x1))
    c.x1 <- x1 + x1d
    c.y1 <- y1 + y1d
    c.x2 <- x2 + x2d
    c.y2 <- y2 + y2d
    midx <- (x1 + x2)/2
    midy <- (y1 + y2)/2
    spx <- midx - lambda * 1/2 * (c.y2 - c.y1)
    spy <- midy + lambda * 1/2 * (c.x2 - c.x1)
    sh.col <- rep(sh.col, length.out = length(c.x1))
    sh.lty <- rep(sh.lty, length.out = length(c.x1))
    sh.lwd <- rep(sh.lwd, length.out = length(c.x1))
    lc.x <- lc.y <- numeric(length(c.x1))
    for (i in seq_len(length(c.x1))) {
      if (lambda[i] == 0) {
        segments(c.x1[i], c.y1[i], c.x2[i], c.y2[i],
                 lwd = sh.lwd[i], col = sh.col[i], lty = sh.lty[i])
        phi <- atan2(y1[i] - y2[i], x1[i] - x2[i])
        r <- sqrt((x1[i] - x2[i])^2 + (y1[i] - y2[i])^2)
        lc.x[i] <- x2[i] + 2/3 * r * cos(phi)
        lc.y[i] <- y2[i] + 2/3 * r * sin(phi)
      }
      else {
        spl <- xspline(x = c(c.x1[i], spx[i], c.x2[i]),
                       y = c(c.y1[i], spy[i], c.y2[i]), shape = 1,
                       draw = FALSE)
        lines(spl, lwd = sh.lwd[i], col = sh.col[i],
              lty = sh.lty[i])
        if (code %in% c(2, 3)) {
          x1[i] <- spl$x[3 * length(spl$x)/4]
          y1[i] <- spl$y[3 * length(spl$y)/4]
        }
        if (code %in% c(1, 3)) {
          x2[i] <- spl$x[length(spl$x)/4]
          y2[i] <- spl$y[length(spl$y)/4]
        }
        lc.x[i] <- spl$x[2/3 * length(spl$x)]
        lc.y[i] <- spl$y[2/3 * length(spl$y)]
      }
    }
  }
  if (code %in% c(2, 3)) {
    theta <- atan2((by2 - y1) * uin[2], (bx2 - x1) * uin[1])
    Rep <- rep(length(deg.arr), lx)
    p.x2 <- rep(bx2, Rep)
    p.y2 <- rep(by2, Rep)
    ttheta <- rep(theta, Rep) + rep(deg.arr, lx)
    r.arr <- rep(r.arr, lx)
    if (open)
      lines((p.x2 + r.arr * cos(ttheta)/uin[1]), (p.y2 +
                                                    r.arr * sin(ttheta)/uin[2]), lwd = h.lwd, col = h.col.bo,
            lty = h.lty)
    else polygon(p.x2 + r.arr * cos(ttheta)/uin[1], p.y2 +
                   r.arr * sin(ttheta)/uin[2], col = h.col, lwd = h.lwd,
                 border = h.col.bo, lty = h.lty)
  }
  if (code %in% c(1, 3)) {
    x1 <- bx1
    y1 <- by1
    tmp <- x1
    x1 <- x2
    x2 <- tmp
    tmp <- y1
    y1 <- y2
    y2 <- tmp
    theta <- atan2((y2 - y1) * uin[2], (x2 - x1) * uin[1])
    lx <- length(x1)
    Rep <- rep(length(deg.arr), lx)
    p.x2 <- rep(x2, Rep)
    p.y2 <- rep(y2, Rep)
    ttheta <- rep(theta, Rep) + rep(deg.arr, lx)
    r.arr <- rep(r.arr, lx)
    if (open)
      lines((p.x2 + r.arr * cos(ttheta)/uin[1]), (p.y2 +
                                                    r.arr * sin(ttheta)/uin[2]), lwd = h.lwd, col = h.col.bo,
            lty = h.lty)
    else polygon(p.x2 + r.arr * cos(ttheta)/uin[1], p.y2 +
                   r.arr * sin(ttheta)/uin[2], col = h.col, lwd = h.lwd,
                 border = h.col.bo, lty = h.lty)
  }
  list(lab.x = lc.x, lab.y = lc.y)
}

