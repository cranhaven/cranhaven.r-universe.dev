#turning it into a function
#'@param t phylogenetic tree
#'@param base.pair vector which contains the basepair change data to be displayed on the tree. Length should equal number of nodes
#'@param base.pair.color vector for colors of the basepair labels
plot.evolution<-function(t, base.pair, base.pair.col=NULL){
  
  #getting node ages
  
  #get node and tip ages
  ages<-dispRity::tree.age(t, 2.94, order = "past", fossil = TRUE, digits = 3)
  
  #get only node ages
  node_ages<-ages[(length(t$tip.label)+1):length(ages$ages),]
  
  data<-data.frame(matrix(nrow=length(TreeSim::getx(t)), ncol=4))
  colnames(data)<-c("sp","edge", "hmin", "hmax")
  data$hmin<-node_ages$ages
  data$hmax<-node_ages$ages
  data$sp<-as.numeric(node_ages$elements)
  data$edge<-as.numeric(node_ages$elements)
  data<-FossilSim::as.fossils(test.obj)
  bp_data<-base.pair
  
  plot(data[2:length(data$sp),],tree = t, pch=bp_data, fossil.col=base.pair.col)}

#plot

plot.evolution(t=t, test.obj.bp, base.pair.col=c("red", "green", "blue", "yellow"))

#Part 4: Make everything work without using it as a fossil class#

#outline of what the object should look like

test<-data.frame(sp = numeric(), edge = numeric(), hmin = numeric(), hmax = numeric(), bpchange=character())
class(test)<-"evol"

setClass("evol", slots= data.frame(sp = numeric(), edge = numeric(), hmin = numeric(), hmax = numeric(), bpchange=character()))



#change plot.fossils function to plot.evolution

#first: plot dots only
#plot fossils function
plot.evol = function(x, tree, show.fossils = TRUE, show.tree = TRUE, show.ranges = FALSE,
                     # age info/options
                     show.strata = FALSE, strata = 1, max.age = NULL, interval.ages = NULL, binned = FALSE, show.axis = TRUE,
                     # proxy stuff
                     show.proxy = FALSE, proxy.data = NULL,
                     show.preferred.environ = FALSE, preferred.environ = NULL,
                     # taxonomy
                     show.taxonomy = FALSE, taxonomy = NULL, show.unknown = FALSE, rho = 1,
                     # tree appearance
                     root.edge = TRUE, hide.edge = FALSE, edge.width = 1, show.tip.label = FALSE, align.tip.label = FALSE, reconstructed = FALSE,
                     # fossil appearance
                     fossil.col = 1, range.col = rgb(0,0,1), extant.col = 1, taxa.palette = "viridis",
                     col.axis = "gray35", cex = 1.2, pch = evol$bp_change, ...) {
  
  evol = x
  
  # hard coded options for tree appearance
  edge.color = "black"
  edge.lty = 1
  font = 3 # italic
  tip.color = "black"
  label.offset = 0.02
  align.tip.label.lty = 3
  underscore = FALSE
  plot = TRUE
  node.depth = 1
  no.margin = FALSE
  adj = NULL
  srt = 0
  
  if(!show.tree) align.tip.label = TRUE
  
  if(class(evol)[1]!="evol")
    stop("x must be an object of class \"evol\"")
  
  if(!"phylo" %in% class(tree))
    stop("tree must be an object of class \"phylo\"")
  
  if(!all( as.vector(na.omit(evol$edge)) %in% tree$edge))
    stop("Mismatch between evol and tree objects")
  
  if(!(rho >= 0 && rho <= 1))
    stop("rho must be a probability between 0 and 1")
  
  # tolerance for extant tips and interval/ fossil age comparisons
  tol = min((min(tree$edge.length)/100), 1e-8)
  
  # If there are no extant samples, simulate extant samples
  #if(!any( abs(evol$hmax) < tol )){
  #do we need this?
  # evol = sim.extant.samples(evol, tree = tree, rho = rho)
  #}
  
  if(is.null(tree$root.edge))
    root.edge = FALSE
  
  if(is.null(max.age))
    ba = tree.max(tree, root.edge = root.edge)
  else ba = max.age
  
  offset = 0 # distance from youngest tip to present
  if(!is.null(tree$origin.time)) offset = min(n.ages(tree))
  
  # note max.age defined above based on the complete tree
  if(reconstructed){
    out = reconstructed.tree.evol.objects(evol, tree)
    evol = out$evol
    tree = out$tree
    if(is.null(tree$root.edge)) root.edge = FALSE
  }
  
  # check the tree
  Ntip <- length(tree$tip.label)
  if (Ntip < 2) {
    warning("found less than 2 tips in the tree")
    return(NULL)
  }
  
  if (any(tabulate(tree$edge[, 1]) == 1))
    stop("there are single (non-splitting) nodes in your tree; you may need to use collapse.singles()")
  
  if(!ape::is.rooted(tree))
    stop("tree must be rooted")
  
  if(is.null(tree$edge.length))
    stop("tree must have edge lengths")
  
  Nnode <- tree$Nnode
  if (any(tree$edge < 1) || any(tree$edge > Ntip + Nnode))
    stop("tree badly conformed; cannot plot. Check the edge matrix.")
  ROOT <- Ntip + 1
  
  # check interval ages & proxy data
  if(any(evol$hmin != evol$hmax)) binned = TRUE
  
  if(show.strata || show.proxy){
    if( (is.null(interval.ages)) && (is.null(strata)) )
      stop("To plot interval info specify interval.ages OR number of strata, else use show.strata = FALSE")
  }
  
  if(show.proxy && is.null(proxy.data))
    stop("Specify sampling profile")
  
  if(show.proxy){
    if(!is.null(interval.ages)){
      if( (length(interval.ages) - 1) != length(proxy.data) )
        stop("Make sure number of sampling proxy data points matches the number of intervals")
    } else {
      if(strata != length(proxy.data))
        stop("Make sure number of sampling proxy data points matches the number of intervals")
    }
    if(any(is.na(proxy.data)))
      stop("Function can't handle NA proxy values right now, please use 0")
  }
  
  # check taxonomy data
  if(show.taxonomy && is.null(taxonomy))
    stop("Specify taxonomy using \"taxonomy\"")
  if(show.taxonomy && !"taxonomy" %in% class(taxonomy))
    stop("taxonomy must be an object of class \"taxonomy\"")
  if(show.taxonomy && !all(evol$edge %in% taxonomy$edge))
    stop("Mismatch between fossils and taxonomy objects")
  if(show.taxonomy && !all(tree$edge %in% taxonomy$edge))
    stop("Mismatch between tree and taxonomy objects")
  
  # collect data for plotting the tree
  type = "phylogram"
  direction = "rightwards"
  horizontal = TRUE # = "rightwards"
  
  xe = tree$edge # used in the last part of the fxn
  yy = numeric(Ntip + Nnode)
  TIPS = tree$edge[tree$edge[, 2] <= Ntip, 2]
  yy[TIPS] = 1:Ntip
  
  yy = ape::node.height(tree)
  xx = ape::node.depth.edgelength(tree)
  
  if(root.edge)
    xx = xx + tree$root.edge
  
  # x.lim is defined by max(ba, tree age)
  if(ba - offset > max(xx))
    xx = xx + (ba - offset - max(xx))
  
  x.lim = c(0, NA)
  pin1 = par("pin")[1]
  strWi = graphics::strwidth(tree$tip.label, "inches", cex = par("cex"))
  xx.tips = xx[1:Ntip] * 1.04
  alp = try(stats::uniroot(function(a) max(a * xx.tips + strWi) - pin1, c(0, 1e+06))$root, silent = TRUE)
  if (is.character(alp)) {
    tmp = max(xx.tips)
    if (show.tip.label)
      tmp = tmp * 1.5
  } else {
    tmp = if (show.tip.label)
      max(xx.tips + strWi/alp)
    else max(xx.tips)
  }
  if (show.tip.label)
    tmp = tmp + label.offset
  x.lim[2] = tmp
  
  if(show.unknown)
    y.lim = c(0, Ntip)
  else y.lim = c(1, Ntip)
  
  use.species.ages = FALSE
  
  # define interval ages
  if(show.strata || show.axis || binned || show.proxy){
    if( (is.null(interval.ages)) ){
      s1 = (ba - offset) / strata # horizon length (= max age of youngest horizon)
      horizons.max = seq(s1 + offset, ba, length = strata)
      horizons.min = horizons.max - s1
      s1 = horizons.max - horizons.min
    } else {
      horizons.min = utils::head(interval.ages, -1)
      horizons.max = interval.ages[-1]
      ba = max(horizons.max)
      s1 = rev(horizons.max - horizons.min)
      strata = length(horizons.max)
    }
    # the following is to catch datasets with hmin != hmax combinations that differ from user specified interval bounds
    if(binned & any(evol$hmax != evol$hmin)){
      if(length( unlist( sapply(evol$hmax, function(x){
        if(x < tol) return(1)
        which(abs(horizons.max - x) < tol) }))) != length(evol$hmax)){
        use.species.ages = TRUE
        binned = FALSE
      }
    }
  }
  
  # the order in which you use plot and par here is very important
  if(show.proxy){
    old.par = par("usr", "mar", "oma", "xpd", "mgp","fig")
    par(mar=c(1, 3.5, 0, 0.5)) # to reduce the margins around each plot - bottom, left, top, right -- this is harder to manipulate
    par(oma=par("oma") + c(2, 0, 2, 0)) # to add an outer margin to the top and bottom of the graph -- bottom, left, top, right
    par(xpd=NA) # allow content to protrude into outer margin (and beyond)
    par(mgp=c(1.5, .5, 0)) # to reduce the spacing between the figure plotting region and the axis labels -- axis label at 1.5 rows distance, tick labels at .5 row
    par(fig=c(0,1,0,0.4))
    graphics::plot.default(0, type = "n", xlim = x.lim, ylim = y.lim, xlab = "", ylab = "", axes = FALSE, asp = NA, ...)
    par(fig=c(0,1,0.4,1))
  }
  else{
    old.par = par("xpd", "mar")
    par(xpd=NA)
    par(mar=c(5,4,2.5,2) + 0.1) ## default is c(5,4,4,2) + 0.1
    # open a new plot window
    graphics::plot.default(0, type = "n", xlim = x.lim, ylim = y.lim, xlab = "", ylab = "", axes = FALSE, asp = NA, ...)
    
  }
  
  if (plot) {
    
    # add colored strata
    # rect(xleft, ybottom, xright, ytop)
    if(show.strata || show.axis || show.proxy){
      
      # y-axis:
      #y.bottom = 0
      #y.top = max(yy)+1
      y.bottom = 0.5
      y.top = max(yy) + 0.5
      
      # x-axis:
      #s1 = ba / strata
      x.left = 0 - (ba - offset - max(xx))
      x.right = x.left + s1[1]
      cc = 1 # color switch
      
      axis.strata = x.left
      
      for(i in 1:strata){
        if(cc %% 2 == 0)
          col="grey90"
        else
          col="grey95"
        if(show.strata)
          rect(xleft = x.left, xright = x.right, ybottom = y.bottom, ytop = y.top, col=col, border=NA)
        x.left = x.right
        x.right = x.left + s1[i+1]
        cc = cc + 1
        axis.strata = c(axis.strata, x.left)
      }
      
      x.labs = rev(round(c(offset, horizons.max),2))
      
      if(show.proxy)
        labs = FALSE
      else
        labs = x.labs
      
      if(show.axis){
        axis(1, col = col.axis, at = axis.strata, labels = labs, lwd = 2, line = 0.5, col.axis = col.axis, cex.axis = .9)
        if(!show.proxy)
          mtext(1, col = 'grey35', text="Time before present", line = 3, cex = 1.2)
      }
    }
    
    # plot the tree
    if(show.tree)
      ape::phylogram.plot(tree$edge, Ntip, Nnode, xx, yy, horizontal, edge.color, edge.width, edge.lty)
    
    # format the root edge
    if (root.edge && show.tree && !hide.edge) {
      rootcol = if(length(edge.color) == 1)
        edge.color
      else "black"
      rootw = if(length(edge.width) == 1)
        edge.width
      else 1
      rootlty = if(length(edge.lty) == 1)
        edge.lty
      else 1
      
      # plot the root edge
      segments(xx[ROOT] - tree$root.edge, yy[ROOT], xx[ROOT], yy[ROOT], col = rootcol, lwd = rootw, lty = rootlty)
    }
    
    # format tip labels
    if (show.tip.label) {
      
      # x and y co-ordinates
      adj = 0
      MAXSTRING <- max(graphics::strwidth(tree$tip.label, cex = cex))
      loy <- 0
      lox <- label.offset + MAXSTRING * 1.05 * adj
      
      if (is.expression(tree$tip.label))
        underscore <- TRUE
      if (!underscore)
        tree$tip.label <- gsub("_", " ", tree$tip.label)
      
      if (align.tip.label) {
        xx.tmp <- max(xx[1:Ntip])
        yy.tmp <- yy[1:Ntip]
        segments(xx[1:Ntip], yy[1:Ntip], xx.tmp, yy.tmp, lty = align.tip.label.lty)
      }
      else {
        xx.tmp <- xx[1:Ntip]
        yy.tmp <- yy[1:Ntip]
      }
      text(xx.tmp + lox, yy.tmp + loy, tree$tip.label,adj = adj, font = font, srt = srt, cex = cex, col = tip.color)
    }
    
    if(length(evol$sp)){
      # binned but assigned to an independent set of interval ages
      if(use.species.ages)
        evol$h = (evol$hmax - evol$hmin)/2 + evol$hmin
      # not binned
      else if(!binned) evol$h = evol$hmin
      # binned & already assigned to intervals
      else if (binned & any(evol$hmin != evol$hmax))
        evol$h = evol$hmax
      # binned but not assigned to intervals
      else if(binned & all(evol$hmin == evol$hmax))
        evol$h = sim.interval.ages(evol, tree, interval.ages = c(0, horizons.max))$hmax
      
      evol$col = fossil.col
      
      # taxonomy colours
      if(show.taxonomy){
        sps = unique(evol$sp)
        col = grDevices::hcl.colors(length(sps), palette = taxa.palette)
        j = 0
        for(i in sps){
          j = j + 1
          evol$col[which(evol$sp == i)] = col[j]
        }
      }
      
      evol$col[which(evol$h < tol)] = extant.col
      #needs to be changed later
      if(show.fossils || show.ranges){
        if(binned) {
          evol$r = sapply(evol$h - offset, function(x) {
            if(x < tol) return(max(xx) - x)
            y = max(which(abs(horizons.max - x) < tol))
            max(xx) - horizons.max[y] + (rev(s1)[y]/2) })
        } else {
          evol$r = max(xx) - evol$h + offset
        }
      }
      
      # fossils unaffiliated with taxonomy
      if( show.unknown && any(is.na(evol$sp)) ){
        unknownx = evol[ which(is.na(evol$sp)), ]$r
        unknowny = rep(0, length(unknownx))
        points(unknownx, unknowny, cex = cex, pch = pch, col = fossil.col)
        evol = na.omit(evol)
      }
      
      # evol
      if(show.fossils & !show.ranges){
        points(evol$r, yy[evol$edge], cex = cex, pch = pch, col = evol$col)
      }
      
      # ranges
      if(show.ranges){
        
        # for show.taxonomy = TRUE
        # fetch oldest and youngest edge associated with a species
        # deal with the oldest part of the ranges
        # deal with the youngest part of the ranges
        # everyting inbetween
        if(show.taxonomy){
          
          for(i in unique(evol$sp)){
            
            mn = min(evol$h[which(evol$sp == i)])
            mx = max(evol$h[which(evol$sp == i)])
            edge.mn = evol$edge[which(evol$sp == i & evol$h == mn)]
            edge.mx = evol$edge[which(evol$sp == i & evol$h == mx)]
            
            edges = find.edges.inbetween(edge.mn, edge.mx, tree)
            
            col = evol$col[which(evol$sp == i)]
            
            for(j in edges){
              
              # singletons
              if(mn == mx) {
                range = evol$r[which(evol$edge == edge.mn & evol$sp == i)]
              }
              
              # single edge
              else if(edge.mn == edge.mx) range = evol$r[which(evol$edge == edge.mn
                                                               & evol$sp == i)]
              
              # multiple edges: FA edge
              else if(j == edge.mx) {
                range =  c(evol$r[which(evol$edge == edge.mx & evol$sp == i)],
                           max(xx) + offset - taxonomy$end[which(taxonomy$edge == j & taxonomy$sp == i)])
              }
              # multiple edges: LA edge
              else if(j == edge.mn){
                range =  c(evol$r[which(evol$edge == edge.mn & evol$sp == i)],
                           max(xx) + offset - taxonomy$start[which(taxonomy$edge == j & taxonomy$sp == i)])
              }
              # multiple edges: in-between edges
              else{
                range =  c(max(xx) + offset - taxonomy$start[which(taxonomy$edge == j & taxonomy$sp == i)],
                           max(xx) + offset - taxonomy$end[which(taxonomy$edge == j & taxonomy$sp == i)])
              }
              # plot ranges
              sp = yy[j]
              
              w = 0.1
              
              # plot ranges & fossils
              if(length(range) > 1){
                rect(min(range), sp+w, max(range), sp-w, col=adjustcolor(col, alpha.f = 0.3))
                if(show.fossils)
                  points(range, rep(sp, length(range)), cex = cex, pch = pch, col = col)
                else
                  points(c(min(range), max(range)), c(sp, sp), cex = cex, pch = pch, col = col)
              } else # plot singletons
                points(range, sp, cex = cex, pch = pch, col = col)
              
            }
          }
        } else{
          for(i in unique(evol$edge)) {
            
            range = evol$r[which(evol$edge == i)]
            
            sp = yy[i]
            
            w = 0.1
            
            # plot ranges & fossils
            if(length(range) > 1){
              rect(min(range), sp+w, max(range), sp-w, col=adjustcolor(range.col, alpha.f = 0.2))
              if(show.fossils)
                points(range, rep(sp, length(range)), cex = cex, pch = pch, col = fossil.col)
              else
                points(c(min(range), max(range)), c(sp, sp), cex = cex, pch = pch, col = fossil.col)
            } else # plot singletons
              points(range, sp, cex = cex, pch = pch, col = fossil.col)
          }
        }
      }
      evol$r = NULL
    }
    
    # water depth profile
    add.depth.axis = TRUE
    if(show.proxy){
      add.depth.profile(proxy.data, axis.strata, strata, show.axis, add.depth.axis,
                        show.preferred.depth = show.preferred.environ, PD = preferred.environ, x.labs = x.labs)
    }
  }
  
  if(!is.na(old.par[1])) par(old.par)
  L <- list(type = type, use.edge.length = TRUE,
            node.pos = NULL, node.depth = node.depth, show.tip.label = show.tip.label,
            show.node.label = FALSE, font = font, cex = cex,
            adj = adj, srt = srt, no.margin = no.margin, label.offset = label.offset,
            x.lim = x.lim, y.lim = y.lim, direction = direction,
            tip.color = tip.color, Ntip = Ntip, Nnode = Nnode, root.time = tree$root.time,
            align.tip.label = align.tip.label)
  assign("last_plot.phylo", c(L, list(edge = xe, xx = xx, yy = yy)),
         envir = ape::.PlotPhyloEnv)
  invisible(L)
  
  
}
