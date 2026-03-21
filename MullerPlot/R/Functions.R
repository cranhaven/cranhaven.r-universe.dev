FillOrderList <- function(ssp,tempid.order) {
  # This recursive function sorts species based on their parental relation
  parent.tempid.list <- get("parent.tempid.list", envir = environment())
  childs <- parent.tempid.list$temp.id[base::which(parent.tempid.list$parent.id==ssp)]
  if (length(childs)>0) {
    tempid.order <- c(tempid.order,childs)
    for (ssp in childs) {
      base::environment(FillOrderList) <- environment()
      tempid.order <- FillOrderList(ssp,tempid.order)
    }
  }
  return(tempid.order)
}

FindLiveSuccessors <- function(ssp,live.child) {
  # This recursive function finds which succcessor of species "ssp" is present
  ssp.children.list <- get("ssp.children.list", envir = environment())
  live.ssp <- get("live.ssp", envir = environment())
  for (id.child in ssp.children.list[[ssp]]) {
    if (id.child %in% live.ssp) {
      live.child[ssp][[1]] <- c(live.child[ssp][[1]],id.child)
    } else {
      base::environment(FindLiveSuccessors) <- environment()
      live.child <- FindLiveSuccessors(id.child,live.child)
    }
    if (!base::is.null(live.child[id.child][[1]])) {
      live.child[ssp][[1]] <- c(live.child[ssp][[1]],live.child[id.child][[1]])
    }
  }
  return(live.child)
}

FindTotalpop <- function(ssp,total.pop) {
  # This recursive fuction calculates the sum of population of species "ssp" and its successors
  live.child <- get("live.child", envir = environment())
  pop.dyn <- get("pop.dyn", envir = environment())
  time.points <- get("time.points", envir = environment())
  total.pop[t,ssp] <- 0
  n.child <- base::length(live.child[[ssp]])
  if (n.child>0) {
    for (id.child in live.child[[ssp]]) {
      base::environment(FindTotalpop) <- environment()
      total.pop <- FindTotalpop(id.child,total.pop)
      total.pop[t,ssp] <- total.pop[t,ssp]+total.pop[t,id.child]
    }
  }
  times <- pop.dyn$times[base::which(pop.dyn$sp.id==ssp)]
  if (time.points[t] %in% times) {
    total.pop[t,ssp] <- total.pop[t,ssp]+pop.dyn$abundances[base::intersect(base::which(pop.dyn$sp.id==ssp),base::which(pop.dyn$times==time.points[t]))]
  }
  return(total.pop)
}

FindPolygonPos <- function(ssp,list.poly) {
  # This recursive function calculates the position of the corners of polygons which represents
  # population of species "ssp"
  live.child <- get("live.child", envir = environment())
  pop.dyn <- get("pop.dyn", envir = environment())
  time.points <- get("time.points", envir = environment())
  n.child <- base::length(live.child[[ssp]])
  if (n.child>0) {
    times <- pop.dyn$times[base::which(pop.dyn$sp.id==ssp)]
    if (time.points[t] %in% times) {
      gap <- pop.dyn$abundances[base::intersect(base::which(pop.dyn$sp.id==ssp),base::which(pop.dyn$times==time.points[t]))]/(n.child+1)
    } else {
      gap <- 0
    }
    for (id.child in live.child[[ssp]]) {
      list.poly[[1]] <- list.poly[[1]]+gap
      list.poly[[2]][t,id.child][[1]] <- c(list.poly[[2]][t,id.child][[1]],list.poly[[1]])
      base::environment(FindPolygonPos) <- environment()
      list.poly <- FindPolygonPos(id.child,list.poly)
    }
  } else {
    times <- pop.dyn$times[base::which(pop.dyn$sp.id==ssp)]
    if (time.points[t] %in% times) {
      gap <- pop.dyn$abundances[base::intersect(base::which(pop.dyn$sp.id==ssp),base::which(pop.dyn$times==time.points[t]))]
    } else {
      gap <- 0
    }
  }
  list.poly[[1]] <- list.poly[[1]]+gap
  list.poly[[2]][t,ssp][[1]] <- c(list.poly[[2]][t,ssp][[1]],list.poly[[1]])
  return(list.poly)
}

DrawPolygon <- function(ssp,color) {
  # This function draws the polygon which represents species "ssp"
  first.time <- get("first.time", envir = environment())
  last.time <- get("last.time", envir = environment())
  time.interval.method <- get("time.interval.method", envir = environment())
  time.points <- get("time.points", envir = environment())
  polygon.pos <- get("polygon.pos", envir = environment())
  t.serie <- base::seq(first.time[ssp],last.time[ssp],1)
  if (time.interval.method=="equal") {
    x.polygon <- c(t.serie,base::rev(t.serie))
  } else {
    x.polygon <- c(time.points[t.serie],base::rev(time.points[t.serie]))
  }
  temp <- base::unlist(polygon.pos[first.time[ssp]:last.time[ssp],ssp])
  y.polygon <- c(temp[base::seq(1,base::length(temp),2)],temp[base::seq(base::length(temp),1,-2)])
  graphics::polygon(x.polygon,y.polygon,col=color,border = TRUE)
}

PlotPopulationDynamic <- function(ssp,color.list) {
  # This recursive function plots all the polygons by calling function "DrawPolygon"
  ssp.children.list <- get("ssp.children.list", envir = environment())
  base::environment(DrawPolygon) <- environment()
  DrawPolygon(ssp,color.list[ssp])
  n.child <- base::length(ssp.children.list[[ssp]])
  if (n.child>0) {
    for (id.child in ssp.children.list[[ssp]]) {
      base::environment(PlotPopulationDynamic) <- environment()
      PlotPopulationDynamic(id.child,color.list)
    }
  }
}
