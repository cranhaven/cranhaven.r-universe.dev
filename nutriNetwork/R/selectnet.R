#-------------------------------------------------------------------------------#
# Package: nutriNetwork                                                         #
# Author: Pariya Behrouzi                                                       #
# Email: <pariya.Behrouzi@gmail.com>                                            #
#-------------------------------------------------------------------------------#

model.selection = function( result, criterion, lower.upper, ebic.gamma = 0.5,  ncores = 1, loglik_Y=TRUE, verbose = TRUE)
{
  if(inherits(result,  "nutriNetwork"))
  {
    p	 = ncol(result$path[[1]])
    nrho = length(result$rho)
    n	 = nrow(result$data)

  }else{
    stop("nutriNetwork.object should belong to the nutriNetwork class. \n ")
  }
  loglik = result$loglik

  result$df <- sapply(1:nrho, function(x) sum(as.matrix(result$Theta[[x]])[upper.tri(result$Theta[[x]])] != 0 ))
  if (criterion == "ebic")
  {
    if(verbose)
    {
      cat("Calculating extended Bayesian information criterion (ebic) selection ...")
      flush.console()
    }
    result$ebic.scores	= - 2 * loglik + ( log(n) * result$df ) + ( 4 * ebic.gamma * log(p) * result$df )
    result$opt.index	= which.min(result$ebic.scores)
    result$opt.theta	= result$Theta[[result$opt.index]]
    colnames(result$opt.theta) = colnames(result$data)
    result$opt.adj		= abs(sign(as.matrix(result$opt.theta))) - diag(rep(1,p))
    colnames(result$opt.adj) = colnames(result$opt.theta)
    result$opt.rho		= result$rho[result$opt.index]
  }

  if (criterion == "aic")
  {
    if(verbose)
    {
      cat("Calculating Akaike information criterion (AIC) selection ...")
      flush.console()
    }

    result$aic.scores	= ( - 2 * loglik ) + ( 2 * result$df )
    result$opt.index	= which.min(result$aic.scores)
    result$opt.theta	= result$Theta[[result$opt.index]]
    colnames(result$opt.theta) = colnames(result$data)
    result$opt.adj		= abs(sign(result$opt.theta)) - diag(rep(1,p))
    colnames(result$opt.adj) = colnames(result$opt.theta)
    result$opt.rho		= result$rho[result$opt.index]
  }

  if( sum(result$opt.adj ) < p - 2 )
  {
    ind <- unlist(lapply(1:length(result$rho), function(i) sum(result$adj[[i]])))
    result$opt.index	= which(ind >  ncol(result$data))[1] + 1
    if( (is.na(result$opt.index)) || (result$opt.index > length(result$rho))) result$opt.index = length(result$rho)
    result$opt.theta	= result$Theta[[result$opt.index]]
    colnames(result$opt.theta) = colnames(result$data)
    result$opt.adj		= abs(sign(result$opt.theta)) - diag(rep(1,p))
    colnames(result$opt.adj) = colnames(result$opt.theta)
    result$opt.rho		= result$rho[result$opt.index]
  }

  if(verbose)
  {
    cat("done.\n")
    flush.console()
  }
  result <- list(opt.adj=result$opt.adj, opt.theta=result$opt.theta, opt.rho=result$opt.rho, opt.index=result$opt.index, theta=result$Theta, path= result$path, ES=result$ES, Z=result$Z, rho=result$rho, loglik=result$loglik, data=result$data )
  return(result)
}


selectnet = function(nutriNetwork.obj, opt.index = NULL, criteria = NULL, ebic.gamma = 0.5, ncores = NULL, verbose=TRUE)
{
	if(! is.null(opt.index)){
		if(is.null(ncores)) ncores <- 1
		sel <- list( )
		sel$opt.adj		<-  nutriNetwork.obj$path[[opt.index]]
		sel$opt.theta	<- nutriNetwork.obj$Theta[[opt.index]]
		sel$opt.rho		<- nutriNetwork.obj$rho[opt.index]
		sel$opt.index	<- opt.index
	}else{
		if(is.null(ncores)) ncores <- detectCores() - 1
		if(is.null(criteria)) criteria <- "ebic"
		if(!ncores) ncores = 1
		sel	= model.selection( nutriNetwork.obj, criterion = criteria, lower.upper=lower.upper, ebic.gamma=ebic.gamma, ncores = ncores, verbose = verbose)
	}

	theta <- as.matrix(sel$opt.theta)
	rownames(theta) <- colnames(theta)
	par.cor <- calculate.strength.theta(theta)
	par.cor[upper.tri(par.cor)] <- 0
	par.cor <- par.cor + t(par.cor)
	diag(par.cor ) <- 1
	par.cor[par.cor > 1] <- 1
	sel$par.cor <- Matrix(par.cor)

	rm(par.cor, theta, nutriNetwork.obj)

	class(sel) = "select"
	return(sel)
}

readkey <- function()
{
    cat ("Press [enter] to continue")
    line <- readline()
}

#-----------------------------------------------------#
#different plots for class "select"                   #
# Author: Pariya Behrouzi                             #
# Email: <pariya.Behrouzi@gmail.com>                  #
#-----------------------------------------------------#
plot.select = function(x, vis= NULL, xlab= NULL, ylab= NULL, n.mem = NULL, vertex.label = FALSE, ..., layout = NULL, label.vertex = "all", vertex.size = NULL, vertex.color = NULL , edge.color = "gray29", sel.nod.label = NULL, label.size = NULL, w.btw= 800, w.within = 10,
                  sign.edg= TRUE, edge.width= NULL, edge.label= NULL,  max.degree= NULL, layout.tree= NULL, root.node= NULL, degree.node= NULL, curve= FALSE, pos.legend= "bottomleft", cex.legend= 0.8, iterl = NULL, temp = NULL, tk.width = NULL, tk.height= NULL)
{
	#if(class(x) != "select") stop("The input of this plot function should be from \"select\" class (More info in: selectnet( ) ). \n")
  if( ! inherits(x, "select")) stop("The input of this plot function should be from \"select\" class (More info in: selectnet( ) ). \n")
	if(is.null(vis)) vis <-  "CI"
	if(is.null(xlab)) xlab <- "variables"
	if(is.null(ylab)) ylab <- "variables"
	plot.new()
	par(mfrow= c(1,1))

	if(vis == "image.parcorMatrix") image(Matrix(x$par.cor), xlab=xlab, ylab=ylab, main= "partial correlation matrix", sub="")
	if(vis == "image.adj") image(Matrix(x$opt.adj), xlab=xlab, ylab=ylab, main= "adjacency matrix", sub="")
	if(vis == "image.precision") image(Matrix(x$opt.theta), xlab=xlab, ylab=ylab, main= "precision matrix", sub="")

	if(vis == "CI" ){

		if(! vertex.label) {
			vertex.label = NA
		}else{
			if(!is.null(colnames(x$opt.adj) ))
				{
					vertex.label = colnames(x$opt.adj)
			}else{
					vertex.label= NA
			}
		}
		if(is.null(vertex.size)) vertex.size <- 7
		if(is.null(cex.legend)) cex.legend <- 0.8

		adj = graph.adjacency(as.matrix(x$opt.adj), mode="undirected", diag=FALSE)
		if(is.null(n.mem))
			{
				memberships = 1
				vertex.color = "red"
		}else{
			LG = length(n.mem)
			memberships = NULL
			i = 1
			while( i <= LG)
				{
					grp <- rep(i, n.mem[i])
					memberships = c(memberships, grp)
					i = i + 1
				}
			color <- sample(rainbow(max(memberships)+10, alpha=0.3), max(memberships))
			vertex.color = color[memberships]
			names(memberships) <- colnames(x$opt.adj)
			E(adj)$weight=apply(get.edgelist(adj), 1, weight.community,memberships, w.btw, w.within)
		}

		if(is.null(n.mem)){
			layout	= layout.fruchterman.reingold
		}else{
			layout = layout.fruchterman.reingold(adj, weights=E(adj)$weight)
		}

		plot(adj, layout= layout, vertex.color= vertex.color , edge.color='gray40', vertex.size = vertex.size, vertex.label = vertex.label, vertex.label.dist = 0, main= "Selected graph")
		if(length(memberships) > 1) legend(pos.legend, paste("group", 1:length(n.mem)), cex=cex.legend, col= color, pch=rep(20,10))
		readkey()

		#if(is.null(xlab)) xlab <- ""
		#if(is.null(ylab)) ylab <- ""
		image(as.matrix(x$opt.adj), xaxt="n", yaxt="n", col = gray.colors(256) ,main="Conditional dependence relationships" , cex=0.8)
		title(ylab = ylab, cex.lab = 1, line = .5)
		title(xlab = xlab, cex.lab = 1, line = .5)
	}

	if(vis == "parcor.network"){
	  if(is.null(edge.width)) edge.width = FALSE
	  if(is.null(edge.label)) edge.label = FALSE
	  if(is.null(label.size)) label.size <-  0.9
	  if(is.null(vertex.size)) vertex.size <- 5
	  if(is.null(layout.tree)) layout.tree = FALSE
	  if(is.null(root.node)) root.node <- 1
	  if(is.null(degree.node)) degree.node <- 0
	  if(is.null(cex.legend)) cex.legend <- 0.8
	  if(is.null(vertex.color)) vertex.color <- "lightblue3"

	  p <- ncol(x$par.cor)
	  par.cor <- as.matrix(x$par.cor)

	  if(sign.edg == TRUE)
	  {
	    adj <- graph.adjacency(par.cor, weighted=TRUE, diag=FALSE, mode= "lower")
	    E(adj)$color[(E(adj)$weight < 0) ] <-  'darkblue'
	    E(adj)$color[(E(adj)$weight > 0) ] <-  'red3'
	    E(adj)$lty[abs(E(adj)$weight) >= 0.90 ] <- 1
	    E(adj)$lty[(abs(E(adj)$weight) < 0.90) & ( abs(E(adj)$weight) >= 0.65 )] <- 6
	    E(adj)$lty[(abs(E(adj)$weight) < 0.65) & (abs(E(adj)$weight) >= 0.35 )] <- 5
	    E(adj)$lty[(abs(E(adj)$weight) < 0.35) & ( abs(E(adj)$weight) >= 0.10 )] <- 2
	    E(adj)$lty[ (abs(E(adj)$weight) < 0.10) & ( abs(E(adj)$weight) >= 0.00 )] <- 3
	  }else{
	    path <-  abs(sign(par.cor)) - diag(rep(1,p))
	    adj <- graph.adjacency(path, mode="undirected")
	    E(adj)$color <- "gray40"
	  }
	  V(adj)$label.cex <- label.size
	  V(adj)$label <- colnames(par.cor)

	  if(layout.tree == TRUE)
	  {
	    pathA <-  abs(sign(par.cor)) - diag(rep(1,p))
	    A <- graph.adjacency(pathA, mode="undirected")
	    layout <- layout_as_tree(A, root = root.node)
	  }else{
	    pathA <- abs(sign(par.cor)) - diag(rep(1,p))
	    A <- graph.adjacency(pathA, mode="undirected")
	    #if(is.null(layout)) layout <- layout_with_fr(A)
	    if(is.null(layout)) layout <- layout_with_kk(A)
	  }

	  if(layout.tree == TRUE){
	    if(is.null(degree.node)) {
	      deg <- 0
	      vertex.label.dist <- 0
	    }
	    if(!is.null(degree.node)) {
	      deg <- degree.node
	      vertex.label.dist <- 1
	    }
	  }else{
	    deg <- degree.node
	    vertex.label.dist <- 0
	  }
	  if(edge.label == TRUE) edge.label=round(E(adj)$weight,2) else edge.label= NULL
	  if(edge.width == TRUE) edge.width = E(adj)$weight else edge.width= NULL

	  if((!is.null(max.degree)) && (is.null(sel.nod.label)) ) plot(adj, vertex.label=ifelse(degree(adj) >= max.degree, V(adj)$label, NA), layout=layout, edge.curved = curve,  vertex.color=vertex.color,
	                                                               vertex.size=vertex.size, layout = layout, vertex.label.color="black", vertex.label.degree =deg , label.degree= deg, vertex.label.dist= vertex.label.dist)
	  if((!is.null(sel.nod.label)) && (is.null(max.degree))) plot(adj, edge.label= edge.label, edge.width= edge.width, vertex.label=ifelse(V(adj)$label %in% sel.nod.label, V(adj)$label, NA ), layout=layout, edge.curved = curve,  vertex.color=vertex.color,
	                                                              vertex.size=vertex.size, layout = layout, vertex.label.color="black",  vertex.label.degree =deg, label.degree= deg, vertex.label.dist= vertex.label.dist)
	  if((is.null(max.degree) ) && (is.null(sel.nod.label))) plot(adj, vertex.label= colnames(par.cor), layout=layout, edge.curved = curve,  vertex.color=vertex.color,
	                                                              vertex.size=vertex.size, layout = layout, vertex.label.color="black", vertex.label.degree =deg, label.degree= deg, vertex.label.dist= vertex.label.dist)

	  if(! is.null(E(adj)$lty)) legend(pos.legend, legend=c( ">= 0.90", "0.90-0.65" , "0.65-0.35", "0.35-0.10", "0.10-0.00") , col="black", cex=cex.legend,  lty= c(1, 6, 5, 2, 3), title=" |partial corr|" )

	}

	if(is.null(tk.width))  tk.width <- 1000
	if(is.null(tk.height)) tk.height <- 1000
	if(vis == "interactive"){
		adj <- as.matrix(x$opt.adj)

		if(is.null(vertex.size)) vertex.size <- 7
		if(is.null(label.vertex)) label.vertex <- "all"
		if(is.null(vertex.color)) vertex.color <- "red"
		if(is.null(edge.color)) edge.color <- "gray29"
		if(is.null(sel.nod.label)) sel.nod.label <- NULL
		if((label.vertex == "some") && (is.null(sel.nod.label )) ) stop("Please select some vertex label(s) or fix label.vertex to either none or all.")

		p <- ncol(adj)
		A <- graph.adjacency(adj, mode= "undirected")

				if(is.null(n.mem))
			{
				memberships = 1
				#vertex.color = "red"
		}else{
			LG = length(n.mem)
			memberships = NULL
			i = 1
			while( i <= LG)
				{
					grp <- rep(i, n.mem[i])
					memberships = c(memberships, grp)
					i = i + 1
				}
			#color <- sample(rainbow(max(memberships)+10, alpha=0.3), max(memberships))
			#vertex.color = color[memberships]
			names(memberships) <- colnames(x$opt.adj)
			E(A)$weight=apply(get.edgelist(A), 1, weight.community,memberships, weigth.within= w.btw, weight.between= w.within )
		}

		if(is.null(n.mem)){
			#if(is.null(layout)) layout <- layout_with_fr(A)
			layout	= layout.fruchterman.reingold
		}else{
			layout = layout.fruchterman.reingold(A, weights=E(A)$weight)
		}

		if(is.null(layout)) layout <- layout_with_fr(A)
		if(is.null(label.size)) label.size <- 1
		V(A)$label.cex <- label.size

		if(label.vertex == "none")
		{
			V(A)$label <- NA
			tkplot(A, layout=layout, vertex.color=vertex.color, edge.color=edge.color, vertex.size=vertex.size, vertex.label.dist=0, canvas.width = tk.width, canvas.height = tk.height)
		}

		if(label.vertex == "some")
		{
			V(A)$label <- colnames(adj)
			tkplot(A, vertex.label=ifelse(V(A)$label %in% sel.nod.label, V(A)$label, NA ), layout=layout, vertex.color=vertex.color, edge.color=edge.color, vertex.size=vertex.size, vertex.label.dist=0, canvas.width = tk.width, canvas.height = tk.height)
		}
		if(label.vertex == "all")
		{
			V(A)$label <- colnames(adj)
			tkplot(A, vertex.label=colnames(adj) , layout=layout, vertex.color=vertex.color, edge.color=edge.color, vertex.size=vertex.size, vertex.label.dist=0, canvas.width = tk.width, canvas.height = tk.height)
		}
	}

	if(vis == "parcor.interactive"){
	  adj <- as.matrix(x$par.cor)
	  #%adj <- as.matrix(x$opt.adj)

	  if(is.null(vertex.size)) vertex.size <- 7
	  if(is.null(label.vertex)) label.vertex <- "all"
	  if(is.null(vertex.color)) vertex.color <- "red"
	  #if(is.null(edge.color)) edge.color <- "gray29"
	  if(is.null(sel.nod.label)) sel.nod.label <- NULL
	  if((label.vertex == "some") && (is.null(sel.nod.label )) ) stop("Please select some vertex label(s) or fix label.vertex to either none or all.")

	  p <- ncol(adj)
	  A <- graph.adjacency(adj, weighted=TRUE, diag=FALSE, mode= "lower")

	  E(A)$color[(E(A)$weight < 0) ] <-  'darkblue'
	  E(A)$color[(E(A)$weight > 0) ] <-  'red3'

	  if(is.null(n.mem))
	  {
	    memberships = 1

	  }else{
	    LG = length(n.mem)
	    memberships = NULL
	    i = 1
	    while( i <= LG)
	    {
	      grp <- rep(i, n.mem[i])
	      memberships = c(memberships, grp)
	      i = i + 1
	    }

	    names(memberships) <- colnames(x$opt.adj)
	    E(A)$weight=apply(get.edgelist(A), 1, weight.community,memberships, weigth.within= w.btw, weight.between= w.within )#membership network
	  }

	  if(is.null(n.mem)){
	    layout	<- layout.fruchterman.reingold
	  }else{
	    #layout = layout.fruchterman.reingold(A, weights=E(A)$weight) #membership network
	    layout <- layout_with_kk(A)
	  }
	  if(is.null(temp)) temp <- sqrt(vcount(A))
	  if(is.null(iterl)) iterl <- 500
	  if(is.null(layout)) layout <- layout_with_fr(A, coords = NULL, dim = 2, niter = iterl, start.temp = temp,  weights= E(A)$weight)
	  if(is.null(label.size)) label.size <- 1
	  V(A)$label.cex <- label.size

	  if(label.vertex == "none")
	  {
	    V(A)$label <- NA
	    tk <- tkplot(A, layout=layout, vertex.color=vertex.color, edge.color=edge.color, vertex.size=vertex.size, vertex.label.dist=0, canvas.width = tk.width, canvas.height = tk.height)
	  }

	  if(label.vertex == "some")
	  {
	    V(A)$label <- colnames(adj)
	    tk <- tkplot(A, vertex.label=ifelse(V(A)$label %in% sel.nod.label, V(A)$label, NA ), layout=layout, vertex.color=vertex.color, vertex.size=vertex.size, vertex.label.dist=0, canvas.width = tk.width, canvas.height = tk.height)
	  }
	  if(label.vertex == "all")
	  {
	    V(A)$label <- colnames(adj)
	    tk <- tkplot(A, vertex.label=colnames(adj) , layout=layout, vertex.color=vertex.color, edge.color=edge.color, vertex.size=vertex.size, vertex.label.dist=0 , canvas.width = tk.width, canvas.height = tk.height)
	  }
	  #return(tkplot.getcoords(tk))
	}
}

calculate.strength.theta <- function(theta){
  p <- ncol(theta)
  cond.cor <- matrix(NA, ncol=p, nrow=p)
  for(i in 1:nrow(theta))
  {
    for(j in 1:ncol(theta))
    {
      cond.cor[i,j] <- - theta[i,j]/ (sqrt(theta[i,i])* sqrt(theta[j,j]))
    }
  }
  id <- colnames(theta)
  rownames(cond.cor) <- id
  colnames(cond.cor) <- id
  return(cond.cor)
}


#-----------------------------------------------------#
#   		Summary for class "select"                    #
#-----------------------------------------------------#
print.select = function(x, ...){
	cat("To plot selected graph: plot(<YOUR OUTPUT NAME>) \n")
	cat("To visualize an interactive network: plot(<YOUR OUTPUT NAME>, vis= \"interactive\") \n")
	cat("To plot partial correlations: image(<YOUR OUTPUT NAME>$par.cor) \n")
}


weight.community=function(row,membership,weigth.within,weight.between){
  if(as.numeric(membership[which(names(membership)==row[1])])==as.numeric(membership[which(names(membership)==row[2])])){
    weight=weigth.within
  }else{
    weight=weight.between
  }
  return(weight)
}
