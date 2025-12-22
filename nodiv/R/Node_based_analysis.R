
##################################################################
# Functions for use in node-based analysis of clade distribution #
##################################################################


##############INTERNAL FUNCTIONS (NOT TO BE EXPORTED)###############


#subrow_data.frame <- function(data.frame, index)
#  as.data.frame(lapply(data.frame, function(var) var[index]), stringsAsFactors = FALSE)

Node_comm <- function(nodiv_data, node, names = TRUE)
# returns a samplelist of sites occupied by at least one member of the node
{
  # node : the internal (ape) number of the node
  if (node < Ntip(nodiv_data$phylo)) #if it is in fact a tip
    nodespecs = node else nodespecs <- Node_species(nodiv_data, node, names = FALSE)

  nodecom <- which(rowSums(nodiv_data$comm[,nodespecs]) > 0)
  if (names) nodecom <- nodiv_data$coords[nodecom,]
  return(nodecom)
}

identify_node <- function(node, tree)
{
  if (inherits(tree, "nodiv_data"))
    tree <- tree$phylo
  if (!inherits(tree, "phylo"))
    stop("tree must be an object of type phylo or nodiv_data")

  if (!is.vector(node)) stop("node must be either numeric or character")
  if (length(node)>1) warning("node was had length > 1 - only the first element was used")

  node <- node[1]

  if (is.character(node))
  {
    if (is.null(tree$node.label))
      stop("node could not be matched, as the phylogeny does not have node labels")
    node <- match(node, tree$node.label) + Ntip(tree)
    if (is.na(node))
      stop("the node could not be matched to the node labels")
  }

  if (node %in% 1:Ntip(tree)){
    warning(paste("The node number",node,"did not exist. Adding Ntip(tree) to create a usable number"))
    node <- node + Ntip(tree)  #It is an open question whether this should be here, or whether it may just lead to errors.
  }
  if (!node %in% nodenumbers(tree))
    stop("Undefined node")

  node
}

############ EXPORTED FUNCTIONS #############################

## Functions relating trees and nodes

# returns the internal node number of the basal node on the phylogeny
basal_node <- function(tree)
{
  if (inherits(tree, "nodiv_data"))
    tree <- tree$phylo
  if (!inherits(tree, "phylo"))
    stop("tree must be an object of type phylo or nodiv_data")
  return(Ntip(tree) + 1)
}

# returns a vector with the internal numbers of all nodes on the tree
nodenumbers <- function(tree)
{
  if (inherits(tree, "nodiv_data"))
    tree <- tree$phylo
  if (!inherits(tree, "phylo"))
    stop("tree must be of type phylo or nodiv_data")
  return(1:Nnode(tree) + Ntip(tree))
}

nodes <- function(tree, all = FALSE){
  if (inherits(tree, "nodiv_data"))
    tree <- tree$phylo
  if (!inherits(tree, "phylo"))
    stop("tree must be of type phylo or nodiv_data")

  ret <- nodenumbers(tree)
  names(ret) <- tree$node.label

  if (!all){
    if (is.null(tree$node.label)){
      warning("Object had no node labels - returning all node numbers")
      return(ret)
    }
    ret <- ret[!names(ret) == ""]
    ret <- ret[order(names(ret))]
  }
  return(ret)
}


Descendants <- function(node, tree)
{
  if (inherits(tree, "nodiv_data"))
    tree <- tree$phylo
  if (!inherits(tree, "phylo"))
    stop("tree must be an object of type phylo or nodiv_data")
  node <- identify_node(node, tree)
  return(tree$edge[ tree$edge[,1] == node , 2])
}


Parent <- function(node, tree)
{
  if (inherits(tree, "nodiv_data"))
    tree <- tree$phylo
  if (!inherits(tree, "phylo"))
    stop("tree must be an object of type phylo or nodiv_data")
  suppressWarnings(node_local <- identify_species(node, tree))
  if (is.na(node_local))
    node_local <- identify_node(node, tree)
  if (node_local == basal_node(tree))   # If the node is the basal node it does not have a parent node
    return (NA)
  return(tree$edge[ tree$edge[,2] == node_local , 1])
}

Sister <- function(node, tree)
{
  if (inherits(tree, "nodiv_data"))
    tree <- tree$phylo
  if (!inherits(tree, "phylo"))
    stop("tree must be an object of type phylo or nodiv_data")
  suppressWarnings(nodesp <- identify_species(node, tree))
  if (!is.na(nodesp))
    node <- nodesp else
      node <- identify_node(node, tree)
  if (node == basal_node(tree) )   # If the node is the basal node it does not have a sister node
    return (NA)
  sisters = Descendants(Parent(node, tree), tree)
  return(sisters[! sisters == node])
}

MostRecentAncestor <- function(tips, tree)
{
  if (inherits(tree, "phylo")){
    ns <- Create_node_by_species_matrix(tree)
  } else {
    if (inherits(tree, "nodiv_data")){
      ns <- tree$node_species
      tree <- tree$phylo
    } else stop("tree must be an object of type phylo or nodiv_data") 
  }
  
  if (length(tips) == 1)
    return(integer())

  if (is.character(tips))
    suppressWarnings(tips <- identify_species(tips, tree))

  Ntips <- Ntip(tree)
  i <- 0
  seq.nod <- apply(ns, 2, function(x) which(x == 1) + Ntips)
  for (i in seq_along(seq.nod))
    seq.nod[[i]] <- c(seq.nod[[i]], i)  
  
  .getMRCA <- function(seq.nod, tip) {
    sn <- seq.nod[tip]
    MRCA <- Ntips + 1
    i <- 2
    repeat {
      x <- unique(unlist(lapply(sn, "[", i)))
      if (length(x) != 1)
        break
      MRCA <- x
      i <- i + 1
    }
    MRCA
  }
  j <- .getMRCA(seq.nod, tips)
  return(j)
}



####### Functions summarizing nodiv_data on nodes


Node_size <- function(nodiv_data, node = NULL)
{
  .local <- function(nodiv_data, node)
  {
    node <- identify_node(node, nodiv_data)
    return(sum(nodiv_data$node_species[node - Nspecies(nodiv_data),]))
  }
  if (!inherits(nodiv_data, "nodiv_data"))
    stop("nodiv_data must be an object of type nodiv_data or nodiv_result")
  if (is.null(node))
    node <- nodenumbers(nodiv_data)
  if (length(node) == 1)
    return(.local(nodiv_data, node)) else
      return(sapply(node, function(nod) .local(nodiv_data, nod)))
}


Node_species <- function(nodiv_data, node, names = TRUE)
{
  if (!inherits(nodiv_data, "nodiv_data")){
    if (inherits(nodiv_data, "phylo")){
      return(Node_spec(nodiv_data, node, names))
    } else
        stop("nodiv_data must be an object of type nodiv_data or phylo")
  }
  node <- identify_node(node, nodiv_data)

  ret <- which(nodiv_data$node_species[node-Nspecies(nodiv_data),] > 0)
  if (names)
    ret <- species(nodiv_data)[ret]
  return(ret)
}


Node_sites <- function(nodiv_data, node, names = TRUE)
{
	if(!inherits(nodiv_data, "nodiv_data"))
    stop("nodiv_data must be an object of type nodiv_data or nodiv_result")
  node <- identify_node(node, nodiv_data)

	if (node < Ntip(nodiv_data$phylo))
	  nodespecs = node else nodespecs <- Node_species(nodiv_data, node, names = FALSE)

	nodecom <- which(rowSums(nodiv_data$comm[,nodespecs], na.rm = T) > 0)
	if(names) nodecom <- nodiv_data$coords[nodecom,]
  return(nodecom)
}


Node_occupancy <- function(nodiv_data, node = NULL)
{
  .local <- function(nodiv_data, node)
  {
    node <- identify_node(node, nodiv_data)
    return(length(Node_sites(nodiv_data, node)))
  }
  if (!inherits(nodiv_data, "nodiv_data"))
    stop("nodiv_data must be an object of type nodiv_data or nodiv_result")
  if (is.null(node))
    node <- nodenumbers(nodiv_data)
  if (length(node) == 1)
    return(.local(nodiv_data, node)) else
      return(sapply(node, function(nod) .local(nodiv_data, nod)))
}

Node_age <- function(tree, node = NULL){
  if (inherits(tree, "nodiv_data"))
    tree <- tree$phylo
  if (!inherits(tree, "phylo"))
    stop("tree must be an object of type phylo or nodiv_data")
  if (is.null(node)){
    bb <- branching.times(tree)
    return(max(bb) - bb)
  }
  suppressWarnings(nodesp <- identify_species(node, tree))
  if (!is.na(nodesp))
    node <- nodesp else
      node <- identify_node(node, tree)
  
  ret <- 0

  if (is.null(tree$edge.length))
    edge.length <- rep(1, nrow(tree$edge)) else edge.length <- tree$edge.length
  
  while(!node == basal_node(tree)){
    ed <- which(tree$edge[,2] == node)
    node <- Parent(node, tree)
    ret <- ret + edge.length[ed]
  }
  
  node_local = node
  ret
}

DRscore <- function(x) UseMethod("DRscore")

DRscore.phylo <- function(x){
  cn <- Create_node_by_species_matrix(x)[-1,] #we do not need the basal node
  DRbase(x, cn)
}

DRscore.nodiv_data <- function(x){
  DRbase(x$phylo, x$node_species[-1,])
}

DRbase <- function(tree, cn){

  # number of descendants from a node
  descnum <- rowSums(cn)

  # get the lengths of edges leading to tips and nodes
  edgelengths <- tree$edge.length[order(tree$edge[,2])]
  nodeedge <- edgelengths[-(1:Ntip(tree))]
  tipedge <- edgelengths[(1:Ntip(tree))]

  # and normalize by decendants
  nodeedge <- nodeedge / descnum

  # apply this to the nodiv matrix
  nodevals <- cn * nodeedge

  # summarize for each species
  brsums <- colSums(nodevals) + tipedge

  # return the inverse
  1/brsums
}
