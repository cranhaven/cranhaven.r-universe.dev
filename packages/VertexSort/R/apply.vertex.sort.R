apply.vertex.sort <- function(edges) {
  # Apply the vertex sort algorithm on a set of edges forming a directed network to elucidate the
  # the hierarchical structure of a network. It will sort the nodes of the network in levels
  # then the levels will be grouped in 3 layers (top, core and bottom
  # Args:
  #   edges: dataframe of two columns (actor_id and target_id) containing the edges of the network
  #          needs to be sorted by the vertex.sort function
  # Returns:
  #   list listing: graph, edges, traits (a dataframe of 4 columns (orf, type, layers and levels)),
  #                 actors (like kinases, phosphatases or transcription factors) , targets,
  #                 top.actors (actors in top layer), core.actors, bottom.actors, excluded.actors,
  #                 disconnected.actors (disconnected from the network), disconnected.targets,
  #                 levels.no (network levels number), nodes.in.levels
  # abbreviations: biggest_connected_comp (bcc)
  orfs <- unique( c(edges$actor_id, edges$target_id) )

  traits           <- data.frame(orf=orfs, type="", layer="", level="", stringsAsFactors=F)
  rownames(traits) <- traits$orf
  traits[edges$actor_id, "type"] <- "Actor"
  traits[traits$type=="", "type"] <- "Target"


  # generate the graph
  nodes_id        <- 1:nrow(traits)
  names(nodes_id) <- traits$orf
  from <- nodes_id[edges$actor_id]
  to   <- nodes_id[edges$target_id]

  vgraph <- graph.empty() + vertices(nodes_id, id=nodes_id, orf=traits$orf, type=traits$type, level=0, layer="")
  vgraph[from=from, to=to] <- TRUE

  size_of_connected_comp <- sapply(V(vgraph)$id, function(x) length(subcomponent(vgraph,x,"all")))
  size_of_bcc            <- max(size_of_connected_comp)
  disconnected_nodes   <- which(size_of_connected_comp!=size_of_bcc)
  disconnected_actors  <- sort(V(vgraph)[V(vgraph)$id %in% disconnected_nodes & V(vgraph)$type %in% c("Actor")]$orf)
  disconnected_targets <- sort(V(vgraph)[V(vgraph)$id %in% disconnected_nodes & V(vgraph)$type %in% c("Target")]$orf)

  if ( length(disconnected_nodes)!=0 ) {
    g <- delete.vertices(vgraph, disconnected_nodes)
    V(g)$id <- as.vector(V(g))
  } else {
    g <- vgraph
  }

  g_with_loops <- g #this graph will be used for the network properties analysis code


  ##################################################################
  # remove self interactions
  ##################################################################
  interactions <- edges

  edges <- get.edgelist(g, names=F)
  self_edges <- which(edges[,1]==edges[,2])
  if ( length(self_edges)>0 ) {
    edges_to_delete <- self_edges
    g <- delete.edges(g, edges_to_delete)
  }
  g_without_loops <- g


  ##################################################################
  # identify strongly connected components
  ##################################################################
  scc     <- clusters(g, mode="strong")
  sccs_id <- names(table(scc$membership))[table(scc$membership)>=2]
  if ( length(sccs_id)==0 ) {
    return("This graph cannot be sorted, because it does not contain a strongly connected component")
  }
  scc_no    <- length(sccs_id)
  sccs_size <- sapply(sccs_id, function(x) length(which(scc$membership==x)))
  biggest_scc_size <- max(sccs_size)
  biggest_sccs_no  <- length(which(sccs_size==biggest_scc_size))
  if ( biggest_sccs_no>1 )
    return("This graph cannot be sorted using the Vertex Sort algorithm, because it contains more than one strongly
      connected component having the same size (the biggest size)")

  #############################################################
  # Collapse all sccs
  #############################################################
  i <- 1
  actors_in_scc <- NULL
  sccs_no <- scc_no
  while ( sccs_no>0 ) {
    scc_nodes_id  <- lapply(sccs_id, function(x) which(scc$membership==x))     # IDs of the nodes in the clusters
    actors_in_scc[[i]] <- V(g)[scc_nodes_id[[1]]]$orf
    g <- Collapse_subgraph(g, scc_nodes_id[[1]])
    scc <- clusters(g, mode="strong")
    sccs_id <- names(table(scc$membership))[table(scc$membership)>=2]
    sccs_no <- length(sccs_id)
    i <- i+1
  }


  ##################################################################
  # transpose graph
  ##################################################################
  trans_g                  <- Transpose_graph(g)
  nodes_g_without_loops_no <- vcount(g_without_loops)
  nodes_orfs               <- V(g_without_loops)$orf

  # assign levels to nodes in the graph and to nodes in its transpose using the leaf removal algorithm
  g_nodes_levels       <- Leaf_removal_algorithm(g, nodes_g_without_loops_no, nodes_orfs)
  trans_g_nodes_levels <- Leaf_removal_algorithm(trans_g, nodes_g_without_loops_no, nodes_orfs)
  levels_no            <- max(g_nodes_levels)

  #reverse order of levels of the transposed graph
  trans_g_nodes_levels <- (levels_no+1)-trans_g_nodes_levels

  ##################################################################
  # Sort actors in different levels (top, core and bottom levels)
  # combine the g_nodes_levels and the trans_g_nodes_levels
  ##################################################################
  nodes_levels  <- cbind(g_nodes_levels, trans_g_nodes_levels)
  nodes_levels  <- t(apply(nodes_levels, 1, sort))
  actors_levels <- nodes_levels[nodes_levels[,1]>1,]
  actors_levels <- actors_levels-1
  levels_no     <- levels_no-1

  biggest_scc_index <- which(lapply(actors_in_scc, length)==biggest_scc_size)
  biggest_scc       <- actors_in_scc[[biggest_scc_index]]
  core_level        <- unique(c(actors_levels[biggest_scc,]))

  if ( length(core_level)>1 ) {   # if core level span many levels
    return("This graph cannot be sorted, because the core layer spans many levels")
  } else {
    core_layer_actors <- sort(names(which(actors_levels[,1]==core_level & actors_levels[,2]==core_level)))
    top_layer_actors  <- sort(names(which(actors_levels[,1]>core_level | (actors_levels[,2]==levels_no & actors_levels[,1]!=1))))
    bottom_layer_actors <- sort(names(which(actors_levels[,2]<core_level | (actors_levels[,1]==1 & actors_levels[,2]!=levels_no))))
    actors              <- sort(union(top_layer_actors, union(core_layer_actors, bottom_layer_actors)))
    targets             <- sort(V(g_without_loops)[degree(g_without_loops, V(g_without_loops), "out")==0]$orf)
    targets             <- setdiff(targets, union(disconnected_actors, disconnected_targets))
    excluded_actors     <- sort(names(which( (actors_levels[,1]==1 & actors_levels[,2]==levels_no) |
                             (actors_levels[,1]<=core_level & actors_levels[,2]>=core_level &
                             actors_levels[,1]!=actors_levels[,2] & actors_levels[,1]>1 &
                             actors_levels[,2]<levels_no) )))

    nodes_in_levels  <- apply(actors_levels, 1, function(x) paste("Level:", paste(sort(unique(x)), collapse="-")) )
    nodes_in_levels  <- factor(nodes_in_levels)
    levels_labels    <- levels(nodes_in_levels)
    nodes_in_levels  <- sapply(levels_labels, function(x) sort(names(nodes_in_levels[which(nodes_in_levels == x)])))
    nodes_in_levels  <- lapply((rev(c(list("Level: 0"=targets), nodes_in_levels))),sort)

    V(g_with_loops)[V(g_with_loops)$orf %in% top_layer_actors]$layer     <- "Top"
    V(g_with_loops)[V(g_with_loops)$orf %in% core_layer_actors]$layer    <- "Core"
    V(g_with_loops)[V(g_with_loops)$orf %in% bottom_layer_actors]$layer  <- "Bottom"
    V(g_with_loops)[V(g_with_loops)$orf %in% excluded_actors]$layer      <- "Excluded actor"
    V(g_with_loops)[V(g_with_loops)$orf %in% disconnected_actors]$layer  <- "Disconnected actors"
    V(g_with_loops)[V(g_with_loops)$orf %in% disconnected_targets]$layer <- "Disconnected targets"

    V(g_with_loops)$level <- sapply(V(g_with_loops)$orf, function(x) nodes_in_levels[x])

    ##################################################################
    # traits data.frame will be returned in order to be able to export nodes attributes to cytoscape
    ##################################################################
    traits[top_layer_actors, "layer"]     <- "Top"
    traits[core_layer_actors, "layer"]    <- "Core"
    traits[bottom_layer_actors, "layer"]  <- "Bottom"
    traits[targets, "layer"]              <- "Zero"
    traits[excluded_actors, "layer"]      <- "Excluded actor"

    traits[disconnected_actors, "layer"]  <- "Disconnected actor"
    traits[disconnected_targets, "layer"] <- "Disconnected target"
    traits[actors, "level"]               <- apply(actors_levels[actors,], 1, function(x)
                                                      paste(sort(unique(x)), collapse="-") )
    traits[targets, "level"]              <- "0"

    return(list(graph=vgraph, edges=interactions, traits=traits, actors=sort(actors),
                targets=targets, top.actors=top_layer_actors, core.actors=core_layer_actors,
                bottom.actors=bottom_layer_actors, excluded.actors=excluded_actors,
                disconnected.actors=disconnected_actors, disconnected.targets=disconnected_targets,
                levels.no=levels_no, nodes.in.levels=nodes_in_levels))
  }
}
