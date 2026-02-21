
#' Visualize GO DAG
#'
#' @param Elegans   The igraph object
#' @param nodeinfo  The graph structure
#' @param title     The title of the plot
#' @param species   The organism of interest
#' @param ont       The ontology
#' @param org       Organism
#' @return A graph of the simplified GO tree
#' @keywords internal
#' @importFrom network %v%<- as.network
#'
#' @importFrom ggraph ggraph geom_edge_fan geom_node_point geom_node_text create_layout circle
#' @importFrom dplyr distinct %>% rename mutate arrange id
# @importFrom ggplot2 scale_x_continuous

drawGraph <- function(Elegans, nodeinfo, title, species, ont, org){
  level <- from <- label <- NULL

  if(toupper(ont) == "BP"){
    y <- xx.ch
  }else if(toupper(ont) == "MF"){
    y <- xx.ch1
  }else if(toupper(ont) == "CC"){
    y <- xx.ch2
  }

  # Preprocess data
  title <- title
  edges <- unique(as.data.frame(Elegans))

  # Get levels
  levels <- as.data.frame(nodeinfo)
  num <- max(unlist(levels))

  # Count total number of terms
  GOTotal <- lapply(1:length(species), function(x){
    length(species[[as.character(x)]])})
  GOTotal <- unlist(GOTotal)

  # Edges
  colnames(edges) <- c("source", "target")

  source <- edges %>%
    distinct(source) %>%
    rename(id = source)

  target <- edges %>%
    distinct(target) %>%
    rename(id = target)

  # Nodes
  node.data <- data.frame(id=1:num)
  nodes <- node.data %>%
    mutate(label = GOTotal) %>%
    arrange(id)

  nodes
  edges <- as.matrix(edges)

  # Network
  n <- network::network(as.network(edges), vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)

  tags <- c()
  for (i in 1:network::network.size(n)) {
    tags[i] <- which(levels == i, arr.ind = T)[1]
  }

  n %v% "level"  <- tags

  # Generate layout
  start <- 1
  increment <- 1/max(tags)
  traces <- list()
  for (i in 1:network::network.size(n)) {
    position <- which(levels == i, arr.ind = T)
    if(position[2] == 1){
      traces[[i]] <- c(0, start)
    }
    else if(position[2] == 2){
      traces[[i]] <- c(0.5, start)
    }
    else{
      traces[[i]] <- c(1, start)
    }

    next.pos <- which(levels == i+1, arr.ind = T)

    if(!is.na(next.pos[1])){
      if(next.pos[1] != position[1]){
        start <- start - increment
      }
    }
  }

  layout <- do.call(rbind, traces)

  #a_net <- graph_from_data_frame(edges)
  a_net <- create_layout(n, layout = layout)
  a_net$name <- as.character(nodes$label)

  # Connection between nodes
  a <- nodeinfo
  connect <- a[,2]
  storeCount <- matrix(0, nrow = dim(a)[1],ncol = dim(a)[2])

  for(x in 1:length(connect)){
    if(connect[x] != 0 && x != dim(a)[1]){
      childTerms <- lapply(species[[as.character(connect[x])]], function(x){ # Finding children terms
        y[[x]]
      })
      childTerms <- unique(unlist(childTerms))
      currentPos <- which(connect[x] == connect)
      nextPos <- a[currentPos + 1,]
      for(i in 1:length(nextPos)){
        if (nextPos[i] != 0){
          storeCount[currentPos,i] <- length(which(species[[as.character(nextPos[i])]] %in% childTerms))
        }
      }
    }
  }

  storeCount <- as.data.frame(storeCount)
  colnames(storeCount) <- c("a","b","c")


  # Plot network
  p <- ggraph(a_net) + ggplot2::geom_hline(yintercept = unique(a_net$y), color = "white", size=0.5, show.legend = TRUE ) +
    ggplot2::geom_vline(xintercept = c(0, 0.5,1), color = "white", size=0.5) +
    geom_edge_fan(aes(color = as.factor(from)),arrow = arrow(length = unit(3, "pt"), type = "closed"),
                  start_cap = circle(3, 'mm'), end_cap = circle(3, 'mm'),show.legend = TRUE) +
    geom_node_point(aes(colour = as.factor(tags), size = 10, stroke = 2.7)) +
    geom_node_text(aes(label = nodes$label), color= "#3f2a1d", fontface = "bold", size = 3.5) +
    theme(plot.title = element_text(face = "bold", size = rel(1.1), hjust = 0.5), plot.caption = element_blank(),
          axis.ticks.y =element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_text(size = 10, face = "bold"),
          axis.text.y = element_blank(),legend.position = "none", panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank()) +
    scale_x_continuous(limits = c(-0.2,2.3),breaks=c(0,0.5,1),labels = c("JN","RN","LN")) +  guides(size = "none") +
    annotate("text", x = 1.2, y = unique(a_net$y)  , label = paste("L", 0:(max(tags)-1), ": ", "   "), fontface = 2, hjust = 0) +
    annotate("text", x = 1.4, y = unique(a_net$y), label = paste("J = ", storeCount$a[1:max(tags)], sep = ""), hjust = 0) +
    annotate("text", x = 1.7, unique(a_net$y), label = paste("R = ", storeCount$b[1:max(tags)], sep = ""), hjust = 0) +
    annotate("text", x = 2.0, unique(a_net$y), label = paste("L = ", storeCount$c[1:max(tags)], sep = ""), hjust = 0)


  return(p)

}









