#' Interactively add Guidelines for Reporting About Network Data (GRAND) information to an igraph object
#'
#' @param G An \link[igraph]{igraph} object, with weights/signs (if present) stored in `E(G)$weight`
#' @param help boolean: Should interview prompts be accompanied by explanations and examples?
#'
#' @return An \link[igraph]{igraph} object
#' @noRd
.grand_interview <- function(G, help = FALSE) {

  #### Initialize GRAND if not present ####
  if (!("grand" %in% names(igraph::graph_attr(G)))) {
    G$grand$name <- NA
    G$grand$doi <- NA
    G$grand$url <- NA
    G$grand$vertex1 <- NA
    G$grand$vertex2 <- NA
    G$grand$positive <- NA
    G$grand$negative <- NA
    G$grand$weight <- NA
    G$grand$level <- NA
    G$grand$mode <- NA
    G$grand$year <- NA
  }

  #### Identify graph characteristics ####
  directed <- igraph::is_directed(G)
  bipartite <- igraph::is_bipartite(G)
  weighted <- "weight" %in% names(igraph::edge_attr(G))
  if (weighted) {signed <- all(igraph::E(G)$weight %in% c(-1,1))} else {signed <- FALSE}

  #### Data ####
  #Name
  if (help) {cat("Some networks are known by a specific name (e.g., the \"Zachary Karate Club\" network).\n\n")}
  if (!is.na(G$grand$name)) {  #If present
    cat("This network is named", G$grand$name,".\n")
    new <- .scan("Enter a new name, NA to remove the name, or press <return> to keep the existing name.", what = "anything")
if (length(new)==0) {} else {if (is.na(new)) {G$grand$name <- NA} else {G$grand$name <- new}}
  } else {  #If not present
    new <- .scan("What is the name of this network? (press <return> to skip)", what = "anything")
    if (length(new)==0) {} else {if (is.na(new)) {} else {G$grand$name <- new}}
  }

  #DOI
  if (help) {cat("Some networks are described in detail in a published article.\nFor example, the Karate Club network is described in an article with DOI 10.1086/jar.33.4.3629752.\n\n")}
  if (!is.na(G$grand$doi)) {  #If present
    cat("This network is described in DOI", G$grand$doi,".\n")
    new <- .scan("Enter a new DOI, NA to remove the DOI, or press <return> to keep the existing DOI.", what = "anything")
if (length(new)==0) {} else {if (is.na(new)) {G$grand$doi <- NA} else {G$grand$doi <- new}}
  } else {  #If not present
    new <- .scan("What is the DOI of the manuscript that describes this network? (press <return> to skip)", what = "anything")
    if (length(new)==0) {} else {if (is.na(new)) {} else {G$grand$doi <- new}}
  }

  #URL
  if (help) {cat("Some network data are available for download via a public repository or other website.\nFor example, the Karate Club network data is available from https://networks.skewed.de/net/karate.\n\n")}
  if (!is.na(G$grand$url)) {  #If present
    cat("This network is available at", G$grand$url,".\n")
    new <- .scan("Enter a new URL, NA to remove the URL, or press <return> to keep the existing URL.", what = "anything")
if (length(new)==0) {} else {if (is.na(new)) {G$grand$url <- NA} else {G$grand$url <- new}}
  } else {  #If not present
    new <- .scan("What is the URL where these network data can be obtained? (press <return> to skip)", what = "anything")
    if (length(new)==0) {} else {if (is.na(new)) {} else {G$grand$url <- new}}
  }

  #Mode
  if (help) {cat("Network data can be collected or generated in a variety of ways.\n\n")}
  if (!is.na(G$grand$mode)) {  #If present
    cat("This network data was collected or generated via", G$grand$mode,".\n")
    new <- utils::select.list(c("Survey", "Interview", "Sensor", "Observation", "Archival", "Simulation", "Other", "NA", "Keep existing method"),
                       title = "Please choose a new data collection/generation method")
    if (new=="NA") {G$grand$mode <- NA}
    if (new %in% c("Survey", "Interview", "Sensor", "Observation", "Archival", "Simulation", "Other")) {G$grand$mode <- new}
  } else {  #If not present
    new <- utils::select.list(c("Survey", "Interview", "Sensor", "Observation", "Archival", "Simulation", "Other", "NA"),
                       title = "How were these network data collected or generated?")
    if (new %in% c("Survey", "Interview", "Sensor", "Observation", "Archival", "Simulation", "Other")) {G$grand$mode <- new}
  }
  if ("Other" %in% G$grand$mode) {  #Clarify "other" mode
    new <- .scan("What word or phrase describes how these network data were collected or generated?", what = "anything")
    if (length(new)==0) {G$grand$mode <- NA} else {G$grand$mode <- new}
  }

  #Year
  if (help) {cat("A network often describes a system as it appeared at a specific moment in time.\n\n")}
  if (!is.na(G$grand$year)) {  #If present
    cat("These network data was collected in", G$grand$year,".\n")
    new <- .scan("Enter a new year, NA to remove the year, or press <return> to keep the existing year.", what = "integer")
if (length(new)==0) {} else {if (is.na(new)) {G$grand$year <- NA} else {G$grand$year <- new}}
  } else {  #If not present
    new <- .scan("In what year were these network data were collected? (press <return> to skip)", what = "integer")
    if (length(new)==0) {} else {if (is.na(new)) {} else {G$grand$year <- new}}
  }

  #### Nodes ####
  #Representation (unipartite)
  if (!bipartite) {
    if (help) {
      cat("The nodes in a network usually represent a specific kind of entity.\n")
      cat("For example, in a social network the nodes often represent PEOPLE.\n")
      cat("In a spatial or transportation network, the nodes might represent PLACES.\n\n")
    }

    if (!is.na(G$grand$vertex1)) {  #If present
      cat("This network contains",igraph::gorder(G),"nodes, which represent", G$grand$vertex1,".\n")
      new <- .scan("Enter a new node description, NA to remove the description, or press <return> to keep the existing description.", what = "anything")
if (length(new)==0) {} else {if (is.na(new)) {G$grand$vertex1 <- NA} else {G$grand$vertex1 <- new}}
    } else {  #If not present
      cat("This network contains",igraph::gorder(G),"nodes.\n")
      new <- .scan("What do these nodes represent? (press <return> to skip)", what = "anything")
      if (length(new)==0) {} else {if (is.na(new)) {} else {G$grand$vertex1 <- new}}
    }
  }

  #Representation (bipartite)
  if (bipartite) {
    if (help) {
      cat("A bipartite network is composed of two different types of nodes.\n")
      cat("In an igraph object, they are classified as `FALSE` or `TRUE` type nodes,\n")
      cat("These two types of nodes usually represent different kinds of entities.\n")
      cat("For example, in an authorship network, the nodes might represent AUTHORS and the PAPERS they wrote.\n")
      cat("In a legislative network, the nodes might represent POLITICIANS and the BILLS they voted on.\n\n")
    }

    if (!is.na(G$grand$vertex1)) {  #If present
      cat("This network contains",sum(igraph::V(G)$type==FALSE)," `FALSE` nodes, which represent", G$grand$vertex1,".\n")
      new <- .scan("Enter a new node description, NA to remove the description, or press <return> to keep the existing description.", what = "anything")
if (length(new)==0) {} else {if (is.na(new)) {G$grand$vertex1 <- NA} else {G$grand$vertex1 <- new}}
    } else {  #If not present
      cat("This network contains",sum(igraph::V(G)$type==FALSE), "`FALSE` nodes.\n")
      new <- .scan("What do these nodes represent? (press <return> to skip)", what = "anything")
      if (length(new)==0) {} else {if (is.na(new)) {} else {G$grand$vertex1 <- new}}
    }

    if (!is.na(G$grand$vertex2)) {  #If present
      cat("This network contains",sum(igraph::V(G)$type==TRUE)," `TRUE` nodes, which represent", G$grand$vertex2,".\n")
      new <- .scan("Enter a new node description, NA to remove the description, or press <return> to keep the existing description.", what = "anything")
if (length(new)==0) {} else {if (is.na(new)) {G$grand$vertex2 <- NA} else {G$grand$vertex2 <- new}}
    } else {  #If not present
      cat("This network contains",sum(igraph::V(G)$type==TRUE), "`TRUE` nodes.\n")
      new <- .scan("What do these nodes represent? (press <return> to skip)", what = "anything")
      if (length(new)==0) {} else {if (is.na(new)) {} else {G$grand$vertex2 <- new}}
    }
  }

  #### Edges ####
  #Representation (non-signed)
  if (!signed) {
    if (help & !bipartite) {
      cat("The nodes in a network are connected by edges.\n")
      cat("These edges usually represent a specific kind of relationship.\n")
      cat("For example, in a social network the edges may represent FRIENDSHIP.\n\n")
    }

    if (help & bipartite) {
      cat("The two types of nodes in a bipartite network are connected by edges.\n")
      cat("These edges usually represent a specific kind of relationship.\n")
      cat("For example, in a network of authors and papers, the edges may represent AUTHORSHIP.\n\n")
    }

    if (!is.na(G$grand$positive)) {  #If present
      cat("This network contains",igraph::gsize(G),"edges, which represent", G$grand$positive,".\n")
      new <- .scan("Enter a new edge description, NA to remove the description, or press <return> to keep the existing description.", what = "anything")
if (length(new)==0) {} else {if (is.na(new)) {G$grand$positive <- NA} else {G$grand$positive <- new}}
    } else {  #If not present
      cat("This network contains",igraph::gsize(G),"edges.\n")
      new <- .scan("What do these edges represent? (press <return> to skip)", what = "anything")
      if (length(new)==0) {} else {if (is.na(new)) {} else {G$grand$positive <- new}}
    }
  }

  #Representation (signed)
  if (signed) {
    if (help & !bipartite) {
      cat("The nodes in a signed network are connected by positive or negative edges.\n")
      cat("These edges usually represent specific kinds of relationships.\n")
      cat("For example, in a social network the positive edges may represent FRIENDSHIP,\n")
      cat("and the negative edges may represent ENEMYSHIP.\n\n")
    }

    if (help & bipartite) {
      cat("The two types of nodes in a signed bipartite network are connected by positive or negative edges.\n")
      cat("These edges usually represent a specific kind of relationship.\n")
      cat("For example, in a network of legislators and bills, the positive edges may represent SUPPORT,\n")
      cat("and the negative edges may represent OPPOSITION.\n\n")
    }

    if (!is.na(G$grand$positive)) {  #If present
      cat("This network contains",sum(igraph::E(G)$weight==1),"positive edges, which represent", G$grand$positive,".\n")
      new <- .scan("Enter a new edge description, NA to remove the description, or press <return> to keep the existing description.", what = "anything")
if (length(new)==0) {} else {if (is.na(new)) {G$grand$positive <- NA} else {G$grand$positive <- new}}
    } else {  #If not present
      cat("This network contains",sum(igraph::E(G)$weight==1),"positive edges.\n")
      new <- .scan("What do these edges represent? (press <return> to skip)", what = "anything")
      if (length(new)==0) {} else {if (is.na(new)) {} else {G$grand$positive <- new}}
    }

    if (!is.na(G$grand$negative)) {  #If present
      cat("This network contains",sum(igraph::E(G)$weight==-1),"negative edges, which represent", G$grand$negative,".\n")
      new <- .scan("Enter a new edge description, NA to remove the description, or press <return> to keep the existing description.", what = "anything")
if (length(new)==0) {} else {if (is.na(new)) {G$grand$negative <- NA} else {G$grand$negative <- new}}
    } else {  #If not present
      cat("This network contains",sum(igraph::E(G)$weight==-1),"negative edges.\n")
      new <- .scan("What do these edges represent? (press <return> to skip)", what = "anything")
      if (length(new)==0) {} else {if (is.na(new)) {} else {G$grand$negative <- new}}
    }
  }

  #Weight definition
  if (weighted & !signed) {
    if (help) {
      cat("In weighted networks, the weights may represent many different things:\n")
      cat("Weights that represent FREQUENCY capture how often the relationship occurs (e.g., frequency of communication).\n")
      cat("Weights that represent INTENSITY capture how strong the relationship is (e.g., intensity of social closeness).\n")
      cat("Weights that represent MULTIPLEXITY capture how many types of relationships exist (e.g., friend and co-worker).\n\n")
    }

    if (!is.na(G$grand$weight)) {  #If present
      cat("The weights in this network represent", G$grand$weight,".\n")
      new <- utils::select.list(c("Frequency", "Intensity", "Multiplexity", "Other", "NA", "Keep existing definition"),
                         title = "Please choose a new definition of the edge weights")
      if (new=="NA") {G$grand$weight <- NA}
      if (new %in% c("Frequency", "Intensity", "Multiplexity", "Other")) {G$grand$weight <- new}
    } else {  #If not present
      new <- utils::select.list(c("Frequency", "Intensity", "Multiplexity", "Other", "NA"),
                         title = "What do the edge weights in this network represent?")
      if (new %in% c("Frequency", "Intensity", "Multiplexity", "Other")) {G$grand$weight <- new}
    }
    if ("Other" %in% G$grand$weight) {  #Clarify "other" mode
      new <- .scan("What word or phrase describes what the edge weights represent?", what = "anything")
      if (length(new)==0) {G$grand$weight <- NA} else {G$grand$weight <- new}
    }
  }

  #Weight measurement
  if (weighted & !signed) {
    if (help) {
      cat("In weighted networks, the weights may be measured at different levels:\n")
      cat("Weights measured on a CONTINUOUS scale can take any numeric value (e.g., correlation).\n")
      cat("Weights measured on a COUNT scale can take positive integer values (e.g., number of emails sent).\n")
      cat("Weights measured on a ORDINAL scale take a limited set of ordered values (e.g., very close, close, not close).\n")
      cat("Weights measured on a CATEGORICAL scale take values that represent discrete categories (e.g., friend, co-worker).\n\n")
    }

    if (!is.na(G$grand$level)) {  #If present
      cat("The weights in this network are measured at the", G$grand$level, "level.\n")
      new <- utils::select.list(c("Continuous", "Count", "Ordinal", "Categorical", "Other", "NA", "Keep existing level"),
                         title = "Please choose a new level of measurement for the edge weights")
      if (new=="NA") {G$grand$level <- NA}
      if (new %in% c("Continuous", "Count", "Ordinal", "Categorical", "Other")) {G$grand$level <- new}
    } else {  #If not present
      new <- utils::select.list(c("Continuous", "Count", "Ordinal", "Categorical", "Other", "NA"),
                         title = "How are the edge weights measured in this network?")
      if (new %in% c("Continuous", "Count", "Ordinal", "Categorical")) {G$grand$level <- new}
    }
    if ("Other" %in% G$grand$level) {  #Clarify "other" level
      new <- .scan("What word or phrase describes how the edge weights are measured?", what = "anything")
      if (length(new)==0) {G$grand$level <- NA} else {G$grand$level <- new}
    }
  }

  #### Return object ####
  return(G)
}
