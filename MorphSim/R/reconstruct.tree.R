#' Color branches for plotting a reconstructed tree
#'
#' @description
#' This function generates colors for branches when plotting a reconstructed tree
#' from a morpho object containing fossil data. Branches that are part of the
#' reconstructed tree or have fossils along them are colored black; all others are grey.
#'
#' @param data A morpho object which contains fossil data and a time-calibrated tree.
#'
#' @return A list of length 2:
#'   \item{b.colours}{Vector of branch colors for plotting.}
#'   \item{rem}{Indices of branches with fossils.}
#'
#' @importFrom ape nodepath
#'
reconstruct.tree <- function(data) {

  # Reconstruct the tree and extract tips
  recon <- FossilSim::reconstructed.tree.fossils.objects(
    data$fossil, data$trees$TimeTree,
    tip_order = "youngest_first"
  )

  tps <- recon$tree$tip.label
  reconTreeTips <- gsub("_1$", "", tps[grepl("_1$", tps)])

  tree  <- data$trees$TimeTree
  ntips <- ape::Ntip(tree)

  # Initialise colour vector
  b.colours <- rep("grey", length(tree$edge.length))

  #  Mark reconstructed-tree branches as black
  tip_matches <- which(tree$tip.label %in% reconTreeTips)

  mark_path_black <- function(node) {
    path <- ape::nodepath(tree, from = node, to = ntips + 1)
    edges <- which(tree$edge[, 2] %in% path)
    b.colours[edges] <<- "black"
  }

  lapply(tip_matches, mark_path_black)

  ## older fossil sampling events
  old <- c()
  if (any(as.numeric(sub("t", "", reconTreeTips)) >
          length(data$trees$EvolTree$tip.label))){

    old_number <- which(as.numeric(sub("t", "", reconTreeTips)) >
                          length(data$trees$EvolTree$tip.label))
    old <- c(old, reconTreeTips[old_number])

  }

  #  For remaining branches, update based on fossils
  rem <- integer(0)
  # get the branch old are on
  if (length(old) > 0){
    for ( o in 1:length(old)){
      ed <- which (data$trees$EvolTree$edge[,2] == as.numeric(sub("t", "", old[o])))
      rem <- c(rem, ed)
    }
  }
  #  Identify fossil-bearing branches
  fossil.branches <- data$fossil$ape.branch

  # branches still grey
  remaining <- which(b.colours == "grey")



  for (rb in remaining) {
    if (rb %in% fossil.branches) {
      node <- tree$edge[rb, 2]
      path <- ape::nodepath(tree, from = node, to = ntips + 1)

      # colour path
      edges <- which(tree$edge[, 2] %in% path)
      b.colours[edges] <- "black"

      # store branches that correspond to fossil tips
      if (any(path <= ntips)) {
        rem <- c(rem, rb)
      }
    }
  }

  # Return list of colours and fossil-branches
  r.cols <- list(NA, NA)
  r.cols[[1]] <- b.colours
  r.cols[[2]] <-  as.matrix(rem)

  return(r.cols)
}


#' Get reconstructed matrix
#' @description This function returns the moprhological matrix for tips in the
#' reconstructed tree.
#'
#' @param data A `morpho` object with fossil data
reconstruct.matrix <- function(data){

  if (!is.morpho(data)) stop ("Error: must provide a morpho object")
  if (is.null(data$fossil)) stop ("Error: need a fossil object for reconstruction")

  r_tree <- FossilSim::reconstructed.tree.fossils.objects(fossils  = data$fossil,
                                                          tree = data$trees$TimeTree,
                                                          tip_order = "youngest_first")

  SA_tips <- c()
  tps <- unname(r_tree$tree$tip.label)
  matches <- grepl("_1$", tps )
  # Extract elements that match
  reconTreeTips <- gsub("_1$", "", tps[matches])

  ## add these tip labels to the file + plus all sampled ancestor
  seq_tips <- which(names(data$sequences$tips) %in% reconTreeTips)

  ## older fossil sampling events
  old <- c()
  if (any(as.numeric(sub("t", "", reconTreeTips)) >
          length(data$trees$EvolTree$tip.label))){

    old_number <- which(as.numeric(sub("t", "", reconTreeTips)) >
                          length(data$trees$EvolTree$tip.label))
    old <- c(old, reconTreeTips[old_number])

  }

  matches <- grepl("_2$", tps )
  reconSA <-  gsub("_2$", "", tps[matches])


  transformation <- matrix(ncol = 2, nrow = length(reconSA))
  colnames(transformation) <- c("Morphsim", "Fossilsim")
  if (length(reconSA) > 0){

    for (l in 1:length(reconSA)){
      t_label <- which(data$trees$TimeTree$tip.label == reconSA[l])
      b_num <- which(data$trees$TimeTree$edge[,2] == t_label)
      spec_min <- min(data$fossil$hmin[data$fossil$ape.branch == b_num])
      spec_num <- data$fossil$specimen[ data$fossil$hmin == spec_min ]
      SA_tips <- rbind(SA_tips, c(paste0(spec_num, "_", b_num)))

      transformation[l,"Morphsim"] <- SA_tips[l]
      transformation[l,"Fossilsim"] <- reconSA[l]
    }

    total_tips <- c(data$sequences$tips[c(seq_tips)], data$sequences$SA[c(SA_tips)])
  } else {
    total_tips <- data$sequences$tips[c(seq_tips)]
  }

  ## need to change the sequence names to match the reconstructed tree
  for ( l in 1:length(seq_tips)){
    current <- names(data$sequences$tips[c(seq_tips)[l]])
    names(total_tips)[names(total_tips) == current ] <- paste0(current, "_1")
  }

  if (length(reconSA) > 0){
    for( l in 1:length(SA_tips)){
      rematch <- unname(transformation[l,"Fossilsim"])
      names(total_tips)[names(total_tips) == SA_tips[l]] <- paste0(rematch, "_2")
    }
  }

  # deals with fossil sampling events not on tips
  if (length(old) > 0){
    sa_names <- strsplit(names(data$sequences$SA), "_")
    name_matrix <- t(sapply(sa_names, function(x) as.numeric(x)))
    for ( o in 1:length(old)){
      ed <- which (data$trees$EvolTree$edge[,2] == as.numeric(sub("t", "", old[o])))
      matches <- name_matrix[name_matrix[,2] == ed, , drop=FALSE]
      youngest <- matches[order(matches[,1]), , drop=FALSE][1,]
      fs_name <- paste0(old[o], "_1")
      ms_name <- paste0(youngest[1], "_",youngest[2])
      total_tips[[fs_name]] <- data$sequences$SA[[ms_name]]
    }
  }
  return(total_tips)

}


