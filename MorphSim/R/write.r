#' Write morpho data to file
#'
#' @description
#' Export components of a morpho object to file. Can write trees, character
#' matrices, or fossil ages in various formats.
#'
#' @param data A morpho object
#' @param file File name
#' @param type type to write: "tree", "matrix", or "ages"
#' @param reconstructed If TRUE, write the reconstructed version. Default FALSE.
#' @param uncertainty Numeric. Age uncertainty for fossil ages. Default 0.
#'
#' @return No return value, called for its side effect of writing data to a file.
#'
#' @importFrom ape write.tree write.nexus.data
#'
#' @export
#'
#' @examples
#' data(morpho_data)
#' tmp <- tempfile(fileext = ".tre")
#' write.morpho(morpho_data, file = tmp, type = "tree")
#'
write.morpho <- function(data, file, type = "tree",
                         reconstructed = FALSE, uncertainty = 0) {

  if (!is.morpho(data)) stop("Error: data must be a morpho object")
  if (is.null(file)) stop("Error: No file name specified")

  if (type == "tree") {
    if (reconstructed) {
      write.recon.tree(data, file)
    } else {
      ape::write.tree(data$trees$EvolTree, file)
    }

  } else if (type == "matrix") {
    if (reconstructed) {
      write.recon.matrix(data, file)
    } else {
      ape::write.nexus.data(data$sequences$tips, file, format = "standard")
    }

  } else if (type == "ages") {
    if (reconstructed) {
      write.recon.tsv(data, file, uncertainty)
    } else {
      write.tsv(data, file, uncertainty)
    }

  } else {
    stop("Error: 'type' must be 'tree', 'matrix', or 'ages'")
  }
}



#' Write reconstructed tree to file
#'
#' @description
#' Write the reconstructed tree to Newick string
#'
#' @param data Morpho object
#' @param file File name
#'
#' @return
#' No return value, called for its side effect of writing data to a file.
#'
#' @examples
#' data(morpho_data)
#' tmp <- tempfile(fileext = ".tre")
#' write.recon.tree(data = morpho_data, file = tmp)
#'
#' @export
#'
write.recon.tree <- function (data = NULL, file = NULL) {

  if (is.null(data) || !inherits(data, "morpho")) {
    stop("Error: `data` must be a morpho object.")
  }

  if (is.null(file)) stop("Error: No file name specified")

  if(is.null(data$fossil)) stop ("Error: Cannot reconstruct tree as no fossil data in morpho object")

  r_tree <- FossilSim::reconstructed.tree.fossils.objects(fossils  = data$fossil,
                                                          tree = data$trees$TimeTree,
                                                          tip_order = "youngest_first")
  ape::write.tree(r_tree$tree, file = file)
}

#' Write reconstructed character matrix to file
#'
#' @description
#' Write the character matrix for the reconstructed tree to a nexus file
#'
#' @param data Morpho object
#' @param file File name
#'
#' @return
#' No return value, called for its side effect of writing data to a file.
#'
#' @examples
#' data(morpho_data)
#' tmp <- tempfile(fileext = ".nex")
#' write.recon.matrix(data = morpho_data, file = tmp)
#'
#' @export
#'
write.recon.matrix <- function (data, file = NULL) {

  if (is.null(data) || !inherits(data, "morpho")) {
    stop("Error: `data` must be a morpho object.")
  }

  if (is.null(file)) stop("Error: No file name specified")
  if(is.null(data$fossil)) stop ("Error: Cannot reconstruct tree as no fossil data in morpho object")


  mat <- reconstruct.matrix(data)
  ape::write.nexus.data(mat, file = file, format = "standard")

  }


#' Write the taxa ages
#'
#' @description
#' Writes the ages of the specimens in the true tree to a file. The tsv format used
#' here is directly compatible with RevBayes
#'
#' @param data Morpho object
#' @param file File name
#' @param uncertainty Numeric. Adds uncertainty to fossil ages in the morpho object.
#'  The ages in the object are point estimates by default; setting `uncertainty`
#'  will create an age range of ± this value (in millions of years).
#'
#' @return
#' No return value, called for its side effect of writing data to a file.
#'
#' @examples
#' data(morpho_data)
#' tmp <- tempfile(fileext = ".tsv")
#' write.tsv(data = morpho_data, file = tmp)
#'
#' @export
write.tsv <- function (data, file, uncertainty = 0) {

  if (is.null(data) || !inherits(data, "morpho")) {
    stop("Error: `data` must be a morpho object.")
  }

  if (is.null(file)) stop("Error: No file name specified")

  ## ages of full tree
  tip_depths <- ape::node.depth.edgelength(data$trees$TimeTree)[1:length(data$trees$TimeTree$tip.label)]
  tree_height <- max(ape::node.depth.edgelength(data$trees$TimeTree))
  tip_ages <-   round(abs(tree_height - tip_depths),3)
  # extant_tips <- data$trees$TimeTree$tip.label[abs(tip_depths - tree_height) < 1e-8]

  cat("taxon", "min_age", "max_age", sep = "\t", "\n", file = file)
  for ( i in 1:length(tip_ages)){
    if (tip_ages[i] == 0){
      cat(data$trees$TimeTree$tip.label[i], tip_ages[i] ,
          tip_ages[i], sep = "\t", file = file, append = T )
      cat("\n", file = file, append = TRUE)
    } else {
      cat(data$trees$TimeTree$tip.label[i], (tip_ages[i] -  uncertainty) ,
          (tip_ages[i] + uncertainty), sep = "\t", file = file, append = T )
      cat("\n", file = file, append = TRUE)
    }
  }
}

#' Write the taxa ages of reconstructed tree
#'
#' @description
#' Writes the ages of the specimen in the reconstructed tree to a file. The tsv format used
#' here is directly compatible with RevBayes
#'
#' @param data Morpho object
#' @param file File name
#' @param uncertainty Numeric. Adds uncertainty to fossil ages in the morpho object.
#'  The ages in the object are point estimates by default; setting `uncertainty`
#'  will create an age range of ± this value (in millions of years).
#'
#' @return
#' No return value, called for its side effect of writing data to a file.
#'
#' @examples
#' data(morpho_data)
#' tmp <- tempfile(fileext = ".tsv")
#' write.recon.tsv(data = morpho_data, file = tmp)
#'
#' @export

write.recon.tsv <- function (data, file, uncertainty = 0){

  if (is.null(data) || !inherits(data, "morpho")) {
    stop("Error: `data` must be a morpho object.")
  }

  if (is.null(file)) stop("Error: No file name specified")

  if(is.null(data$fossil)) stop ("Error: Cannot reconstruct tree as no fossil data in morpho object")


  r_tree <- FossilSim::reconstructed.tree.fossils.objects(fossils  = data$fossil,
                                                          tree = data$trees$TimeTree,
                                                          tip_order = "youngest_first")
  transformations <- morphsim_fossilsim(data)

  cat("taxon", "min_age", "max_age", sep = "\t", "\n", file = file)


  ## true tree tips
  tps <- unname(r_tree$tree$tip.label)
  matches <- grepl("_1$", tps )
  # Extract elements that match
  reconTreeTips <- gsub("_1$", "", tps[matches])

  ## add these tip labels to the file + plus all sampled ancestor
  seq_tips <- which(names(data$sequences$tips) %in% reconTreeTips)


  for ( i in 1:length(reconTreeTips)){
    ord <-  which(data$trees$TimeTree$tip.label ==reconTreeTips[i])
    node_pos <- ape::node.depth.edgelength(data$trees$TimeTree)[ord]
    tree_height <- max(ape::node.depth.edgelength(data$trees$TimeTree))
    tip_ages <-   round(abs(node_pos - tree_height),3)

    if(tip_ages == 0){
      nm <- paste0(reconTreeTips[i], "_1")
      cat(nm, tip_ages, tip_ages,
          sep = "\t", file = file, append = T )
      cat("\n", file = file, append = TRUE)
    } else {
      nm <- paste0(reconTreeTips[i], "_1")
      if (tip_ages - uncertainty < 0){
        min_age <- 0
      } else {
        min_age <- tip_ages - uncertainty
      }
      cat(nm,min_age, (tip_ages + uncertainty),
          sep = "\t", file = file, append = T )
      cat("\n", file = file, append = TRUE)

    }
  }

  ## sampled ancestors

  if (length(transformations[,"Morphsim"]) > 0){
    for (i in 1:length(transformations[,1])){

      parts <- as.numeric(strsplit(transformations[i, "Morphsim"], "_")[[1]])
      specimen_num <- parts[1]
      branch_num   <- parts[2]

      # Subset the data frame to get hmin
      hmin <- data$fossil$hmin[data$fossil$ape.branch == branch_num &
                                 data$fossil$specimen  == specimen_num]

      nm <- paste0(transformations[i, "Fossilsim"], "_2")
      if (hmin - uncertainty < 0){
        min_age <- 0
      } else {
        min_age <- hmin - uncertainty
      }
      cat(nm,min_age, (hmin + uncertainty),
          sep = "\t", file = file, append = T )
      cat("\n", file = file, append = TRUE)
    }
  }
}

#' Match sampled ancestor labels
#'
#' @description
#' Match the sampled ancestor labels from \code{Morphsim} and \code{Fossilsim}
#' @param data Morpho object containing fossils
#' @return
#' A character matrix mapping sampled ancestor labels between the naming
#' conventions used by \code{Morphsim} and \code{Fossilsim}
#'
#' @examples
#' data(morpho_data)
#' morphsim_fossilsim <- function(data = morpho_data)
#'
morphsim_fossilsim <- function (data = NULL){


  if(is.null(data$fossil)) stop("Error: Morpho object does not contian fossils")

  r_tree <- FossilSim::reconstructed.tree.fossils.objects(fossils  = data$fossil,
                                                          tree = data$trees$TimeTree,
                                                          tip_order =  "youngest_first")
  SA_tips <- c()
  tps <- unname(r_tree$tree$tip.label)
  matches <- grepl("_1$", tps )
  # Extract elements that match
  reconTreeTips <- gsub("_1$", "", tps[matches])

  ## add these tip labels to the file + plus all sampled ancestor
  seq_tips <- which(names(data$sequences$tips) %in% reconTreeTips)

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
  }
   return(transformation)
}





