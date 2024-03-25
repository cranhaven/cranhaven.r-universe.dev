#' Save a Phylogenetic Tree Object
#'
#' Create a phylogenetic tree PDF using a phylo tree object and a specimen dataframe.
#'
#' @param phyloTree A phylo tree object.
#' @param specimen_dataframe A specimen dataframe.
#' @param label_offset A numerical value to set the label offset distance.
#' @param label_size A numerical value to set the label size.
#' @param tree_file_name An optional character string to name the PDF file.
#' @param openPDF A logical value to state whether to open the PDF file.
#' @param savePDF A logical value to state whether to save the PDF file to the working directory. If FALSE the file will be saved to a temporary directory but will still be able to open.
#'
#' @return A PDF file.
#' @export
#'
#' @examples # create and plot a phylo tree
#'specdf_Anth <- querySpecData("Antheraea polyphemus")[1:10,]
#'
#'DNABin_Anth <- genDNABin(specdf_Anth)
#'
#'DNAStringset_Anth <- genDNAStringSet(DNABin_Anth)
#'
#'DNAStringSet_Anth_manipulated <- ManipStringSet(DNAStringset_Anth)
#'
#'Phytree_Anth <- genPhytree(DNAStringSet_Anth_manipulated)
#'
#'savePhytree(
#'  phyloTree = Phytree_Anth,
#'  specimen_dataframe = specdf_Anth,
#'  tree_file_name = "Anth_phylo_tree.pdf",
#'  openPDF = FALSE,
#'  savePDF = FALSE
#')
savePhytree <- function(phyloTree, specimen_dataframe, label_offset = 0.000055, label_size = 0.3,
                        tree_file_name = "Phylogenetic_Tree.pdf", openPDF = FALSE, savePDF = FALSE){

  ### restore directory
  orig_dir <- getwd()
  temp_dir <- tempdir()

  setwd(temp_dir)
  on.exit(setwd(orig_dir), add = TRUE)

  ### restore options
  orig_par <- par(no.readonly = TRUE)
  on.exit(par(orig_par), add = TRUE)

  ###
  if(savePDF == TRUE){
    setwd(orig_dir)
  }

  ###
  phyloTree$edge.length <- phyloTree$edge.length + (mean(phyloTree$edge.length) * 0.1)

  ###
  phyloTree$tip.label <- paste(
    row.names(specimen_dataframe), "|",
    c(specimen_dataframe$species_name), "|",
    c(specimen_dataframe$country)
  )

  ###
  grDevices::pdf(tree_file_name, width = 12, height = 8)
  graphics::par(mar=c(0, 0, 0, 0))
  plot(
    phyloTree,
    cex = label_size,
    label.offset = label_offset
  )

  ###
  grDevices::dev.off()

  ### R command to open the file
  if(openPDF == TRUE){
    system(paste("open", shQuote(tree_file_name)))
  }

}
