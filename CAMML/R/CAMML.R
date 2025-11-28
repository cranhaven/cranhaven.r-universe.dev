#
# CAMML.R
#
# @author courtney.t.schiebout.gr@dartmouth.edu
#

#
# Takes in a Seurat Object of single-cell RNA-seq data and a data frame for the gene set or sets that represent genes and their gene weights.
# CAMML will then perform weighted VAM to calculate a score for each cell type in each cell.
# 
# Inputs:
#
# -seurat: The single-cell RNA-seq Seurat Object, post-filtering, normalization, and scaling.
# -gene.set.df: A data frame containing ensembl ID, gene weight, and gene set membership information for each gene.
# The data frame should have specific column labels: 
# "cell.type" for which gene set a gene belongs to
# "ensembl.id" for each gene's ensembl ID
# optional:
# "gene.weight" for each gene's gene weight
# "gene.symbol" for each gene's colloquial ID
#
# Output:
#
#   Updated Seurat object with weighted VAM CDFs.
#   
#


CAMML <- function(seurat, gene.set.df){
  len <- nrow(seurat@assays$RNA)
  
  if (missing(seurat)) {
    stop("Missing Seurat Object.")
  }
  if (missing(gene.set.df)) {
    stop("Missing gene set data frame.")
  }
  if(!is.data.frame(gene.set.df)){
    stop("Gene set data frame is not a data frame.")
  }
  if (is.null(gene.set.df$cell.type)) {
    stop("Missing cell types in gene set data frame. Please check your column names.")
  }
  if (is.null(gene.set.df$ensembl.id)) {
    stop("Missing ensembl ids in gene set data frame. Please check your column names.")
  }

  
  df <- gene.set.df[!duplicated(gene.set.df),]
  labs <- sort(unique(df$cell.type))
  
  #set number of gene sets
  num.sets = length(labs)
  
  #set gene set collection to the Ensembl IDs present in each gene set
  gene.set.collection = list()
  for (i in 1:num.sets) {
    gene.set.name = labs[i]
    gene.set.rows = which(df$cell.type == gene.set.name)
    gene.set.ensembl.ids = df$ensembl.id[gene.set.rows]
    gene.set.collection[[i]] = gene.set.ensembl.ids
  }
  
  #set the names
  names(gene.set.collection) = labs
  
  if (grepl("^ENSG",df$ensembl.id[1])){
    symbol <- org.Hs.egSYMBOL2EG
    ensem <- org.Hs.egENSEMBL
    cut <- 15
  }
  if (grepl("^ENSMUSG",df$ensembl.id[1])){
    symbol <- org.Mm.egSYMBOL2EG
    ensem <- org.Mm.egENSEMBL
    cut <- 18
  }
  if (grepl("^ENSDARG",df$ensembl.id[1])){
    symbol <- org.Dr.egSYMBOL2EG
    ensem <- org.Dr.egENSEMBL
    cut <- 18
  }
  
  symbol2entrez = mappedkeys(symbol)
  # Convert to a list
  symbol2entrez = as.list(symbol[symbol2entrez])
  # Convert Gene Symbols to Entrez IDs 
  gene.symbols = rownames(seurat)
  num.ids = length(gene.symbols)
  entrez.ids = rep(NA, num.ids)
  
  for (i in 1:num.ids) {
    entrez.id = gene.symbols[i]
    id.index = (which(names(symbol2entrez) == entrez.id))
    if (length(id.index > 0)) {
      # only use the first mapped ensembl id
      entrez.ids[i] =(symbol2entrez[[id.index]][1])
    }
  }
  
  # Get the entrez gene IDs that are mapped to an Ensembl ID
  entrez2ensembl = mappedkeys(ensem)
  # Convert to a list
  entrez2ensembl = as.list(ensem[entrez2ensembl])
  
  num.ids = length(entrez.ids)
  ensembl.ids = rep(NA, num.ids)
  
  for (i in 1:num.ids) {
    entrez.id = entrez.ids[i] 
    id.index = (which(names(entrez2ensembl) == entrez.id))
    if (length(id.index > 0)) {
      # only use the first mapped ensembl id
      ensembl.ids[i] =(entrez2ensembl[[id.index]][1])
    }
  }
  
  gene.set.collection = createGeneSetCollection(gene.ids=ensembl.ids,
                                                gene.set.collection=gene.set.collection)
  for (i in 1:length(gene.set.collection)){
    if (!is.null(nrow(gene.set.collection[[i]]))){
      gene.set.collection[[i]] <- gene.set.collection[[i]][1,]
    }
    names(gene.set.collection[[i]]) <- substr(names(gene.set.collection[[i]]),1,cut)
    gene.set.collection[[i]] <- gene.set.collection[[i]][!duplicated(names(gene.set.collection[[i]]))]
  }
  #remove NA genes
  df <- df[!is.na(df$ensembl.id),]
  
  if (is.null(df$gene.weight)) {
    message("gene.weights not provided, defaulting all weights to 1")
    gene.w = rep(1, len)
  }
  else{
    #build list
    gene.w <- list()
    
    #save gene weights that have a non-NA gene ID
    for (j in 1:length(gene.set.collection)){
      gwi <- c()
      for (i in 1:length(names(gene.set.collection[[j]]))){
        gwi <- c(gwi, which(df$ensembl.id==names(gene.set.collection[[j]][i])))
      }
      gwi <- intersect(gwi, which(df$cell.type == names(gene.set.collection)[j]))
      gene.w[[j]] <- (as.double(df$gene.weight[gwi]))
    }
  }
  
  #run VAM with ensembl gene set
  
  
  seurat = vamForSeurat(seurat.data=seurat, gene.weights = gene.w,
                        gene.set.collection=gene.set.collection, center=FALSE, gamma=TRUE, sample.cov=F, return.dist=T)
  
  seurat <- RenameAssays(seurat, VAMcdf="CAMML")
  DefaultAssay(object = seurat) = "CAMML"
  
  return(seurat)
} 




