#
# BuildGeneSets.R
#
# @author courtney.t.schiebout.gr@dartmouth.edu
#
#
# Takes in a Seurat Object or expression matrix and the labels for each cell, as well as a logFC cut-off and a species designation.
# The function then uses edgeR differential expression analysis and Bioconductor's annotation data packages to build gene sets for the provided labels.
# Gene weights will also be provided and can be customized by user input.
# 
# 
# Inputs:
#
# -exp.data: Either the expression matrix or Seurat Object of the reference data, already normalized.
# -labels: A vector of cell type labels for the expression matrix. 
# The default is the Idents of the Seurat Object.
# -cutoff.type: Should be the same length as "cutoff" and should designate which value the cutoff(s) is for. Can include "logfc","fc", and "-logp".
# "logfc" will use the log2 fold change as a cutoff
# "fc" will use the fold change as a cutoff
# "-logp" will use the -log10(p-value) as a cutoff
# -cutoff: The desired cut-off(s) for genes to include in the gene sets.
# -species: Either "Hs", "Mm" or "Dr for human, mouse, or zebrafish respectively. 
# This is used to convert the gene symbols to ensembl IDs.
# -weight.type: Either "logfc", "fc", or "-logp". Dictates what value will be returned for gene weights. 
# "logfc" will return the log2 fold change
# "fc" will return the fold change
# "-logp" will return the negative log10 of the p-value
#
# Output:
#
#   A data frame of the gene set information
#   
#

BuildGeneSets <- function(exp.data, labels = as.character(Idents(exp.data)),  cutoff.type = "logfc", cutoff = 2, species = "Hs", weight.type = "logfc") {
  #access human reference data
  if (! is.matrix(exp.data)){
    if (is(exp.data, "Seurat")){
      colcount <- exp.data@assays$RNA@counts
    }
    else{
      stop("The expression data provided is neither a matrix nor a Seurat Object.")
    }
  }
  else {
    colcount <- exp.data
  }
  
  if (length(labels) != ncol(colcount)){
    stop("Error! There should be a label for every cell in the expression matrix.")
  }
  
  if (!all(cutoff.type %in% c("fc","logfc","-logp"))){
    stop("Error! Cut-off type should be one of the following: \"fc\",\"logfc\",\"-logp\"")
  }
  
  if (length(cutoff) != length(cutoff.type)){
    stop("There must be a corresponding cut-off value for each cut-off type.")
  }
  
  if ("logfc" %in% cutoff.type){
    if (!is.numeric(cutoff[cutoff.type == "logfc"])|cutoff[cutoff.type == "logfc"] <= 0){
      stop("Error! The logfc cut-off should be a number greater than 0.")
    }
  }
  
  if ("fc" %in% cutoff.type){
    if (!is.numeric(cutoff[cutoff.type == "fc"])|cutoff[cutoff.type == "fc"] <= 1){
      stop("Error! The fc cut-off should be a number greater than 1.")
    }
  }
  
  if ("-logp" %in% cutoff.type){
    if (!is.numeric(cutoff[cutoff.type == "-logp"])|cutoff[cutoff.type == "-logp"] <= 1){
      stop("Error! The -log10(p-value) cut-off should be a number greater than 1.")
    }
  }
  
  if (! species %in% c("Hs","Mm","Dr")){
    stop("Species provided is not valid. Please give either \"Hs\" for human, \"Mm\" for mouse, or \"Dr\" for zebrafish.")
  }
  
  if (! weight.type %in% c("logfc","fc","-logp")){
    stop("Weight types need to be one of the following: \"logfc\",\"fc\", or \"-log10p\"")
  }
  
  #set labels
  labs <- as.character(unique(labels))
  labs <- sort(labs)
  
  #edgeR DE analysis pipeline
  v <- data.frame()
  
  for (i in 1:length(labs)){
    d <- DGEList(counts=(colcount), group = ifelse(labels == labs[i],1,0))
    d <- calcNormFactors(d)
    d1 <- estimateCommonDisp(d, verbose=T)
    d1 <- estimateTagwiseDisp(d1)
    et12 <- exactTest(d1, pair = c(1,2))
    
    gp <- et12$table
    gp <- gp[order(gp[,1], decreasing = T),]
    
    #save gene symbols
    if ("fc" %in% cutoff.type){
      ind <- which(cutoff.type == "fc")
      gp <- gp[(2^(gp[,1]))>cutoff[ind],]
    }

    if ("logfc" %in% cutoff.type){
      ind <- which(cutoff.type == "logfc")
      gp <- gp[(gp[,1])>cutoff[ind],]
    }
    if ("-logp" %in% cutoff.type){
      ind <- which(cutoff.type == "-logp")
      gp <- gp[(-log10(gp[,3]))>cutoff[ind],]
      gp <- gp[(gp[,1])>0,]
    }
    
    r <- rownames(gp)
    if (weight.type == "logfc"){
      gw <- gp[,1]
    }
    if (weight.type == "fc"){
      gw <- gp[,1]
      gw <- 2^gw
    }
    if (weight.type == "-logp"){
      gw <- gp[,3]
      gw <- -log10(gw)
    }
    print(paste(labs[i],":",length(r),"genes."))
    
    v <- rbind(v,cbind(rep(labs[i], length(r)), r,gw))
  }
  df <- data.frame(v)
  
  if (species == "Hs"){
    symbol <- org.Hs.egSYMBOL2EG
    ensem <- org.Hs.egENSEMBL
  }
  if (species == "Mm"){
    symbol <- org.Mm.egSYMBOL2EG
    ensem <- org.Mm.egENSEMBL
  }
  if (species == "Dr"){
    symbol <- org.Dr.egSYMBOL2EG
    ensem <- org.Dr.egENSEMBL
  }
  
  symbol2entrez = mappedkeys(symbol)
  # Convert to a list
  symbol2entrez = as.list(symbol[symbol2entrez])
  # Convert Gene Symbols to Entrez IDs 
  gene.symbols = (df$r)
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
  
  df$ensembl_gene = ensembl.ids
  
  colnames(df)[colnames(df) == "r"] <- "gene.symbol"
  colnames(df)[colnames(df) == "V1"] <- "cell.type"
  colnames(df)[colnames(df) == "gw"] <- "gene.weight"
  colnames(df)[colnames(df) == "ensembl_gene"] <- "ensembl.id"
  
  df <- df[order(df$cell.type),]
  
  return(df)
}



