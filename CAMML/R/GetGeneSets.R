#
# BuildGeneSets.R
#
# @author courtney.t.schiebout.gr@dartmouth.edu
#

#
# Loads the gene.set.collection and gene.weights for a previously developed gene set.
# 
# 
# Inputs:
#
# -data: One of the following: "immune.cells","skin.immune.cells","T.subset.cells", or "mouse.cells"
#
# Output:
#
#   A list of the gene.set.collection [[1]] and the gene.weights [[2]]
#   
#

GetGeneSets <- function(data = "immune.cells"){
  if (!data %in% c("immune.cells","skin.immune.cells","T.subset.cells","mouse.cells")){
    stop("Error! The dataset called doesn't exist.")
  }
  else{
    if (data == "immune.cells"){
      gene.set.df <- read.csv(paste(system.file(package = "CAMML"),"/extdata/zheng.csv", sep = ""))[,-1]
      species <- "Hs"
    }
    if (data == "skin.immune.cells"){
      gene.set.df <- read.csv(paste(system.file(package = "CAMML"),"/extdata/melanoma.csv", sep = ""))[,-1]
      species <- "Hs"
    }
    if (data == "T.subset.cells"){
      gene.set.df <- read.csv(paste(system.file(package = "CAMML"),"/extdata/tspec.csv", sep = ""))[,-1]
      species <- "Hs"
    }
    if (data == "mouse.cells"){
      gene.set.df <- read.csv(paste(system.file(package = "CAMML"),"/extdata/mouse.csv", sep = ""))[,-1]
      species <- "Mm"
    }
  }
  
  df <- data.frame(gene.set.df)
  labs <- sort(unique(names(table(df$V1))))
  
  if (species == "Hs"){
    symbol <- org.Hs.egSYMBOL2EG
    ensem <- org.Hs.egENSEMBL
  }
  if (species == "Mm"){
    symbol <- org.Mm.egSYMBOL2EG
    ensem <- org.Mm.egENSEMBL
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




