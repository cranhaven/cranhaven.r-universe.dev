## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(GOxploreR)

## -----------------------------------------------------------------------------
# The cellular component gene ontology terms will be retrieve and their levels
Gene2GOTermAndLevel(genes = c(10212, 9833, 6713), organism = "Homo sapiens", domain = "CC") 

# The biological process gene ontology terms will be retrieve and their levels
Gene2GOTermAndLevel(genes = c(100000642, 30592, 58153, 794484), organism = "Danio rerio") 

# The molecular function gene ontology terms will be retrieve and their levels
Gene2GOTermAndLevel(genes = c(100009600, 18131, 100017), organism = "Mouse", domain = "MF") 


## -----------------------------------------------------------------------------
# Retrieve the level of a GO biological process term
goterms <- c("GO:0009083","GO:0006631","GO:0006629","GO:0014811","GO:0021961")
GOTermBPOnLevel(goterm = goterms)

# Retrieve the level of a GO molecular function term
goterms <- c("GO:0005515","GO:0016835","GO:0046976","GO:0015425","GO:0005261")
GOTermMFOnLevel(goterm = goterms)

# Retrieve the level of a GO cellular component term 
goterms <- c("GO:0055044","GO:0030427","GO:0036436","GO:0034980","GO:0048226")
GOTermCCOnLevel(goterm = goterms)

## -----------------------------------------------------------------------------
# Retrieve all the GO-terms from a particular GO BP level 
Level2GOTermBP(level = 1, organism = "Human")

# Retrieve all the GO-terms from a particular GO MF level
Level2GOTermMF(level = 14, organism = "Rat")

# Retrieve all the GO-terms from the general GO CC level
Level2GOTermCC(level = 14)


## -----------------------------------------------------------------------------
# Get all leaf nodes from a GO BP level 
Level2LeafNodeBP(level = 2, organism = "Danio rerio")

# Get all leaf nodes from a GO MF level 
Level2LeafNodeMF(level = 12)

# Get all leaf nodes from a GO CC level 
Level2LeafNodeCC(level = 10, organism = "Schizosaccharomyces pombe")

## -----------------------------------------------------------------------------
# All jump nodes from the GO BP level
head(Level2JumpNodeBP(level = 2, organism = "Homo sapiens"))
# All jump nodes from the GO MF level
head(Level2JumpNodeMF(level = 3, organism = "Homo sapiens"))
# All jump nodes from the GO CC level
head(Level2JumpNodeCC(level = 7, organism = "Homo sapiens"))

## -----------------------------------------------------------------------------
# All regular nodes from the BP level
head(Level2RegularNodeBP(level = 9, organism = "Zebrafish"))
# All regular nodes from the MF level
head(Level2RegularNodeMF(level = 7, organism = "Homo sapiens"))
# All jump nodes from the CC level
head(Level2RegularNodeCC(level = 7))

## -----------------------------------------------------------------------------
# All GO-terms on a particular GO BP level that are not leaf nodes 
Level2NoLeafNodeBP(level = 16, organism = "Homo sapiens")

# All GO-terms on a particular GO MF level that are not leaf nodes 
Level2NoLeafNodeMF(level = 10, organism = "Caenorhabditis elegans")

# All GO-terms on a particular GO CC level that are not leaf nodes 
Level2NoLeafNodeCC(level = 12, organism = "Homo sapiens")


## -----------------------------------------------------------------------------
goterm <- c("GO:0009083","GO:0006631","GO:0006629","GO:0016835","GO:0046976","GO:0048226")

# Returns the categories of the GO-terms in the list
getGOcategory(goterm = goterm)


## ----fig.height= 3.4, fig.cap= "Degree distribution of the biological process GO-terms on level 4."----
# Degree distribution of the GO-terms on a particular GO BP level
degreeDistBP(level = 4)

## ----fig.height= 3.4, fig.cap= "Degree distribution of the molecular function GO-terms on level 2."----
# Degree distribution of the GO-terms on a particular GO MF level
degreeDistMF(level = 2)


## ----fig.height= 3.4, fig.cap= "Degree distribution of the cellular component GO-terms on level 10."----
# Degree distribution of the GO-terms on a particular GO CC level
degreeDistCC(level = 10)

## -----------------------------------------------------------------------------
# Get the level of a GO BP term's children 
GOTermBP2ChildLevel(goterm = "GO:0007635")

# Get the level of a GO MF term's children 
GOTermMF2ChildLevel(goterm = "GO:0098632")

# Get the level of a GO CC term's children 
GOTermCC2ChildLevel(goterm = "GO:0071735")

## -----------------------------------------------------------------------------
# Biological process GO-term descendant terms
GO2DecBP(goterm = "GO:0044582")
# Molecular function GO-term descendant terms
GO2DecMF(goterm = "GO:0008553")
# Cellular component GO-term descendant terms
GO2DecCC(goterm = "GO:0031233")

## -----------------------------------------------------------------------------
# Represent all the BP gene association GO-terms for human as an edgelist
head(GetDAG(organism = "Human", domain = "BP"))

# Represent all the MF gene association GO-terms for Mouse as an edgelist
head(GetDAG(organism = "Mouse", domain = "MF"))

# Represent all the CC gene association GO-terms for Caenorhabditis elegans as an edgelist
head(GetDAG(organism = "Caenorhabditis elegans", domain = "CC"))

## ----message=FALSE------------------------------------------------------------
# The GO-terms in each node category of the reduced Caenorhabditis elegans GO-DAG
head(visRDAGMF(organism = "Caenorhabditis elegans", plot = FALSE))

# RN GO-terms on level 1 can be access as follows
visRDAGMF(organism = "Caenorhabditis elegans", plot = FALSE)$"L1 RN"

# JN GO-terms on level 9 can be access as follows
visRDAGMF(organism = "Caenorhabditis elegans", plot = FALSE)$"L9 JN"

# LN GO-terms on level 14 can be access as follows
visRDAGMF(organism = "Caenorhabditis elegans", plot = FALSE)$"L11 LN"

## ----message=FALSE, fig.cap="\\label{fig:figs1}Visualization of a reduced GO-DAG for Caenorhabditis elegans."----
# Represent the molecular function GO-DAG for organism Caenorhabditis elegans
visRDAGMF(organism = "Caenorhabditis elegans", plot = TRUE)[["plot"]]

## ----size= 0.05, fig.cap="Visualization of a reduced sub-GO-DAG of BPs for  Human."----
Terms <- c("GO:0022403","GO:0000278","GO:0006414","GO:0006415","GO:0006614",
           "GO:0045047","GO:0072599","GO:0006613","GO:0000279","GO:0000087",
           "GO:0070972","GO:0000184","GO:0000280","GO:0007067","GO:0006413",
           "GO:0048285","GO:0006412","GO:0000956","GO:0006612","GO:0019080",
           "GO:0019083","GO:0016071","GO:0006402","GO:0043624","GO:0043241",
           "GO:0006401","GO:0072594","GO:0022904","GO:0019058","GO:0032984",
           "GO:0045333","GO:0006259","GO:0051301","GO:0022900","GO:0006396",
           "GO:0060337","GO:0071357","GO:0034340","GO:0002682","GO:0051320",
           "GO:0045087","GO:0051325","GO:0022411","GO:0016032","GO:0044764",
           "GO:0022415","GO:0051329","GO:0050776","GO:0030198","GO:0043062")

# visualization the DAG node categories of the given biological process GO-terms
visRsubDAGBP(goterm = Terms, organism = "Human")

## ----size= 0.05, fig.cap="\\label{fig:rank}The hierarchy levels for a list of GO-terms (y-axis) are shown in purple and the hierarchy levels for the maximal depth of paths in the GO-DAG passing through these GO-terms is shown in red."----
Terms <- c("GO:0000278","GO:0006414","GO:0022403","GO:0006415","GO:0006614",
           "GO:0045047","GO:0072599","GO:0006613","GO:0000184","GO:0070972",
           "GO:0006413","GO:0000087","GO:0000280","GO:0000279","GO:0006612",
           "GO:0000956","GO:0048285","GO:0019080","GO:0019083","GO:0043624",
           "GO:0006402","GO:0032984","GO:0006401","GO:0072594","GO:0019058",
           "GO:0051301","GO:0016071","GO:0006412","GO:0002682","GO:0022411",
           "GO:0001775","GO:0046649","GO:0045321","GO:0050776","GO:0007155",
           "GO:0022610","GO:0060337","GO:0071357","GO:0034340","GO:0016032",
           "GO:0044764","GO:0006396","GO:0010564","GO:0002684","GO:0006259",
           "GO:0051249","GO:0045087")

# Ordering of the GO-terms in the list
distRankingGO(goterm = Terms, domain = "BP", plot = TRUE)

## ----size= 0.05, fig.cap="\\label{fig:rank2}"---------------------------------
Terms <- c("GO:0000278","GO:0006414","GO:0022403","GO:0006415","GO:0006614",
           "GO:0045047","GO:0072599","GO:0006613","GO:0000184","GO:0070972",
           "GO:0006413","GO:0000087","GO:0000280","GO:0000279","GO:0006612",
           "GO:0000956","GO:0048285","GO:0019080","GO:0019083","GO:0006402",
           "GO:0032984","GO:0006401","GO:0072594","GO:0019058","GO:0051301",
           "GO:0016071","GO:0006412","GO:0002682","GO:0022411","GO:0001775",
           "GO:0046649","GO:0045321","GO:0050776","GO:0007155","GO:0060337",
           "GO:0071357","GO:0034340","GO:0016032","GO:0006396","GO:0010564",
           "GO:0002684","GO:0006259","GO:0051249","GO:0045087")

# Ordering of the GO-terms in the list
scoreRankingGO(goterm = Terms, domain = "BP", plot = FALSE)

## -----------------------------------------------------------------------------
Terms <- c("GO:0042254", "GO:0022613", "GO:0034470", "GO:0006364", "GO:0016072",
           "GO:0034660", "GO:0006412", "GO:0006396", "GO:0007005", "GO:0032543",
           "GO:0044085", "GO:0044281", "GO:0044257", "GO:0030163", "GO:0006082",
           "GO:0044248", "GO:0006519", "GO:0009056", "GO:0019752", "GO:0043436")

# We Prioritize the given biological process GO-terms

prioritizedGOTerms(lst = Terms, organism = "Human", sp = TRUE, domain = "BP")


## -----------------------------------------------------------------------------
# All the biological process gene association GO-terms for Human and their GO-level
head(GO4Organism(organism = "Human", domain = "BP"))

# All the molecular function gene association GO-terms for Mouse and their GO-level
head(GO4Organism(organism = "Mouse", domain = "MF"))

# All the cellular component gene association GO-terms for Rat and their GO-level
head(GO4Organism(organism = "Rat", domain = "CC"))

## ----refmgr references, results="asis", echo=FALSE----------------------------
# PrintBibliography(bib) 

