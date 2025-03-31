## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(MethEvolSIM)

## -----------------------------------------------------------------------------
# Example: a single sample with 3 genomic structures
# (1) island with 10 partially-methylated sites
# (2) non-island with 5 methylated sites
# (3) island with 15 unmethylated sites

data <- list(rep(0.5, 10),  # Partially methylated
             rep(1,5),      # Methylated
             rep(0,15))     # Unmethylated
data

## -----------------------------------------------------------------------------
# Example: data from 3 tips of a tree,
# each with 3 genomic structures
data <- list(
    # Tip 1
    list(c(rep(0.5,5), rep(0,5)),  # 5 partially methylated, 5 unmethylated
         c(rep(1,4), 0.5),         # 4 methylated, 1 unmethylated
         c(rep(0,7), rep(0.5,8))), # 7 unmethylated, 8 partially methylated
    # Tip 2
    list(c(rep(0.5,9), 1), # 9 partially methylated, 1 methylated
         c(rep(0.5,4), 1), # 4 partially methylated, 1 methylated
         c(rep(0,8), rep(0.5,7))), # 8 unmethylated, 7 partially-methylated
    # Tip 3
    list(c(1, rep(0,8), 1), # first and last methylated, rest unmethylated
         c(rep(0.5,3), rep(1,2)), # 3 methylated, 1 unmethylated
         c(rep(0.5,15)))) # all partially methylated

## -----------------------------------------------------------------------------
non_categorized_data <- list(
  # Tip 1
    list(c(0.1, 0.7, 0.9), # First structure
         c(0.3, 0.5, 0.9)), # Second structure
  # Tip 2
    list(c(0.2, 0.8, 0.6), # First structure
         c(0.9, 0.4, 0.7)) # Second structure
  )
  
# Transform the data with custom thresholds
categorized_data <- categorize_siteMethSt(data, u_threshold = 0.15, m_threshold = 0.85)

categorized_data

## -----------------------------------------------------------------------------
# Example tree in Newick format for the above data
newick_tree <- "((tip1:1, tip2:1):1, tip3:2);"

# Example tree as a phylo object from the ape package
library(ape)
phylo_tree <- read.tree(text = newick_tree)
phylo_tree$tip.label

## -----------------------------------------------------------------------------
# 1 tip / sample / replicate
sample_n <- 1
index_islands <- c(1, 3)
index_nonislands <- c(2, 4)
data <- list(c(.5, .5, 0, 0, 0, .5), c(.5, 0, 0, .5), c(.5, .5, 0), c(0, 1, .5)) # tip 1
get_islandMeanFreqP(index_islands, data, categorized_data = T, sample_n)
get_nonislandMeanFreqP(index_nonislands, data, categorized_data = T, sample_n)
get_islandMeanFreqM(index_islands, data, categorized_data = T, sample_n)
get_nonislandMeanFreqM(index_nonislands, data, categorized_data = T, sample_n)

## -----------------------------------------------------------------------------
# 2 tip / sample / replicate
sample_n <- 2
index_islands <- c(1, 3)
index_nonislands <- c(2, 4)
data <- list(
  list(c(.5, .5, 0, 0, 0, .5), c(.5, 0, 0, .5), c(.5, .5, 0), c(0, 0, .5)), # tip 1
  list(c(0, .5, .5, 1, 1, .5), c(1, .5, 1, .5), c(0, .5, .5), c(1, .5, 1)) # tip 2
  )
get_islandMeanFreqP(index_islands, data, categorized_data = T, sample_n)
get_nonislandMeanFreqP(index_nonislands, data, categorized_data = T, sample_n)
get_islandMeanFreqM(index_islands, data, categorized_data = T, sample_n)
get_nonislandMeanFreqM(index_nonislands, data, categorized_data = T, sample_n)

## -----------------------------------------------------------------------------
# 1 tip / sample / replicate
sample_n <- 1
index_islands <- c(1, 3)
index_nonislands <- c(2, 4)
data <- list(c(.5, .5, 0, 1, 1, .5), c(.5, 0, 1, .5), c(.5, .5, 0), c(0, 0, .5))
get_islandSDFreqP(index_islands, data, categorized_data = T, sample_n)
get_nonislandSDFreqP(index_nonislands, data, categorized_data = T, sample_n)
get_islandSDFreqM(index_islands, data, categorized_data = T, sample_n)
get_nonislandSDFreqM(index_nonislands, data, categorized_data = T, sample_n)

## -----------------------------------------------------------------------------
# 2 tip / sample / replicate
sample_n <- 2
index_islands <- c(1, 3)
index_nonislands <- c(2, 4)
data <- list(
  list(c(.5, .5, 0, 0, 0, 1), c(.5, 0, 0, .5), c(1, .5, 0), c(0, 0, .5)), # tip 1
  list(c(0, .5, .5, 1, 1, .5), c(1, .5, 1, .5), c(0, .5, .5), c(1, .5, 1)) # tip 2
  )
get_islandSDFreqP(index_islands, data, categorized_data = T, sample_n)
get_nonislandSDFreqP(index_nonislands, data, categorized_data = T, sample_n)
get_islandSDFreqM(index_islands, data, categorized_data = T, sample_n)
get_nonislandSDFreqM(index_nonislands, data, categorized_data = T, sample_n)

## -----------------------------------------------------------------------------
# 1 tip / sample / replicate
sample_n <- 1
index_islands <- c(1, 3)
index_nonislands <- c(2, 4) 
data <- list(c(.5, 0, 0, 0, .5, .5, .5, .5, .5, 1, .5, 0, 0, 0, .5, .5, .5, .5, 
               .5, 1, .5, 0, 0, 0, .5, .5, .5, .5, .5, 1), # 30 sites
               c(.5, 1, 1, 1, .5, .5, 1, 1, 1, .5, .5, 1, 1, 1, .5, .5, 1, 1, 1, 
                 .5, .5, 1, 1, 1, .5), # 25 sites
               c(.5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, 
                 .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 
                 0, 0, .5, 1), # 40 sites
               c(1, 1, 1, .5, .5, .5, 0, 0, 0, .5, 1, 1, 1, .5, .5, .5, 0, 0, 0, 
                 .5, 1, 1, 1, .5, .5, .5, 0, 0, 0, .5, 
                 .5, 0, 0, 0, .5)) # 35 sites
compute_meanCor_i(index_islands, minN_CpG = 10, 
                  shore_length = 5, data, sample_n = 1, categorized_data = T)
compute_meanCor_ni(index_nonislands, minN_CpG = 10, 
                   shore_length = 5, data, sample_n = 1, categorized_data = T)

## -----------------------------------------------------------------------------
# 2 tip / sample / replicate
sample_n <- 2
index_islands <- c(1, 3)
index_nonislands <- c(2, 4)
data <- list(
  # tip 1
    list(c(.5, 0, 0, 0, .5, .5, .5, .5, .5, 1, .5, 0, 0, 0, .5, .5, .5, .5, .5, 
           1, .5, 0, 0, 0, .5, .5, .5, .5, .5, 1), # 30 sites
         c(.5, 1, 1, 1, .5, .5, 1, 1, 1, .5, .5, 1, 1, 1, .5, .5, 1, 1, 1, .5, 
           .5, 1, 1, 1, .5), # 25 sites
         c(.5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1, 
           .5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, 
           .5, 1), # 40 sites
         c(1, 1, 1, .5, .5, .5, 0, 0, 0, .5, 1, 1, 1, .5, .5, .5, 0, 0, 0, .5, 
           1, 1, 1, .5, .5, .5, 0, 0, 0, .5, .5, 0, 0, 0, .5)), # 35 sites
  # tip 2
    list(c(.5, 0, 0, .5, .5, .5, 0, 0, .5, 1, .5, 0, 0, 0, 0, .5, .5, 1, 1, 1, 
           .5, 0, 0, 0, .5, .5, 1, 1, 1, 1), # 30 sites
         c(.5, .5, 1, 1, .5, .5, 1, 1, 1, .5, .5, 0, 0, 0, .5, .5, 1, 1, 1, .5, 
           .5, 1, 1, 1, .5), # 25 sites
         c(.5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, .5, .5, 0, 0, .5, 1, 
           1, 1, 1, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, .5, 1, .5, 0, 0, 
           .5, 1), # 40 sites
         c(1, 1, 1, .5, .5, .5, 0, 0, 0, .5, 1, 1, 1, 1, .5, .5, 0, 0, 0, .5, 1, 
           1, 1, .5, .5, .5, .5, .5, 0, .5, .5, .5, .5, 0, .5)) # 35 sites
  )
compute_meanCor_i(index_islands, minN_CpG = 10, 
                  shore_length = 5, data, sample_n = 2, categorized_data = T)
compute_meanCor_ni(index_nonislands, minN_CpG = 10, 
                   shore_length = 5, data, sample_n = 2, categorized_data = T)

## -----------------------------------------------------------------------------
# Set example tree and methylation data
tree <- "((a:1.5,b:1.5):2,(c:2,d:2):1.5);"
data <- list(
    list(rep(1,10), rep(0,5), rep(1,8)), # tip a
    list(rep(1,10), rep(0.5,5), rep(0,8)), # tip b
    list(rep(1,10), rep(0.5,5), rep(0,8)), # tip c
    list(c(rep(0,5), rep(0.5, 5)), c(0, 0, 1, 1, 1), c(0.5, 1, rep(0, 6)))) # d 

# Set the index for islands and non-island structures
index_islands <- c(2)
index_nonislands <- c(1, 3)

MeanSiteFChange_cherry(data = data, 
                       categorized_data = T, 
                       tree = tree, 
                       index_islands = index_islands, 
                       index_nonislands = index_nonislands)

## -----------------------------------------------------------------------------
# Example with data from a single island structure 
# and three tips

tree <- "((bla:1,bah:1):2,booh:2);"

data <- list(
  #Tip 1
  list(c(rep(1,9), rep(0,1))), # m
  #Tip 2
  list(c(rep(0.5,10))), # p
  #Tip 3
  list(c(rep(0.5,9), rep(0.5,1)))) # p
  
index_islands <- c(1)
  
computeFitch_islandGlbSt(index_islands, data, tree, 
                         u_threshold = 0.1, m_threshold = 0.9)

## -----------------------------------------------------------------------------
# Example: data from a genomic region consisting on 3 structures with 10 sites each
# one island, one non-island, one island
# and a tree with 8 tips
tree <- "(((a:1,b:1):1,(c:1,d:1):1):1,((e:1,f:1):1,(g:1,h:1):1):1);"
data <- list(
  #Tip 1
  list(c(rep(1,5), rep(0,5)), # p
       c(rep(0,9), 1), 
        c(rep(1,8), rep(0.5,2))), # m
  #Tip 2
  list(c(rep(0.5,9), rep(0.5,1)), # p
        c(rep(0.5,9), 1), 
       c(rep(0,8), rep(0.5,2))), # u
  #Tip 3
  list(c(rep(1,9), rep(0.5,1)), # m
       c(rep(0.5,9), 1), 
       c(rep(0.5,8), rep(0.5,2))), # p
  #Tip 4
  list(c(rep(1,9), rep(0.5,1)), # m
       c(rep(1,9), 0), 
       c(rep(0.5,8), rep(0.5,2))), # p
  #Tip 5
  list(c(rep(0,5), rep(0,5)), # u
       c(rep(0,9), 1), 
       c(rep(0.5,8), rep(0.5,2))), # p
  #Tip 6
  list(c(rep(0,9), rep(0.5,1)), # u
       c(rep(0.5,9), 1), 
       c(rep(1,8), rep(0.5,2))), # m
  #Tip 7
  list(c(rep(0,9), rep(0.5,1)), # u
       c(rep(0.5,9), 1), 
       c(rep(0,8), rep(0.5,2))), # u
  #Tip 8
  list(c(rep(0,9), rep(0.5,1)), # u
       c(rep(1,9), 0), 
       c(rep(0,9), rep(0.5,1)))) # u
  
index_islands <- c(1,3)
  
computeFitch_islandGlbSt(index_islands, data, tree, 
                         u_threshold = 0.1, m_threshold = 0.9)

## -----------------------------------------------------------------------------
mean(computeFitch_islandGlbSt(index_islands, data, tree, 
                              u_threshold = 0.1, m_threshold = 0.9))

## -----------------------------------------------------------------------------
# Set example tree and methylation data
  tree <- "((a:1,b:1):2,c:2);"
  data <- list(
    #Tip a
    list(c(rep(1,9), rep(0,1)), # Structure 1: island
         c(rep(0,9), 1), # Structure 2: non-island
         c(rep(0,9), rep(0.5,1))),  # Structure 3: island
    #Tip b
    list(c(rep(0,9), rep(0.5,1)),  # Structure 1: island
         c(rep(0.5,9), 1), # Structure 2: non-island
         c(rep(0,9), rep(0,1))), # Structure 3: island
    #Tip c
    list(c(rep(1,9), rep(0.5,1)),  # Structure 1: island
         c(rep(0.5,9), 1), # Structure 2: non-island
         c(rep(0,9), rep(0.5,1)))) # Structure 3: island
  
  
  index_islands <- c(1,3)
  
  mean_CherryFreqsChange_i(data, categorized_data = T,
                           index_islands, tree,
                           pValue_threshold = 0.05)

## -----------------------------------------------------------------------------
# Set example tree and methylation data
tree <- "((a:1,b:1):2,(c:2,d:2):1);"
  data <- list(
    #Tip a
    list(c(rep(1,9), rep(0,1)), # Structure 1: island
         c(rep(0,9), 1), # Structure 2: non-island
         c(rep(0,9), rep(0,1))), # Structure 3: island
    #Tip b
    list(c(rep(0,9), rep(0.5,1)), # Structure 1: island
         c(rep(0.5,9), 1), # Structure 2: non-island
         c(rep(0,9), rep(0,1))),# Structure 3: island
    #Tip c
    list(c(rep(0,9), rep(0.5,1)), # Structure 1: island
         c(rep(0.5,9), 1), # Structure 2: non-island
         c(rep(1,9), rep(0,1))),# Structure 3: island
    #Tip d
    list(c(rep(0,9), rep(0.5,1)), # Structure 1: island
         c(rep(0.5,9), 1), # Structure 2: non-island
         c(rep(1,8), rep(0.5,2)))) # Structure 3: island
  
  
  index_islands <- c(1,3)
  
  mean_TreeFreqsChange_i(tree, data, categorized_data = T,
                           index_islands,
                           pValue_threshold = 0.05)

