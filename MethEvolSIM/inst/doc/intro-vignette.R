## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(MethEvolSIM)

## -----------------------------------------------------------------------------
# Define the tree:
tree <- "((a:1, b:1):2, c:2);"
# Define the genomic structure. Here 1 island ("U") containing 100 CpGs 
# surrounded by 2 non-island structures ("M") each containing 10 CpGs
infoStr <- data.frame(n = c(10, 100, 10),
                      globalState = c("M", "U", "M"))
# Simulate 1 replicate of data at the tree tips
output <- simulate_evolData(infoStr = infoStr, tree = tree)

## -----------------------------------------------------------------------------
# In replicate 1, tip 2: name of the tip of the tree
output$data[[1]][[2]]$name
# In replicate 1, tip 2: sequence of methylation states of left non-island structure
output$data[[1]][[2]]$seq[[1]]
# In replicate 1, tip 2: sequence of methylation states of island structure
output$data[[1]][[2]]$seq[[2]]
# In replicate 1, tip 2: sequence of methylation states of right non-island structure
output$data[[1]][[2]]$seq[[3]]

## -----------------------------------------------------------------------------
# Example 3 structures of length 100 each:
# one non-island containing 100 CpG sites,
# one island containing 100 CpG sites,
# and one non-island containing 100 CpG sites
infoStr <- data.frame(n = c(100, 100, 100),
                        globalState = c("M", "U", "M"))
infoStr

## -----------------------------------------------------------------------------
# Example for 2 islands: first of length 1, second of length 15
infoStr <- data.frame(n = c(1, 15),
                      globalState = c("U", "U"))
infoStr

## -----------------------------------------------------------------------------
# Example 3 structures of length 100 with customized initial methylation state
# equilibrium frequencies
infoStr <- data.frame(n = c(100, 100, 100),
                      globalState = c("M", "U", "M"),
                      u_eqFreq = c(0.1, 0.8, 0.1),
                      p_eqFreq = c(0.1, 0.1, 0.1),
                      m_eqFreq = c(0.8, 0.1, 0.8))
infoStr

## -----------------------------------------------------------------------------
default_paramValues <- get_parameterValues()
default_paramValues

## -----------------------------------------------------------------------------
# Example: modification of parameter iota to value 0.2
default_paramValues$iota <- 0.2
default_paramValues

## -----------------------------------------------------------------------------
# Example with customized initial methylation frequencies and customized parameter values
custom_infoStr <- data.frame(n = c(100, 100, 100),
                             globalState = c("M", "U", "M"),
                             u_eqFreq = c(0.1, 0.8, 0.1),
                             p_eqFreq = c(0.1, 0.1, 0.1),
                             m_eqFreq = c(0.8, 0.1, 0.8))
custom_params <- get_parameterValues()
custom_params$mu <- 0.005
initial_customD <- simulate_initialData(infoStr = custom_infoStr, params = custom_params)
# Returns customized parameters and simulated data
initial_customD$params
initial_customD$data

## -----------------------------------------------------------------------------
# Example with sampled initial methylation frequencies and default parameter values
custom_infoStr <- data.frame(n = c(100, 100, 100),
                             globalState = c("M", "U", "M"))
initialD <- simulate_initialData(infoStr = custom_infoStr)
# Returns default parameters
initialD$params
initialD$data

## -----------------------------------------------------------------------------
combiStr_object <- initialD$data
get_parameterValues(rootData = combiStr_object)

## ----eval=FALSE---------------------------------------------------------------
# # E.g. access fist structure (non-island)
# combiStr_object$get_singleStr(1)

## -----------------------------------------------------------------------------
# E.g. get the methylation equilibrium frequencies of the first singleStructureGenerator
combiStr_object$get_singleStr(1)$get_eqFreqs()

## -----------------------------------------------------------------------------
# E.g. get the sequence of methylation states of the second singleStructureGenerator
# Encoded as: 1 for unmethylated, 2 for partially-methylated, 3 for methylated
combiStr_object$get_singleStr(2)$get_seq()

## -----------------------------------------------------------------------------
# Example with customized initial methylation frequencies, customized parameter values 
# and default dt (0.01)
tree <- "((a:1, b:1):2, c:2, (d:3.7, (e:4, f:1):3):5);"
custom_infoStr <- data.frame(n = c(100, 100, 100),
                             globalState = c("M", "U", "M"),
                             u_eqFreq = c(0.1, 0.8, 0.1),
                             p_eqFreq = c(0.1, 0.1, 0.1),
                             m_eqFreq = c(0.8, 0.1, 0.8))
custom_params <- get_parameterValues()
custom_params$mu <- 0.005
evolD <- simulate_evolData(infoStr = custom_infoStr, tree = tree, params = custom_params, n_rep = 3, only_tip = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# # Returns customized parameters, tree used, time step length for SSE process used (dt)
# # and simulated data
# evolD$data
# evolD$dt
# evolD$tree
# evolD$params

## -----------------------------------------------------------------------------
# The simulated data is returned as a list. Each element of the list corresponds to a
# simulation replicate:
length(evolD$data)
rep1 <- evolD$data[[1]]
rep2 <- evolD$data[[2]]
rep3 <- evolD$data[[3]]

## -----------------------------------------------------------------------------
# When only_tip is TRUE, each replicate contains for each tip: its name and the 
# sequence of methylation states. E.g.
rep1_tip1 <- rep1[[1]]
rep1_tip1$name
# Seq is a list with the methylation states of each simulated structure.
# In this example, 3 structures: non-island, island and non-island ...
length(rep1_tip1$seq)
# Each with 100 CpGs
length(rep1_tip1$seq[[1]])

## -----------------------------------------------------------------------------
tree <- "((a:1, b:1):2, c:2, (d:3.7, (e:4, f:1):3):5);"
custom_infoStr <- data.frame(n = c(100, 100, 100),
                             globalState = c("M", "U", "M"),
                             u_eqFreq = c(0.1, 0.8, 0.1),
                             p_eqFreq = c(0.1, 0.1, 0.1),
                             m_eqFreq = c(0.8, 0.1, 0.8))
custom_params <- get_parameterValues()
custom_params$mu <- 0.005
evolD <- simulate_evolData(infoStr = custom_infoStr, tree = tree, params = custom_params, n_rep = 3, only_tip = FALSE)

## -----------------------------------------------------------------------------
# When only_tip is FALSE. $data output contains the simulated data and the 
# information on the relationship between tree branches.
names(evolD$data)

## -----------------------------------------------------------------------------
# The information of the tree branches can be accessed using $branchInTree. 
treeStr <- evolD$data$branchInTree
# It is a list in which each element index represents the index of the branch
root <- treeStr[[1]] # First branch (or tree root)
b2 <- treeStr[[2]] # Second branch 
# ...
# Each branch contains information of the offspring and parent indexes.
root$parent_index # The tree root does not have parent branches
root$offspring_index # The tree root has 3 daughter branches: 2, 5 and 6. 
b2$parent_index # Branch 2 therefore has as parent branch the root.
b2$offspring_index # Branch 2 also has 2 daughter branches: 3 and 4

## -----------------------------------------------------------------------------
# The simulated data can be accessed using $sim_data.
sim_data <- evolD$data$sim_data
# As before, each list element corresponds to a simulation replicate. 
# E.g. replicate 3
rep3 <- sim_data[[3]]
# In each replicate, each element of the list corresponds to a tree branch
# the indexes correspond to the information in $branchInTree
root <- rep3[[1]]
b2 <- rep3[[2]]
b3 <- rep3[[3]]

## -----------------------------------------------------------------------------
# Each tree branch contains:
# - the branch name (NULL for tree root and inner nodes and 
# tip name for the tree tips):
root$name
b2$name
b3$name

## -----------------------------------------------------------------------------
# - the information on IWE events that happened in that branch. 
root$IWE # The root always has NULL because its branch length is 0
# The rest of the branches have FALSE when no IWE happened or a list containing
# $islands corresponds to the island index(es) to which the event(s) happened 
# $times corresponds to the branch time in which the event happened.
b2$IWE
b3$IWE

## -----------------------------------------------------------------------------
# - A list with each element representing the sequence of methylation states of 
# each simulated island and non-island
# Encoded as 0 for unmethylated, 0.5 for partially methylated and 1 for methylated
root$seq[[1]] # First sequence (non-island)
root$seq[[2]] # Second sequence (island)

## -----------------------------------------------------------------------------
# - A list with each element representing the methylation frequencies for the states: unmethylated, partially-methylated and methylated
root$eqFreqs[[1]][1] # First structure of root (non-island) equilibrium frequency for unmethylated state
root$eqFreqs[[1]][2] # First structure of root (non-island) equilibrium frequency for partially-methylated state
root$eqFreqs[[1]][3] # First structure of root (non-island) equilibrium frequency for methylated state

## -----------------------------------------------------------------------------
# Example with customized initial methylation frequencies, customized parameter values and default dt (0.01)
tree <- "((a:1, b:1):2, c:2, (d:3.7, (e:4, f:1):3):5);"
custom_infoStr <- data.frame(n = c(100, 100, 100),
                             globalState = c("M", "U", "M"),
                             u_eqFreq = c(0.1, 0.8, 0.1),
                             p_eqFreq = c(0.1, 0.1, 0.1),
                             m_eqFreq = c(0.8, 0.1, 0.8))
custom_params <- get_parameterValues()
custom_params$mu <- 0.005
initialD <- simulate_initialData(infoStr = custom_infoStr, params = custom_params)$data
evolD <- simulate_evolData(rootData =initialD, tree = tree)

