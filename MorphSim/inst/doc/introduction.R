## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----tree---------------------------------------------------------------------
library(MorphSim)
library(TreeSim)
library(FossilSim)

set.seed(1234)
tree <- TreeSim::sim.bd.taxa(n = 10, numbsim = 1,
                              lambda = 0.1, mu = 0.05)[[1]]

## ----basic--------------------------------------------------------------------
morpho_data <- sim.morpho(
  time.tree = tree,
  k = 2,
  trait.num = 10,
  br.rates = 0.1
)
morpho_data

## ----morpho_check-------------------------------------------------------------
is.morpho(morpho_data)

## ----access-------------------------------------------------------------------
# Tip data as a matrix
get.matrix(morpho_data, seq = "tips")

# Transition history for a specific trait (including root state)
get.transitions(morpho_data, trait = 1)

## ----clock--------------------------------------------------------------------
# Strict clock
strict_data <- sim.morpho(time.tree = tree, k = 2, trait.num = 10,
                           br.rates = 0.1)

# Relaxed clock (random rates per branch for illustration)
relaxed_rates <- runif(nrow(tree$edge), min = 0.01, max = 0.2)
relaxed_data <- sim.morpho(time.tree = tree, k = 2, trait.num = 10,
                            br.rates = relaxed_rates)

## ----mkv----------------------------------------------------------------------
mkv_data <- sim.morpho(time.tree = tree, k = 2, trait.num = 10,
                        br.rates = 0.1, variable = TRUE)

## ----acrv_gamma---------------------------------------------------------------
gamma_data <- sim.morpho(
  time.tree = tree, k = 2, trait.num = 10, br.rates = 0.1,
  ACRV = "gamma", alpha.gamma = 1, ACRV.ncats = 4
)

# The discrete rate categories
gamma_data$model$RateVar

# Which category was assigned to each trait
gamma_data$model$RateVarTrait

## ----acrv_lgn-----------------------------------------------------------------
lgn_data <- sim.morpho(
  time.tree = tree, k = 2, trait.num = 10, br.rates = 0.1,
  ACRV = "lgn", meanlog = 0, sdlog = 1, ACRV.ncats = 4
)

# The discrete rate categories
lgn_data$model$RateVar

## ----acrv_user----------------------------------------------------------------
user_data <- sim.morpho(
  time.tree = tree, k = 2, trait.num = 10, br.rates = 0.1,
  ACRV = "user", define.ACRV.rates = c(0.5, 1, 1.5, 2),
  ACRV.ncats = 4
)

## ----mkv_gamma----------------------------------------------------------------
mkvg_data <- sim.morpho(
  time.tree = tree, k = 3, trait.num = 10, br.rates = 0.1,
  variable = TRUE, ACRV = "gamma", alpha.gamma = 1, ACRV.ncats = 4
)

## ----custom_q-----------------------------------------------------------------
ord_Q <- matrix(c(
  -0.5, 0.5, 0.0,
   0.3333333, -0.6666667, 0.3333333,
   0.0, 0.5, -0.5
), nrow = 3, byrow = TRUE)

ordered_data <- sim.morpho(
  time.tree = tree, k = 3, trait.num = 10, br.rates = 0.1,
  define.Q = ord_Q
)

## ----root_state---------------------------------------------------------------
fixed_root <- sim.morpho(
  time.tree = tree, k = 3, trait.num = 10,
  br.rates = 0.1, root.state = 0
)

## ----strict-------------------------------------------------------------------
strict_data <- sim.morpho(
  time.tree = tree, k = 3, trait.num = 10,
  br.rates = 0.1, full.states = TRUE
)

## ----partitions---------------------------------------------------------------
part_data <- sim.morpho(
  time.tree = tree,
  k = c(2, 3, 4),
  trait.num = 20,
  partition = c(10, 5, 5),
  br.rates = 0.1
)

# Model specification per partition
part_data$model$Specified

## ----combine------------------------------------------------------------------
# Partition 1: ordered characters with a custom Q matrix
ord_Q <- matrix(c(
  -0.5, 0.5, 0.0,
   0.3333333, -0.6666667, 0.3333333,
   0.0, 0.5, -0.5
), nrow = 3, byrow = TRUE)

partition_1 <- sim.morpho(
  time.tree = tree, k = 3, trait.num = 10,
  br.rates = 0.1, define.Q = ord_Q
)

# Partition 2: unordered binary characters with MkV + Gamma
partition_2 <- sim.morpho(
  time.tree = tree, k = 2, trait.num = 10,
  br.rates = 0.1, variable = TRUE,
  ACRV = "gamma", alpha.gamma = 1, ACRV.ncats = 4
)

# Combine into a single morpho object
combined <- combine.morpho(partition_1, partition_2)

# The combined object has 20 traits total
length(combined$sequences$tips[[1]])

# Model specifications from both partitions are preserved
combined$model$Specified

## ----fossils------------------------------------------------------------------
# Simulate fossil and extant sampling
f <- FossilSim::sim.fossils.poisson(rate = 0.1, tree = tree,
                                     root.edge = FALSE)
f2 <- FossilSim::sim.extant.samples(fossils = f, tree = tree, rho = 0.5)

# Simulate morphological data with fossils
fossil_data <- sim.morpho(
  time.tree = tree, k = c(2,3), trait.num = 10, partition = c(5,5),
  br.rates = 0.1, fossil = f2
)

# Sampled ancestor data
names(fossil_data$sequences$SA)

## ----missing_random-----------------------------------------------------------
missing_random <- sim.missing.data(
  data = fossil_data, method = "random",
  seq = "tips", probability = 0.3
)

## ----missing_partition--------------------------------------------------------
part_fossil <- sim.morpho(
  time.tree = tree, k = c(2, 3), trait.num = 10,
  partition = c(5, 5), br.rates = 0.1, fossil = f2
)

# Hard tissues (partition 1): 10% missing
# Soft tissues (partition 2): 70% missing
missing_part <- sim.missing.data(
  data = part_fossil, method = "partition",
  seq = "tips", probability = c(0.1, 0.7)
)

## ----missing_rate-------------------------------------------------------------
gamma_fossil <- sim.morpho(
  time.tree = tree, k = 2, trait.num = 20, br.rates = 0.1,
  ACRV = "gamma", alpha.gamma = 1, ACRV.ncats = 4, fossil = f2
)

# Slow characters preserved, fast characters increasingly lost
missing_rate <- sim.missing.data(
  data = gamma_fossil, method = "rate",
  seq = "tips", probability = c(0, 0.2, 0.5, 0.8)
)

## ----missing_trait------------------------------------------------------------
missing_trait <- sim.missing.data(
  data = fossil_data, method = "trait",
  seq = "tips", probability = 1, traits = c(1, 2, 3)
)

## ----missing_taxa-------------------------------------------------------------
# Remove all data from the first two taxa
taxa_to_remove <- names(fossil_data$sequences$tips)[1:2]
missing_taxa <- sim.missing.data(
  data = fossil_data, method = "taxa",
  seq = "tips", probability = 1, taxa = taxa_to_remove
)

## ----missing_extinct, eval = FALSE--------------------------------------------
# missing_extinct <- sim.missing.data(
#   data = fossil_data, method = "extinct",
#   seq = "tips", probability = 0.5
# )

## ----missing_trait_taxa-------------------------------------------------------
missing_trait_taxa <- sim.missing.data(
  data = fossil_data, method = "trait_taxa",
  seq = "tips", probability = 0.8,
  traits = c(1, 2, 3),
  taxa = taxa_to_remove
)

## ----stats--------------------------------------------------------------------
stats <- stats.morpho(fossil_data)
stats$Statistics
stats$Tree

## ----convergent---------------------------------------------------------------
# Which traits are convergent?
get.convergent(fossil_data)

# Detail for a specific trait
get.convergent(fossil_data, trait = 1)

## ----transitions--------------------------------------------------------------
get.transitions(fossil_data, trait = 1)

## ----plot_basic, fig.height = 6-----------------------------------------------
plot(fossil_data, trait = 6, timetree = FALSE,
     show.fossil = FALSE, reconstructed = FALSE,
     show.convergent = FALSE)

## ----plot_fossils, fig.height = 6---------------------------------------------
plot(fossil_data, trait = 6, timetree = TRUE,
     show.fossil = TRUE, reconstructed = FALSE,
     show.convergent = FALSE)

## ----plot_recon, fig.height = 6-----------------------------------------------
plot(fossil_data, trait = 6, timetree = TRUE,
     show.fossil = TRUE, reconstructed = TRUE,
     show.convergent = FALSE)

## ----plot_convergent, fig.height = 6------------------------------------------
plot(fossil_data, trait = 6, timetree = TRUE,
     show.fossil = TRUE, reconstructed = TRUE,
     show.convergent = TRUE)

## ----plot_default, eval = FALSE-----------------------------------------------
# plot(fossil_data, trait = 7)

## ----grid, fig.height = 5-----------------------------------------------------
plotMorphoGrid(fossil_data, seq = "tips")

## ----export, eval = FALSE-----------------------------------------------------
# # Full tree
# write.morpho(fossil_data, file = "tree.tre", type = "tree")
# 
# # Reconstructed tree
# write.morpho(fossil_data, file = "recon_tree.tre", type = "tree",
#              reconstructed = TRUE)
# 
# # Character matrix
# write.morpho(fossil_data, file = "matrix.nex", type = "matrix")
# 
# # Reconstructed matrix
# write.morpho(fossil_data, file = "recon_matrix.nex", type = "matrix",
#              reconstructed = TRUE)
# 
# # Fossil ages (compatible with RevBayes)
# write.morpho(fossil_data, file = "ages.tsv", type = "ages")
# 
# # With age uncertainty
# write.morpho(fossil_data, file = "ages.tsv", type = "ages",
#              uncertainty = 2)

## ----export_ape, eval = FALSE-------------------------------------------------
# ape::write.tree(fossil_data$trees$EvolTree, file = "full_tree.tre")
# ape::write.nexus.data(fossil_data$sequences$tips, file = "full_matrix.nex",
#                       format = "standard")

## ----ggtree, eval = FALSE-----------------------------------------------------
# library(treeio)
# library(ggtree)
# library(ggplot2)
# library(tibble)
# 
# # Convert tip data to a data frame
# tip_df <- as.data.frame(t(as.data.frame(fossil_data$sequences$tips)))
# colnames(tip_df) <- paste0("trait_", seq_len(ncol(tip_df)))
# tip_df$node <- match(rownames(tip_df),
#                      fossil_data$trees$EvolTree$tip.label)
# 
# # Create treedata object
# td <- treedata(phylo = fossil_data$trees$EvolTree,
#                data = as_tibble(tip_df))
# 
# # Plot tree with coloured tip points for one trait
# ggtree(td) +
#   geom_tiplab(offset = 0.01) +
#   geom_tippoint(aes(color = factor(trait_1)), size = 3) +
#   scale_color_brewer(palette = "Set2", name = "State") +
#   theme(legend.position = "right")
# 
# # Plot tree with heatmap of all traits
# heat_df <- tip_df[, grep("trait_", colnames(tip_df))]
# heat_df[] <- lapply(heat_df, factor)
# 
# gheatmap(ggtree(td) + geom_tiplab(offset = 0.05),
#          heat_df, offset = 0.02, width = 0.5,
#          colnames_angle = 90, hjust = 1) +
#   scale_fill_brewer(palette = "Set2", name = "State")

