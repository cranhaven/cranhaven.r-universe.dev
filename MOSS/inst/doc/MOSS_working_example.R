## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  results = "hide",
  comment = "#>"
)
knitr::knit_hooks$set(optipng = knitr::hook_optipng)
knitr::opts_chunk$set(time_it = TRUE)

## -----------------------------------------------------------------------------
# Load package MOSS.
library("MOSS")

# Simulating omic blocks.
sim_data <- simulate_data()

# Extracting simulated omic blocks.
sim_blocks <- sim_data$sim_blocks

# Extracting subjects and features labels.
lab.sub <- sim_data$labels$lab.sub
lab.feat <- sim_data$labels$lab.feat

## -----------------------------------------------------------------------------
# Check dimensions and objects class.
lapply(sim_blocks, dim)
lapply(sim_blocks, function(x) class(x[, 1]))

# Showing how the data was generated.
set.seed(42)
O1 <- matrix(data = 0, nrow = 5e2, ncol = 1e3)
O2 <- O1
O1[1:20, 1:150] <- 1
O1 <- O1 + rnorm(5e5, mean = 0, sd = 0.5)
O2[71:90, 71:200] <- 1
O2 <- O2 + rnorm(5e5, mean = 0, sd = 0.5)

# Simulating a continous response blocks.
O3 <- 3 * O1 - 5 * O2 + rnorm(5e5, mean = 0, sd = 0.5)

# Creating a vector labeling clusters of subjects.
aux <- rep("Background", 500)
aux[1:20] <- "Group 1"
aux[71:90] <- "Group 2"
all.equal(aux, lab.sub)

# Generating a classification response.
O4 <- as.matrix(aux)

# Storing all blocks within a list.
all.equal(sim_blocks, list(
  "Block 1" = O1,
  "Block 2" = O2,
  "Block 3" = O3,
  "Block 4" = O4
))

# Creating a vector labeling signal and background features.
aux <- rep("Background features", 3000)
aux[c(1:150, 1072:1200, 2001:2200)] <- "Signal features"
all.equal(aux, lab.feat)

## ----eval = FALSE-------------------------------------------------------------
#  # Visualizing the simulated omic blocks with a heatmap.
#  library("ComplexHeatmap")
#  
#  # Highlighting groups of subjects and features.
#  Sub.ann <- rowAnnotation(
#    `Sub labels` = lab.sub,
#    col = list(`Sub labels` = c("Group 1" = "#44015480", "Group 2" = "#F1605D80", "Background" = "#21908C80")),
#    show_annotation_name = F
#  )
#  
#  Feat.ann <- HeatmapAnnotation(
#    `Feat labels` = lab.feat,
#    col = list(`Feat labels` = c("Background features" = "#47039FCC", "Signal features" = "#FA9E3BCC")),
#    show_annotation_name = F
#  )
#  
#  # Creating heatmap.
#  draw(Heatmap(do.call("cbind", sim_blocks[-4]), # Excluding the categorical response.
#    left_annotation = Sub.ann,
#    top_annotation = Feat.ann,
#    row_title = "Subjects",
#    column_title = "Features",
#    show_heatmap_legend = F,
#    cluster_columns = T,
#    cluster_rows = T
#  ),
#  annotation_legend_side = "bottom"
#  )

## -----------------------------------------------------------------------------
library("ggplot2")
library("ggthemes")
library("viridis")
set.seed(43)
out <- moss(
  data.blocks = sim_blocks[-4], # a list with omic blocks
  method = "pca", # This is the default method.
  K.X = 100, # Number of PCs require. Default is K.X = 5.
  plot = TRUE
) # Allows graphical display.

## -----------------------------------------------------------------------------
# Showing scree plot.
out$scree_plot

## -----------------------------------------------------------------------------
# Showing scatterplot of the first two PCs.
out$PC1_2_plot

## -----------------------------------------------------------------------------
out <- moss(
  data.blocks = sim_blocks[-4], # a list with omic blocks
  method = "pca", # This is the default method.
  tSNE = TRUE,
  plot = TRUE
)
out$tSNE_plot

## ----eval=TRUE----------------------------------------------------------------
out <- moss(
  data.blocks = sim_blocks[-4],
  method = "pca",
  tSNE = list("perp" = 50, "n.samples" = 1, "n.iter" = 1e3),
  cluster = TRUE,
  plot = TRUE
)
out$clus_plot

## ----eval=TRUE----------------------------------------------------------------
out <- moss(
  data.blocks = sim_blocks[-4], # a list with omic blocks
  method = "pca", # This is the default method.
  tSNE = list("perp" = 50, "n.samples" = 1, "n.iter" = 1e3),
  cluster = TRUE, # Delimiting cluster via DBSCAN.
  clus.lab = lab.sub,
  plot = TRUE
)
out$clus_plot

## ----eval=TRUE----------------------------------------------------------------
out$subLabels_vs_PCs

## ----eval=TRUE----------------------------------------------------------------
set.seed(43)
out <- moss(
  data.blocks = sim_blocks[-4], # Feed moss a list with omic blocks
  method = "pca", # This is the default method.
  K.X = 5, # Number of PC (Defaults to 5).
  nu.v = seq(1, 200, by = 10),
  nu.u = seq(1, 100, by = 2), # Same, but for subjects.
  alpha.v = 0.5, # This is the EN parameter for features.
  alpha.u = 1,
  tSNE = TRUE, # This tells moss to project the 5 PC onto 2-D via tSNE.
  cluster = TRUE,
  clus.lab = lab.sub,
  plot = TRUE
)

## ----echo = TRUE--------------------------------------------------------------
out$tun_dgSpar_plot

## ----eval=TRUE----------------------------------------------------------------
library("gridExtra")

# Did we select the correct 'signal subjects'?
d <- data.frame(
  Features = rep(1:500, 5),
  Loadings = as.vector(out$sparse$u),
  Features_groups = rep(lab.sub, 5),
  PC = paste0("PC ", rep(1:5, each = 500))
)

# Storing subjects' results.
q.s <- ggplot(d, aes(
  x = Features, y = Loadings, col = Features_groups,
  pch = Features_groups
)) +
  facet_grid(~PC, scales = "free_y") +
  scale_color_manual(values = c("#21908C80", "#44015480", "#F1605D80")) +
  scale_x_continuous("Degree of sparsity for subjects (x100)", breaks = seq(0, 500, 100), labels = seq(0, 5, 1)) +
  scale_y_continuous("Left eigenvectors") +
  geom_point() +
  theme_minimal() +
  labs(
    col = "Subjects\ngroups",
    pch = "Subjects\ngroups"
  )

# Did we select the correct 'signal features'?
d <- data.frame(
  Features = rep(1:3e3, 5),
  Loadings = as.vector(out$sparse$v),
  Features_groups = rep(lab.feat, 5),
  PC = paste0("PC ", rep(1:5, each = 3e3))
)

# Storing features' results.
q.f <- ggplot(d, aes(
  x = Features, y = Loadings, col = Features_groups,
  pch = Features_groups
)) +
  facet_grid(~PC, scales = "free_y") +
  scale_color_manual(values = c("#47039FCC", "#FA9E3BCC")) +
  scale_x_continuous("Degrees of sparsity for features (x1000)", breaks = seq(0, 3e3, 1e3), labels = seq(0, 3, 1)) +
  scale_y_continuous("Right eigenvectors (Loadings)") +
  geom_point() +
  theme_minimal() +
  labs(
    col = "Features\ngroups",
    pch = "Features\ngroups"
  )

# Putting plots together.
grid.arrange(q.s, q.f)

## ----eval=TRUE----------------------------------------------------------------
# What axes of variation are associated with clusters of subjects?
out$clusters_vs_PCs

## ----eval=TRUE----------------------------------------------------------------
set.seed(43)
out <- moss(
  data.blocks = sim_blocks[-4],
  method = "pls",
  K.X = 50, # Number of latent dimension to approx. X.
  K.Y = 5, # Number of PC (Defaults to 5).
  nu.v = seq(1, 200, by = 10),
  nu.u = seq(1, 100, by = 2),
  alpha.v = 0.5, 
  alpha.u = 1,
  tSNE = TRUE,
  cluster = TRUE,
  clus.lab = lab.sub,
  plot = TRUE,
  resp.block = 3
) # Using the third block as a multivariate response.
out$clus_plot

## ----eval=TRUE----------------------------------------------------------------
set.seed(43)
out <- moss(
  data.blocks = sim_blocks[-4],
  method = "pls",
  K.X = 50,
  K.Y = 5, 
  nu.v = seq(1, 200, by = 10),
  nu.u = seq(1, 100, by = 2),
  alpha.v = 0.5,
  alpha.u = 1,
  tSNE = TRUE, # This tells moss to project the 5 PC onto 2-D via tSNE.
  clus = TRUE,
  clus.lab = lab.sub,
  plot = TRUE,
  resp.block = 3,
  use.fbm = TRUE
)
out$clus_plot

