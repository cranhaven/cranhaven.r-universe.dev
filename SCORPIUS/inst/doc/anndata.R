## ----setup, echo=FALSE--------------------------------------------------------
set.seed(1)
knitr::opts_chunk$set(eval = reticulate::py_module_available("anndata"))

## ----load_data, message=FALSE-------------------------------------------------
library(SCORPIUS)
library(anndata)

data(ginhoux)

adata <- AnnData(
  X = ginhoux$expression, 
  obs = ginhoux$sample_info
)

# short hand notation
group_name <- adata$obs[["group_name"]]

adata

## ----perform_mds--------------------------------------------------------------
space <- reduce_dimensionality(adata$X, dist = "spearman", ndim = 3)

## ----show_dimred--------------------------------------------------------------
draw_trajectory_plot(
  space,
  progression_group = group_name, 
  contour = TRUE
)

## ----infer_trajectory---------------------------------------------------------
traj <- infer_trajectory(space)

## ----plot_trajectory----------------------------------------------------------
draw_trajectory_plot(
  space, 
  progression_group = group_name,
  path = traj$path,
  contour = TRUE
)

## ----find_tafs----------------------------------------------------------------
gimp <- gene_importances(adata$X, traj$time, num_permutations = 0, num_threads = 8)
gene_sel <- gimp[1:50,]
expr_sel <- adata$X[,gene_sel$gene]

## ----visualise_tafs, fig.keep='first'-----------------------------------------
draw_trajectory_heatmap(expr_sel, traj$time, group_name)

## ----moduled_tafs, fig.keep='first'-------------------------------------------
modules <- extract_modules(scale_quantile(expr_sel), traj$time, verbose = FALSE)
draw_trajectory_heatmap(expr_sel, traj$time, group_name, modules)

## -----------------------------------------------------------------------------
adata$obsm[["X_mds"]] <- space
adata$uns[["trajectory_path"]] <- traj$path
adata$obs[["trajectory_pseudotime"]] <- traj$time
adata$var[["trajectory_importance"]] <- gimp[match(adata$var_names, gimp$gene), ]$importance

adata

