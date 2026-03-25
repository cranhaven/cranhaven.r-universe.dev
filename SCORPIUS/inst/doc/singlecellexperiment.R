## ----setseed, echo=FALSE------------------------------------------------------
set.seed(1)

## ----load_data, message=FALSE-------------------------------------------------
library(SCORPIUS)
library(SingleCellExperiment)

data(ginhoux)

sce <- SingleCellExperiment(
  assays = list(counts = t(ginhoux$expression)),
  colData = ginhoux$sample_info
)

# short hand notation
group_name <- colData(sce)$group_name

sce

## ----perform_mds--------------------------------------------------------------
space <- reduce_dimensionality(t(assay(sce, "counts")), dist = "spearman", ndim = 3)

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
gimp <- gene_importances(
  t(assay(sce, "counts")),
  traj$time,
  num_permutations = 0,
  num_threads = 8
)
gene_sel <- gimp[1:50,]
expr_sel <- t(assay(sce, "counts"))[,gene_sel$gene]

## ----visualise_tafs, fig.keep='first'-----------------------------------------
draw_trajectory_heatmap(expr_sel, traj$time, group_name)

## ----moduled_tafs, fig.keep='first'-------------------------------------------
modules <- extract_modules(scale_quantile(expr_sel), traj$time, verbose = FALSE)
draw_trajectory_heatmap(expr_sel, traj$time, group_name, modules)

## -----------------------------------------------------------------------------
reducedDims(sce) <- SimpleList(MDS = space)
colData(sce)$trajectory_path <- traj$path
colData(sce)$trajectory_pseudotime <- traj$time
rowData(sce)$trajectory_importance <- gimp[match(rownames(sce), gimp$gene),]$importance

sce

