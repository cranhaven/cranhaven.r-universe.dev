## ----setseed, echo=F----------------------------------------------------------
set.seed(1)

## ----generate_data, message=FALSE---------------------------------------------
library(SCORPIUS)
dataset <- generate_dataset(num_genes = 500, num_samples = 384, num_groups = 4)

## ----show_expression----------------------------------------------------------
dataset$expression[1:6, 1:6]

## ----show_sample_info---------------------------------------------------------
head(dataset$sample_info)

## ----perform_mds--------------------------------------------------------------
expression <- dataset$expression
group_name <- dataset$sample_info$group_name
space <- reduce_dimensionality(expression, "spearman", ndim = 3)

## ----show_dimred--------------------------------------------------------------
draw_trajectory_plot(space, progression_group = group_name, contour = TRUE)

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
gimp <- gene_importances(expression, traj$time, num_permutations = 0, num_threads = 8)
gene_sel <- gimp[1:50,]
expr_sel <- expression[,gene_sel$gene]

## ----visualise_tafs, fig.keep='first'-----------------------------------------
draw_trajectory_heatmap(expr_sel, traj$time, group_name)

## ----moduled_tafs, fig.keep='first'-------------------------------------------
modules <- extract_modules(scale_quantile(expr_sel), traj$time, verbose = FALSE)
draw_trajectory_heatmap(expr_sel, traj$time, group_name, modules)

