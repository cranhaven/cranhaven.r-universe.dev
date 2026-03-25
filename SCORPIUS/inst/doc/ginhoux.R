## ----setseed, echo=FALSE------------------------------------------------------
set.seed(1)

## ----load_data, message=FALSE-------------------------------------------------
library(SCORPIUS)
data(ginhoux)

## ----show_expression----------------------------------------------------------
ginhoux$expression[1:6, 1:6]

## ----show_sample_info---------------------------------------------------------
head(ginhoux$sample_info)

## ----perform_mds--------------------------------------------------------------
expression <- ginhoux$expression
group_name <- ginhoux$sample_info$group_name
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

## ----echo=F-------------------------------------------------------------------
# reverse the trajectory. This does not change the results in any way,
# other than the heatmap being ordered more logically.
# traj <- reverse_trajectory(traj)

## ----visualise_tafs, fig.keep='first'-----------------------------------------
draw_trajectory_heatmap(expr_sel, traj$time, group_name)

## ----moduled_tafs, fig.keep='first'-------------------------------------------
modules <- extract_modules(scale_quantile(expr_sel), traj$time, verbose = FALSE)
draw_trajectory_heatmap(expr_sel, traj$time, group_name, modules)

