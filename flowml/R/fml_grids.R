list.grids = list(
  # if tuneLength is used instead of tuneGrid
  none = NULL,
  # pcr
  pcr_v001 = expand.grid(
    ncomp = seq(1:50)),
  pcr_v002 = expand.grid(
    ncomp = seq(1:5)),
  # pls
  pls_v001 = expand.grid(
    ncomp = seq(1:50)),
  # glmnet (ridge/lasso)
  glmnet_v001 = expand.grid(
    alpha = seq(0, 1, by=0.1), # balance between ridge and lasso
    lambda = seq(0, 1, by=0.1)), # penalty
  # svmRadial
  svmRadial_v001 = expand.grid(
    sigma = 1e-4,
    C = 2**seq(-3, 4, by=0.5)),
  svmRadial_v002 = expand.grid(
    sigma = 10**(-6:-1),
    C = 2**seq(-3, 4, by=0.5)),
  # xgbTree
  xgbTree_v003 = expand.grid(
    nrounds = 1000,
    eta = c(0.001, 0.005, 0.01, 0.02, 0.05, 0.1),
    max_depth = c(4, 6, 8),
    gamma = c(3, 4, 5),
    subsample = c(0.5, 0.75),
    min_child_weight = c(3),
    colsample_bytree = 1),
  xgbTree_vTest = expand.grid(
    nrounds = 1000,
    eta = c(0.001, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.3),
    max_depth = c(2, 4, 6, 8, 10),
    gamma = c(1, 2, 3),
    subsample = c(0.5, 0.75, 1),
    min_child_weight = c(1, 2, 3),
    colsample_bytree = 1),
  # spls
  spls_v001 = expand.grid(
    eta = seq(from = 0.1, to = 0.9, by = 0.2),
    K = 1:10,
    kappa = 0.5),
  # rf
  ranger_v001 = expand.grid(
    splitrule='variance',
    mtry = 1:15,
    min.node.size = 1:3),
  ranger_v002 = expand.grid(
    splitrule='variance',
    mtry = 4:30,
    min.node.size = 3:10),
  # GBM
  gbm_v001 = expand.grid(
    interaction.depth = c(1, 5, 9),
    n.trees = (1:30)*50,
    shrinkage = c(0.01, 0.1),
    n.minobsinnode = 10),
  gbm_v002 = expand.grid(
    interaction.depth = c(1, 5),
    n.trees = (20:60)*50,
    shrinkage = c(0.001, 0.01, 0.1),
    n.minobsinnode = 10),
  gbm_v003 = expand.grid(
    interaction.depth = c(1, 5),
    n.trees = (20:60)*50,
    shrinkage = c(0.001, 0.01, 0.1),
    n.minobsinnode = 1)
)


# ml.bounds = list(
#   # spls
#   spls_v001 = data.table(
#     type = c('float', 'int'),
#     lower = c(0.001, 1),
#     upper = c(0.999, 20)
#   )
#
# )