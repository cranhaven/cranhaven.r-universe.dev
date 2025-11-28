## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
load("../R/sysdata.rda")

## ----setup--------------------------------------------------------------------
library(fdacluster)
true_groups <- c(rep(1, 20), rep(2, 10))

## ----eval=FALSE---------------------------------------------------------------
# out_manual <- fdakmeans(
#   x = simulated30$x,
#   y = simulated30$y,
#   n_clusters = 2,
#   seeds = c(1, 21),
#   warping_class = "affine",
#   centroid_type = "mean",
#   metric = "normalized_l2",
#   cluster_on_phase = FALSE,
#   use_verbose = FALSE
# )

## -----------------------------------------------------------------------------
knitr::kable(table(out_manual$memberships, true_groups))

## ----eval=FALSE---------------------------------------------------------------
# withr::with_seed(1234, {
#   initial_seeds <- replicate(10, sample.int(30, 2, replace = FALSE), simplify = FALSE)
#   outs_manual <- lapply(initial_seeds, \(.seeds) {
#     fdakmeans(
#       x = simulated30$x,
#       y = simulated30$y,
#       n_clusters = 2,
#       seeds = .seeds,
#       warping_class = "affine",
#       centroid_type = "mean",
#       metric = "normalized_l2",
#       cluster_on_phase = FALSE,
#       use_verbose = FALSE
#     )
#   })
# })

## -----------------------------------------------------------------------------
tibble::tibble(
  Initialization = initial_seeds |> 
    sapply(\(.seeds) paste(.seeds, collapse = ",")), 
  `Misclassification Rate (%)` = sapply(outs_manual, \(.clus) {
    tbl <- table(.clus$memberships, true_groups)
    round(min(tbl[1, 1] + tbl[2, 2], tbl[1, 2] + tbl[2, 1]) / 30 * 100, 2)
  })
) |> 
  knitr::kable()

## ----eval=FALSE---------------------------------------------------------------
# withr::with_seed(1234, {
#   outs_kpp <- replicate(10, {
#     fdakmeans(
#       x = simulated30$x,
#       y = simulated30$y,
#       n_clusters = 2,
#       seeding_strategy = "kmeans++",
#       warping_class = "affine",
#       centroid_type = "mean",
#       metric = "normalized_l2",
#       cluster_on_phase = FALSE,
#       use_verbose = FALSE
#     )
#   }, simplify = FALSE)
# })

## -----------------------------------------------------------------------------
tibble::tibble(
  Run = 1:10, 
  `Misclassification Rate (%)` = sapply(outs_kpp, \(.clus) {
    tbl <- table(.clus$memberships, true_groups)
    round(min(tbl[1, 1] + tbl[2, 2], tbl[1, 2] + tbl[2, 1]) / 30 * 100, 2)
  })
) |> 
  knitr::kable()

## ----eval=FALSE---------------------------------------------------------------
# withr::with_seed(1234, {
#   outs_ekpp <- replicate(10, {
#     fdakmeans(
#       x = simulated30$x,
#       y = simulated30$y,
#       n_clusters = 2,
#       seeding_strategy = "exhaustive-kmeans++",
#       warping_class = "affine",
#       centroid_type = "mean",
#       metric = "normalized_l2",
#       cluster_on_phase = FALSE,
#       use_verbose = FALSE
#     )
#   }, simplify = FALSE)
# })

## -----------------------------------------------------------------------------
tibble::tibble(
  Run = 1:10, 
  `Misclassification Rate (%)` = sapply(outs_ekpp, \(.clus) {
    tbl <- table(.clus$memberships, true_groups)
    round(min(tbl[1, 1] + tbl[2, 2], tbl[1, 2] + tbl[2, 1]) / 30 * 100, 2)
  })
) |> 
  knitr::kable()

## ----eval=FALSE---------------------------------------------------------------
# out <- fdakmeans(
#   x = simulated30$x,
#   y = simulated30$y,
#   n_clusters = 2,
#   seeding_strategy = "hclust",
#   warping_class = "affine",
#   centroid_type = "mean",
#   metric = "normalized_l2",
#   cluster_on_phase = FALSE,
#   use_verbose = FALSE
# )

## -----------------------------------------------------------------------------
knitr::kable(table(out_hclust$memberships, true_groups))

