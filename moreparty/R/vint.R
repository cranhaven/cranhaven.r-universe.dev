# #
# # The Friedman 1 benchmark problem
# #
# 
# # Load required packages
# library(gbm)
# library(ggplot2)
# library(mlbench)
# 
# # Simulate training data
# trn <- gen_friedman(500, seed = 101)  # ?vip::gen_friedman
# 
# #
# # NOTE: The only interaction that actually occurs in the model from which
# # these data are generated is between x.1 and x.2!
# #
# 
# # Fit a GBM to the training data
# set.seed(102)  # for reproducibility
# fit <- gbm(y ~ ., data = trn, distribution = "gaussian", n.trees = 1000,
#            interaction.depth = 2, shrinkage = 0.01, bag.fraction = 0.8,
#            cv.folds = 5)
# best_iter <- gbm.perf(fit, plot.it = FALSE, method = "cv")
# 
# # Quantify relative interaction strength
# all_pairs <- combn(paste0("x.", 1:10), m = 2)
# res <- NULL
# for (i in seq_along(all_pairs)) {
#   interact <- vint(fit, feature_names = all_pairs[, i], n.trees = best_iter)
#   res <- rbind(res, interact)
# }
# 
# # Plot top 20 results
# top_20 <- res[1L:20L, ]
# ggplot(top_20, aes(x = reorder(Variables, Interaction), y = Interaction)) +
#   geom_col() +
#   coord_flip() +
#   xlab("") +
#   ylab("Interaction strength")

# object = iris.cf
# feature_names = c("Sepal.Length", "Sepal.Width", "Petal.Length")
# progress = "none"
# parallel = FALSE
# paropts = NULL

vip_vint <- function(object, feature_names, progress = "none", parallel = FALSE,
                 paropts = NULL, ...) {
  # warning("This function is experimental, use at your own risk!", call. = FALSE)
  # FIXME: Should we force `chull = FALSE` in the call to `pdp::partial()`?
  all.pairs <- utils::combn(feature_names, m = 2)
  ints <- plyr::aaply(
    all.pairs, .margins = 2, .progress = progress, .parallel = parallel,
    .paropts = paropts,
    .fun = function(x) {
      pd <- pdp::partial(object, pred.var = x, ...)
      mean(c(
        stats::sd(tapply(pd$yhat, INDEX = pd[[x[1L]]], FUN = stats::sd)),
        stats::sd(tapply(pd$yhat, INDEX = pd[[x[2L]]], FUN = stats::sd))
      ))
    })
  ints <- data.frame(
    "Variables" = paste0(all.pairs[1L, ], "*", all.pairs[2L, ]),
    "Interaction" = ints
  )
  ints <- ints[order(ints$Interaction, decreasing = TRUE), ]
  # tibble::as_tibble(ints)
}
