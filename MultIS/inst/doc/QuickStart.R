## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

set.seed(42)

require(MultIS)

## ----eval=FALSE, include=FALSE------------------------------------------------
#  # Example was generated using the following code
#  simData <- simulate(ro_compartments = 1,
#                      tps = seq(1, 2*365, 60),
#                      nr_clones = 7,
#                      target_vcn = 6,
#                      clonal_variability = 0.4,
#                      measurement_noise = 0.2,
#                      use_amplification = FALSE,
#                      simulate_clones_params = list(
#                        nr_clones = 7, tps = seq(0, 2 * 365, 60),
#                        prolif = list(type = "nDistributedFixed", n_distributed_mean = 0.3,
#                                      n_distributed_sigma = 0, with_lf = TRUE, cc = 10000),
#                        diff = list(type = "nDistributedFixed", n_distributed_mean = 0.2,
#                                    n_distributed_sigma = 0, with_lf = FALSE),
#                        initdist = list(type = "equal", equal_equc = (0.3/0.2) * 100))
#  )
#  save(simData, file = 'example.RData', version = 2)
#  write.table(simData$is_readouts, file = 'example_readouts.csv', sep = ',')

## -----------------------------------------------------------------------------
dat <- read.table(file = "example_readouts.csv",
                  sep = ",", header = TRUE, row.names = 1, check.names = FALSE)
dat <- as.matrix(dat)
class(dat) <- c(class(dat), "timeseries")

## ---- echo=FALSE--------------------------------------------------------------
knitr::kable(dat[1:10,], row.names = TRUE, digits = 2)

## ---- fig.width=6, fig.height=4, fig.align="center"---------------------------
plot(dat)

## ----QS-Filtering, fig.width=6, fig.height=4, fig.align="center"--------------
filteredDat <- MultIS::filter_at_tp_biggest_n(dat, at = "720", n = 10L)
plot(filteredDat)

## ----message=FALSE, warning=FALSE, echo=FALSE---------------------------------
similarityMatrix <- MultIS::get_similarity_matrix(dat, parallel = FALSE)

is1 = which.max(unlist(
  lapply(1:(ncol(similarityMatrix) - 2),
         function(i) {
           similarityMatrix[i, i + 1] +           # maximize
             (1 - similarityMatrix[i + 1, i + 2]) # minimize
         })
  ))
is2 = is1 + 1
is3 = is1 + 2

## ----QS-rSquareSim, warning=F, fig.width=12, fig.height=4, fig.align="center"----
r2 = round(summary(stats::lm(y ~ 0 + x, data = data.frame(
    x = dat[is1, ], y = dat[is2, ])))$r.squared, 3)

p1 <- MultIS::plot_rsquare(dat, is1, is2) +
  ggplot2::ggtitle(label = bquote(R^2 == .(r2))) +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))


r2 = round(summary(stats::lm(y ~ 0 + x, data = data.frame(
    x = dat[is2, ], y = dat[is3, ])))$r.squared, 3)

p2 <- MultIS::plot_rsquare(dat, is2, is3) +
  ggplot2::ggtitle(label = bquote(R^2 == .(r2))) +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
                    
gridExtra::grid.arrange(p1, p2, ncol = 2)

## ----QS-similarityMatrix, warning=F-------------------------------------------
similarityMatrix <- MultIS::get_similarity_matrix(dat, parallel = FALSE)

## ----QS-similarityMatrixHeatmap, warning=F, fig.width=7.2, fig.height=6, fig.align="center"----
plot(similarityMatrix)

## ----QS-clusteringC3, warning=F, fig.width=12, fig.height=6, fig.align="center"----
clusterObjC2 <- MultIS::reconstruct(readouts = dat,
                                    target_communities = 2,
                                    method = "kmedoids",
                                    cluster_obj = TRUE,
                                    sim = similarityMatrix)
clusterObjC4 <- MultIS::reconstruct(readouts = dat,
                                    target_communities = 4,
                                    method = "kmedoids",
                                    cluster_obj = TRUE,
                                    sim = similarityMatrix)
p1 <- plot(clusterObjC2)
p2 <- plot(clusterObjC4)

gridExtra::grid.arrange(p1, p2, ncol = 2)

## -----------------------------------------------------------------------------
bestNrCluster <- MultIS::find_best_nr_cluster(
  data = dat,
  sim = similarityMatrix,
  method_reconstruction = "kmedoids",
  method_evaluation = "silhouette",
  return_all = TRUE)
plotDf <- data.frame(
  k = as.numeric(names(bestNrCluster)),
  score = as.numeric(bestNrCluster)
)
ggplot2::ggplot(plotDf, ggplot2::aes(x = k, y = score, group = 1)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(ggplot2::aes(col = (score == max(score)))) +
  ggplot2::scale_color_manual(values = c("TRUE" = "#FF0000", "FALSE" = "#000000")) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none")

## ----QS-Silhouette, warning=F, fig.width=7.2, fig.height=6, fig.align="center"----
bestNrCluster <- MultIS::find_best_nr_cluster(
  data = dat,
  sim = similarityMatrix,
  method_reconstruction = "kmedoids",
  method_evaluation = "silhouette",
  return_all = FALSE)

## -----------------------------------------------------------------------------
clusterObjBest <- MultIS::reconstruct(
  readouts = dat,
  target_communities = bestNrCluster,
  method = "kmedoids",
  cluster_obj = TRUE,
  sim = similarityMatrix)
plot(clusterObjBest)

## -----------------------------------------------------------------------------
load("example.RData")

## -----------------------------------------------------------------------------
str(simData, max.level = 1)

## ---- echo=FALSE--------------------------------------------------------------
knitr::kable(simData$barcodeReadouts[1:10,], digits = 2, row.names = TRUE)

## ----QS-Bushman-Clone-Readouts, fig.width=12, fig.height=8, fig.align="center"----
p1 <- plot(simData$clone_counts) + ggplot2::ggtitle("Basic clonal simulation")
p2 <- plot(simData$clone_readouts) + ggplot2:: ggtitle("Added clonal differences")
p3 <- plot(simData$is_counts) + ggplot2::ggtitle("Superimposition of integration sites")
p4 <- plot(simData$is_readouts) + ggplot2::ggtitle("Added measurement noise")
gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

## ----QS-Mappings, results="asis", echo=F--------------------------------------
mapping <- data.frame(Clone = unique(simData$mapping[,"Clone"]))
mapping$IS <- sapply(mapping$Clone, function(e) {
                      paste(summary(simData$mapping[simData$mapping[, "Clone"] == e, "IS"])[c("Min.", "Max.")], collapse = " - ")
                    })
knitr::kable(mapping)

## ----QS-ARI, warning=F, fig.width=6, fig.height=4, fig.align="center"---------
similarityMatrix <- MultIS::get_similarity_matrix(simData$is_readouts,
                                                  parallel = FALSE)
aris <- sapply(3:12, function(k) {
  clusterObj <- MultIS::reconstruct(simData$is_readouts,
                                    target_communities = k,
                                    cluster_obj = TRUE,
                                    sim = similarityMatrix)
  mclust::adjustedRandIndex(clusterObj$mapping[,"Clone"], 
                            simData$mapping[,"Clone"])  
})
arisDF <- data.frame(
  k = 3:12,
  ARI = aris,
  stringsAsFactors = F
)
ggplot2::ggplot(arisDF, ggplot2::aes(x = k, y = ARI, colour = col)) +
  ggplot2::geom_line(colour = "black") +
  ggplot2::geom_point(size = 4, ggplot2::aes(color = (ARI == max(ARI)))) +
  ggplot2::scale_color_manual(values = c("TRUE" = "#FF0000", "FALSE" = "#000000")) +
  ggplot2::scale_x_continuous(breaks = 3:12) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none",
                 text = ggplot2::element_text(size = 16))

