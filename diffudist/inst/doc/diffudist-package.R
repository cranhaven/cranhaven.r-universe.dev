## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(diffudist)

## ----echo = TRUE, eval = FALSE------------------------------------------------
#  get_distance_matrix(g, tau, type = "Normalized Laplacian", weights = NULL,
#                      as_dist = FALSE, verbose = TRUE)

## ----load-pkgs, echo = TRUE, message = FALSE, warning = FALSE-----------------
library(igraph)
library(diffudist)
igraph_options(vertex.frame.color = "white", vertex.color = "#00B4A6")
# dataset
df <- read.delim(url("http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/zachary.dat"), 
                 skip = 41, sep = " ", header = FALSE)
A <- as.matrix(df[, -1])
colnames(A) <- rownames(A) <- c("Mr Hi", paste("Actor", 2:33), "John A")
karate <- graph_from_adjacency_matrix(A, mode = "undirected", weighted = TRUE)
V(karate)$name <- c("H", 2:33, "A")
V(karate)$Faction <- 1
V(karate)$Faction[c(9, 10, 15, 16, 19, 21, 23:34)] <- 2
V(karate)$color <- viridis::viridis(3, direction = -1)[V(karate)$Faction]

## ----karate-plot, echo = TRUE-------------------------------------------------
plot(karate, edge.width = E(karate)$weight)

## ----unw-karate-ddms----------------------------------------------------------
karate_unw_ddm_list <- lapply(1:10, function(tau) {
  get_distance_matrix(karate, tau = tau, weights = NA, verbose = FALSE)
})

## -----------------------------------------------------------------------------
#  karate_unw_ddm_plots <- lapply(c(1, 2, 4, 8), function(i) {
#    ddm <- karate_unw_ddm_list[[i]]
#    plot_distance_matrix(ddm, cex = 1.3, title = bquote(tau==.(i)))
#  })
#  invisible(lapply(karate_unw_ddm_plots, show))

## ----w-karate-ddms------------------------------------------------------------
karate_w_ddm_list <- lapply(1:10, function(tau) {
  get_distance_matrix(karate, tau = tau, verbose = FALSE)
})

## -----------------------------------------------------------------------------
res_clu <- hclust(d = as.dist(karate_w_ddm_list[[1]]))
memb <- cutree(res_clu, k = 2)
V(karate)$cluster <- cutree(res_clu, k = 2)
plot(karate, vertex.shape = c("circle", "square")[V(karate)$cluster])

## -----------------------------------------------------------------------------
#  karate_w_ddm_plots <- lapply(c(1, 2, 4, 8), function(i) {
#    ddm <- karate_w_ddm_list[[i]]
#    plot_distance_matrix(ddm, cex = 1.2, title = bquote(tau==.(i)))
#  })
#  invisible(lapply(karate_w_ddm_plots, show))

## ----avg-ddms, echo = c(1:3), warning = FALSE, message = FALSE----------------
karate_unw_ddm_avg <- get_mean_distance_matrix(karate_unw_ddm_list)
karate_w_ddm_avg <- get_mean_distance_matrix(karate_w_ddm_list)
# then divide by max() and plot as before (but with  show_dendro = FALSE)
p1 <- plot_distance_matrix(
  karate_unw_ddm_avg / max(karate_unw_ddm_avg), 
  show_dendro = FALSE, cex = 1.4, title = "unweighted karate club network") +
  ggplot2::guides(fill = "none")
p2 <- plot_distance_matrix(
  karate_w_ddm_avg / max(karate_w_ddm_avg), 
  show_dendro = FALSE, cex = 1.4, title = "weighted karate club network")

## -----------------------------------------------------------------------------
#  show(p1)
#  show(p2)

## ----PRRW-transition-matrix---------------------------------------------------
alpha <- .85
N <- gorder(karate)
D_inv_karate <- diag(1 / strength(karate))
W_karate <- as_adjacency_matrix(karate, sparse = FALSE, attr = "weight")
T_prrw <- alpha * D_inv_karate %*% W_karate + (1 - alpha) / N 
# its corresponding Laplacian would be:
# L_prrw <- diag(rep(1, N)) - T_prrw
ddm_prrw <- get_distance_matrix_from_T(Pi = T_prrw, tau = 2, verbose = FALSE)

## -----------------------------------------------------------------------------
plot_distance_matrix(ddm_prrw, cex = 1.2)

