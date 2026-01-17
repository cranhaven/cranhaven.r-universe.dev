## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 16,
  fig.height = 8,
  fig.retina = NULL,
  out.width = "100%"
)

## ----include = FALSE----------------------------------------------------------
if (Sys.getenv("RGL_USE_NULL") == "" && !interactive()) {
  Sys.setenv(RGL_USE_NULL = "TRUE")
}

## ----setup--------------------------------------------------------------------
library(QuAnTeTrack)

## ----eval=FALSE---------------------------------------------------------------
# PaluxyRiver <- tps_to_track(
#   system.file("extdata", "PaluxyRiver.tps", package = "QuAnTeTrack"),
#   scale = 0.004341493,
#   missing = FALSE,
#   NAs = NULL
# )

## ----echo=FALSE---------------------------------------------------------------
PaluxyRiver <- tps_to_track(
  system.file("extdata", "PaluxyRiver.tps", package = "QuAnTeTrack"),
  scale = 0.004341493,
  missing = FALSE,
  NAs = NULL
)

## ----eval=FALSE---------------------------------------------------------------
# MountTom <- tps_to_track(
#   system.file("extdata", "MountTom.tps", package = "QuAnTeTrack"),
#   scale = 0.004411765,
#   missing = TRUE,
#   NAs = matrix(c(7, 3), nrow = 1, ncol = 2),
#   R.L.side = c(
#     "R", "L", "L", "L", "R", "L", "R", "R", "L", "L", "L", "L", "L",
#     "R", "R", "L", "R", "R", "L", "R", "R", "R", "R"
#   )
# )

## ----echo=FALSE---------------------------------------------------------------
MountTom <- tps_to_track(
  system.file("extdata", "MountTom.tps", package = "QuAnTeTrack"),
  scale = 0.004411765,
  missing = TRUE,
  NAs = matrix(c(7, 3), nrow = 1, ncol = 2),
  R.L.side = c(
    "R", "L", "L", "L", "R", "L", "R", "R", "L", "L", "L", "L", "L", 
    "R", "R", "L", "R", "R", "L", "R", "R", "R", "R"
  )
)

## ----eval=FALSE---------------------------------------------------------------
# sbMountTom <- subset_track(MountTom, tracks = c(1, 2, 3, 4, 7, 8, 9, 13, 15, 16, 18))
# print(sbMountTom)

## ----echo=FALSE---------------------------------------------------------------
sbMountTom <- subset_track(MountTom, tracks = c(1, 2, 3, 4, 7, 8, 9, 13, 15, 16, 18))
print(sbMountTom)

## ----echo=TRUE----------------------------------------------------------------
plot_track(PaluxyRiver)

## ----echo=TRUE----------------------------------------------------------------
plot_track(MountTom)

## ----echo=TRUE----------------------------------------------------------------
plot_track(PaluxyRiver, plot = "Footprints")

## ----echo=TRUE----------------------------------------------------------------
plot_track(MountTom, plot = "Footprints")

## ----echo=TRUE----------------------------------------------------------------
plot_track(PaluxyRiver, plot = "Tracks")

## ----echo=TRUE----------------------------------------------------------------
plot_track(MountTom, plot = "Tracks")

## ----echo=TRUE----------------------------------------------------------------
labels <- paste("Track", seq_along(MountTom[[1]]))
plot_track(MountTom, plot.labels = TRUE, labels = labels, cex.l = 4, box.p = 0.3, alpha.l = 0.7)

## ----echo=TRUE----------------------------------------------------------------
plot_track(PaluxyRiver, plot = "Footprints", colours = c("red", "orange"), shape.f = c(15, 18))

## ----eval=FALSE---------------------------------------------------------------
# params_paluxy <- track_param(PaluxyRiver)

## ----echo=FALSE---------------------------------------------------------------
params_paluxy <- track_param(PaluxyRiver)

## ----eval=FALSE---------------------------------------------------------------
# params_mount <- track_param(MountTom)

## ----echo=FALSE---------------------------------------------------------------
params_mount <- track_param(MountTom)

## ----eval=FALSE---------------------------------------------------------------
# H_paluxyriver <- c(3.472, 2.200)
# velocity_paluxyriver <- velocity_track(PaluxyRiver, H = H_paluxyriver)

## ----echo=FALSE---------------------------------------------------------------
H_paluxyriver <- c(3.472, 2.200)
velocity_paluxyriver <- velocity_track(PaluxyRiver, H = H_paluxyriver)

## ----eval=FALSE---------------------------------------------------------------
# H_mounttom <- c(
#   1.380, 1.404, 1.320, 1.736, 1.364, 1.432, 1.508, 1.768, 1.600,
#   1.848, 1.532, 1.532, 0.760, 1.532, 1.688, 1.620, 0.636, 1.784, 1.676, 1.872,
#   1.648, 1.760, 1.612
# )
# velocity_mounttom <- velocity_track(MountTom, H = H_mounttom)

## ----echo=FALSE---------------------------------------------------------------
H_mounttom <- c(
  1.380, 1.404, 1.320, 1.736, 1.364, 1.432, 1.508, 1.768, 1.600,
  1.848, 1.532, 1.532, 0.760, 1.532, 1.688, 1.620, 0.636, 1.784, 1.676, 1.872,
  1.648, 1.760, 1.612
)
velocity_mounttom <- velocity_track(MountTom, H = H_mounttom)

## ----eval=FALSE---------------------------------------------------------------
# H_paluxyriver <- c(3.472, 2.200)
# Method_paluxyriver <- c("A", "B")
# velocity_paluxyriver_diff <- velocity_track(PaluxyRiver, H = H_paluxyriver, method = Method_paluxyriver)

## ----echo=FALSE---------------------------------------------------------------
H_paluxyriver <- c(3.472, 2.200)
Method_paluxyriver <- c("A", "B")
velocity_paluxyriver_diff <- velocity_track(PaluxyRiver, H = H_paluxyriver, method = Method_paluxyriver)

## ----echo=TRUE----------------------------------------------------------------
plot_velocity(PaluxyRiver, velocity_paluxyriver_diff, param = "RSL")

## ----echo=TRUE----------------------------------------------------------------
plot_velocity(MountTom, velocity_mounttom, param = "V")

## ----echo=TRUE----------------------------------------------------------------
plot_velocity(PaluxyRiver, velocity_paluxyriver_diff, param = "RSL", lwd = 1.5, 
              colours = c("purple", "orange", "pink", "gray"), legend = FALSE)

## ----echo=TRUE----------------------------------------------------------------
plot_velocity(MountTom, velocity_mounttom, param = "V", lwd = 2, 
              colours = c("blue", "green", "yellow", "red"))

## ----echo=TRUE, results='hide'------------------------------------------------
plot_direction(PaluxyRiver, plot_type = "boxplot")

## ----echo=TRUE, results='hide'------------------------------------------------
plot_direction(MountTom, plot_type = "boxplot")

## ----echo=TRUE, results='hide'------------------------------------------------
plot_direction(PaluxyRiver, plot_type = "polar_steps")

## ----echo=TRUE, results='hide'------------------------------------------------
plot_direction(MountTom, plot_type = "polar_steps")

## ----echo=TRUE, results='hide'------------------------------------------------
plot_direction(PaluxyRiver, plot_type = "polar_average")

## ----echo=TRUE, results='hide'------------------------------------------------
plot_direction(MountTom, plot_type = "polar_average")

## ----echo=TRUE, results='hide'------------------------------------------------
plot_direction(PaluxyRiver, plot_type = "faceted")

## ----echo=TRUE, results='hide'------------------------------------------------
plot_direction(MountTom, plot_type = "faceted")

## ----echo=TRUE, results='hide'------------------------------------------------
plot_direction(PaluxyRiver, plot_type = "polar_average", y_breaks_manual = c(1, 2))

## ----echo=TRUE, results='hide'------------------------------------------------
plot_direction(PaluxyRiver, plot_type = "polar_steps", y_labels_position = -90)

## ----eval=FALSE---------------------------------------------------------------
# test_velocity(PaluxyRiver, velocity_paluxyriver_diff, analysis = "ANOVA")

## ----echo=FALSE---------------------------------------------------------------
test_velocity(PaluxyRiver, velocity_paluxyriver_diff, analysis = "ANOVA")

## ----eval=FALSE---------------------------------------------------------------
# test_velocity(MountTom, velocity_mounttom, analysis = "ANOVA")

## ----echo=FALSE---------------------------------------------------------------
test_velocity(MountTom, velocity_mounttom, analysis = "ANOVA")

## ----eval=FALSE---------------------------------------------------------------
# test_velocity(PaluxyRiver, velocity_paluxyriver_diff, analysis = "Kruskal-Wallis")

## ----echo=FALSE---------------------------------------------------------------
test_velocity(PaluxyRiver, velocity_paluxyriver_diff, analysis = "Kruskal-Wallis")

## ----eval=FALSE---------------------------------------------------------------
# test_velocity(MountTom, velocity_mounttom, analysis = "Kruskal-Wallis")

## ----echo=FALSE---------------------------------------------------------------
test_velocity(MountTom, velocity_mounttom, analysis = "Kruskal-Wallis")

## ----eval=FALSE---------------------------------------------------------------
# test_velocity(PaluxyRiver, velocity_paluxyriver_diff, analysis = "GLM")

## ----echo=FALSE---------------------------------------------------------------
test_velocity(PaluxyRiver, velocity_paluxyriver_diff, analysis = "GLM")

## ----eval=FALSE---------------------------------------------------------------
# test_velocity(MountTom, velocity_mounttom, analysis = "GLM")

## ----echo=FALSE---------------------------------------------------------------
test_velocity(MountTom, velocity_mounttom, analysis = "GLM")

## ----eval=FALSE---------------------------------------------------------------
# mode_velocity(velocity_paluxyriver_diff)

## ----echo=FALSE---------------------------------------------------------------
mode_velocity(velocity_paluxyriver_diff)

## ----eval=FALSE---------------------------------------------------------------
# mode_velocity(velocity_mounttom)

## ----echo=FALSE---------------------------------------------------------------
mode_velocity(velocity_mounttom)

## ----eval=FALSE---------------------------------------------------------------
# test_direction(PaluxyRiver, analysis = "ANOVA")

## ----include=FALSE------------------------------------------------------------
test_dir_paluxy_anova <- test_direction(PaluxyRiver, analysis = "ANOVA")

## ----echo=FALSE---------------------------------------------------------------
print(test_dir_paluxy_anova)

## ----eval=FALSE---------------------------------------------------------------
# test_direction(MountTom, analysis = "ANOVA")

## ----include=FALSE------------------------------------------------------------
test_dir_mount_anova <- test_direction(MountTom, analysis = "ANOVA")

## ----echo=FALSE---------------------------------------------------------------
print(test_dir_mount_anova)

## ----eval=FALSE---------------------------------------------------------------
# test_direction(PaluxyRiver, analysis = "Kruskal-Wallis")

## ----include=FALSE------------------------------------------------------------
test_dir_paluxy_Kruskal <- test_direction(PaluxyRiver, analysis = "Kruskal-Wallis")

## ----echo=FALSE---------------------------------------------------------------
print(test_dir_paluxy_Kruskal)

## ----eval=FALSE---------------------------------------------------------------
# test_direction(MountTom, analysis = "Kruskal-Wallis")

## ----include=FALSE------------------------------------------------------------
test_dir_mount_Kruskal <- test_direction(MountTom, analysis = "Kruskal-Wallis")

## ----echo=FALSE---------------------------------------------------------------
print(test_dir_mount_Kruskal)

## ----eval=FALSE---------------------------------------------------------------
# test_direction(PaluxyRiver, analysis = "GLM")

## ----include=FALSE------------------------------------------------------------
test_dir_paluxy_GLM <- test_direction(PaluxyRiver, analysis = "GLM")

## ----echo=FALSE---------------------------------------------------------------
print(test_dir_paluxy_GLM)

## ----eval=FALSE---------------------------------------------------------------
# test_direction(MountTom, analysis = "GLM")

## ----include=FALSE------------------------------------------------------------
test_dir_mount_GLM <- test_direction(MountTom, analysis = "GLM")

## ----echo=FALSE---------------------------------------------------------------
print(test_dir_mount_GLM)

## ----eval=FALSE---------------------------------------------------------------
# sim_unconstrained_paluxy <- simulate_track(PaluxyRiver, nsim = 100, model = "Unconstrained")
# print(sim_unconstrained_paluxy[1:10])

## ----echo=FALSE---------------------------------------------------------------
sim_unconstrained_paluxy <- simulate_track(PaluxyRiver, nsim = 100, model = "Unconstrained")
print(sim_unconstrained_paluxy[1:10])

## ----eval=FALSE---------------------------------------------------------------
# sim_directed_paluxy <- simulate_track(PaluxyRiver, nsim = 100, model = "Directed")
# print(sim_directed_paluxy[1:10])

## ----echo=FALSE---------------------------------------------------------------
sim_directed_paluxy <- simulate_track(PaluxyRiver, nsim = 100, model = "Directed")
print(sim_directed_paluxy[1:10])

## ----eval=FALSE---------------------------------------------------------------
# sim_constrained_paluxy <- simulate_track(PaluxyRiver, nsim = 100, model = "Constrained")
# print(sim_constrained_paluxy[1:10])

## ----echo=FALSE---------------------------------------------------------------
sim_constrained_paluxy <- simulate_track(PaluxyRiver, nsim = 100, model = "Constrained")
print(sim_constrained_paluxy[1:10])

## ----eval=FALSE---------------------------------------------------------------
# sim_unconstrained_mount <- simulate_track(sbMountTom, nsim = 100, model = "Unconstrained")
# print(sim_unconstrained_mount[1:10])

## ----echo=FALSE---------------------------------------------------------------
sim_unconstrained_mount <- simulate_track(sbMountTom, nsim = 100, model = "Unconstrained")
print(sim_unconstrained_mount[1:10])

## ----eval=FALSE---------------------------------------------------------------
# sim_directed_mount <- simulate_track(sbMountTom, nsim = 100, model = "Directed")
# print(sim_directed_mount[1:10])

## ----echo=FALSE---------------------------------------------------------------
sim_directed_mount <- simulate_track(sbMountTom, nsim = 100, model = "Directed")
print(sim_directed_mount[1:10])

## ----eval=FALSE---------------------------------------------------------------
# sim_constrained_mount <- simulate_track(sbMountTom, nsim = 100, model = "Constrained")
# print(sim_constrained_mount[1:10])

## ----echo=FALSE---------------------------------------------------------------
sim_constrained_mount <- simulate_track(sbMountTom, nsim = 100, model = "Constrained")
print(sim_constrained_mount[1:10])

## ----echo=TRUE----------------------------------------------------------------
plot_sim(PaluxyRiver, sim_unconstrained_paluxy)

## ----echo=TRUE----------------------------------------------------------------
plot_sim(PaluxyRiver, sim_directed_paluxy,
  colours_sim = c("#E69F00", "#56B4E9"),
  alpha_sim = 0.4, lwd_sim = 1,
  colours_act = c("black", "black"), alpha_act = 0.7, lwd_act = 2
)

## ----echo=TRUE----------------------------------------------------------------
plot_sim(PaluxyRiver, sim_constrained_paluxy,
  colours_sim = c("#E69F00", "#56B4E9"),
  alpha_sim = 0.6, lwd_sim = 0.1,
  alpha_act = 0.5, lwd_act = 2
)

## ----echo=TRUE----------------------------------------------------------------
plot_sim(sbMountTom, sim_unconstrained_mount)

## ----echo=TRUE----------------------------------------------------------------
plot_sim(sbMountTom, sim_directed_mount,
  colours_sim = c("#6BAED6", "#FF7F00", "#1F77B4", "#D62728", 
                  "#2CA02C", "#9467BD", "#8C564B", "#E377C2", 
                  "#7F7F7F", "#BCBD22", "#17BECF"),
  alpha_sim = 0.3, lwd_sim = 1.5,
  alpha_act = 0.8, lwd_act = 2
)

## ----echo=TRUE----------------------------------------------------------------
plot_sim(sbMountTom, sim_constrained_mount,
  colours_sim = c("#6BAED6", "#FF7F00", "#1F77B4", "#D62728", 
                  "#2CA02C", "#9467BD", "#8C564B", "#E377C2", 
                  "#7F7F7F", "#BCBD22", "#17BECF"),
  alpha_sim = 0.5, lwd_sim = 0.2,
  alpha_act = 0.6, lwd_act = 2
)

## ----eval=FALSE---------------------------------------------------------------
# simil_dtw_directed_paluxy <- simil_DTW_metric(PaluxyRiver, test = TRUE,
#                                               sim = sim_directed_paluxy,
#                                               superposition = "Centroid")

## ----include=FALSE------------------------------------------------------------
simil_dtw_directed_paluxy <- simil_DTW_metric(PaluxyRiver, test = TRUE, 
                                              sim = sim_directed_paluxy, 
                                              superposition = "Centroid")

## ----echo=FALSE---------------------------------------------------------------
print(simil_dtw_directed_paluxy)

## ----eval=FALSE---------------------------------------------------------------
# simil_frechet_directed_paluxy <- simil_Frechet_metric(PaluxyRiver, test = TRUE,
#                                               sim = sim_directed_paluxy,
#                                               superposition = "Centroid")

## ----include=FALSE------------------------------------------------------------
simil_frechet_directed_paluxy <- simil_Frechet_metric(PaluxyRiver, test = TRUE, 
                                              sim = sim_directed_paluxy, 
                                              superposition = "Centroid")

## ----echo=FALSE---------------------------------------------------------------
print(simil_frechet_directed_paluxy)

## ----eval=FALSE---------------------------------------------------------------
# simil_dtw_constrained_mount <- simil_DTW_metric(sbMountTom, test = TRUE,
#                                                 sim = sim_constrained_mount,
#                                                 superposition = "Origin")

## ----include=FALSE------------------------------------------------------------
simil_dtw_constrained_mount <- simil_DTW_metric(sbMountTom, test = TRUE, 
                                                sim = sim_constrained_mount, 
                                                superposition = "Origin")

## ----echo=FALSE---------------------------------------------------------------
print(simil_dtw_constrained_mount)

## ----eval=FALSE---------------------------------------------------------------
# simil_frechet_constrained_mount <- simil_Frechet_metric(sbMountTom, test = TRUE,
#                                                 sim = sim_constrained_mount,
#                                                 superposition = "Origin")

## ----include=FALSE------------------------------------------------------------
simil_frechet_constrained_mount <- simil_Frechet_metric(sbMountTom, test = TRUE, 
                                                sim = sim_constrained_mount, 
                                                superposition = "Origin")

## ----echo=FALSE---------------------------------------------------------------
print(simil_frechet_constrained_mount)

## ----eval=FALSE---------------------------------------------------------------
# int_directed_paluxy <- track_intersection(PaluxyRiver, test = TRUE, H1 = "Lower",
#                                           sim = sim_directed_paluxy,
#                                           origin.permutation = "None")
# print(int_directed_paluxy)

## ----include=FALSE------------------------------------------------------------
int_directed_paluxy <- track_intersection(PaluxyRiver, test = TRUE, H1 = "Lower", 
                                          sim = sim_directed_paluxy, 
                                          origin.permutation = "None")

## ----echo=FALSE---------------------------------------------------------------
print(int_directed_paluxy)

## ----eval=FALSE---------------------------------------------------------------
# int_constrained_mount <- track_intersection(sbMountTom, test = TRUE, H1 = "Higher",
#                                             sim = sim_constrained_mount,
#                                             origin.permutation = "Conv.Hull")
# print(int_constrained_mount)

## ----include=FALSE------------------------------------------------------------
int_constrained_mount <- track_intersection(sbMountTom, test = TRUE, H1 = "Higher", 
                                            sim = sim_constrained_mount, 
                                            origin.permutation = "Conv.Hull")

## ----echo=FALSE---------------------------------------------------------------
print(int_constrained_mount)

## ----eval=FALSE---------------------------------------------------------------
# combined_metrics_paluxy <- combined_prob(PaluxyRiver, metrics = list(
#   simil_dtw_directed_paluxy,
#   simil_frechet_directed_paluxy,
#   int_directed_paluxy
# ))

## ----include=FALSE------------------------------------------------------------
combined_metrics_paluxy <- combined_prob(PaluxyRiver, metrics = list(
  simil_dtw_directed_paluxy,
  simil_frechet_directed_paluxy,
  int_directed_paluxy
))

## ----echo=FALSE---------------------------------------------------------------
print(combined_metrics_paluxy)

## ----eval=FALSE---------------------------------------------------------------
# H_mounttom_subset <- c(
#   1.380, 1.404, 1.320, 1.736, 1.432, 1.508, 1.768, 0.760, 1.688, 1.620, 1.784
# )
# velocity_sbmounttom <- velocity_track(sbMountTom, H = H_mounttom_subset)

## ----echo=FALSE---------------------------------------------------------------
H_mounttom_subset <- c(
  1.380, 1.404, 1.320, 1.736, 1.432, 1.508, 1.768, 0.760, 1.688, 1.620, 1.784
)
velocity_sbmounttom <- velocity_track(sbMountTom, H = H_mounttom_subset)

## ----eval=FALSE---------------------------------------------------------------
# clustering_mounttom <- cluster_track(
#   data = sbMountTom,
#   veltrack = velocity_sbmounttom,
#   variables = c("Sinuosity", "Straightness", "Velocity", "TurnAng")
# )

## ----include=FALSE------------------------------------------------------------
clustering_mounttom <- cluster_track(
  data = sbMountTom,
  veltrack = velocity_sbmounttom,
  variables = c("Sinuosity", "Straightness", "Velocity", "TurnAng")
)

## ----echo=FALSE---------------------------------------------------------------
print(clustering_mounttom)

## ----eval=FALSE---------------------------------------------------------------
# clustering_mounttom$clust$classification

## ----echo=FALSE---------------------------------------------------------------
clustering_mounttom$clust$classification

## ----eval=FALSE---------------------------------------------------------------
# clustering_mounttom$clust$z

## ----echo=FALSE---------------------------------------------------------------
clustering_mounttom$clust$z

