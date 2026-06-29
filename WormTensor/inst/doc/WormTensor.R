## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----Load libraries, echo=TRUE------------------------------------------------
library(WormTensor)

## ----worm_download, echo=TRUE-------------------------------------------------
object <- worm_download()

## ----as_worm_tensor, echo=TRUE------------------------------------------------
object <- as_worm_tensor(object$Ds)

## ----worm_membership, echo=TRUE-----------------------------------------------
object <- worm_membership(object, k=6)

## ----worm_clustering, echo=TRUE-----------------------------------------------
object <- worm_clustering(object)

## ----worm_evaluate, echo=TRUE-------------------------------------------------
object <- worm_evaluate(object)

## ----worm_visualize, echo=TRUE------------------------------------------------
object <- worm_visualize(object)

## ----pipe_operation, echo=TRUE------------------------------------------------
worm_download()$Ds |>
    as_worm_tensor() |>
        worm_membership(k=6) |>
            worm_clustering() |>
                worm_evaluate() |>
                    worm_visualize() -> object

## ----pipe_operation with Labels, echo=TRUE------------------------------------
# Sample Labels
worm_download()$Ds |>
    as_worm_tensor() |>
        worm_membership(k=6) |>
            worm_clustering() -> object
labels <- list(
    label1 = sample(3, length(object@clustering), replace=TRUE),
    label2 = sample(4, length(object@clustering), replace=TRUE),
    label3 = sample(5, length(object@clustering), replace=TRUE))
# WormTensor (with Labels)
worm_download()$Ds |>
    as_worm_tensor() |>
        worm_membership(k=6) |>
            worm_clustering() |>
                worm_evaluate(labels) |>
                    worm_visualize() -> object_labels

## ----worm_distance, echo=TRUE-------------------------------------------------
# Toy data (data of 3 animals)
n_cell_x <- 13
n_cell_y <- 24
n_cell_z <- 29
n_cells <- 30
n_time_frames <- 100

# animal_x : 13 cells, 100 time frames
animal_x <- matrix(runif(n_cell_x*n_time_frames),
    nrow=n_cell_x, ncol=n_time_frames)
rownames(animal_x) <- sample(seq(n_cells), n_cell_x)
colnames(animal_x) <- seq(n_time_frames)

# animal_y : 24 cells, 100 time frames
animal_y <- matrix(runif(n_cell_y*n_time_frames),
    nrow=n_cell_y, ncol=n_time_frames)
rownames(animal_y) <- sample(seq(n_cells), n_cell_y)
colnames(animal_y) <- seq(n_time_frames)

# animal_z : 29 cells, 100 time frames
animal_z <- matrix(runif(n_cell_z*n_time_frames),
    nrow=n_cell_z, ncol=n_time_frames)
rownames(animal_z) <- sample(seq(n_cells), n_cell_z)
colnames(animal_z) <- seq(n_time_frames)

# Input list for worm_distnce
X <- list(animal_x=animal_x,
    animal_y=animal_y,
    animal_z=animal_z)

# Pipe Operation
# tsne.perplexity must be adjusted for data size
worm_distance(X, "mSBD") |>
    as_worm_tensor() |>
        worm_membership(k=6) |>
            worm_clustering() |>
                worm_evaluate() |>
                    worm_visualize(tsne.perplexity=5) -> object

## ----sessionInfo, echo=FALSE--------------------------------------------------
sessionInfo()

