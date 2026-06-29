.dist2mat <- function(x) {
    as.matrix(x)
}

# Toy data
n_cell_x <- 13
n_cell_y <- 24
n_cell_z <- 29
n_cells <- 30
n_time_frames <- 100

# 13 cells, 100 time frames
animal_x <- matrix(runif(n_cell_x * n_time_frames),
    nrow = n_cell_x,
    ncol = n_time_frames
)
rownames(animal_x) <- sample(seq(n_cells), n_cell_x)
colnames(animal_x) <- seq(n_time_frames)

# 24 cells, 100 time frames
animal_y <- matrix(runif(n_cell_y * n_time_frames),
    nrow = n_cell_y,
    ncol = n_time_frames
)
rownames(animal_y) <- sample(seq(n_cells), n_cell_y)
colnames(animal_y) <- seq(n_time_frames)

# 29 cells, 100 time frames
animal_z <- matrix(runif(n_cell_z * n_time_frames),
    nrow = n_cell_z,
    ncol = n_time_frames
)
rownames(animal_z) <- sample(seq(n_cells), n_cell_z)
colnames(animal_z) <- seq(n_time_frames)

# Positive Control of Difference between SBD and mSBD
animal_z[2, ] <- -animal_x[1, ]

# Input list for worm_distnce
X <- list(
    animal_x = animal_x,
    animal_y = animal_y,
    animal_z = animal_z
)

############ Euclid ############
## dist object
Ds_Euclid <- worm_distance(X, "Euclid")
expect_true(object.size(Ds_Euclid) != 0)
expect_equal(length(Ds_Euclid), 3)
expect_true(all(lapply(Ds_Euclid, function(x) {is(x, "dist")})))

## matrix object
Ms_Euclid <- lapply(Ds_Euclid, .dist2mat)
Ms_Euclid |>
    lapply(isSymmetric) |>
    unlist() |>
    all() |>
    expect_true()
############# SBD #############
## dist object
Ds_SBD <- worm_distance(X, "SBD")
expect_true(object.size(Ds_SBD) != 0)
expect_equal(length(Ds_SBD), 3)
expect_true(all(lapply(Ds_SBD, function(x) {is(x, "dist")})))
## matrix object
Ms_SBD <- lapply(Ds_SBD, .dist2mat)
Ms_SBD |>
    lapply(isSymmetric) |>
    unlist() |>
    all() |>
    expect_true()
############# mSBD #############
## dist object
Ds_mSBD <- worm_distance(X, "mSBD")
expect_true(object.size(Ds_mSBD) != 0)
expect_equal(length(Ds_mSBD), 3)
expect_true(all(lapply(Ds_mSBD, function(x) {is(x, "dist")})))
## matrix object
Ms_mSBD <- lapply(Ds_mSBD, .dist2mat)
Ms_mSBD |>
    lapply(isSymmetric) |>
    unlist() |>
    all() |>
    expect_true()
##### Euclid, SBD, mSBD ######
expect_false(identical(Ds_Euclid, Ds_SBD))
expect_false(identical(Ds_Euclid, Ds_mSBD))
expect_false(identical(Ms_Euclid, Ms_SBD))
expect_false(identical(Ms_Euclid, Ms_mSBD))
