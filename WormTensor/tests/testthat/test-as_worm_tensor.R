# Pipe Operation
worm_download()$Ds |>
    as_worm_tensor() -> object

expect_true(length(object@dist_matrices) != 0)
expect_true(length(object@n_animals) != 0)
expect_true(length(object@union_cellnames) != 0)
expect_true(length(object@n_union_cells) != 0)
