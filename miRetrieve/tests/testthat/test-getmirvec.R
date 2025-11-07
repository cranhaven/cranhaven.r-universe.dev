library(miRetrieve)
library(testthat)

vec1 <- c("miR-1", "miR-2", "miR-3")

vec2 <- c("miR-2", "miR-3", "miR-4")

shared <- get_shared_mir_vec(vec1, vec2)

test_that("Tests that shared miRNAs are received from vectors", {
    expect_type(shared, "character")
    expect_length(shared, 2)
})

distinct_mir <- get_distinct_mir_vec(vec1, vec2)

test_that("Tests that distinct miRNAs are received from vectors", {
    expect_type(distinct_mir, "character")
    expect_length(distinct_mir, 1)
})
