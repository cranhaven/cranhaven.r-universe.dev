test_that("An example for a real data model fitting", {
data(data)
data(adjacency_matrix)
result=GDILM_SEIRS_Par_Est(data,adjacency_matrix,2,2,2,0.5, 0.5, 1, 2, 1, 1, 1, 20, 2)
expect_type(result, "list")
})
