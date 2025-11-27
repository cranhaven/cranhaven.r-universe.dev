library(graph)
V = letters[1:12]
g2 = randomEGraph(V, edges=20)

test_that("proper class argument is provided", {
  expect_error(rSpectral::spectral_graphNEL(V), ".*graphNEL.*")
  expect_named(rSpectral::spectral_graphNEL(g2),c('names','membership'))
})
