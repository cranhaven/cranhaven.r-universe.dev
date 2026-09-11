data(valentesnsList)


# Figuring out which individuals have at least three observations
ids_3 <- table(valentesnsList$id)
ids_3 <- names(ids_3)[which(ids_3 >= 3)] |> as.integer()

valentesnsList_3 <- valentesnsList
ids_3 <- which(valentesnsList_3$id %in% ids_3)
valentesnsList_3$id <- valentesnsList_3$id[ids_3]
valentesnsList_3$Y <- valentesnsList_3$Y[ids_3,,drop=FALSE]
valentesnsList_3$X <- valentesnsList_3$X[ids_3,,drop=FALSE]

mymodel <- with(valentesnsList_3, {
new_defm(
  id = id,
  Y = Y,
  X = X,
  order = 2
)})

mymodel |>
  td_formula("{0y0} > {y0} > {0y0}")
  
motif_names <- names(mymodel)

expect_equal(
  motif_names, "Motif {alcohol-}>{alcohol+}>{alcohol-}"
)
