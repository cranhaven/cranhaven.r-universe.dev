paramEstimadosFinal=c(1,2,3,1,2,3,1,2,3)
names=c("Bact1", "Bact2","Bact3")
E=3

tat=TableFBM(paramEstimadosFinal,names,E)
cat(tat, sep = "\n")

test_that("TableFBM", {
  expect_type(tat, "character")
})
