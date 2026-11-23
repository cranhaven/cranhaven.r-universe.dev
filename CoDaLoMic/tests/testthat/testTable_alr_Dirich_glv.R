pam.ini=rbind(c(0.1,0.2,0.3),c(0.4,0.5,0.6))
paramEstimadosFinal=c(5, as.vector(pam.ini))
E=3
especie=cbind(c(0.2,0.4,0.4),c(0.1,0.1,0.8),c(0.5,0.1,0.4))
names=c("a","b","c")

tat=Table_alr_Dirich_glv(paramEstimadosFinal,especie,names,E)
cat(tat, sep = "\n")


test_that("Table_alr_Dirich_glv", {
  expect_type(tat, "character")
})
