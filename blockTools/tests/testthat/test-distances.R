test_that("Euclidean distances are correctly calculated", {
  
  euclid <- round(as.matrix(dist(x100[, c("b1", "b2")])), 1)
  
  b.out <- block(x100, 
                 id.vars = "id", 
                 block.vars = c("b1", "b2"), 
                 distance = "euclidean")
  
  bl.dists <- round(b.out$blocks[[1]]["Distance"], 1)
  bl.ids <- createBlockIDs(b.out, data = x100, id.var = "id")
  
  euclid.dists <- NULL
  
  for(i in 1:(nrow(x100) / 2)){
    which.r <- which(bl.ids %in% i)[1]
    which.c <- which(bl.ids %in% i)[2]
    euclid.dists <- append(euclid.dists, euclid[which.r, which.c])
  }
  
  expect_identical(unlist(unname(bl.dists)), euclid.dists)
})