data(karate,package='igraphdata')
exp_mem10<-c(5,5,5,5,4,4,4,5,1,2,4,6,5,5,1,1,4,5,1,5,1,5,1,3,3,3,1,3,3,1,1,3,1,1)
exp_mod10<- 0.4078731
exp_mem11<-c(4,2,5,5,4,4,4,5,1,1,4,4,5,5,1,1,4,2,1,2,1,2,1,3,3,3,1,3,3,1,1,3,1,1)
exp_mod11<- 0.3776298
test_that("proper class argument is provided", {
  expect_error(rSpectral::spectral_igraph_membership('karate'),'.*igraph.*')
  expect_error(rSpectral::spectral_igraph_communities('karate'),'.*igraph.*')
  expect_named(rSpectral::spectral_igraph_membership(karate),
               c('names','membership'))
  expect_named(rSpectral::spectral_igraph_communities(karate),
               c('vcount', 'algorithm', 'membership', 'modularity', 'names'), 
               ignore.order = TRUE)
})

test_that('membership fix_neig=0 is correct',{
  c<-rSpectral::spectral_igraph_communities(karate)
  expect_equal(igraph::compare(c$membership,exp_mem10,'rand'),1)
  expect_equal(c$modularity,exp_mod10,tolerance=1e-5)
  expect_equal(c$algorithm,'spectral')
})

test_that('membership fix_neig=1 is correct',{
  c<-rSpectral::spectral_igraph_communities(karate,fix_neig = 1)
  expect_equal(igraph::compare(c$membership,exp_mem11,'rand'),1)
  expect_equal(c$modularity,exp_mod11,tolerance=1e-5)
  expect_equal(c$algorithm,'spectral')
})