skip_if_not_installed("ggplot2")
require(ggplot2)

sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

suppressMessages(attach(sdf))

test_that("density plot with facets and PVs",{
  p <- sdf %>% 
       ggplot(.,aes(x=composite,weight=origwt)) +
       geom_density() +
       facet_wrap(vars(sdracem))
  expect_equal(class(p),c("gg","ggplot"))  
})

test_that("boxplot with facets and PVs",{
  p <- sdf %>% 
    ggplot(.,aes(x=composite,weight=origwt)) +
    geom_boxplot() +
    facet_wrap(vars(b003501))
  expect_equal(class(p),c("gg","ggplot"))  
})

