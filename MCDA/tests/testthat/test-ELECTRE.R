#library(MCDA)
test_that("ELCTRE3 works", {
  scores <- matrix( c(-0.2,-2.3,-2.4,-1,3,9,10,7), nrow = 4, 
                    dimnames=list(c("School-A","School-B","School-C","School-D"),
                                  c("Location","Quality")))
  q <- c( 0.2, 1)
  p <- c(   1, 2)
  v <- c( 3.5, 4)
  w <- c(0.25, 0.75)
  
  res <- ELECTRE3(scores, q, p, v, w)
  expect_equal(res$concordance, 
               matrix(c(1.00,0.25,0.25,0.25,
                        0.75,1.00,1.00,0.75,
                        0.75,1.00,1.00,0.75,
                        0.8125,0.25,0.25,1.00), nrow=4, ncol=4, byrow=TRUE),
               ignore_attr=TRUE)
  expect_equal(sum(res$discordance), 4.7, ignore_attr=TRUE)
  expect_equal(sum(res$credibility), 10.2291667, ignore_attr=TRUE)
  expect_equal(res$dominance, 
               matrix(c(1,0,0,0,
                        1,1,1,1,
                        1,1,1,1,
                        1,0,0,1), nrow=4, ncol=4, byrow=TRUE), 
               ignore_attr=TRUE)
  expect_equal(res$scoring, c(-3,2,2,-1))
})

