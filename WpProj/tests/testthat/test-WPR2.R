wpr2.data <- function(n, p, s) {
  x <- matrix( rnorm( p * n ), nrow = n, ncol = p )
  x_ <- t(x)
  beta <- (1:p)/p
  y <- x %*% beta + rnorm(n)
  post_beta <- matrix(beta, nrow=p, ncol=s) + rnorm(p*s, 0, 0.1)
  post_mu <- x %*% post_beta
  transp <- "exact"
  model.size <- c(2,4,8)
  
  test <- WpProj(X = x, eta = post_mu, theta = post_beta, method = "binary program",
                 solver = "ecos")
  
  proj <- WpProj(x, post_mu, post_beta)
  sel <- WpProj(x, post_mu, post_beta, method = "binary program")
  
  out <- list(test, proj, sel)
  dist <- distCompare(out, list(parameters = post_beta, predictions = post_mu), power = 2, quantity = c("parameters", "predictions"))
  # if(sum(grepl("dist", colnames(dist$predictions)))>1) browser()
  return(dist)
}

wpr2.prep <- function(n, p, s) {
  out <- wpr2.data(n,p,s)
  
  
  r2 <- WpProj:::WPR2.distcompare(predictions = NULL, projected_model = out, power = 2, method = "exact")
  return(r2)
}

test_that("WPR2 works", {
  set.seed(203402)
  
  n <- 32
  p <- 10
  s <- 21
  
  x <- matrix( stats::rnorm( p * n ), nrow = n, ncol = p )
  x_ <- t(x)
  beta <- (1:p)/p
  y <- x %*% beta + stats::rnorm(n)
  post_beta <- matrix(beta, nrow=p, ncol=s) + stats::rnorm(p*s, 0, 0.1)
  post_mu <- x %*% post_beta
  transp <- "exact"
  model.size <- c(2,4,8)
  
  test <- WpProj::WpProj(X = x, eta =  post_mu, theta = post_beta, method = "binary program",
                         solver = "ecos")
  
  proj <- WpProj:::WpProj(x, post_mu, post_beta)
  sel <- WpProj:::WpProj(x, post_mu, post_beta, method = "binary program")
  
  out <- list(test, proj, sel)
  
  
  dist <- WpProj:::distCompare(out, list(parameters = post_beta, predictions = post_mu), power = 2, quantity = c("parameters", "predictions"))
  
  r2 <- WpProj:::WPR2.distcompare(predictions = post_mu, projected_model = dist, power = 2, method = "exact")
  r2 <- WpProj:::WPR2.distcompare(predictions = NULL, projected_model = dist, power = 2, method = "exact")
  
  maxes <- tapply(dist$predictions$dist, dist$predictions$groups, max)
  r2_check <- 1 - dist$predictions$dist^2/maxes[as.numeric(dist$predictions$groups)]^2
    
  r2_mat <- WpProj:::WPR2.matrix(post_mu, test$fitted.values[[1]], p = 2, method  ="exact")
  r2_mat_check <- 1 - (WpProj::wasserstein(t(post_mu), t(test$fitted.values[[1]]),
                                         p = 2, ground_p = 2,
                                         method = "exact", 
                                         observation.orientation = "colwise")^2/
    WpProj::wasserstein(t(post_mu), 
                       t(matrix(colMeans(post_mu), nrow(post_mu),
                          ncol(post_mu), byrow=TRUE)),
                       p = 2, ground_p = 2,
                       method = "exact", 
                       observation.orientation = "colwise")^2) 
  
  testthat::expect_silent(r2_wpproj <- WpProj:::WPR2.list(post_mu, out, p = 2, method  ="exact"))
  testthat::expect_silent(r2_wpproj <- WpProj:::WPR2(post_mu, out, p = 2, method  ="exact"))
  
  names(out) <- c("BP", "L2", "relaxed bp")
  out$BP$fitted.values <- out$BP$fitted.values
  out$L2$fitted.values <- out$L2$fitted.values
  out$`relaxed bp`$fitted.values <- out$`relaxed bp`$fitted.values
  
  r2_wpproj <- WpProj:::WPR2(post_mu, out, p = 2, method  ="exact")
  r2_wpproj_check <- 1 - (WpProj::wasserstein(post_mu, proj$fitted.values[[1]],
                                            p = 2, ground_p = 2,
                                            method = "exact", 
                                            observation.orientation = "colwise")^2/
                           WpProj::wasserstein(post_mu, 
                                              matrix(colMeans(post_mu), nrow(post_mu),
                                                     ncol(post_mu), byrow=TRUE),
                                              p = 2, ground_p = 2,
                                              method = "exact", 
                                              observation.orientation = "colwise")^2)
  
  testthat::expect_equivalent(r2$r2, r2_check)
  testthat::expect_equivalent(r2_mat[1,1], r2_mat_check)
  testthat::expect_equivalent(r2_wpproj$r2[r2_wpproj$groups == "L2"][1], r2_wpproj_check, )
})

testthat::test_that("WPR2 combining works", {
  set.seed(203402)
  
  n <- 32
  p <- 10
  s <- 21
  
  out1 <- wpr2.prep(n,p,s)
  out2 <- wpr2.prep(n,p,s)
  # debugonce(distCompare)
  
  comb <- combine.WPR2(out1,out2)
  comb2 <- combine.WPR2(list(out1,out2))
  
  testthat::expect_equal(comb, comb2)
  
})

testthat::test_that("WPR2 plotting works", {
  set.seed(203402)
  
  n <- 64
  p <- 10
  s <- 50
  reps <- 3
  out <- lapply(1:reps, function(i) wpr2.prep(n,p,s))
  # debugonce(combine.WPR2)
  comb <- combine.WPR2(out)
  # debugonce(plot.WPR2)
  p <- plot(comb)
  testthat::expect_true(ggplot2::is.ggplot(p))
})
