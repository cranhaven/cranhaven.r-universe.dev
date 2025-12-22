# this code was (partly) used to generate the objects based on which the
# tests below are conducted
if (FALSE) {
  n <- 100
  K <- 2
  cond_method <- rep("forest", 3)
  S <- 10
  gamma <- exp(seq(-4, 10, length.out = 20))
  DML <- "DML1"
  aN <- NULL
  do_regsDML <- TRUE
  do_safety <- FALSE
  do_regDML <- FALSE
  do_regDML_all_gamma <- FALSE
  do_DML <- do_regDML || do_regsDML || do_safety
  safety_factor <- 0.7
  params <- NULL
  level <- 0.95
  parallel <- "no"
  ncpus <- 1L
  cl <- NULL

  ####################
  # generate data
  ####################

  discrete_1 <- function(n, beta0 = 1) {
    W <- as.numeric(rnorm(n, 0, 1) >= 0)
    A <- cbind(W + 0.0005 * (rnorm(n, 0, 1) >= 1.5),
               W + 0.05 * rnorm(n, 0, 1),
               W + rnorm(n, 0, 1))
    H <- 1 * W + 0.25 * rnorm(n, 0, 1)
    X <- cbind(A[, 1] + W, W + 0.05 * rnorm(n, 0, 1), W + rnorm(n, 0, 1))
    Y <- X %*% rep(beta0, 3) + H - W + 0.25 * rnorm(n, 0, 1)
    return(list(aa = as.matrix(A), xx = as.matrix(X),
                ww = data.frame(w = W), yy = as.matrix(Y)))
  }

  discrete_2 <- function(n, beta0 = 1) {
    W <- cbind(as.numeric(rnorm(n) <= 0.2533471),
               as.numeric(rnorm(n) <= -0.2533471))
    A <- 1 * rnorm(n, 0, 1)
    H <- 1 * W[, 1] + 0.25 * rnorm(n, 0, 1)
    X <- W
    Y <- X %*% rep(beta0, ncol(X)) + H - tanh(W[, 2]) + 0.25 * rnorm(n, 0, 1)
    return(list(aa = as.matrix(A), xx = as.matrix(X),
                ww = data.frame(w = W), yy = as.matrix(Y)))
  }

  data_3 <- function(n, beta0 = 1) {
    W <- rnorm(n, 0, 1)
    A <- cbind(W, rnorm(n, 0, 1))
    index <- 50
    A[1:index, 1] <- rnorm(index, 0, 1)
    H <- rnorm(n, 0, 1)
    X <- W
    Y <- beta0 * X + H
    return(list(aa = as.matrix(A), xx = as.matrix(X),
                ww = data.frame(w = W), yy = as.matrix(Y)))
  }

  data_4 <- function(n, beta0 = 1) {
    W <- rnorm(n, 0, 1)
    A <- W
    index <- 1
    A[index] <- A[index] + 0.000001 * rnorm(length(index), 0, 1)
    H <- rnorm(n, 0, 1)
    X <- W
    Y <- beta0 * X + H
    return(list(aa = as.matrix(A), xx = as.matrix(X),
                ww = data.frame(w = W), yy = as.matrix(Y)))
  }

  ####################
  # save regsdml-results
  ####################

  set.seed(5)
  data <- discrete_1(n)
  a <- data$aa
  w <- data$ww
  x <- data$xx
  y <- data$yy
  data <- NULL
  res1 <- list(beta_all = beta_all, beta_all_unlist = beta_all_unlist,
               aa = a, ww = w, xx = x, yy = y,
               K = K, gamma = gamma,
               DML = DML, do_DML = do_DML, do_regsDML = do_regsDML,
               do_regDML = do_regDML,
               do_regDML_all_gamma = do_regDML_all_gamma,
               safety = safety, cond_method = cond_method,
               params = params, do_parallel = do_parallel,
               parallel = parallel, S = S, ncpus = ncpus, cl = cl,
               beta_all_new = NULL)
  save(res1, file = "res1.RData")

  set.seed(5)
  data <- discrete_2(n)
  a <- data$aa
  w <- data$ww
  x <- data$xx
  y <- data$yy
  data <- NULL
  res2 <- list(beta_all = beta_all, beta_all_unlist = beta_all_unlist,
               aa = a, ww = w, xx = x, yy = y,
               K = K, gamma = gamma,
               DML = DML, do_DML = do_DML, do_regsDML = do_regsDML,
               do_regDML = do_regDML,
               do_regDML_all_gamma = do_regDML_all_gamma,
               safety = safety, cond_method = cond_method,
               params = params, do_parallel = do_parallel,
               parallel = parallel, S = S, ncpus = ncpus, cl = cl,
               beta_all_new = NULL)
  save(res2, file = "res2.RData")


  set.seed(50)
  data <- data_3(n)
  a <- data$aa
  w <- data$ww
  x <- data$xx
  y <- data$yy
  data <- NULL
  cond_method <- rep("ols", 3)
  res3 <- list(beta_all = beta_all, beta_all_unlist = beta_all_unlist,
               aa = a, ww = w, xx = x, yy = y,
               K = K, gamma = gamma,
               DML = DML, do_DML = do_DML, do_regsDML = do_regsDML,
               do_regDML = do_regDML,
               do_regDML_all_gamma = do_regDML_all_gamma,
               safety = safety, cond_method = cond_method,
               params = params, do_parallel = do_parallel,
               parallel = parallel, S = S, ncpus = ncpus, cl = cl,
               beta_all_new = beta_all_new)
  save(res3, file = "res3.RData")

  set.seed(5)
  data <- data_4(n)
  a <- data$aa
  w <- data$ww
  x <- data$xx
  y <- data$yy
  data <- NULL
  DML <- "DML2"
  cond_method = rep("ols", 3)
  res4 <- list(beta_all = beta_all, beta_all_unlist = beta_all_unlist,
               aa = a, ww = w, xx = x, yy = y,
               K = K, gamma = gamma,
               DML = DML, do_DML = do_DML, do_regsDML = do_regsDML,
               do_regDML = do_regDML,
               do_regDML_all_gamma = do_regDML_all_gamma,
               safety = safety, cond_method = cond_method,
               params = params, do_parallel = do_parallel,
               parallel = parallel, S = S, ncpus = ncpus, cl = cl,
               beta_all_new = NULL)
  save(res4, file = "res4.RData")
}


####################
# perform tests
####################

test_that("error and warnings work with (partly) singular design", {
  load("res1.RData")
  return_W_E_res <-
    return_W_E_res_fun(beta_all = res1$beta_all, beta_all_unlist = res1$beta_all_unlist,
                       aa = res1$aa, ww = res1$ww, xx = res1$xx, yy = res1$yy,
                       K = res1$K, gamma = res1$gamma,
                       DML = res1$DML, do_DML = res1$do_DML, do_regsDML = res1$do_regsDML,
                       do_regDML = res1$do_regDML,
                       do_regDML_all_gamma = res1$do_regDML_all_gamma,
                       safety = res1$safety, cond_method = res1$cond_method,
                       params = res1$params, do_parallel = res1$do_parallel,
                       parallel = res1$parallel, S = res1$S, ncpus = res1$ncpus, cl = res1$cl,
                       beta_all_new = NULL)
  expect_null(return_W_E_res$error)
  expect_equal(return_W_E_res$warningMsgs,
               "The response has five or fewer unique values.  Are you sure you want to do regression?")

  load("res2.RData")
  return_W_E_res <-
    return_W_E_res_fun(beta_all = res2$beta_all, beta_all_unlist = res2$beta_all_unlist,
                       aa = res2$aa, ww = res2$ww, xx = res2$xx, yy = res2$yy,
                       K = res2$K, gamma = res2$gamma,
                       DML = res2$DML, do_DML = res2$do_DML, do_regsDML = res2$do_regsDML,
                       do_regDML = res2$do_regDML,
                       do_regDML_all_gamma = res2$do_regDML_all_gamma,
                       safety = res2$safety, cond_method = res2$cond_method,
                       params = res2$params, do_parallel = res2$do_parallel,
                       parallel = res2$parallel, S = res2$S, ncpus = res2$ncpus, cl = res2$cl,
                       beta_all_new = NULL)
  expect_equal(return_W_E_res$errors,
               "Essentially perfect fit, not enough non-NA results among the S repetitions.\nSingularity occured in: DML\nPlease rerun regsdml without it or try using 'DML = DML2'.")
  expect_null(return_W_E_res$warningMsgs)

  load("res3.RData")
  return_W_E_res <-
    return_W_E_res_fun(beta_all = res3$beta_all, beta_all_unlist = res3$beta_all_unlist,
                       aa = res3$aa, ww = res3$ww, xx = res3$xx, yy = res3$yy,
                       K = res3$K, gamma = res3$gamma,
                       DML = res3$DML, do_DML = res3$do_DML, do_regsDML = res3$do_regsDML,
                       do_regDML = res3$do_regDML,
                       do_regDML_all_gamma = res3$do_regDML_all_gamma,
                       safety = res3$safety, cond_method = res3$cond_method,
                       params = res3$params, do_parallel = res3$do_parallel,
                       parallel = res3$parallel, S = res3$S, ncpus = res3$ncpus, cl = res3$cl,
                       beta_all_new = res3$beta_all_new)
  expect_equal(return_W_E_res$warningMsgs,
               "Essentially perfect fit: do S more repetitions.")
  expect_null(return_W_E_res$errors)

  load("res4.RData")
  return_W_E_res <-
    return_W_E_res_fun(beta_all = res4$beta_all, beta_all_unlist = res4$beta_all_unlist,
                       aa = res4$aa, ww = res4$ww, xx = res4$xx, yy = res4$yy,
                       K = res4$K, gamma = res4$gamma,
                       DML = res4$DML, do_DML = res4$do_DML, do_regsDML = res4$do_regsDML,
                       do_regDML = res4$do_regDML,
                       do_regDML_all_gamma = res4$do_regDML_all_gamma,
                       safety = res4$safety, cond_method = res4$cond_method,
                       params = res4$params, do_parallel = res4$do_parallel,
                       parallel = res4$parallel, S = res4$S, ncpus = res4$ncpus, cl = res4$cl,
                       beta_all_new = NULL)
  expect_equal(return_W_E_res$warningMsgs, "Essentially perfect fit: DML summary may be unreliable.")
  expect_null(return_W_E_res$errors)
})
