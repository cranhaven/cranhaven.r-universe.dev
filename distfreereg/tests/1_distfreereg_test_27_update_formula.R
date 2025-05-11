ident_list <- function(x, y){
  stopifnot(is.list(x), is.list(y), length(x) == length(y))
  unx <- unlist(x)
  uny <- unlist(y)
  stopifnot(length(unx) == length(uny))
  output <- logical(length(unx))
  names(output) <- names(unx)
  for(i in 1:length(unx)){
    output[i] <- identical(unx[[i]], uny[[i]],
                           ignore.environment = TRUE,
                           ignore.bytecode = TRUE)
  }
  return(names(output)[which(!output)])
}

library(distfreereg)
set.seed(20240320)
n <- 1e2
func <- function(x, theta) theta[1] + theta[2]*x[1] + theta[3]*x[2]
Sig <- diag(rexp(n))
theta <- c(2,5,1)
X <- matrix(rexp(2*n, rate = 1), ncol = 2)
Y <- distfreereg:::f2ftheta(f = func, X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))

form <- c ~ a + b
data <- data.frame(a = X[,1], b = X[,2], c = Y)

set.seed(20240320)
dfr_0 <- distfreereg(data = data, test_mean = form, covariance = list(Sigma = Sig),
                     verbose = FALSE, ordering = "natural")


set.seed(20240320)
new_data1 <- data
new_data1[["c"]] <- data[["c"]]^2
dfr_1a <- distfreereg(data = new_data1, test_mean = form, covariance = list(Sigma = Sig),
                      verbose = FALSE, ordering = "natural")
set.seed(20240320)
dfr_1b <- update(dfr_0, data = new_data1)
setequal(ident_list(dfr_1a, dfr_1b), c("call", "test_mean", "model.call", "model.terms"))# TRUE


set.seed(20240320)
new_data2 <- data
new_data2[["b"]] <- data[["b"]]^2
dfr_2a <- distfreereg(data = new_data2, test_mean = form, covariance = list(Sigma = Sig),
                      verbose = FALSE, ordering = "natural")
set.seed(20240320)
dfr_2b <- update(dfr_0, data = new_data2)
setequal(ident_list(dfr_2a, dfr_2b), c("call", "test_mean", "model.call", "model.terms"))# TRUE


set.seed(20240320)
dfr_3a <- distfreereg(data = data, test_mean = form, covariance = list(Sigma = Sig %*% Sig),
                      verbose = FALSE, ordering = "natural")
dfr_3b <- update(dfr_0, covariance = list(Sigma = Sig %*% Sig))
setequal(ident_list(dfr_3a, dfr_3b), c("call", "test_mean", "model.call", "model.terms"))# TRUE


# set.seed(20240320)
# dfr_4a <- distfreereg(data = data, test_mean = form, covariance = list(Sigma = Sig),
#                       verbose = FALSE, J = dfr_0[["J"]]^2, ordering = "natural")
# dfr_4b <- update(dfr_0, J = dfr_0[["J"]]^2)
# setequal(ident_list(dfr_4a, dfr_4b), c("call", "test_mean", "model.call", "model.terms"))# TRUE
# 
# 
# set.seed(20240320)
# dfr_5a <- distfreereg(data = data, test_mean = form, covariance = list(Sigma = Sig),
#                       verbose = FALSE, fitted_values = dfr_0[["fitted_values"]]^2, ordering = "natural")
# dfr_5b <- update(dfr_0, fitted_values = dfr_0[["fitted_values"]]^2)
# setequal(ident_list(dfr_5a, dfr_5b), c("call", "test_mean", "model.call", "model.terms"))# TRUE


set.seed(20240320)
dfr_6a <- distfreereg(data = data, test_mean = form, covariance = list(Sigma = Sig),
                      verbose = FALSE, ordering = "natural")
dfr_6b <- update(dfr_0, ordering = "natural")
set.seed(20240320)
dfr_6c <- update(dfr_0, ordering = "natural")
message('identical(dfr_6a[["epsp"]], dfr_6b[["epsp"]]) (should be TRUE): ', identical(dfr_6a[["epsp"]], dfr_6b[["epsp"]]))
message('identical(dfr_6a[["mcsim_stats"]], dfr_6b[["mcsim_stats"]]) (should be FALSE): ', identical(dfr_6a[["mcsim_stats"]], dfr_6b[["mcsim_stats"]]))
setequal(ident_list(dfr_6a, dfr_6c), c("test_mean", "model.call", "model.terms"))# TRUE


set.seed(20240320)
dfr_7a <- distfreereg(data = data, test_mean = form, covariance = list(Sigma = Sig),
                      verbose = FALSE, B = 1e2, ordering = "natural")
dfr_7b <- update(dfr_0, B = 1e2)
set.seed(20240320)
dfr_7c <- update(dfr_0, B = 1e2)
message('identical(dfr_7a[["epsp"]], dfr_7b[["epsp"]]) (should be TRUE): ', identical(dfr_7a[["epsp"]], dfr_7b[["epsp"]]))
message('identical(dfr_7a[["mcsim_stats"]], dfr_7b[["mcsim_stats"]]) (should be FALSE): ', identical(dfr_7a[["mcsim_stats"]], dfr_7b[["mcsim_stats"]]))
setequal(ident_list(dfr_7a, dfr_7c), c("call", "test_mean", "model.call", "model.terms"))# TRUE


set.seed(20240320)
new_ordering <- sample(1:n)
set.seed(20240320)
dfr_8a <- distfreereg(data = data, test_mean = form, covariance = list(Sigma = Sig),
                      verbose = FALSE, override = list(res_order = new_ordering))
dfr_8b <- update(dfr_0, override = list(res_order = new_ordering))
set.seed(20240320)
dfr_8c <- update(dfr_0, override = list(res_order = new_ordering))
message('identical(dfr_8a[["epsp"]], dfr_8b[["epsp"]]) (should be TRUE): ', identical(dfr_8a[["epsp"]], dfr_8b[["epsp"]]))
message('identical(dfr_8a[["mcsim_stats"]], dfr_8b[["mcsim_stats"]]) (should be FALSE): ', identical(dfr_8a[["mcsim_stats"]], dfr_8b[["mcsim_stats"]]))
setequal(ident_list(dfr_8a, dfr_8c), c("call", "test_mean", "model.call", "model.terms"))# TRUE


set.seed(20240320)
new_r <- dfr_0[["r"]][,3:1]
set.seed(20240320)
dfr_9a <- distfreereg(data = data, test_mean = form, covariance = list(Sigma = Sig),
                      verbose = FALSE, override = list(r = new_r), ordering = "natural")
dfr_9b <- update(dfr_0, override = list(r = new_r))
set.seed(20240320)
dfr_9c <- update(dfr_0, override = list(r = new_r))
message('identical(dfr_9a[["epsp"]], dfr_9b[["epsp"]]) (should be TRUE): ', identical(dfr_9a[["epsp"]], dfr_9b[["epsp"]]))
message('identical(dfr_9a[["mcsim_stats"]], dfr_9b[["mcsim_stats"]]) (should be FALSE): ', identical(dfr_9a[["mcsim_stats"]], dfr_9b[["mcsim_stats"]]))
setequal(ident_list(dfr_9a, dfr_9c), c("call", "test_mean", "model.call", "model.terms"))# TRUE


set.seed(20240320)
new_mc <- lapply(dfr_0[["mcsim_stats"]], FUN = function(x) x^2)
set.seed(20240320)
dfr_10a <- distfreereg(data = data, test_mean = form, covariance = list(Sigma = Sig),
                       verbose = FALSE, override = list(mcsim_stats = new_mc), ordering = "natural")
dfr_10b <- update(dfr_0, override = list(mcsim_stats = new_mc))
setequal(ident_list(dfr_10a, dfr_10b), c("call", "test_mean", "model.call", "model.terms"))# TRUE


set.seed(20240320)
new_form <- c ~ a + I(b^2)
set.seed(20240320)
dfr_11a <- distfreereg(data = data, test_mean = new_form, covariance = list(Sigma = Sig),
                       verbose = FALSE, ordering = "natural")
set.seed(20240320)
dfr_11b <- update(dfr_0, test_mean = new_form)
setequal(ident_list(dfr_11a, dfr_11b), c("call", "test_mean", "model.call", "model.terms"))# TRUE


set.seed(20240320)
dfr_12a <- distfreereg(data = data, test_mean = form, covariance = list(Sigma = Sig),
                       verbose = FALSE, stat = "KSmin", ordering = "natural")
dfr_12b <- update(dfr_0, stat = "KSmin")
set.seed(20240320)
dfr_12c <- update(dfr_0, stat = "KSmin")
message('identical(dfr_12a[["mcsim_stats"]], dfr_12b[["mcsim_stats"]]) (should be FALSE): ', identical(dfr_12a[["mcsim_stats"]], dfr_12b[["mcsim_stats"]]))
setequal(ident_list(dfr_12a, dfr_12c), c("call", "test_mean", "model.call", "model.terms"))# TRUE
