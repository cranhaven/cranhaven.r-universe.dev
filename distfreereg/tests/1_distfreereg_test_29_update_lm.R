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
w <- 1/diag(Sig)
theta <- c(2,5,1)
X <- matrix(rexp(2*n, rate = 1), ncol = 2)
Y <- distfreereg:::f2ftheta(f = func, X)(theta) +
  as.vector(distfreereg:::rmvnorm(n = n, reps = 1, mean = rep(0,n), SqrtSigma = distfreereg:::matsqrt(Sig)))

form <- c ~ a + b
data <- data.frame(a = X[,1], b = X[,2], c = Y)
m <- lm(formula = form, data = data, weights = w)

set.seed(20240320)
dfr_0 <- distfreereg(test_mean = m, verbose = FALSE)


# set.seed(20240320)
# dfr_4a <- distfreereg(test_mean = m, verbose = FALSE,
#                       J = dfr_0[["J"]]^2)
# dfr_4b <- update(dfr_0, J = dfr_0[["J"]]^2)
# message('identical(dfr_4a[-c(1,7)], dfr_4b[-c(1,7)], ignore.environment = TRUE) (should be TRUE): ', identical(dfr_4a[-c(1,7)], dfr_4b[-c(1,7)], ignore.environment = TRUE))
# 
# set.seed(20240320)
# dfr_5a <- distfreereg(test_mean = m, verbose = FALSE,
#                       fitted_values = dfr_0[["fitted_values"]]^2)
# dfr_5b <- update(dfr_0, fitted_values = dfr_0[["fitted_values"]]^2)
# message('identical(dfr_5a[-c(1,7)], dfr_5b[-c(1,7)], ignore.environment = TRUE) (should be TRUE): ', identical(dfr_5a[-c(1,7)], dfr_5b[-c(1,7)], ignore.environment = TRUE))


set.seed(20240320)
dfr_6a <- distfreereg(test_mean = m, verbose = FALSE,
                      ordering = "natural")
dfr_6b <- update(dfr_0, ordering = "natural")
set.seed(20240320)
dfr_6c <- update(dfr_0, ordering = "natural")
message('identical(dfr_6a[-c(1,7)], dfr_6b[-c(1,7)], ignore.environment = TRUE) (should be FALSE): ', identical(dfr_6a[-c(1,7)], dfr_6b[-c(1,7)], ignore.environment = TRUE))
setequal(ident_list(dfr_6a, dfr_6c), c())# TRUE


set.seed(20240320)
dfr_7a <- distfreereg(test_mean = m, verbose = FALSE, B = 1e2)
dfr_7b <- update(dfr_0, B = 1e2)
set.seed(20240320)
dfr_7c <- update(dfr_0, B = 1e2)
message('identical(dfr_7a[-c(1,7)], dfr_7b[-c(1,7)], ignore.environment = TRUE) (should be FALSE): ', identical(dfr_7a[-c(1,7)], dfr_7b[-c(1,7)], ignore.environment = TRUE))
setequal(ident_list(dfr_7a, dfr_7c), c("call"))# TRUE


set.seed(20240320)
new_ordering <- sample(1:n)
set.seed(20240320)
dfr_8a <- distfreereg(test_mean = m, verbose = FALSE,
                      override = list(res_order = new_ordering))
dfr_8b <- update(dfr_0, override = list(res_order = new_ordering))
set.seed(20240320)
dfr_8c <- update(dfr_0, override = list(res_order = new_ordering))
message('identical(dfr_8a[-c(1,7)], dfr_8b[-c(1,7)], ignore.environment = TRUE) (should be FALSE): ', identical(dfr_8a[-c(1,7)], dfr_8b[-c(1,7)], ignore.environment = TRUE))
setequal(ident_list(dfr_8a, dfr_8c), c("call"))# TRUE


set.seed(20240320)
new_r <- dfr_0[["r"]][,3:1]
set.seed(20240320)
dfr_9a <- distfreereg(test_mean = m, verbose = FALSE,
                      override = list(r = new_r))
dfr_9b <- update(dfr_0, override = list(r = new_r))
set.seed(20240320)
dfr_9c <- update(dfr_0, override = list(r = new_r))
message('identical(dfr_9a[-c(1,7)], dfr_9b[-c(1,7)], ignore.environment = TRUE) (should be FALSE): ', identical(dfr_9a[-c(1,7)], dfr_9b[-c(1,7)], ignore.environment = TRUE))
setequal(ident_list(dfr_9a, dfr_9c), c("call"))# TRUE


set.seed(20240320)
new_mc <- lapply(dfr_0[["mcsim_stats"]], FUN = function(x) x^2)
set.seed(20240320)
dfr_10a <- distfreereg(test_mean = m, verbose = FALSE,
                       override = list(mcsim_stats = new_mc))
dfr_10b <- update(dfr_0, override = list(mcsim_stats = new_mc))
setequal(ident_list(dfr_10a, dfr_10b), c("call"))# TRUE


set.seed(20240320)
new_m <- lm(c ~ a + I(b^2), data = data, weights = w)
dfr_11a <- distfreereg(test_mean = new_m, verbose = FALSE)
set.seed(20240320)
dfr_11b <- update(dfr_0, test_mean = new_m)
setequal(ident_list(dfr_11a, dfr_11b), c("call", "test_mean.terms"))# TRUE


set.seed(20240320)
dfr_12a <- distfreereg(test_mean = m, verbose = FALSE,
                       stat = "KSmin")
dfr_12b <- update(dfr_0, stat = "KSmin")
set.seed(20240320)
dfr_12c <- update(dfr_0, stat = "KSmin")
message('identical(dfr_12a[-c(1,7,17,18)], dfr_12b[-c(1,7,17,18)], ignore.environment = TRUE) (should be FALSE): ', identical(dfr_12a[-c(1,7,17,18)], dfr_12b[-c(1,7,17,18)], ignore.environment = TRUE))
setequal(ident_list(dfr_12a, dfr_12c), c("call"))# TRUE
