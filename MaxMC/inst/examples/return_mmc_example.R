# Generate typical data found inside the function mmc()
S0 <-  0
y <-  rnorm(100)
statistic <-  function(y, v) mean(y) * v
dgp <-  function(y, v) sample(y, replace = TRUE)
est <- 1
lower <- -1
upper <- 1
N <-  99
type <-  "geq"
method <- "GenSA"
alpha <- 0.05
control <- MaxMC:::get_control(method)
call <- call("mc",y= y, statistic = statistic, dgp = dgp, N = 99, type = type)
seed <-  NULL
lmc <- list(pval = 0.10)
pval <-  0.65
opt_result <- list(value = 0.10)
opt_trace  <- as.data.frame(matrix(data = NA, control$maxit, 3,
                                   dimnames = list(NULL,c("ite","pval","max"))))

# Combine data and return it as an object of class 'mmc'
MaxMC:::return_mmc(S0 = S0, y = y, statistic = statistic, dgp = dgp, est = est, lower = lower,
                   upper = upper, N = N, type = type, method = method, alpha = alpha,
                   control = control, call = call, seed = seed, lmc = lmc,
                   opt_result = opt_result, opt_trace = opt_trace)
