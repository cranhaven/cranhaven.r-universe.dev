# Generate typical data found inside the function mc()
S0 <-  0
y <-  rnorm(100)
statistic <-  function(y) mean(y)
dgp <-  function(y) sample(y, replace = TRUE)
N <-  99
type <-  "geq"
call <- call("mc",y= y, statistic = statistic, dgp = dgp, N = 99, type = type)
seed <-  NULL
pval <-  0.06


# Combine data and return it as an object of class 'mc'
MaxMC:::return_mc(S0 = S0, y = y, statistic = statistic, dgp = dgp,
                  N = N, type = type, call = call, seed = seed, pval= pval)
