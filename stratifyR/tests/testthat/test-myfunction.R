#Generate RT data
set.seed(12546)
data <- rtriangle(n=1000, a=2, b=8, c=2) #right-triangular since a=c
hist(data)
res <- strata.data(data, h = 2, n=500) # a 2-strata solution
summary(res)