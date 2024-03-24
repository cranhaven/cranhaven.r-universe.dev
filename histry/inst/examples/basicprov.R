library(histry)
library(roprov)
set.seed(0)
x = 5L
l = 5
y = rnorm(x)
w = 10
z = y + w
k = l + mtcars$wt
provstore = histryProvDF()
gr = fullprovgraph(provstore)
