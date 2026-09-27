library(itertools)
library(foreach)

a <- array(1:24, c(2, 3, 4))

# Combine function that adds all of its arguments together
madd <- function(m1, ...) {
  for(m in list(...))
    m1 <- m1 + m
  m1
}

# Split the 3D array into matrices that are all added together
# by the specified combine function
XY.1 <-
  foreach(m=iarray(a, 3), .combine='madd') %do% {
    m
  }

# Split the 3D array into vectors that are individually summed
# and then combined into a matrix
XY.2 <-
  foreach(ita=iarray(a, c(1,2)), .combine='cbind') %:%
    foreach(x=ita, .combine='c') %do% {
      sum(x)
    }

# Compare the two results
print(all(XY.1 == XY.2))

# Split the 3D array into matrices that are individually summed
# and the combined into a vector
X.1 <-
  foreach(m=iarray(a, 1), .combine='c') %do% {
    sum(m)
  }

# Check the results in two ways
print(all(X.1 == rowSums(array(a, c(2, 12)))))
print(all(X.1 == rowSums(XY.1)))
