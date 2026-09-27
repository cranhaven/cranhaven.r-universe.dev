library(iterors)

n <- 100
it <- ihasNext(icount(n))

total <- 0
while (hasNext(it))
  total <- total + nextOr(it)

print(total == sum(seq(length=n)))
