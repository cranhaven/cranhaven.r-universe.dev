fuzzywilcox <-
function(x, y) {

  ##require("fuzzyRankTests")
  fuzzytestobj <- fuzzyRankTests::fuzzy.ranksum.test(x[y==levels(y)[1]], x[y==levels(y)[2]])

  u <- runif(1)
  lowerind <- max(which((u-fuzzytestobj$values)>0))

  randompval <- runif(1, min=fuzzytestobj$knots[lowerind], max=fuzzytestobj$knots[lowerind+1])

  return(randompval)

}
