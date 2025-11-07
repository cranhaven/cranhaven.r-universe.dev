quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

create_distances <- function(groupSize, groupNumber, groupDistance) {
  N <- groupSize * groupNumber
  m <- matrix(runif(N*N), nrow=N, ncol=N)
  for(i in 1:N) {
    for(j in 1:i) {
      #diagonal
      if(i==j) {
        m[i,j] <- 0
      #within the same group
      } else if((i-1)%/%groupSize == (j-1)%/%groupSize) {
        m[i,j] <- runif(1)
        m[j,i] <- m[i,j]
      #outside the group
      } else {
        m[i,j] <- groupDistance + runif(1);
        m[j,i] <- m[i,j]
      }
    }
  }
  return(m)
}


create_special_distances <- function(groupSize, groupNumber, groupDistance) {
  N <- groupSize * groupNumber
  m <- matrix(runif(N*N), nrow=N, ncol=N)
  for(i in 1:N) {
    for(j in 1:N) {
      m[i,j] <- NA
    }
  }
  c <- 0
  for(i in 1:N) {
    for(j in 1:i) {
      #diagonal
      if(i==j) {
        m[i,j] <- 0
      #related to first group
      } else if(i<=groupSize) {
        #within the first group
        #related to first element
        if(j==1) {
          m[i,j] <- groupDistance
        #otherwise
        } else {
          m[i,j] <- (2 + c*runif(1)) * groupDistance
        }
        m[j,i] <- m[i,j]
      #also related to first group
      } else if(j<=groupSize) {
        if((i-1)%%groupSize == 1 & (j-1)%%groupSize == 1) {
          m[i,j] <- (12 + c*runif(1)) * groupDistance;
        } else {
          m[i,j] <- (10 + c*runif(1)) * groupDistance
        }
        m[j,i] <- m[i,j]
      #within same group other then first
      } else if((i-1)%/%groupSize == (j-1)%/%groupSize) {
        #related to first element
        if(((j-1)%%groupSize)==0) {
          m[i,j] <- (1 + c*runif(1)) * groupDistance
        #related to second element
        } else if(((j-1)%%groupSize)==1) {
          m[i,j] <- (5 + c*runif(1)) *groupDistance
        #otherwise
        } else {
          m[i,j] <- (2 + c*runif(1)) * groupDistance
        }
        m[j,i] <- m[i,j]
      #otherwise
      } else {
        if((i-1)%%groupSize == 1 & (j-1)%%groupSize == 1) {
          m[i,j] <- (12 + c*runif(1)) * groupDistance;
        } else {
          m[i,j] <- (10 + c*runif(1)) * groupDistance;
        }
        m[j,i] <- m[i,j]
      }
    }
  }
  return(m)
}

test_that("size works for constructed distanceMatrices", {
  for(nGroup in 5:10) {
    for(nGroups in 5:10) {
      m <- create_distances(nGroup, nGroups, 3.0)
      dm <- as.dist(m, upper=FALSE)
      cc <- quiet(CoreCollection(dm, nGroups))
      expect_equal(nrow(cc$core), nGroups)
    }
  }
})

test_that("seed works", {
  skip_on_cran()
  nGroup <- 5
  nGroups <- 10
  seed <- 1234567
  m <- create_distances(nGroup, nGroups, 3.0)
  dm <- as.dist(m, upper=FALSE)
  n <- nGroup * nGroups
  comparisonWithoutSeed = TRUE
  #compute cores of multiple sizes
  for(i in (1+1):(n-1)) {
    cc1 <- quiet(CoreCollection(dm, i, seed=seed))
    cc2 <- quiet(CoreCollection(dm, i, seed=seed))
    cc3 <- quiet(CoreCollection(dm, i))
    expect_identical(cc1$core, cc2$core)
    comparisonWithoutSeed <- comparisonWithoutSeed & identical(cc1$core, cc3$core)
  }
  #but this should (almost certainly) be false
  expect_false(comparisonWithoutSeed)
})

 test_that("preselected set works",{
   testthat::skip_on_cran()
   nGroup <- 5
   nGroups <- 10
   n <- nGroup * nGroups
   m <- create_distances(nGroup, nGroups, 3.0)
   dm <- as.dist(m, upper=FALSE)
   for(s in (nGroup+1):(n-1)) {
     preselected <- sort(as.character(c((s-nGroup):(s-1))))
     cc <- quiet(CoreCollection(dm, s, preselected))
     #check set of preselected
     preselected2 <- sort(cc$preselected)
     preselected3 <- sort(as.character(rownames(cc$core))[(cc$core)$preselected == TRUE])
     expect_equal(preselected, preselected2)
     expect_equal(preselected, preselected3)
   }
 })

test_that("method a-ne works", {
  nGroup <- 4
  nGroups <- 3
  n <- nGroup * nGroups
  m <- create_special_distances(nGroup, nGroups, 3.0)
  dm <- as.dist(m, upper=FALSE)
  cc <- quiet(CoreCollection(dm, nGroups, coreSelectMethod = "A-NE"))
  expect_equal(rownames(cc$core), as.character(seq(1,nGroup*nGroups,nGroup)))
})

test_that("method e-ne works", {
  nGroup <- 4
  nGroups <- 3
  n <- nGroup * nGroups
  m <- create_special_distances(nGroup, nGroups, 3.0)
  dm <- as.dist(m, upper=FALSE)
  cc <- quiet(CoreCollection(dm, nGroups, coreSelectMethod = "E-NE"))
  expect_equal(sort(rownames(cc$core)), sort(as.character(seq(2,nGroup*nGroups,nGroup))))
})

test_that("method e-e works", {
  #actually testing e-ne...
  nGroup <- 4
  nGroups <- 3
  n <- nGroup * nGroups
  m <- create_special_distances(nGroup, nGroups, 3.0)
  dm <- as.dist(m, upper=FALSE)
  cc <- quiet(CoreCollection(dm, nGroups, coreSelectMethod = "E-E"))
  expect_equal(sort(rownames(cc$core)), sort(as.character(seq(2,nGroup*nGroups,nGroup))))
})
