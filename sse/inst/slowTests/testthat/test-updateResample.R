context("Update, resampling")

library(sse)
library(testthat)

## n
n.l <- list(
    n0 = seq(from = 50, to = 100, by = 10),    # ..XXXX.., corse
    n1 = seq(from = 50, to = 100, by = 5),     # ..XXXX.., fine
    n2 = c(seq(from = 50, to = 80, by = 2),    # ..XXXX.., mixed
           seq(from = 85, to = 100, by = 5)),
    n3 = seq(from = 70, to = 100, by = 10),    # ....XX.., corse subset
    n4 = seq(from = 70, to = 150, by = 10),    # ..XXXXXX, corse extendet
    n5 = seq(from = 70, to = 150, by = 10),    # ....XXXX, corse subset, extended
    n6 = seq(from = 0, to = 150, by = 10),     # XXXXXXXX, corse subset, extended
    n7 = seq(from = 160, to = 200, by = 10),   # ........XX, corse
    n8.e = c(seq(from = 50, to = 80, by = 2),  # mixed with dublicated element
             seq(from = 80, to = 100, by = 5)),
    n9 = 80)

## theta
theta.l <- n.l
theta.l <- lapply(theta.l, function(x) {
  x / 100
})
names(theta.l) <- sub("n", "theta", names(theta.l))

## xi
xi.l <- list(
    xi0 = seq(from = 1, to = 3, by = 1),     # ..XXXX.., corse
    xi1 = seq(from = 1, to = 3, by = 0.5),   # ..XXXX.., fixie
    xi2 = c(seq(from = 1, to = 1, by = 1),   # ..XXXX.., mixed
            seq(from = 1.5, to = 3, by = 0.5)),
    xi3 = seq(from = 2, to = 3, by = 1),     # ....XX.., corse subset
    xi4 = seq(from = 1, to = 5, by = 1),     # ..XXXXXX, corse extendet
    xi5 = seq(from = 2, to = 5, by = 1),     # ....XXXX, corse subset, extended
    xi6 = seq(from = 0.5, to = 5, by = 0.5), # XXXXXXXX, corse subset, extended
    xi7 = seq(from = 4, to = 5, by = 1),     # ........XX, corse
    xi8.e = c(seq(from = 1, to = 2, by = 1), # mixed with dublicated element
            seq(from = 2, to = 5, by = 0.5)),
    xi9 = 2)                                 # length 1 and existing element


attach(n.l)
attach(theta.l)
attach(xi.l)



##
powFun0 <- function(psi) {
  n  <-  n(psi)
  theta  <-  theta(psi)
  xi <- xi(psi)
  return(as.logical( (n * theta * xi) %% 2))
}
powFun1 <- function(psi) {
  n  <-  n(psi)
  theta  <-  theta(psi)
  return(as.logical(n %/% 2))
}

psi0 <- powPar(n = n0,
               theta = theta0,
               xi = xi0)

calc0 <- powCalc(psi0, statistic = powFun0, n.iter = 99)

pow0 <- powEx(calc0, theta = 1, power = 0.9)

pow1.2.1.rf <- refine(pow0)



## correct result (cores) for  n, theta, xi with powFun0
res0 <- function(n, theta, xi){
  res.array <- array((n %*% t(theta)),
                     ## last dimension 1 because of powfun:
                     dim = c(length(n), length(theta), length(xi), 1))
  for (i in seq(along.with = xi)) {
    res.array[, , i, ] <- as.logical((res.array[, , i, ] * xi[i]) %% 2)
  }
  return(res.array)
}



## ------------------------------------------------------------------
## auto test
for (n.i in names(n.l)) {
  for (theta.i in names(theta.l)) {
    for (xi.i in names(xi.l)) {
      
      ## changing all n, theta and xi AND n.iter
      what <- paste0(
"update(calc0,
        n = ", n.i, ",
        theta = ", theta.i, ",
        xi = ", xi.i, ")@core")
      
      ## additionally changing the statistic also
      what2 <- paste0(
"update(calc0,
       n = ", n.i, ",
       theta = ", theta.i, ",
       xi = ", xi.i, ", statistic = powFun0)@core")
      
      ## additionally changing the n.iter also
      what3 <- paste0(
"update(calc0,
       n = ", n.i, ",
       theta = ", theta.i, ",
       xi = ", xi.i, ", n.iter = 111)@core")
      cat("\n---------------------------------\n")
      cat(what)
      cat("\n")
      res <- paste0("res0(", n.i, ", ", theta.i, ", ", xi.i, ")")
      if (grepl(".e", n.i, fixed = TRUE)
          | grepl(".e", theta.i, fixed = TRUE)
          | grepl(".e", xi.i, fixed = TRUE)) {
        ## expect an error
        expect_error(eval(parse(text = what)))
      } else {
        ## if any n0 theta0 or xi0: expect a warning if not 7
        if (n.i == "n0" & !(theta.i == "theta7" | xi.i == "xi7")
            | theta.i == "theta0" & !(n.i == "n7" | xi.i == "xi7")
            | xi.i == "xi0" & !(n.i == "n7" | theta.i == "theta7")) {
          expect_warning(eval(parse(text = what)))
        }
        ## should work
        eval(parse(text = paste0("expect_equal(",
                                 paste0("suppressWarnings(",
                                        what,
                                        ")"),
                                 ",",
                                 res,
                                 ", check.attributes = FALSE)")))

        ## should work but completely new evaluation
        cat("\n")
        cat(what2)
        cat("\n")
        eval(parse(text = paste0("expect_equal(",
                                 what2,
                                 ",",
                                 res,
                                 ", check.attributes = FALSE)")))
        
        ## should work but completely new iterations
        cat("\n")
        cat(what2)
        cat("\n")
        eval(parse(text = paste0("expect_equal(",
                                 what2,
                                 ",",
                                 res,
                                 ", check.attributes = FALSE)")))
      }
    }
  }
}

### ------------------------------------------------------------------





test_that("update without any changes", {
#
  expect_equal(update(calc0)@core, res0(n0, theta0, xi0), check.attributes = FALSE)
})



test_that("n.iter", {
  ## increasing
  calc0.500 <- update(calc0, n.iter = 500)
  expect_equal(calc0@core, res0(n0, theta0, xi0), check.attributes = FALSE)
  expect_equal(calc0.500@iter, 500)
  ## n.iter length > 1
  expect_warning(
      update(calc0, n.iter = c(500,100))
  )
## update of n.iter with value < than available
  expect_error(
      update(calc0, n.iter = 50)
  )
})



## test_that("update statistic only", {
## #
##   expect_equal(update(calc0, statistic = powFun2), result1.2.x, check.attributes = FALSE)
## })

