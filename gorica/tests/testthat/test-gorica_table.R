#Where to put data for automated tests with testthat? and use inst/testdata, then access the files with system.file("testdata",...,package="gorica"
#f <- list.files("tests/testthat/files/Examples", pattern = ".txt", recursive = T, full.names = T)
#file.rename(f, file.path(getwd(), "inst", "testdata", gsub("^.+?(\\d+)/(.+?)\\.txt$", "\\2_\\1\\.txt", f)))
#



tab <- structure(c(933L, 661L, 402L, 260L, 51L, 44L, 26L, 26L), .Dim = c(2L,
                                                                         4L), class = c("table", "matrix", "array"))
res <- gorica(tab, hypothesis = "a:=x[2,1]/(x[1,1]+x[2,1]);b:=x[2,2]/(x[1,2]+x[2,2]);c:=x[2,3]/(x[1,3]+x[2,3]);d:=x[2,4]/(x[1,4]+x[2,4]);a > (b,c,d); a = b & c > d;a >b & b > c & c > d")

test_that("Estimates close", expect_equivalent(res$estimates, c(0.414845917860363, 0.392453007752678, 0.460725801331216, 0.49971752339389
), tolerance = .03))

test_that("Weights close", expect_equivalent(res$fit$gorica_weights, c(H1 = 0.240105532495478, H2 = 0.416619002783427, H3 = 0.179049636489488,
                                                                       H4 = 0.164225828231607), tolerance = .05))



# Example 2 ---------------------------------------------------------------

tab <- structure(c(5L, 1L, 4L, 1L), .Dim = c(2L, 2L), class = c("table",
                                                                "matrix", "array"))
res <- gorica(tab, hypothesis = "x[1,1]=x[1,2] & x[2,1]>x[2,2];x[1,1]>x[1,2] & x[2,1]>x[2,2]")

test_that("Estimates close", expect_equivalent(res$estimates, c(0.449545454545455, 0.0935454545454545, 0.361909090909091, 0.095
), tolerance = .03))


test_that("Weights close", expect_equivalent(res$fit$gorica_weights, c(H1 = 0.529853691907202, H2 = 0.344021796389839, H3 = 0.126124511702959
), tolerance = .03))


# Example 3 ---------------------------------------------------------------

tab <- structure(c(5L, 1L, 3L, 0L), .Dim = c(2L, 2L), class = c("table",
                                                                "matrix", "array"))

res <- gorica(tab, hypothesis = "x[1,1]=x[1,2] & x[2,1]>x[2,2];x[1,1]>x[1,2] & x[2,1]>x[2,2]")

test_that("Estimates close", expect_equivalent(res$estimates, c(0.553555555555556, 0.110111111111111, 0.336333333333333, 0), tolerance = .03))

test_that("Weights close", expect_equivalent(res$fit$gorica_weights, c(H1 = 0.484628998773837, H2 = 0.380174392981796, H3 = 0.135196608244367
), tolerance = .03))



# Example 4 ---------------------------------------------------------------

tab <- structure(c(933L, 661L, 402L, 260L, 51L, 44L, 26L, 26L), .Dim = c(2L,
                                                                         4L), class = c("table", "matrix", "array"))

expect_error(gorica(tab, hypothesis = "a:=x[1,1]/(x[1,1]+x[1,2]+x[1,3]+x[1,4]);b:=x[1,2]/(x[1,1]+x[1,2]+x[1,3]+x[1,4]);c:=x[1,3]/(x[1,1]+x[1,2]+x[1,3]+x[1,4]);d:=x[1,4]/(x[1,1]+x[1,2]+x[1,3]+x[1,4]);a > (b,c,d); a = b & c > d;a >b & b > c & c > d"))


# Example 5 ---------------------------------------------------------------

tab <- structure(c(933L, 661L, 402L, 260L, 0L, 44L, 26L, 26L), .Dim = c(2L,
                                                                        4L), class = c("table", "matrix", "array"))

expect_error(gorica(tab, hypothesis = "a:=(x[1,1]*x[2,2])/(x[1,2]*x[2,1]);b:=(x[1,2]*x[2,4])/(x[1,3]*x[2,2]);c:=(x[1,3]*x[2,4])/(x[1,4]*x[2,3]);a>b&b>c"))


# Example 6 ---------------------------------------------------------------

tab <- structure(c(0L, 661L, 402L, 260L, 51L, 44L, 26L, 26L), .Dim = c(2L,
                                                                       4L), class = c("table", "matrix", "array"))

expect_error(gorica(tab, hypothesis = "a:=x[2,1]/(x[1,1]+x[2,1]);b:=x[2,2]/(x[1,2]+x[2,2]);c:=x[2,3]/(x[1,3]+x[2,3]);d:=x[2,4]/(x[1,4]+x[2,4]);a>(b,c,d)"))



# Example 9 ---------------------------------------------------------------

tab <- structure(c(933L, 0L, 402L, 260L, 51L, 44L, 26L, 26L), .Dim = c(2L,
                                                                       4L), class = c("table", "matrix", "array"))
res <- gorica(tab, hypothesis = "x[2,1],x[2,2],x[2,3],x[2,4],x[1,1],x[1,2],x[1,3],x[1,4];x[2,1]>(x[2,2],x[2,3],x[2,4])&x[2,1]>0.3")

test_that("Estimates close", expect_equivalent(res$estimates, c(0.535445464982778, 0, 0.230638920780712, 0.149242824339839,
                                                                0.029341561423651, 0.0253381171067738, 0.0149196326061998, 0.0150734787600459
), tolerance = .03))


test_that("Weights close", expect_equivalent(res$fit$gorica_weights, c(H1 = 0.824030955425743, H2 = 0.175969044574257), tolerance = .03))


# Example 10 ---------------------------------------------------------------

tab <- structure(c(0L, 6L, 5L, 3L), .Dim = c(2L, 2L), class = c("table",
                                                                "matrix", "array"))

expect_error(gorica(tab, hypothesis = "a:=(x[1,1]*x[2,2])/(x[1,2]*x[2,1]);a<1;a=1;a>1"))


# Example 11 ---------------------------------------------------------------

tab <- structure(c(5L, 3L, 0L, 6L), .Dim = c(2L, 2L), class = c("table",
                                                                "matrix", "array"))

expect_error(gorica(tab, hypothesis = "a:=(x[1,1]*x[2,2])/(x[1,2]*x[2,1]);a<1;a=1;a>1"))


# Example 12 ---------------------------------------------------------------

tab <- structure(c(0L, 5L, 5L, 0L, 6L, 3L, 3L, 6L), .Dim = c(2L, 4L), class = c("table",
                                                                                "matrix", "array"))

expect_error(gorica(tab, hypothesis = "a:=(x[1,1]*x[1,4])/(x[1,2]*x[1,3]);b:=(x[2,1]*x[2,4])/(x[2,2]*x[2,3]);a<1&b<1;a=1&b=1;a>1&b>1", comparison = "none"))

