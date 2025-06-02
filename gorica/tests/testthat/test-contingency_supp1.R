dat <- structure(c(933L, 402L, 51L, 26L, 661L, 260L, 44L, 26L), .Dim = c(2L,
                                                                  4L))


class(dat) <- "table"
#Parameters used in evaluation (eta)
hyp <- "a := x[1,3]/(x[1,1]+x[1,3]);
b := x[2,3]/(x[2,1]+x[2,3]);
c := x[1,4]/(x[1,2]+x[1,4]);
d := x[2,4]/(x[2,2]+x[2,4]);
a>(b,c,d);
a=b&c>d;
a>b&b>c&c>d"


# gorica:::parse_hypothesis(letters[1:4], "a>(b,c,d);
# a=b&c>d;
# a>b&b>c&c>d")

res <- gorica(dat, hyp)

test_that("contingency table supp 1 works best", {
  expect_equivalent(res$fit$gorica_weights, c(0.2347528, 0.419965, 0.1791582, 0.166124), tolerance = .03)
})
