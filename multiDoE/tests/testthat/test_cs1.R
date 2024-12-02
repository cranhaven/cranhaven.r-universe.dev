# instance 2 paper 2016
options(digits = 10)

# setting
facts <- list(c(), 1:3)
units <- list(7, 4)
levels <- 3
etas <- list(1)
criteria <- c('I', 'D', 'A')
model <- "quadratic"

# M ####
k <- length(unlist(facts))
k2 <- k * (k - 1) / 2
Mq <- rbind(
  cbind(1, t(integer(k)), t(rep(1, k)) / 3, t(integer(k2))),
  cbind(integer(k), diag(k) / 3, matrix(0, k, k), matrix(0, k, k2)),
  cbind(rep(1, k) / 3, matrix(0, k, k),
        (4 * diag(k) + 5 * matrix(1, k, k)) / 45, matrix(0, k, k2)),
  cbind(integer(k2), matrix(0, k2, k), matrix(0, k2, k), diag(k2) / 9)
)


# test: MSOpt works ####
test_that("MSOpt works", {
  expect_equal(MSOpt(facts, units, levels, etas, criteria, model),
               structure(
                 list(
                   "facts" = list(c(), 1:3),
                   "nfacts" = 3,
                   "nstrat" = 2,
                   "units" = list(7, 4),
                   "runs" = 28,
                   "etas" = list(1),
                   "avlev" = list(c(-1, 0 , 1), c(-1, 0, 1), c(-1, 0, 1)),
                   "levs" = c(3, 3, 3),
                   "Vinv" = t(solve(diag(28) + 1 * kronecker(diag(7), matrix(1, 4, 4)))),
                   "model"  = 'quadratic',
                   "crit" = c('I', 'D', 'A'),
                   "ncrit" = 3,
                   "M" = Mq
                 ),
                 class = c("MSOpt", "list")
               )
  )
})

# test: Score works ####

file_name <- test_path("tests_data/ex_score_cs1.Rds")
print(file_name)
# saveRDS(design_A, file = file_name)
example <- readRDS(file_name)
msopt <- MSOpt(facts, units, levels, etas, criteria, model)

test_that("Score works",{

    # skip_on_cran()
    expect_equal(
    Score(msopt, example),
    data.frame(
      criteria = c("I", "D", "A"),
      score = c(0.3471977990, 0.1038650765, 0.1290310631)
    ),
    tolerance = 0.000000001
  )
})



# test: MSSearch x I-optimality (CE) ####
set.seed(2)
msopt_I <- MSOpt(facts, units, levels, etas, c("I"), model)
#mssearch_I <- MSSearch(msopt_I, 1)
file_name <- test_path("tests_data/mss_I_cs1.Rds")
#saveRDS(mssearch_I, file = file_name)
mss_I_cs1 <- readRDS(file_name)

set.seed(2)
test_that("MSSearch works 1", {
  expect_equal(
    MSSearch(msopt_I, 1),
    structure(
      list(
        "optsol" = mss_I_cs1$optsol,
        "optsc" = data.frame(
            "criteria" = c("I"),
            "score" = mss_I_cs1$optsc
          ),
        "feval" = mss_I_cs1$feval,
        "trend" = mss_I_cs1$trend
      ),
      class = c("MSSearch", "list")
    )
  )
})

# test: MSSearch x I-optimality (CE) + parametri opz. ####
set.seed(2)
#mssearch_I2 <- MSSearch(msopt_I, 1, "Restarts", 50, "Start", example)
file_name <- test_path("tests_data/mss_I2_cs1.Rds")
#saveRDS(mssearch_I2, file = file_name)
mss_I2_cs1 <- readRDS(file_name)

set.seed(2)
test_that("MSSearch works 2", {
  expect_equal(
    MSSearch(msopt_I, 1, "Restarts", 50, "Start", example),
    structure(
      list(
        "optsol" = mss_I2_cs1$optsol,
        "optsc" = data.frame(
          "criteria" = "I",
          "score" = mss_I2_cs1$optsc
        ),
        "feval" = mss_I2_cs1$feval,
        "trend" = mss_I2_cs1$trend
      ),
      class = c("MSSearch", "list")
    )
  )
})

# test: MSSearch x I,D,A-optimality + parametri opz. ####
set.seed(2)
# mssearch3 <- MSSearch(msopt, c(2/4, 1/4, 1/4),
#                         "Restarts", 50,
#                         "Start", example,
#                         "Normalize", c(rep(0.5, 3), rep(1, 3)))
file_name <- test_path("tests_data/mss_3_cs1.Rds")
mss_3_cs1 <- readRDS(file_name)

set.seed(2)
test_that("MSSearch works 3", {
  expect_equal(
    MSSearch(
      msopt, c(2/4, 1/4, 1/4),
        "Restarts", 50,
        "Start", example,
        "Normalize", c(rep(0.5, 3), rep(1, 3))
    ),
    structure(
      list(
        "optsol" = mss_3_cs1$optsol,
        "optsc" = data.frame(
          "criteria" = c("I", "D", "A"),
          "score" = mss_3_cs1$optsc
        ),
        "feval" = mss_3_cs1$feval,
        "trend" = mss_3_cs1$trend
      ),
      class = c("MSSearch", "list")
    )
  )
})



# test: runTPLS ####

lCrit <- length(criteria)
iters <- 3 * lCrit
restarts <- 30
restInit <- 2

file_name <- test_path("tests_data/tpls_cs1.Rds")
#saveRDS(tpls, file = file_name)
tpls_cs1 <- readRDS(file_name)

# RngSeed
test_that("runTPLSearch works", {
  expect_equal(
    runTPLS(facts, units, criteria, model, iters,
            "Restarts", restarts,
            "RestInit", restInit,
            "RngSeed", 4),
    structure(
      list("ar" = tpls_cs1$ar,
          "stats" = tpls_cs1$stats,
          "megaAR" = tpls_cs1$megaAR),
      class = c("runTPLS", "list")
    )
  )
}
)


# test: optMultiCrit ####
file_name <- test_path("tests_data/opt_cs1.Rds")
#saveRDS(tpls1, file = file_name)
tpls1 <- readRDS(file_name)

#res <- optMultiCrit(tpls1$megaAR)
file_name <- test_path("tests_data/res_cs1.Rds")
#saveRDS(res, file = file_name)
res1 <- readRDS(file_name)

test_that("optMultiCrit works",
          {expect_equal(optMultiCrit(tpls1$megaAR),
                        list("solution" = res1$solution, "scores" = res1$scores)
                        )
            }
          )

# test: OptSingleCrit ####

#res2 <- optSingleCrit(tpls1$megaAR)
file_name <- test_path("tests_data/res2_cs1.Rds")
#saveRDS(res2, file = file_name)
res2 <- readRDS(file_name)

test_that("optSingleCrit works",
          {expect_equal(optSingleCrit(tpls1$megaAR),
                        list("I" = res2$I, "D" = res2$D, "A" = res2$A)
          )
          }
)


