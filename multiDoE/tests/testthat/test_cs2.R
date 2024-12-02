# instance 4 paper 2016
options(digits = 10)

# setting
facts <- list(1, 2, 3)
units <- list(3, 3, 3)
levels <- 3
etas <- list(1, 1)
criteria <- c('Id', 'Ds', 'As')
model <- "interaction"

# M0 ####
k <- 3
k2 <- k * (k - 1) / 2
M0int <- rbind(
  cbind(1, t(integer(k)), t(integer(k2))),
  cbind(integer(k), diag(k) / 3, matrix(0, k, k2)),
  cbind(integer(k2), matrix(0, k2, k), diag(k2) / 9)
  )
M0int[, 1] <- 0
M0int[1, ] <- 0

# W ####
w <- t(rep(1, 3 + 3 * (3 - 1) / 2))
a <- length(w / sum(w))
Wint <- c(w / sum(w)) * diag(a)


# test: MSOpt works ####
msopt_cs2 <- MSOpt(facts, units, levels, etas, criteria, model)
file_name <- test_path("tests_data/msopt_cs2.Rds")
#saveRDS(msopt_cs2, file = file_name)
msopt_cs2 <- readRDS(file = file_name)

test_that("MSOpt works", {
  expect_equal(
    MSOpt(facts, units, levels, etas, criteria, model),
    structure(
      list(
        "facts" = list(1, 2, 3),
        "nfacts" = 3,
        "nstrat" = 3,
        "units" = list(3, 3, 3),
        "runs" = 27,
        "etas" = list(1, 1),
        "avlev" = list(c(-1, 0 , 1), c(-1, 0, 1), c(-1, 0, 1)),
        "levs" = c(3, 3, 3),
        "Vinv" = msopt_cs2$Vinv,
        "model"  = 'interaction',
        "crit" = c('Id', 'Ds', 'As'),
        "ncrit" = 3,
        "M0" = M0int,
        "W" = Wint
      ),
      class = c("MSOpt", "list")
    )
  )
})
