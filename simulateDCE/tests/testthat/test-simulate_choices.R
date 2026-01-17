library(rlang)

designpath <- system.file("extdata", "Rbook", package = "simulateDCE")

design <- system.file("extdata", "Rbook", "design1.RDS", package = "simulateDCE")

# notes <- "This design consists of different heuristics. One group did not attend the methan attribute, another group only decided based on the payment"

notes <- "No Heuristics"

resps <- 40 # number of respondents
nosim <- 2 # number of simulations to run (about 500 is minimum)

# betacoefficients should not include "-"

bcoeff <- list(
  bsq = 0.00,
  bredkite = -0.05,
  bdistance = 0.50,
  bcost = -0.05,
  bfarm2 = 0.25,
  bfarm3 = 0.50,
  bheight2 = 0.25,
  bheight3 = 0.50
)

destype <- "spdesign"


# place your utility functions here
ul <- list(u1 = list(
  v1 = V.1 ~ bsq * alt1.sq,
  v2 = V.2 ~ bfarm2 * alt2.farm2 + bfarm3 * alt2.farm3 + bheight2 * alt2.height2 + bheight3 * alt2.height3 + bredkite * alt2.redkite + bdistance * alt2.distance + bcost * alt2.cost,
  v3 = V.3 ~ bfarm2 * alt3.farm2 + bfarm3 * alt3.farm3 + bheight2 * alt3.height2 + bheight3 * alt3.height3 + bredkite * alt3.redkite + bdistance * alt3.distance + bcost * alt3.cost
))


formattedes <- readdesign(design = design, designtype = "spdesign")
data <- simulateDCE::createDataset(formattedes, respondents = resps)


test_that("simulate_choices() does not error", {
  expect_error(
    simulate_choices(data = data, bcoeff = bcoeff, u = ul),
    regexp = NA  # ← this is key: expect *no* error
  )
})

ds <- simulate_choices(data = data, bcoeff = bcoeff, u = ul)
test_that("random values are unique", {
  expect_equal(dim(table(table(ds$e_1))), 1)
  expect_equal(dim(table(table(ds$e_2))), 1)
  expect_equal(dim(table(table(ds$e_3))), 1)
  expect_equal(dim(table(table(ds$U_1))), 1)
  expect_equal(dim(table(table(ds$U_2))), 1)
  expect_equal(dim(table(table(ds$U_3))), 1)
  expect_true("CHOICE" %in% names(ds))
  expect_equal(length(unique(ds$CHOICE)), 3)
  expect_equal(length(unique(ds$Block)), 10)
  expect_equal(length(unique(ds$Choice_situation)), 100)
  expect_equal(length(unique(ds$ID)), 40)
  expect_equal(length(unique(ds$group)), 1)
})


#### more simple tests

# assume your simulate_choices() is already loaded

#–– 1) small toy dataset & simple utility for testing ––#
df_small <- data.frame(
  ID      = rep(1:5, each = 2),
  price   = rep(c(10, 20), 5),
  quality = rep(c(1,  2), 5)
)

beta <- list(
  bprice   = -1,
  bquality =  2
)

# note: use V.1 / V.2 so that rename_with("\\.", "_") => V_1/V_2
ut <- list(
  u1 = list(
    v1 = V.1 ~ bprice * price + bquality * quality,
    v2 = V.2 ~ 0
  )
)

#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––#

test_that("basic output: columns, types, and deterministic V", {
  res <- simulate_choices(
    data = df_small,
    utility = ut,
    setspp = 2,
    bcoeff = beta
  )

  # must be a data.frame
  expect_s3_class(res, "data.frame")

  # must contain the original columns + group + V_*/e_*/U_*/CHOICE
  expect_true(all(
    c("price","quality","V_1","V_2","e_1","e_2","U_1","U_2","CHOICE")
    %in% names(res)
  ))

  # since V_1 = -1*price + 2*quality, V_2 = 0
  # and these are created *before* the random error is added,
  # they must match exactly:
  expect_equal(res$V_1, -1 * res$price + 2 * res$quality)
  expect_equal(res$V_2, rep(0, nrow(res)))
})

test_that("CHOICE always in {1,2} for two alternatives", {
  res <- simulate_choices(df_small, ut, setspp = 2, bcoeff = beta)
  expect_true(all(res$CHOICE %in% c(1L, 2L)))
})

test_that("preprocess_function must be a function", {
  expect_error(
    simulate_choices(df_small, ut, setspp = 2, bcoeff = beta,
                     preprocess_function = 123),
    "`preprocess_function` must be a function"
  )
})

test_that("preprocess_function merges back on ID", {
  # create a df with explicit ID
  df2 <- df_small
  df2$ID <- 1:nrow(df2)

  # preprocessing returns only ID=1 with extra column
  prep <- function() data.frame(ID = 1, extra = 99)

  res <- simulate_choices(df2, ut, setspp = 2, bcoeff = beta,
                          preprocess_function = prep)

  expect_true("extra" %in% names(res))
  expect_equal(res$extra[1], 99)
  expect_true(all(is.na(res$extra[-1])))
})

test_that("manipulations are applied before utility", {
  # e.g. triple the price
  manip <- list(price = expr(price * 3))

  res <- simulate_choices(df_small, ut, setspp = 2,
                          bcoeff = beta, manipulations = manip)

  # price in result should be three times the original
  expect_equal(res$price, df_small$price * 3)

  # and V_1 = -1 * (price*3) + 2*quality
  expect_equal(res$V_1, -1 * res$price + 2 * res$quality)
})

test_that("decisiongroups produces correct group labels", {
  # 10 rows → break at 50% → two groups of 5 each

  ut2 <- list(
    u1 = list(
      v1 = V.1 ~ bprice * price + bquality * quality,
      v2 = V.2 ~ 0
    ),
    u2 = list(
      v1 = V.1 ~ bprice * price + bquality * quality * 1.5,
      v2 = V.2 ~ 0
    )
  )
  dg   <- c(0, .5, 1)
  df10 <- df_small[1:10, ]
  df10$ID <- 1:10

  res <- simulate_choices(df10, ut2, setspp = 2,
                          bcoeff = beta, decisiongroups = dg)

  # first 5 rows group==1, next 5 group==2
  expect_equal(res$group, rep(1:2, each = 5))
})

#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––#


### MORE GROUP SPECIFIC TESTS


test_that("utilities are applied correctly by group", {
  # 10 rows, alternating prices and qualities
  df10 <- data.frame(
    ID      = 1:10,
    price   = rep(c(10, 20), 5),
    quality = rep(c(1,  2), 5)
  )

  beta <- list(
    bprice   = -1,
    bquality =  2
  )

  ut <- list(
    g1 = list(
      v1 = V.1 ~ bprice * price + bquality * quality,  # group 1
      v2 = V.2 ~ 0
    ),
    g2 = list(
      v1 = V.1 ~ bprice * price + 2 * bquality * quality,  # group 2 → stronger quality
      v2 = V.2 ~ 0
    )
  )

  res <- simulate_choices(
    df10,
    utility = ut,
    setspp  = 2,
    bcoeff  = beta,
    decisiongroups = c(0, 0.5, 1)  # → 2 groups of 5
  )

  # extract utilities
  res1 <- res[res$group == 1, ]
  res2 <- res[res$group == 2, ]

  # expected: V_1 = -1 * price + (2 * quality)
  v1_g1_expected <- with(res1, -1 * price + 2 * quality)
  expect_equal(res1$V_1, v1_g1_expected)

  # group 2 has double quality weight: V_1 = -1 * price + 4 * quality
  v1_g2_expected <- with(res2, -1 * price + 4 * quality)
  expect_equal(res2$V_1, v1_g2_expected)
})


test_that("missing utility group throws error", {
  df10 <- data.frame(
    ID = 1:10,
    price   = rep(c(10, 20), 5),
    quality = rep(c(1,  2), 5)
  )

  beta <- list(bprice = -1, bquality = 2)

  ut <- list(only_one = list(
    v1 = V.1 ~ bprice * price + bquality * quality,
    v2 = V.2 ~ 0
  ))

  expect_error(
    simulate_choices(df10, ut, setspp = 2, bcoeff = beta,
                     decisiongroups = c(0, 0.5, 1)),
    "Length of `utility`.*does not match.*decision groups"
  )
})


test_that("group in data not covered by utility triggers error", {
  df10 <- data.frame(
    ID = 1:10,
    price   = rep(c(10, 20), 5),
    quality = rep(c(1,  2), 5)
  )

  beta <- list(bprice = -1, bquality = 2)

  ut <- list(
    g1 = list(
      v1 = V.1 ~ bprice * price + bquality * quality,
      v2 = V.2 ~ 0
    ),
    g2 = list(
      v1 = V.1 ~ bprice * price + bquality * quality,
      v2 = V.2 ~ 0
    )
  )

  # corrupt the decision group vector
  expect_error(
    simulate_choices(df10, ut, setspp = 2, bcoeff = beta,
                     decisiongroups = c(0, 0.4, 0.8, 1)),  # 3 breaks but only 2 utils
    "Length of `utility`.*does not match.*decision groups"
  )
})

