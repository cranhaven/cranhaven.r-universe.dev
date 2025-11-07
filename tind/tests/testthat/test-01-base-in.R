context("base - arbitrary integer and numeric indices")
# ###################################################################

# test sample size
NN <- 100L

# test samples
ii <- sample.int(1e6, NN)
ii[1L + sample.int(NN - 1L, NN %/% 10L)] <- NA
nn <- rnorm(NN, 100, 100)
nn[1L + sample.int(NN - 1L, NN %/% 10L)] <- NA


test_that("'.validate_i' works correctly", {
    expect_equal(.validate_i(c(1, NA_real_, Inf, 1e10)),
                 c(1L, NA_integer_, NA_integer_, NA_integer_))
})


test_that("'.i2char' works correctly", {
    expect_equal(.i2char(NA_integer_), NA_character_)
    expect_equal(.i2char(integer()), character())
    expect_equal(.i2char(c(a = 1L)), c(a = "1"))
    expect_equal(.i2char(c(a = 1L, b = NA_integer_)),
                     c(a = "1", b = NA_character_))
    iic <- .i2char(ii)
    expect_true(all(diff(nchar(iic)[!is.na(iic)]) == 0L))
    expect_equal(as.integer(iic), ii)
})


test_that("'.i2n' works correctly", {
    expect_equal(.i2n(ii), ii)
    expect_equal(storage.mode(.i2n(ii)), "double")
})


test_that("'.floor_i' and '.ceiling_i' work correctly", {
    ns <- sample.int(10, 2)
    ina <- is.na(ii)
    NN <- NN - sum(ina)
    for (n in ns) {
        fi <- .floor_i(ii, n)
        ci <- .ceiling_i(ii, n)
        expect_equal(is.na(fi), ina)
        expect_equal(is.na(ci), ina)
        iii <- ii[!ina]
        fi <- fi[!ina]
        ci <- ci[!ina]
        expect_equal(.floor_i(fi, n), fi)
        expect_equal(.ceiling_i(ci, n), ci)
        expect_true(all(fi %% n == 0L))
        expect_true(all(ci %% n == 0L))
        expect_true(all(fi <= iii & iii <= ci))
        expect_equal(fi == iii, iii == ci)
        if (n == 1L) {
            expect_equal(fi, iii)
            expect_equal(ci, iii)
        } else {
            expect_equal(fi %% n, rep(0L, NN))
            expect_equal(ci %% n, rep(0L, NN))
        }
    }
})


test_that("'.validate_n' works correctly", {
    expect_equal(.validate_n(c(1, NA_real_, Inf, 1e10)),
                 c(1L, NA_real_, NA_real_, 1e10))
})


test_that("'.n2char' works correctly", {
    expect_equal(.n2char(numeric()), character())
    expect_equal(.n2char(NA_real_), NA_character_)
    expect_equal(.n2char(c(NA_real_, 0)), c(NA_character_, "0"))
    expect_equal(.n2char(c(a = 1e9)), c(a = "1e+09"))
    expect_equal(.n2char(c(a = 1e9, b = NA_integer_)),
                     c(a = "1e+09", b = NA_character_))
    nc <- .n2char(nn)
    expect_true(all(diff(nchar(nc)[!is.na(nc)]) == 0L))
    for (dig in 0:7) {
        nnt <- round(nn, dig)
        nct <- .n2char(nnt)
        expect_true(all(diff(nchar(nct)[!is.na(nct)]) == 0L))
        expect_equal(as.numeric(nct), nnt)
    }
})


test_that("'.floor_n' and '.ceiling_n' work correctly", {
    ns <- sample.int(10, 2)
    nna <- is.na(nn)
    NN <- NN - sum(nna)
    for (n in ns) {
        fi <- .floor_n(nn, n)
        ci <- .ceiling_n(nn, n)
        expect_equal(is.na(fi), nna)
        expect_equal(is.na(ci), nna)
        nnn <- nn[!nna]
        fi <- fi[!nna]
        ci <- ci[!nna]
        expect_equal(.floor_n(fi, n), fi)
        expect_equal(.ceiling_n(ci, n), ci)
        expect_true(all(fi %% n == 0))
        expect_true(all(ci %% n == 0))
        expect_true(all(fi <= nnn & nnn <= ci))
        expect_equal(fi == nnn, nnn == ci)
        expect_equal(fi %% n, rep(0, NN))
        expect_equal(ci %% n, rep(0, NN))
    }
})

