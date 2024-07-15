context("Cpp implementation of nodf")
library(maxnodf)

test_that("nodf_cpp",{
    A <- matrix(0.0, 7, 5)
    A[1,] <- 1.0
    A[2,1:3] <- 1.0
    A[3,1:3] <- 1.0
    A[4,1:1] <- 1.0
    A[5,1:1] <- 1.0
    A[6,1:1] <- 1.0
    A[7,1:1] <- 1.0
    expect_equal(nodf_cpp(A), 0.709677419354839)
    B <- maxnodf(c(14, 13, 52), 0)[[2]]
    expect_equal(nodf_cpp(B), 0.875739644970414)
})

test_that("init_nodf",{
    A <- matrix(0.0, 7, 5)
    A[1,] <- 1.0
    A[2,1:3] <- 1.0
    A[3,1:3] <- 1.0
    A[4,1:1] <- 1.0
    A[5,1:1] <- 1.0
    A[6,1:1] <- 1.0
    A[7,1:1] <- 1.0
    support_data <- init_nodf(A)
    expect_equal(support_data[[1]][[1]] *1.0, computeMT0(A)*1.0)
    expect_equal(support_data[[1]][[2]] *1.0, computeMTt(A)*1.0)
    expect_equal(support_data[[2]][[1]] *1.0, computeFill0(A)*1.0)
    expect_equal(support_data[[2]][[2]] *1.0, computeFillt(A)*1.0)
    expect_equal(support_data[[3]][[1]] *1.0, computeDM0(A)*1.0)
    expect_equal(support_data[[3]][[2]] *1.0, computeDMt(A)*1.0)
    expect_equal(support_data[[4]][[1]] *1.0, computeND0(A)*1.0)
    expect_equal(support_data[[4]][[2]] *1.0, computeNDt(A)*1.0)
    expect_equal(support_data[[5]][[1]] *1.0, computeS(A)[1])
    expect_equal(support_data[[5]][[2]] *1.0, computeS(A)[2])
})

test_that("nodf link addition",{
    A <- matrix(0.0, 7, 5)
    A[1,] <- 1.0
    A[2,1:3] <- 1.0
    A[3,1:3] <- 1.0
    A[4,1:1] <- 1.0
    A[5,1:1] <- 1.0
    A[6,1:1] <- 1.0
    A[7,1:1] <- 1.0
    mt_0 <- computeMT0(A);
    mt_t <- computeMTt(A);
    F0 <- computeFill0(A);
    Ft <- computeFillt(A);
    DM0 <- computeDM0(A);
    DMt <- computeDMt(A);
    ND0 <- computeND0(A);
    NDt <- computeNDt(A);
    S <- computeS(A);


    nodf1 <- nodf_one_link_added_cpp(A, 6, 3, mt_0, mt_t, F0, Ft, DM0, DMt, ND0, NDt, S);
    nodf2 <- nodf_cpp(A)

    mt_02 <- computeMT0(A);
    mt_t2 <- computeMTt(A);
    F02 <- computeFill0(A);
    Ft2 <- computeFillt(A);
    DM02 <- computeDM0(A);
    DMt2 <- computeDMt(A);
    ND02 <- computeND0(A);
    NDt2 <- computeNDt(A);
    S2 <- computeS(A);

    expect_equal(nodf1, nodf2)
    expect_equal(mt_0, mt_02)
    expect_equal(mt_t, mt_t2)
    expect_equal(F0, F02)
    expect_equal(Ft, Ft2)
    expect_equal(DM0, DM02)
    expect_equal(DMt, DMt2)
    expect_equal(ND0, ND02)
    expect_equal(NDt, NDt2)
    expect_equal(S, S2)
})

test_that("nodf link removal",{
    A <- matrix(0.0, 7, 5)
    A[1,] <- 1.0
    A[2,1:3] <- 1.0
    A[3,1:3] <- 1.0
    A[4,1:1] <- 1.0
    A[5,1:1] <- 1.0
    A[6,1:1] <- 1.0
    A[7,1:1] <- 1.0
    mt_0 <- computeMT0(A);
    mt_t <- computeMTt(A);
    F0 <- computeFill0(A);
    Ft <- computeFillt(A);
    DM0 <- computeDM0(A);
    DMt <- computeDMt(A);
    ND0 <- computeND0(A);
    NDt <- computeNDt(A);
    S <- computeS(A);

    nodf1 <- nodf_one_link_removed_cpp(A, 3, 2, mt_0, mt_t, F0, Ft, DM0, DMt, ND0, NDt, S);
    nodf2 <- nodf_cpp(A)

    mt_02 <- computeMT0(A);
    mt_t2 <- computeMTt(A);
    F02 <- computeFill0(A);
    Ft2 <- computeFillt(A);
    DM02 <- computeDM0(A);
    DMt2 <- computeDMt(A);
    ND02 <- computeND0(A);
    NDt2 <- computeNDt(A);
    S2 <- computeS(A);

    expect_equal(nodf1, nodf2)
    expect_equal(mt_0, mt_02)
    expect_equal(mt_t, mt_t2)
    expect_equal(F0, F02)
    expect_equal(Ft, Ft2)
    expect_equal(DM0, DM02)
    expect_equal(DMt, DMt2)
    expect_equal(ND0, ND02)
    expect_equal(NDt, NDt2)
    expect_equal(S, S2)
})
