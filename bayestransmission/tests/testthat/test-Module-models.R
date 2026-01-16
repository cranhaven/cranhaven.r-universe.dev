test_that("models::InsituParameters", {
    isp0 <- CppInsituParams$new()
    expect_equal(isp0$nStates, 0)


    isp1 <- CppInsituParams$new(2)
    expect_equal(isp1$nStates, 2)
    expect_equal(isp1$paramNames, c("Insit.P(unc)", "Insit.P(col)"))

    isp3 <- CppInsituParams$new(
        probs = c(80, 5, 15),
        priors = c(0.1, 0.2, 0.3),
        doit = c(TRUE, TRUE, TRUE)
    )

    expect_equal(isp3$nStates, 3)
    expect_equal(isp3$paramNames, c("Insit.P(unc)", "Insit.P(lat)", "Insit.P(col)"))
    expect_equal(isp3$values, c(0.8, 0.05, 0.15))
    expect_equal(isp3$counts, c(0.1, 0.2, 0.3))
    isp3$counts <- c(90, 3, 7)
    expect_equal(isp3$counts, c(90, 3, 7))


    rand <- RRandom$new()
    isp3$update(rand, TRUE)

    expect_equal(isp3$values, c(89/97, 2/97, 6/97))


})

test_that("models::TestParams", {
    tp1 <- CppTestParams$new(2L)
    expect_equal(tp1$nStates, 2)
    expect_equal(tp1$names, c("Test.P(+|unc)", "Test.P(+|col)"))
    expect_equal(tp1$values, c(0, 0.8))

    tp1$set(0L, 0.1, 1, 0.001, 1)
    expect_equal(tp1$values, c(0.1, 0.8))
    tp1$set(2L, 0.9, 1, 0.8, 1)
    expect_equal(tp1$values, c(0.1, 0.9))

    rand <- RRandom$new()

    expect_error(tp1$getCount(3L, 0L), "Index out of range")
    expect_error(tp1$getCount(0L, 2L), "Index out of range")

    tp1$setCount(0L, 0L, 10)
    expect_equal(tp1$getCount(0L, 0L), 10)
    tp1$setCount(1L, 0L, 20)
    expect_equal(tp1$getCount(1L, 0L), 20)
    tp1$setCount(2L, 0L, 30)
    expect_equal(tp1$getCount(2L, 0L), 30)
    tp1$setCount(0L, 1L, 40)
    expect_equal(tp1$getCount(0L, 1L), 40)
    tp1$setCount(1L, 1L, 50)
    expect_equal(tp1$getCount(1L, 1L), 50)
    tp1$setCount(2L, 1L, 60)
    expect_equal(tp1$getCount(2L, 1L), 60)
    expect_error(tp1$setCount(3L, 0L, 70), "Index out of range")

    tp1$update_max(rand)

    expect_equal(tp1$values, c(39/48, 59/88))

    tp3 <- CppTestParams$new(3)
    # tp3$values

    expect_equal(tp3$nStates, 3)
    expect_equal(tp3$names, c("Test.P(+|unc)", "Test.P(+|lat)", "Test.P(+|col)"))
})
