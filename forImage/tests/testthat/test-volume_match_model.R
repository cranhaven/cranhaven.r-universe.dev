test_that("incorrect model call", {

    x <- data.frame(h = 10, d_one = 10, d_two = 10,
                    area = 10, width = 10, length = 10)


    expect_error(volume.total(x, model = "1x"))
})


test_that("find model argument in column dataframe", {

    x <- data.frame(h = 10, d_one = 10, d_two = 10, model = "1hl")

    expected_1hl <- 523.5987757

    expect_equal(as.numeric(volume.total(x)$vol), expected_1hl)


})

test_that("calculate expected volume", {

    x <- data.frame(h = 10, d_one = 10, d_two = 10,
                    area = 10, width = 10, length = 10)

    expected_1hl <- 523.5987757
    expected_2sl <- 261.7993878
    expected_3hl <- 523.5987757
    expected_4hl <- 261.7993878
    expected_5hl <- 261.7993878
    expected_6fs <- 163.2077
    expected_7fs <- 916.2978574
    expected_8hl <- 785.3981635
    expected_10hl <- 523.5987757
    expected_11fs <- 261.7993878
    expected_12v <- 261.7993878
    expected_13hlsl <- 102.8083792
    expected_14hl <- 785.3981635
    expected_15hl <- 785.3981635
    expected_17fs <- 166.6666667
    expected_axh <- 100



    expect_equal(as.numeric(volume.total(x, model = "1hl")$vol),
                 expected_1hl)
    expect_equal(as.numeric(volume.total(x, model = "2sl")$vol),
                 expected_2sl)
    expect_equal(as.numeric(volume.total(x, model = "3hl")$vol),
                 expected_3hl)
    expect_equal(as.numeric(volume.total(x, model = "4hl")$vol),
                 expected_4hl)
    expect_equal(as.numeric(volume.total(x, model = "5hl")$vol),
                 expected_5hl)
    expect_equal(as.numeric(volume.total(x, model = "6fs")$vol),
                 expected_6fs)
    expect_equal(as.numeric(volume.total(x, model = "7fs")$vol),
                 expected_7fs)
    expect_equal(as.numeric(volume.total(x, model = "8hl")$vol),
                 expected_8hl)
    expect_equal(as.numeric(volume.total(x, model = "10hl")$vol),
                 expected_10hl)
    expect_equal(as.numeric(volume.total(x, model = "11fs")$vol),
                 expected_11fs)
    expect_equal(as.numeric(volume.total(x, model = "12v")$vol),
                 expected_12v)
    expect_equal(as.numeric(volume.total(x, model = "13hlsl")$vol),
                 expected_13hlsl)
    expect_equal(as.numeric(volume.total(x, model = "14hl")$vol),
                 expected_14hl)
    expect_equal(as.numeric(volume.total(x, model = "15hl")$vol),
                 expected_15hl)
    expect_equal(as.numeric(volume.total(x, model = "17fs")$vol),
                 expected_17fs)
    expect_equal(as.numeric(volume.total(x, model = "axh")$vol),
                 expected_axh)
})
