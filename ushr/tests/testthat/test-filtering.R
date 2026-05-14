context("tests on input")

test_that("test correct input data",{

    data_noVL <- data.frame(time = 1:100, id = "S1")

    expect_error(filter_data(data = data_noVL), "'data' must be a data frame with named columns for 'id', 'time', and 'vl'")

    data_nonnumeric <- data.frame(vl = seq(10000, 1, length.out = 5),
                                  time = c("Jan", "Feb", "March", "April", "May"), id = "S1")

    expect_error(filter_data(data = data_nonnumeric), fixed = TRUE,
                 "Column for the time of observations ('time') must be numeric")

    data_noID <- data.frame(vl = seq(10000, 1, length.out = 5),
                                  time = seq(1, 250, length.out = 5), id = c(rep("S1", 3), NA, NA))

    expect_warning(filter_data(data = data_noID), fixed = TRUE,
                   "Some subjects have missing IDs; removing these from the data")

    nodata <- data.frame()

    expect_error(filter_data(data = nodata), "'data' must be a data frame with named columns for 'id', 'time', and 'vl'")

    listdata <- list()

    expect_error(filter_data(data = listdata), "'data' must be a data frame with named columns for 'id', 'time', and 'vl'")

    matrixdata <- matrix()

    expect_error(filter_data(data = matrixdata), "'data' must be a data frame with named columns for 'id', 'time', and 'vl'")
})


context("tests on output")

test_that("test output is a data.frame with named columns",{

    data <- data.frame(vl = seq(10000, 1, length.out = 5),
                            time = seq(1, 250, length.out = 5), id = "S1")

    output <- filter_data(data)

    expect_that(output,is_a("data.frame"))

    expect_equivalent(sort(colnames(output)),c( "id", "time","vl"))

    dataART <- data.frame(vl = seq(10000, 1, length.out = 5),
                       time = seq(1, 250, length.out = 5), id = "S1",
                       ART = 3)

    outputART <- filter_data(dataART)

    expect_equivalent(sort(colnames(outputART)),c( "ART", "id", "time","vl"))
})
