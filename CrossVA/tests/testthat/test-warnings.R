# Test that the odk2openVA produces errors and returns warnings when
# appropriate (e.g., faulty inputs, NAs in output).

context("Test error occurs raised with faulty input.")

test_that("odk2openVA produces errors with faulty input data.", {

    ## version 1.5.1
    record_f_name151 <- system.file("sample",
                                    "who151_odk_export.csv",
                                    package = "CrossVA")
    records151 <- read.csv(record_f_name151, stringsAsFactors = FALSE)

    badRecords151 <- records151[, -4] ## take out ID10004 (wet/dry season)

    expect_error(odk2openVA(badRecords151),
                 "Problem with data: please add above columns to your data frame")

    ## version 1.4.1
    record_f_name141 <- system.file("sample",
                                    "who141_odk_export.csv",
                                    package = "CrossVA")
    records141 <- read.csv(record_f_name141, stringsAsFactors = FALSE)

    badRecords141 <- records141[, -4] ## take out ID10004 (wet/dry season)

    expect_error(odk2openVA(badRecords141),
                 "Problem with data: please add above columns to your data frame")

    ## version 2014
    record_f_name2014 <- system.file("sample",
                                    "who2014_odk_export.csv",
                                    package = "CrossVA")
    records2014 <- read.csv(record_f_name2014, stringsAsFactors = FALSE)

    badRecords2014 <- records2014[, -6] ## take out Id1A220 (is the date of death known)

    expect_error(odk2openVA(badRecords2014),
                 "Problem with data: please add above columns to your data frame")

})

test_that("odk2openVA raises warning with NAs in output.", {

    ## version 1.5.1
    record_f_name151 <- system.file("sample",
                                    "who151_odk_export.csv",
                                    package = "CrossVA")
    records151 <- read.csv(record_f_name151, stringsAsFactors = FALSE)
    records151[1, 41] <- "not_a_number" ## ageInYears2

    expect_warning(odk2openVA_v151(records151),
                   "unexpected input")

    ## version 1.4.1
    record_f_name141 <- system.file("sample",
                                    "who141_odk_export.csv",
                                    package = "CrossVA")
    records141 <- read.csv(record_f_name141, stringsAsFactors = FALSE)
    records141[1, 21] <- "not_a_number" ## ageInYears

    expect_warning(odk2openVA_v141(records141),
                   "unexpected input")

    ## version 2014
    record_f_name2014 <- system.file("sample",
                                    "who2014_odk_export.csv",
                                    package = "CrossVA")
    records2014 <- read.csv(record_f_name2014, stringsAsFactors = FALSE)
    records2014[1, 9] <- "not_a_number" ## ageInYears

    expect_warning(odk2openVA_2014(records2014),
                   "unexpected input")

})

## warning that all values in a column (row?) are missing
## checks on age, sex
