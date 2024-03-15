test_that("Basic gtrack.array.extract functions", {
    expect_error(gtrack.array.extract("test.fixedbin", NULL, .misha$ALLGENOME))
    expect_regression(gtrack.array.extract("test.array", NULL, gintervals(c(1, 2))), "gtrack_array_extract_array_intervals")
    expect_regression(gtrack.array.extract("test.array", c("col1", "col3", "col5"), gintervals(c(1, 2))), "gtrack_array_extract_array_cols_intervals")
})

test_that("gtrack.array.extract with tmpresfile", {
    gtrack.array.extract("test.array", NULL, gintervals(c(1, 2)), file = "tmpresfile")
    withr::defer(unlink("tmpresfile"))
    r <- read.table("tmpresfile", sep = "\t", nrows = 1000)
    expect_regression(r, "gtrack_array_extract_tmpresfile")
})

test_that("gtrack.array.extract with tmpresfile and columns", {
    gtrack.array.extract("test.array", c("col1", "col3", "col5"), gintervals(c(1, 2)), file = "tmpresfile")
    withr::defer(unlink("tmpresfile"))
    r <- read.table("tmpresfile", sep = "\t", nrows = 1000)
    expect_regression(r, "gtrack_array_extract_tmpresfile_cols")
})

test_that("gtrack.array.extract with intervals after sampling", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))
    intervs <- gscreen("test.fixedbin>0.2", gintervals(c(2, 4, 5, 10)))
    set.seed(60427)
    intervs <- intervs[sample(nrow(intervs)), ]
    gtrack.array.extract("test.array", c("col1", "col3", "col5"), intervs, intervals.set.out = "test.testintervs")
    r <- gintervals.load("test.testintervs")
    expect_regression(r, "gtrack_array_extract_sampled_intervals")
})

test_that("Testing gtrack.array column name functions", {
    expect_error(gtrack.array.get_colnames("test.fixedbin"))
    expect_regression(gtrack.array.get_colnames("test.array"), "gtrack_array_colnames_array")
    expect_error(gtrack.array.set_colnames("test.fixedbin", "col1"))
    expect_error(gtrack.array.set_colnames("test.array", "col1"))
    cols <- gtrack.array.get_colnames("test.array")
    gtrack.array.set_colnames("test.array", paste(cols, "blabla", sep = ""))
    r <- gtrack.array.get_colnames("test.array")
    gtrack.array.set_colnames("test.array", cols)
    expect_regression(r, "gtrack_array_set_colnames")
})

test_that("Import and extraction with gtrack.array", {
    withr::local_options(list(.ginteractive = FALSE))
    withr::defer(gtrack.rm("test_track1", TRUE))
    withr::defer(gtrack.rm("test_track2", TRUE))
    withr::defer(unlink(c(f1, f2, f3)))
    f1 <- tempfile()
    gextract("test.sparse", gintervals(c(1, 2)), file = f1)
    f2 <- tempfile()
    gtrack.array.extract("test.array", c("col2", "col3", "col4"), gintervals(c(1, 2)), file = f2)
    f3 <- tempfile()
    gtrack.array.extract("test.array", c("col1", "col3"), gintervals(c(1, 2)), file = f3)

    gtrack.array.import("test_track1", "", f1, f2)
    r1 <- gtrack.array.extract("test_track1", NULL, .misha$ALLGENOME)

    gtrack.array.import("test_track2", "", "test_track1", f3)
    r2 <- gtrack.array.extract("test_track2", NULL, .misha$ALLGENOME)

    r <- list(r1 = r1, r2 = r2)

    gtrack.rm("test_track1", TRUE)
    gtrack.rm("test_track2", TRUE)
    unlink(c(f1, f2, f3))

    expect_regression(r, "gtrack_array_import_extract")
})
