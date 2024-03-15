test_that("gdir.cd works", {
    gdir.cd("test")
    r <- gtrack.ls()
    gdir.cd("..")
    expect_equal(r, c(
        "array", "bigsparse", "computed2d", "fixedbin", "fixedbin_1bp",
        "fixedbin_1bp2", "generated_1d_1", "generated_1d_2", "generated_2d_1",
        "generated_2d_2", "generated_2d_3", "generated_2d_4", "generated_2d_5",
        "generated_2d_6", "rects", "rects_big_rects", "rects_converted",
        "rects_converted2", "rects_new_format", "rects_new_format_backup",
        "rects_old_format", "rects_old_format_backup", "sparse", "sparse2",
        "tmptrack11", "tmptrack12", "tmptrack2"
    ))
})

test_that("gdir.cd works (2)", {
    gdir.cd("test")
    r <- gdir.cwd()
    gdir.cd("..")
    expect_equal(r, file.path(.misha$GROOT, "tracks", "test"))
})

test_that("gdir creates and removes directory correctly", {
    gdir.create("testdir")
    r1 <- dir(gdir.cwd())
    gdir.rm("testdir", force = TRUE)
    r2 <- dir(gdir.cwd())
    expect_true("testdir" %in% r1)
    expect_false("testdir" %in% r2)
})


test_that("gtrack.exists works correctly", {
    expect_false(gtrack.exists("aaaaaaaaaaa.nnnnnnnnnnnnnn"))
    expect_true(gtrack.exists("test.rects"))
})

test_that("gtrack.ls works", {
    expect_equal(gtrack.ls(), c(
        "test.array", "test.bigsparse", "test.computed2d", "test.fixedbin",
        "test.fixedbin_1bp", "test.fixedbin_1bp2", "test.generated_1d_1",
        "test.generated_1d_2", "test.generated_2d_1", "test.generated_2d_2",
        "test.generated_2d_3", "test.generated_2d_4", "test.generated_2d_5",
        "test.generated_2d_6", "test.rects", "test.rects_big_rects",
        "test.rects_converted", "test.rects_converted2", "test.rects_new_format",
        "test.rects_new_format_backup", "test.rects_old_format", "test.rects_old_format_backup",
        "test.sparse", "test.sparse2", "test.tmptrack11", "test.tmptrack12",
        "test.tmptrack2", "test2d", "test2d2", "track2d"
    ))
    expect_equal(gtrack.ls("tes"), c(
        "test.array", "test.bigsparse", "test.computed2d", "test.fixedbin",
        "test.fixedbin_1bp", "test.fixedbin_1bp2", "test.generated_1d_1",
        "test.generated_1d_2", "test.generated_2d_1", "test.generated_2d_2",
        "test.generated_2d_3", "test.generated_2d_4", "test.generated_2d_5",
        "test.generated_2d_6", "test.rects", "test.rects_big_rects",
        "test.rects_converted", "test.rects_converted2", "test.rects_new_format",
        "test.rects_new_format_backup", "test.rects_old_format", "test.rects_old_format_backup",
        "test.sparse", "test.sparse2", "test.tmptrack11", "test.tmptrack12",
        "test.tmptrack2", "test2d", "test2d2"
    ))
    expect_null(gtrack.ls(blalaattr = "bubu"))
    expect_null(gtrack.ls("wig", created.by = "import"))
})

test_that("gtrack modify and extract for fixedbin", {
    gtrack.rm("test.tmptrack", force = TRUE)
    withr::defer(gtrack.rm("test.tmptrack", force = TRUE))
    gtrack.create("test.tmptrack", "", "test.fixedbin")
    intervs <- gscreen("test.fixedbin > 0.17 | is.na(test.fixedbin)", gintervals(c(1, 7)))
    gtrack.modify("test.tmptrack", "test.fixedbin + test.fixedbin", intervs)
    r <- gextract("test.tmptrack", gintervals(c(1, 2)))
    expect_regression(r, "gtrack.modify_and_extract_for_fixedbin")
})

test_that("gtrack modify for sparse", {
    gtrack.rm("test.tmptrack", force = TRUE)
    withr::defer(gtrack.rm("test.tmptrack", force = TRUE))
    gtrack.create("test.tmptrack", "", "test.sparse")
    expect_error(gtrack.modify("test.tmptrack", "test.fixedbin + test.fixedbin", gintervals(1, 1000, 2000)))
})

test_that("gtrack modify for rects", {
    gtrack.rm("test.tmptrack", force = TRUE)
    withr::defer(gtrack.rm("test.tmptrack", force = TRUE))
    gtrack.create("test.tmptrack", "", "test.rects")
    expect_error(gtrack.modify("test.tmptrack", "test.fixedbin + test.fixedbin", gintervals.2d(1, 1000, 2000, 2, 3000, 4000)))
})

test_that("gtrack creation and removal for sparse", {
    gtrack.rm("test.tmptrack", force = TRUE)
    withr::defer(gtrack.rm("test.tmptrack", force = TRUE))
    gtrack.create_sparse("test.tmptrack", "", gintervals(c(1, 2), 100, 2000), c(100, 200))
    r1 <- gtrack.ls()
    gtrack.rm("test.tmptrack", force = TRUE)
    r2 <- gtrack.ls()
    expect_true("test.tmptrack" %in% r1)
    expect_false("test.tmptrack" %in% r2)
})
