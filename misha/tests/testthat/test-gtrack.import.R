test_that("import and extract from s_7_export.txt", {
    intervs <- gscreen("test.fixedbin > 0.1", gintervals(c(1, 2)))
    withr::defer(gtrack.rm("test.tmptrack", force = TRUE))
    gtrack.rm("test.tmptrack", force = TRUE)
    gtrack.import_mappedseq("test.tmptrack", "", "/net/mraid14/export/tgdata/db/tgdb/misha_snapshot/input_files/s_7_export.txt", remove.dups = FALSE)
    r <- gextract("test.tmptrack", intervs)
    expect_regression(r, "track.import_mappedseq.s_7_export")
})

test_that("import and extract from sample-small.sam", {
    intervs <- gscreen("test.fixedbin > 0.1", gintervals(c(1, 2)))
    gtrack.rm("test.tmptrack", force = TRUE)
    withr::defer(gtrack.rm("test.tmptrack", force = TRUE))
    gtrack.import_mappedseq("test.tmptrack", "", "/net/mraid14/export/tgdata/db/tgdb/misha_snapshot/input_files/sample-small.sam", cols.order = NULL, remove.dups = FALSE)
    r <- gextract("test.tmptrack", intervs)
    expect_regression(r, "track.import_mappedseq.sample_small_sam")
})

test_that("import with pileup and binsize from s_7_export.txt", {
    intervs <- gscreen("test.fixedbin > 0.1", gintervals(c(1, 2)))
    gtrack.rm("test.tmptrack", force = TRUE)
    withr::defer(gtrack.rm("test.tmptrack", force = TRUE))
    gtrack.import_mappedseq("test.tmptrack", "", "/net/mraid14/export/tgdata/db/tgdb/misha_snapshot/input_files/s_7_export.txt", remove.dups = FALSE, pileup = 180, binsize = 50)
    r <- gextract("test.tmptrack", intervs)
    expect_regression(r, "track.import_pileup_binsize")
})

test_that("import with gmax data size option", {
    gtrack.rm("test.tmptrack", TRUE)
    withr::defer(gtrack.rm("test.tmptrack", force = TRUE))
    withr::with_options(list(gmax.data.size = 10000), {
        gtrack.2d.import("test.tmptrack", "aaa7", c("/net/mraid14/export/tgdata/db/tgdb/misha_snapshot/input_files/f4"))
    })
    r <- gextract("test.tmptrack", .misha$ALLGENOME)
    expect_regression(r, "track.import_gmax_option")
})
