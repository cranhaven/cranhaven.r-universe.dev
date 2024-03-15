test_that("get readonly attributes of gdb", {
    gdb.set_readonly_attrs(c("created.by", "created.date"))
    r <- gdb.get_readonly_attrs()
    expect_regression(r, "gdb.get_readonly_attrs")
})

test_that("set and revert readonly attributes of gdb", {
    gdb.set_readonly_attrs(c("created.by", "created.date"))
    old_ro_attrs <- gdb.get_readonly_attrs()
    expect_error(gdb.set_readonly_attrs(c(old_ro_attrs, "")))
    gdb.set_readonly_attrs(old_ro_attrs)
    expect_equal(gdb.get_readonly_attrs(), old_ro_attrs)
})

test_that("set NULL and revert readonly attributes", {
    gdb.set_readonly_attrs(c("created.by", "created.date"))
    old_ro_attrs <- gdb.get_readonly_attrs()
    gdb.set_readonly_attrs(NULL)
    expect_null(gdb.get_readonly_attrs())
    gdb.set_readonly_attrs(old_ro_attrs)
    expect_equal(gdb.get_readonly_attrs(), old_ro_attrs)
})

test_that("append and revert readonly attributes", {
    gdb.set_readonly_attrs(c("created.by", "created.date"))
    old_ro_attrs <- gdb.get_readonly_attrs()
    gdb.set_readonly_attrs(c(old_ro_attrs, "testattr1", "testattr2"))
    expect_equal(gdb.get_readonly_attrs(), union(old_ro_attrs, c("testattr1", "testattr2")))
    gdb.set_readonly_attrs(old_ro_attrs)
    expect_equal(gdb.get_readonly_attrs(), old_ro_attrs)
})

test_that("get attributes of track", {
    gdb.set_readonly_attrs(c("created.by", "created.date"))
    expect_equal(gtrack.attr.get("test.fixedbin", "created.by"), "gtrack.create(test.fixedbin, misha.AA, iterator=NULL)")
    expect_equal(gtrack.attr.get("test.fixedbin", "blablabla"), "")
})

test_that("set and get attribute after removing from readonly", {
    gdb.set_readonly_attrs(c("created.by", "created.date"))
    attrs <- gdb.get_readonly_attrs()
    gdb.set_readonly_attrs(c(attrs, "testattr1"))
    attrs <- attrs[attrs != "testattr1"]
    gdb.set_readonly_attrs(attrs)
    gtrack.attr.set("test.fixedbin", "testattr1", "value")
    r <- gtrack.attr.get("test.fixedbin", "testattr1")
    expect_equal(r, "value")
})

test_that("set, reset and export attribute after removing from readonly", {
    gdb.set_readonly_attrs(c("created.by", "created.date"))
    attrs <- gdb.get_readonly_attrs()
    gdb.set_readonly_attrs(c(attrs, "testattr1"))
    attrs <- attrs[attrs != "testattr1"]
    gdb.set_readonly_attrs(attrs)
    gtrack.attr.set("test.fixedbin", "testattr1", "value")
    gtrack.attr.set("test.fixedbin", "testattr1", "")
    r <- gtrack.attr.export("test.fixedbin")
    expect_regression(r, "gtrack.attr.export_after_reset")
})

test_that("export track attributes without parameters", {
    gdb.set_readonly_attrs(c("created.by", "created.date"))
    r <- gtrack.attr.export()
    expect_regression(r, "gtrack.attr.export_no_params")
})

test_that("export attributes of non-existing track", {
    gdb.set_readonly_attrs(c("created.by", "created.date"))
    expect_error(gtrack.attr.export("blablablatrack"))
})

test_that("export specific attributes of tracks", {
    gdb.set_readonly_attrs(c("created.by", "created.date"))
    r1 <- gtrack.attr.export(attrs = c("created.by"))
    r2 <- gtrack.attr.export(attrs = c("created.by", "created.date"))
    r3 <- gtrack.attr.export(c("test.fixedbin", "test.sparse"))
    r4 <- gtrack.attr.export(c("test.fixedbin", "test.sparse"), attrs = c("created.by", "created.date"))

    expect_regression(r1, "gtrack.attr.export_created.by")
    expect_regression(r2, "gtrack.attr.export_created.by_date")
    expect_regression(r3, "gtrack.attr.export_two_tracks")
    expect_regression(r4, "gtrack.attr.export_two_tracks_specific_attrs")
})

test_that("export after importing modified attributes", {
    gdb.set_readonly_attrs(c("created.by", "created.date"))
    attrs <- gdb.get_readonly_attrs()
    gdb.set_readonly_attrs(c(attrs, "testattr1"))
    attrs <- attrs[attrs != "testattr1"]
    gdb.set_readonly_attrs(attrs)

    r1 <- gtrack.attr.export()
    r1$testattr1 <- 1:dim(r1)[1]
    gtrack.attr.import(r1)
    r2 <- gtrack.attr.export()

    expect_regression(r2, "gtrack.attr.export_after_import")
})

test_that("import attributes without replacing existing ones", {
    gdb.set_readonly_attrs(c("created.by", "created.date"))
    attrs <- gdb.get_readonly_attrs()
    gdb.set_readonly_attrs(c(attrs, "testattr1"))
    attrs <- attrs[attrs != "testattr1"]
    gdb.set_readonly_attrs(attrs)

    r1 <- gtrack.attr.export()
    r1$testattr1 <- 1:dim(r1)[1]
    gtrack.attr.import(r1)
    r1$testattr1 <- NULL
    gtrack.attr.import(r1)
    r2 <- gtrack.attr.export()

    expect_regression(r2, "gtrack.attr.export_without_replace")
})

test_that("import attributes with replacing existing ones", {
    gdb.set_readonly_attrs(c("created.by", "created.date"))
    attrs <- gdb.get_readonly_attrs()
    gdb.set_readonly_attrs(c(attrs, "testattr1"))
    attrs <- attrs[attrs != "testattr1"]
    gdb.set_readonly_attrs(attrs)

    r1 <- gtrack.attr.export()
    r1$testattr1 <- 1:dim(r1)[1]
    gtrack.attr.import(r1)
    r1$testattr1 <- NULL
    gtrack.attr.import(r1)
    r2 <- gtrack.attr.export()

    expect_regression(r2, "gtrack.attr.export_with_replace")
})

test_that("import selected track attributes", {
    gdb.set_readonly_attrs(c("created.by", "created.date"))
    attrs <- gdb.get_readonly_attrs()
    gdb.set_readonly_attrs(c(attrs, "testattr1"))
    attrs <- attrs[attrs != "testattr1"]
    gdb.set_readonly_attrs(attrs)

    r1 <- gtrack.attr.export()
    r1$testattr1 <- dim(r1)[1]:1
    r1 <- r1[c("test.fixedbin", "test.sparse"), ]
    gtrack.attr.import(r1)
    r2 <- gtrack.attr.export()

    expect_regression(r2, "gtrack.attr.export_selected_tracks")
})

test_that("import with testattr1 in readonly attributes", {
    gdb.set_readonly_attrs(c("created.by", "created.date"))
    attrs <- gdb.get_readonly_attrs()
    gdb.set_readonly_attrs(c(attrs, "testattr1"))
    attrs <- attrs[attrs != "testattr1"]
    gdb.set_readonly_attrs(c(attrs, "testattr1"))

    r1 <- gtrack.attr.export()
    r1$testattr1 <- 10 + (dim(r1)[1]:1)
    expect_error(gtrack.attr.import(r1))
})
