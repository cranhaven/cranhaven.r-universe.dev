test_that("uuid_v7 returns valid UUID strings", {
    x = uuid_v7(5)

    expect_type(x, "character")
    expect_length(x, 5)
    expect_true(all(uuid_validate(x)))
    expect_true(all(uuid_version(x) == 7L))
})

test_that("uuid_v4 returns valid UUID strings", {
    x = uuid_v4(5)

    expect_type(x, "character")
    expect_length(x, 5)
    expect_true(all(uuid_validate(x)))
    expect_true(all(uuid_version(x) == 4L))
})

test_that("uuid_generate defaults to version 7", {
    x = uuid_generate(3)

    expect_length(x, 3)
    expect_true(all(uuid_validate(x)))
    expect_true(all(uuid_version(x) == 7L))
})

test_that("uuid_validate distinguishes valid and invalid values", {
    x = c(uuid_v7(), "not-a-uuid", NA_character_)
    expect_identical(uuid_validate(x), c(TRUE, FALSE, FALSE))
})

test_that("uuid_version reports version numbers and NA for invalid inputs", {
    x = c(uuid_v1(), uuid_v4(), uuid_v6(), uuid_v7(), "not-a-uuid", NA_character_)
    expect_identical(uuid_version(x), c(1L, 4L, 6L, 7L, NA_integer_, NA_integer_))
})

test_that("uuid_v5 is deterministic for the same namespace and name", {
    x = uuid_v5("dns", c("example.com", "example.net"))
    y = uuid_v5("dns", c("example.com", "example.net"))

    expect_identical(x, y)
    expect_true(all(uuid_version(x) == 5L))
})

test_that("uuid_parse raw output contains 16-byte raw vectors", {
    x = uuid_parse(uuid_v4(3), output = "raw")

    expect_type(x, "list")
    expect_identical(lengths(x), rep(16L, 3L))
    expect_true(all(vapply(x, typeof, character(1)) == "raw"))
})

test_that("uuid_nil and uuid_max behave correctly", {
    expect_identical(uuid_nil(), "00000000-0000-0000-0000-000000000000")
    expect_identical(uuid_max(), "ffffffff-ffff-ffff-ffff-ffffffffffff")
    expect_type(uuid_nil(output = "raw"), "raw")
    expect_type(uuid_max(output = "raw"), "raw")
    expect_length(uuid_nil(output = "raw"), 16L)
    expect_length(uuid_max(output = "raw"), 16L)
})

test_that("vectorized version 5 inputs recycle cleanly", {
    x = uuid_v5(namespace = "dns", name = c("alpha", "beta", "gamma"))

    expect_length(x, 3)
    expect_true(all(uuid_validate(x)))
    expect_true(all(uuid_version(x) == 5L))
})

test_that("uuid_parse fields output is a structured data frame", {
    x = uuid_parse(uuid_v7(2), output = "fields")

    expect_s3_class(x, "data.frame")
    expect_true("uuid_fields" %in% class(x))
    expect_identical(
        names(x),
        c(
            "canonical",
            "version",
            "variant",
            "time_low",
            "time_mid",
            "time_hi_and_version",
            "clock_seq_hi_and_reserved",
            "clock_seq_low",
            "node"
        )
    )
    expect_true(all(x$version == 7L))
})

test_that("invalid version 5 inputs fail clearly", {
    expect_error(
        uuid_v5(namespace = "not-a-uuid", name = "alpha"),
        "`namespace` must be a UUID string"
    )

    expect_error(
        uuid_v5(namespace = c("dns", "url"), name = c("alpha", "beta", "gamma")),
        "common length"
    )

    expect_error(
        uuid_generate(version = "v5", namespace = "dns", name = "alpha", n = 2),
        "only used for versions 1, 4, 6, and 7"
    )
})
