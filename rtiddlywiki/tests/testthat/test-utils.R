test_that("split_field", {
    expect_equal(split_field(""), NULL)
    expect_equal(split_field(NULL), NULL)
    expect_equal(split_field(" [[tag 1]] tag2"), c("tag 1", "tag2"))
    expect_equal(split_field(" [[tag 1]] tag2 "), c("tag 1", "tag2"))
    expect_equal(split_field("[[tag 1]] tag2"), c("tag 1", "tag2"))
    expect_equal(split_field("tag0 [[tag 1]] tag2"), c("tag0", "tag 1", "tag2"))
    expect_equal(split_field("[[tag 1 & 2]] tag2 [tag] [[[[tag 4]]]]"),
                 c("tag 1 & 2", "tag2", "[tag]", "[[tag 4]]"))
    expect_equal(split_field("[[[[tag 4]]]] [[tag 1 & 2]] tag2 [tag]"),
                 c("[[tag 4]]", "tag 1 & 2", "tag2", "[tag]"))

    expect_equal(split_field(" [[tag 1 & 2]] tag2"), c("tag 1 & 2", "tag2"))

})


test_that("save_base64", {
    library(ggplot2)
    p <- cars |>
        ggplot() +
        geom_point(aes(speed, dist))
    expect_no_error(p_base64 <- p |> save_base64())
    expect_length(p_base64, 1)
    expect_true(grepl("data:image/png;base64", p_base64))

    plot <- function() {
        print(p, vp = grid::viewport(0.5, 0.25, 1, 0.5))
        print(p, vp = grid::viewport(0.5, 0.75, 1, 0.5))
    }
    expect_no_error(p_base64 <- plot |> save_base64())
    expect_length(p_base64, 1)
    expect_true(grepl("data:image/png;base64", p_base64))

    expect_no_error(p_base64 <- p |> save_base64(width = 4))
    expect_no_error(p_base64 <- p |> save_base64(height = 4, dpi = 100))
    expect_error(p_base64 <- p |> save_base64(height = "e", dpi = "error"))
})




test_that("tw_table", {
    t_table <- cars |>
        dplyr::slice(1:10) |>
        tw_table()
    expect_length(t_table, 1)
    expect_true(grepl("speed", t_table))
})


