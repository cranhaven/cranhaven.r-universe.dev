test_that("tabs", {
    tab_labels <- c("Tab1", "Tab2", "Tab3")

    tab_content_fun <- function(i, extra_text = "") {
        htmltools::tagList(
            htmltools::tags$p(paste("Content for tab:", tab_labels[i], extra_text)),
            htmltools::tags$img(src = paste0("plot_", i, ".png"), width = "100%")
        )
    }

    html <- tabs(tab_labels, tab_content_fun, checked = 2, extra_text = "Additional details")

    expect_equal(length(html), 3)
})



test_that("create_tabs handles ggplot and data.frame objects", {
    library(ggplot2)

    p <- ggplot(cars) +
        geom_point(aes(x = speed, y = dist))
    df <- head(cars)

    result <- create_tabs(
        plot = p,
        table = df
    )

    expect_s3_class(result, "shiny.tag") # htmltools::tag is S3 class shiny.tag
    expect_true(grepl("plot", as.character(result)))
    expect_true(grepl("table", as.character(result)))
})

test_that("create_tabs handles argument list format correctly", {
    library(ggplot2)

    p <- ggplot(cars) +
        geom_point(aes(x = speed, y = dist))
    df <- head(cars)

    result <- create_tabs(
        plot = list(object = p, width = 4),
        table = list(object = df)
    )

    html_out <- as.character(result)

    expect_true(grepl("table", html_out))
})

test_that("create_tabs throws error on unnamed arguments", {
    expect_error(create_tabs(1, 2), "All tabs must be named")
})

test_that("create_tabs throws error on unsupported object type", {
    expect_error(
        create_tabs(text = list(object = "string")),
        "Unsupported object type"
    )
})
