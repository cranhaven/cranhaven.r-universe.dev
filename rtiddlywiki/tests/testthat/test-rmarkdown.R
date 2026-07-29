
render_rmd <- function(..., output_file = NULL, .env = parent.frame()) {
    temp_input_file <- tempfile(fileext = ".Rmd")
    text <- c(...)
    writeLines(text, temp_input_file)
    if (is.null(output_file)) {
        temp_output_file <- paste0(tools::file_path_sans_ext(temp_input_file), ".json")
    } else {
        temp_output_file <- paste0(tools::file_path_sans_ext(output_file), ".json")
    }
    rmarkdown::render(temp_input_file,
        output_file = output_file,
        quiet = TRUE
    )
    r <- jsonlite::read_json(temp_output_file)
    file.remove(temp_output_file, temp_input_file)
    r
}
# Helper functions for rmarkdown tests
skip_if_not_pandoc <- function() {
    if (!rmarkdown::pandoc_available()) {
        skip("Pandoc is not available")
    }
}

test_that(".is_valid_url", {
    expect_false(.is_valid_url("test"))
    expect_true(.is_valid_url("http://example.com"))
    expect_true(.is_valid_url("https://example.com/"))
    expect_true(.is_valid_url("https://example.com/a"))
})




test_that("Get title", {
    expect_null(.get_title("test"))
    expect_equal(.get_title("title: test"), "test")
    expect_equal(.get_title("title: \"test\""), "test")
    expect_equal(.get_title("title: \"this is a test\""), "this is a test")
    expect_equal(
        .get_title("title: \"this is a \"test\"\""),
        "this is a \"test\""
    )
})


test_that("pretty link", {
    text <- c(
        "See this \\[\\[This is a test\\]\\], link again",
        "this is a new line",
        "this line has \\[\\[link one\\]\\] and \\[\\[label|link two\\]\\]"
    )
    text2 <- .pretty_link(text)
    expect_equal(text2[1], "See this [[This is a test]], link again")
    expect_equal(text2[2], "this is a new line")
    expect_equal(text2[3], "this line has [[link one]] and [[label\\|link two]]")
})

expect_equal(
    .pretty_link("no link here"),
    "no link here"
)

expect_equal(
    .pretty_link("x \\[\\[A\\]\\] y \\[\\[B|C\\]\\] z"),
    "x [[A]] y [[B\\|C]] z"
)

expect_equal(
    .pretty_link("this \\[\\[a\\|b\\]\\] test"),
    "this [[a\\|b]] test"
)

expect_equal(
    .pretty_link("see \\[\\[测试|test\\]\\]"),
    "see [[测试\\|test]]"
)

test_that("markdown link", {
    text <- c(
        "See this [This is a link](), link again",
        "this is a new line",
        "this line has [link one](#link 1) and [link two](#link2)",
        "[a mornal link](https://example.org)"
    )
    text2 <- .markdown_link(text)
    expect_equal(text2[1], "See this [This is a link](), link again")
    expect_equal(text2[2], "this is a new line")
    expect_equal(text2[3], "this line has [link one](#link%201) and [link two](#link2)")
    expect_equal(text2[4], "[a mornal link](https://example.org)")
})



test_that("rmarkdown renders with output file specified", {
    skip_on_cran()
    skip_if_not_pandoc()
    
    output_file <- tempfile()
    rmd <- render_rmd(c(
        "---", "title: \"test\"",
        "output: ",
        "  tiddler_document:",
        "    tags: [\"tag1\", \"tag 2\"]",
        "---", "",
        "# Section 1",
        "This is a test"
    ), output_file = output_file)
    
    expect_true(!is.null(rmd))
})

test_that("rmarkdown renders with quoted title and tags", {
    skip_on_cran()
    skip_if_not_pandoc()
    
    rmd <- render_rmd(c(
        "---", "title: \"test\"",
        "output: ",
        "  tiddler_document:",
        "    tags: [\"tag1\", \"tag 2\"]",
        "---", "",
        "# Section 1",
        "This is a test"
    ))
    expect_equal(rmd$tags, "[[tag1]] [[tag 2]]")
    expect_equal(rmd$title, "test")
})

test_that("rmarkdown renders with unquoted title", {
    skip_on_cran()
    skip_if_not_pandoc()
    
    rmd <- render_rmd(c(
        "---", "title: test",
        "output: ",
        "  tiddler_document:",
        "    tags: [\"tag1\", \"tag 2\"]",
        "---", "",
        "# Section 1",
        "This is a test"
    ))
    expect_equal(rmd$tags, "[[tag1]] [[tag 2]]")
    expect_equal(rmd$title, "test")
})

test_that("rmarkdown renders with bookdown options", {
    skip_on_cran()
    skip_if_not_pandoc()
    
    rmd <- render_rmd(c(
        "---", "title: test",
        "output: ",
        "  tiddler_document:",
        "    tags: [\"tag1\", \"tag 2\"]",
        "    use_bookdown: true",
        "    number_sections: false",
        "---", "",
        "# Section 1",
        "This is a test"
    ))
    expect_equal(rmd$tags, "[[tag1]] [[tag 2]]")
    expect_equal(rmd$title, "test")
    expect_equal(rmd$type, "text/x-markdown")
})

test_that("rmarkdown preserves tiddlywiki double bracket links", {
    skip_on_cran()
    skip_if_not_pandoc()
    
    rmd <- render_rmd(c(
        "---", "title: \"test\"",
        "output: ",
        "  tiddler_document:",
        "    tags: [\"tag1\", \"tag 2\"]",
        "---", "",
        "[[This is a test]]"
    ))
    expect_equal(rmd$text, "[[This is a test]]")
})



test_that("rmarkdown preserves tiddlywiki double bracket links", {
    skip_on_cran()
    skip_if_not_pandoc()
    
    rmd <- render_rmd(c(
        "---", "title: \"test\"",
        "output: ",
        "  tiddler_document:",
        "    tags: [\"tag1\", \"tag 2\"]",
        "---", "",
        "[[This is a test|tiddler]]"
    ))
    expect_equal(rmd$text, "[[This is a test\\|tiddler]]")
})

test_that("escaped pipe is preserved", {
    skip_on_cran()
    skip_if_not_pandoc()

    rmd <- render_rmd(c(
        "---", "title: \"test\"",
        "output:",
        "  tiddler_document:",
        "    tags: [\"tag1\", \"tag 2\"]",
        "---", "",
        "[[A\\|B]]"
    ))

    expect_equal(rmd$text, "[[A\\|B]]")
})

test_that("wikilink inside text is preserved", {
    skip_on_cran()
    skip_if_not_pandoc()

    rmd <- render_rmd(c(
        "---", "title: \"test\"",
        "output:",
        "  tiddler_document:",
        "    tags: [\"tag1\", \"tag 2\"]",
        "---", "",
        "This is [[Test|Tiddler]] in a sentence."
    ))

    expect_equal(
        rmd$text,
        "This is [[Test\\|Tiddler]] in a sentence."
    )
})

test_that("rmarkdown preserves tiddlywiki double bracket links", {
    skip_on_cran()
    skip_if_not_pandoc()
    
    rmd <- render_rmd(c(
        "---", "title: \"test\"",
        "output: ",
        "  tiddler_document:",
        "    tags: [\"tag1\", \"tag 2\"]",
        "---", "",
        "[[This is a test|tiddler]] [[Another tiddler]]"
    ))
    expect_equal(rmd$text, "[[This is a test\\|tiddler]] [[Another tiddler]]")
})



test_that("rmarkdown preserves tiddlywiki transclusion syntax", {
    skip_on_cran()
    skip_if_not_pandoc()
    
    rmd <- render_rmd(c(
        "---", "title: \"test\"",
        "output: ",
        "  tiddler_document:",
        "    tags: [\"tag1\", \"tag 2\"]",
        "---", "",
        "{{This is a test}}"
    ))
    expect_equal(rmd$text, "{{This is a test}}")
})

test_that("rmarkdown encodes markdown links with spaces", {
    skip_on_cran()
    skip_if_not_pandoc()
    
    rmd <- render_rmd(c(
        "---", "title: \"test\"",
        "output: ",
        "  tiddler_document:",
        "    tags: [\"tag1\", \"tag 2\"]",
        "---", "",
        "test [md link](#tiddler 1) and [link](#tiddler2)"
    ))
    expect_equal(rmd$text, "test [md link](#tiddler%201) and [link](#tiddler2)")
})

test_that("rmarkdown renders custom fields", {
    skip_on_cran()
    skip_if_not_pandoc()
    
    rmd <- render_rmd(c(
        "---", "title: \"test\"",
        "output: ",
        "  tiddler_document:",
        "    tags: [\"tag1\", \"tag 2\"]",
        "    fields:",
        "      \"field1\": \"V1\"",
        "      \"field 2\": \"Value 2\"",
        "---", "",
        "{{This is a test}}"
    ))
    expect_equal(rmd$fields$field1, "V1")
    expect_equal(rmd$fields$`field 2`, "Value 2")
})

test_that("rmarkdown embeds ggplot images as base64", {
    skip_on_cran()
    skip_if_not_pandoc()
    skip_if_not_installed("ggplot2")
    
    rmd <- render_rmd(c(
        "---", "title: \"test\"",
        "output: ",
        "  tiddler_document:",
        "    tags: [\"tag1\", \"tag 2\"]",
        "    fields:",
        "      \"field1\": \"V1\"",
        "      \"field 2\": \"Value 2\"",
        "---", "",
        "```{r static-images-1}",
        "",
        "library(ggplot2)",
        "cars |> ",
        "    ggplot() +",
        "    geom_point(aes(speed, dist))",
        "```"
    ))
    expect_equal(grepl("data:image", rmd$text), TRUE)
})

test_that("rmarkdown preserves simple macro syntax", {
    skip_on_cran()
    skip_if_not_pandoc()
    
    rmd <- render_rmd(c(
        "---", "title: \"test\"",
        "output: ",
        "  tiddler_document:",
        "    tags: [\"tag1\", \"tag 2\"]",
        "    fields:",
        "      \"field1\": \"V1\"",
        "      \"field 2\": \"Value 2\"",
        "---", "",
        "",
        "<<macro>>",
        '<<macro p1:"P">>',
        "<<macro ",
        ">>"
    ))
    expect_equal(grepl("<<macro>>", rmd$text), TRUE)
})

test_that("rmarkdown preserves macro with triple quotes", {
    skip_on_cran()
    skip_if_not_pandoc()
    
    rmd <- render_rmd(c(
        "---", "title: \"test\"",
        "output: ",
        "  tiddler_document:",
        "    tags: [\"tag1\", \"tag 2\"]",
        "    fields:",
        "      \"field1\": \"V1\"",
        "      \"field 2\": \"Value 2\"",
        "---", "",
        "",
        '<<alert primary """',
        "This is a test",
        '""">>'
    ))
    expect_equal(grepl('"""', rmd$text), TRUE)
    expect_equal(grepl("<<alert", rmd$text), TRUE)
})

test_that("rmarkdown renders with gfm variant", {
    skip_on_cran()
    skip_if_not_pandoc()
    
    rmd <- render_rmd(c(
        "---", "title: \"test\"",
        "output: ",
        "  tiddler_document:",
        "    tags: [\"tag1\", \"tag 2\"]",
        "    variant: gfm",
        "    fields:",
        "      \"field1\": \"V1\"",
        "      \"field 2\": \"Value 2\"",
        "---", "",
        "",
        '<<alert primary """',
        "This is a test",
        '""">>'
    ))
    expect_equal(grepl('"""', rmd$text), TRUE)
    expect_equal(grepl("<<alert", rmd$text), TRUE)
})

test_that("rmarkdown preserves tiddlywiki widget syntax", {
    skip_on_cran()
    skip_if_not_pandoc()
    
    rmd <- render_rmd(c(
        "---", "title: \"test\"",
        "output: ",
        "  tiddler_document:",
        "    tags: [\"tag1\", \"tag 2\"]",
        "    variant: gfm",
        "    fields:",
        "      \"field1\": \"V1\"",
        "      \"field 2\": \"Value 2\"",
        "---", "",
        "",
        '| <$widget p="C" /> |',
        '| <$widget p="C" /> |',
        '| <$widget p="C" /> |'
    ))
    expect_equal(grepl("<\\$widget", rmd$text), TRUE)
})

test_that("rmarkdown with remote false does not upload to tiddlywiki", {
    skip_on_cran()
    skip_if_not_pandoc()
    skip_if(!is_test_tw())
    skip_if_not_installed("ggplot2")
    
    rmd <- render_rmd(c(
        "---", "title: \"rmarkdown test\"",
        "output: ",
        "  tiddler_document:",
        "    remote: false",
        "---", "",
        "```{r static-images-1}",
        "",
        "library(ggplot2)",
        "cars |> ",
        "    ggplot() +",
        "    geom_point(aes(speed, dist))",
        "```"
    ))
    tiddler <- get_tiddler("rmarkdown test")
    expect_null(tiddler)
})

test_that("rmarkdown with remote true uploads to tiddlywiki", {
    skip_on_cran()
    skip_if_not_pandoc()
    skip_if(!is_test_tw())
    skip_if_not_installed("ggplot2")
    
    rmd <- render_rmd(c(
        "---", "title: \"rmarkdown test\"",
        "output: ",
        "  tiddler_document:",
        "    remote: true",
        sprintf('    host: "%s"', tw_options("host")),
        "    tags: [\"tag1\", \"tag 2\"]",
        "    fields:",
        "      \"field1\": \"V1\"",
        "      \"field 2\": \"Value 2\"",
        "---", "",
        "```{r static-images-1}",
        "",
        "library(ggplot2)",
        "cars |> ",
        "    ggplot() +",
        "    geom_point(aes(speed, dist))",
        "```"
    ))
    tiddler <- get_tiddler("rmarkdown test")
    expect_equal(tiddler$fields$field1, "V1")
    expect_equal(tiddler$fields$`field 2`, "Value 2")
    expect_equal(tiddler$tags, c("tag1", "tag 2"))
    expect_equal(tiddler$title, "rmarkdown test")
    expect_equal(tiddler$type, "text/x-markdown")
    expect_no_error(delete_tiddler("rmarkdown test"))
})

test_that("rmarkdown with remote true and output file uploads to tiddlywiki", {
    skip_on_cran()
    skip_if_not_pandoc()
    skip_if(!is_test_tw())
    skip_if_not_installed("ggplot2")
    
    output_file <- tempfile()
    rmd <- render_rmd(c(
        "---", "title: \"rmarkdown test\"",
        "output: ",
        "  tiddler_document:",
        "    remote: true",
        "    tags: [\"tag1\", \"tag 2\"]",
        "    fields:",
        "      \"field1\": \"V1\"",
        "      \"field 2\": \"Value 2\"",
        "---", "",
        "```{r static-images-1}",
        "",
        "library(ggplot2)",
        "cars |> ",
        "    ggplot() +",
        "    geom_point(aes(speed, dist))",
        "```"
    ), output_file = output_file)
    tiddler <- get_tiddler("rmarkdown test")
    expect_equal(tiddler$fields$field1, "V1")
    expect_equal(tiddler$fields$`field 2`, "Value 2")
    expect_equal(tiddler$tags, c("tag1", "tag 2"))
    expect_equal(tiddler$title, "rmarkdown test")
    expect_equal(tiddler$type, "text/x-markdown")
    expect_no_error(delete_tiddler("rmarkdown test"))
})
