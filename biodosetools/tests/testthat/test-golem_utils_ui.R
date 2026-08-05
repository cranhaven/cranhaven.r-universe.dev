expect_tag_has_display_none <- function(tag, has_display) {
  expect_s3_class(tag, "shiny.tag")

  if (has_display) {
    expect_match(as.character(tag), 'display: none;')
  } else {
    expect_no_match(as.character(tag), 'display: none;')
  }
}

test_that("Test with_red_star works", {
  expect_s3_class(with_red_star("golem"), "shiny.tag")
  expect_equal(
    as.character(with_red_star("Enter your name here")),
    '<span>Enter your name here<span style="color:red">*</span></span>'
  )
})

test_that("Test list_to_li works", {
  expect_s3_class(list_to_li(c("a", "b")), "shiny.tag.list")
  expect_equal(
    as.character(list_to_li(c("a", "b"))),
    "<li>a</li>\n<li>b</li>"
  )
  expect_equal(
    as.character(list_to_li(c("a", "b"), class = "my_li")),
    '<li class="my_li">a</li>\n<li class="my_li">b</li>'
  )
})

test_that("Test list_to_p works", {
  expect_s3_class(
    list_to_p(c(
      "This is the first paragraph",
      "this is the second paragraph"
    )),
    "shiny.tag.list"
  )
  expect_equal(
    as.character(
      list_to_p(c(
        "This is the first paragraph",
        "this is the second paragraph"
      ))
    ),
    "<p>This is the first paragraph</p>\n<p>this is the second paragraph</p>"
  )
  expect_equal(
    as.character(
      list_to_p(
        c(
          "This is the first paragraph",
          "this is the second paragraph"
        ),
        class = "my_li"
      )
    ),
    '<p class="my_li">This is the first paragraph</p>\n<p class="my_li">this is the second paragraph</p>'
  )
})

test_that("Test named_to_li works", {
  expect_s3_class(named_to_li(list(a = "a", b = "b")), "shiny.tag.list")
  expect_equal(
    as.character(named_to_li(list(a = "a", b = "b"))),
    "<li><b>a:</b> a</li>\n<li><b>b:</b> b</li>"
  )
  expect_equal(
    as.character(named_to_li(list(a = "a", b = "b"), class = "mylist")),
    '<li class="mylist"><b>a:</b> a</li>\n<li class="mylist"><b>b:</b> b</li>'
  )
})

test_that("Test tagRemoveAttributes works", {
  a_with_tag <- shiny::tags$p(src = "plop", "pouet")
  expect_s3_class(a_with_tag, "shiny.tag")
  expect_equal(
    as.character(a_with_tag),
    '<p src="plop">pouet</p>'
  )

  a_without_tag <- tagRemoveAttributes(a_with_tag, "src")
  expect_s3_class(a_without_tag, "shiny.tag")
  expect_equal(
    as.character(a_without_tag),
    "<p>pouet</p>"
  )
})

test_that("Test undisplay works", {
  a <- shiny::tags$p(src = "plop", "pouet")
  expect_tag_has_display_none(a, FALSE)
  expect_tag_has_display_none(undisplay(a), TRUE)

  b <- shiny::actionButton("go_filter", "go")
  expect_tag_has_display_none(b, FALSE)
  expect_tag_has_display_none(undisplay(b), TRUE)
})

test_that("Test display works", {
  a_undisplay <- shiny::tags$p(src = "plop", "pouet", style = "display: none;")
  expect_tag_has_display_none(a_undisplay, TRUE)
  expect_tag_has_display_none(display(a_undisplay), FALSE)
})

test_that("Test jq_hide works", {
  expect_s3_class(jq_hide("golem"), "shiny.tag")
  expect_equal(
    as.character(jq_hide("golem")),
    "<script>$('#golem').hide()</script>"
  )
})

test_that("Test rep_br works", {
  expect_s3_class(rep_br(5), "html")
  expect_equal(
    as.character(rep_br(5)),
    "<br/> <br/> <br/> <br/> <br/>"
  )
})

test_that("Test enurl works", {
  expect_s3_class(enurl("https://www.thinkr.fr", "ThinkR"), "shiny.tag")
  expect_equal(
    as.character(enurl("https://www.thinkr.fr", "ThinkR")),
    '<a href="https://www.thinkr.fr">ThinkR</a>'
  )
})

test_that("Test columns wrappers works", {
  expect_s3_class(col_12(), "shiny.tag")
  expect_s3_class(col_10(), "shiny.tag")
  expect_s3_class(col_8(), "shiny.tag")
  expect_s3_class(col_6(), "shiny.tag")
  expect_s3_class(col_4(), "shiny.tag")
  expect_s3_class(col_3(), "shiny.tag")
  expect_s3_class(col_2(), "shiny.tag")
  expect_s3_class(col_1(), "shiny.tag")

  expect_equal(as.character(col_12()), '<div class="col-sm-12"></div>')
  expect_equal(as.character(col_10()), '<div class="col-sm-10"></div>')
  expect_equal(as.character(col_8()), '<div class="col-sm-8"></div>')
  expect_equal(as.character(col_6()), '<div class="col-sm-6"></div>')
  expect_equal(as.character(col_4()), '<div class="col-sm-4"></div>')
  expect_equal(as.character(col_3()), '<div class="col-sm-3"></div>')
  expect_equal(as.character(col_2()), '<div class="col-sm-2"></div>')
  expect_equal(as.character(col_1()), '<div class="col-sm-1"></div>')
})
