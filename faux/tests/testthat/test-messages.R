context("test-messages")

test_that("check", {
  expect_message(message("green"), "\033[32mgreen\033[39m", fixed = 1)
  expect_message(message("pipes?", "no!"), "\033[32mpipes?no!\033[39m", fixed = 1)
})

# test_that("not knit", {
#   # renders without green text marker when knitting
#   
#   txt <- "---\ntitle: 'Test'\n---\n\n```{r}\nfaux:::message('hi')\n```"
#   find <- '<pre><code>## hi</code></pre>'
#   
#   write(txt, "tmp.Rmd")
#   rmarkdown::render("tmp.Rmd", quiet = TRUE)
#   html <- readLines("tmp.html")
#   found <- grep(find, html, fixed = TRUE)
#   expect_true(length(found) == 1)
#   
#   # cleanup
#   file.remove("tmp.Rmd")
#   file.remove("tmp.html")
#   
# })
