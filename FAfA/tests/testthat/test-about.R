test_that("About page summarizes the main version 1.2 additions", {
  html <- as.character(about_ui("about"))
  expect_match(html, "about-whats_new_html", fixed = TRUE)
  expect_match(html, "What's New in FAfA", fixed = TRUE)

  shiny::testServer(about_server, {
    content <- output[["whats_new_html"]][["html"]]
    expect_match(content, "Projects and reproducibility", fixed = TRUE)
    expect_match(content, "Dynamic Fit Index", fixed = TRUE)
    expect_match(content, "Bootstrap Exploratory Graph Analysis", fixed = TRUE)
    expect_match(content, "APA 7 Word", fixed = TRUE)
  })
})
