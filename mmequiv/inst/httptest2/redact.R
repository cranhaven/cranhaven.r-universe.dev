function (response) {
  response |>
    # shorten URL
    httptest2::gsub_response("https://research-mme.wakehealth.edu/", "", fixed = TRUE)
}