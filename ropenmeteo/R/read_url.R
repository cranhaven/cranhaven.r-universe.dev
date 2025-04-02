read_url <- function(url_base, url_path){


  out <- tryCatch({
    out <- httr2::request(url_base) |>
      httr2::req_url_path_append(url_path) |>
      httr2::req_throttle(10 / 60, realm = url_base) |>
      httr2::req_perform() |>
      httr2::resp_body_json(simplifyVector = TRUE)
  }, error = function(err) {
    out <- jsonlite::read_json(file.path(url_base,url_path), simplifyVector = TRUE)
    return(out)
  }, finally = {
    NULL
  })

}
