## ----echo = FALSE, results = "asis"-------------------------------------------
functions <- jsonlite::read_json("supported-functions.json")

purrr::walk(
  functions,
  function(x) {
    cat(paste("###", x$package))
    cat("\n")

    purrr::walk(
      x$functions,
      function(y) {
        cat(paste("-", y, "\n"))
      }
    )

    cat("\n")

    return(invisible(""))
  }
)

