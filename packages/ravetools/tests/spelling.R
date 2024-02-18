if(system.file(package = "spelling") != '') {
  spelling <- asNamespace('spelling')
  tryCatch({
    spelling$spell_check_test(vignettes = TRUE, error = FALSE,
                              skip_on_cran = TRUE)
  }, error = function(...){})
}
