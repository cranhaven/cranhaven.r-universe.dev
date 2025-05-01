.onLoad <- function(libname, pkgname) {
  env <- asNamespace(pkgname)
  assign("DEFAULT_cutter", jiebaR::worker(write = FALSE), envir = env)
  assign("DEFAULT_control1", list(wordLengths = c(1, 25), tokenizer = NLP::as.Token_Tokenizer(NLP::Regexp_Tokenizer("\\s", invert = TRUE))), envir = env)
  assign("DEFAULT_control2", list(wordLengths = c(2, 25), tokenizer = NLP::as.Token_Tokenizer(NLP::Regexp_Tokenizer("\\s", invert = TRUE))), envir = env)
  # trystringistatus <- tryCatch(
  #   expr = {
  #     ifelse(is.function(stringi::stri_enc_detect2), 1, 0)
  #   }, 
  #   error = function(e){
  #     return(0)
  #   }
  # )
  # assign("stringistatus", trystringistatus, envir = env)
  onload_right_locale <- tryCatch(
	expr = {
	  ifelse(.Platform$OS.type == "windows", "Chinese (Simplified)_China.936", "zh_CN.UTF-8")
	}, 
	error = function(e){
	  return(NA)
	}
  )
  op <- options()
  CHI_MISC_OPT <- list(tmp_chi_locale = onload_right_locale)
  CHI_MISC_OPT_name <- names(CHI_MISC_OPT)
  toset <- !(CHI_MISC_OPT_name %in% names(op))
  if(any(toset)) options(CHI_MISC_OPT[toset])
} 
