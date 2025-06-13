compileModel <- function() {

  # check the compiled file extensions
  #   Platform    filename extension
  #   Windoes     .dll
  #   Unix        .so  
  ext <- .Platform$dynlib.ext
  compiledModel <- paste0('model', ext)
  
  if (.Platform$OS.type != "windows"){
    temp_compiled_model <- paste0(tempdir(),'/',compiledModel)
  } else { 
    temp_compiled_model <- paste0(tempdir(),'\\',compiledModel)
    temp_compiled_model = gsub('\\\\','/', temp_compiled_model)
  }
  
  # if library is already loaded unload it to make compilation possible
  if (is.loaded('derivsc')) {
    dyn.unload(temp_compiled_model)
  }
  
  # compile
  callr::rcmd_safe(cmd ="SHLIB", cmdargs = "model.c", wd = tempdir(), stderr = "2>&1")
  
  # load
  dyn.load(temp_compiled_model)
  
  return(temp_compiled_model)
  
}