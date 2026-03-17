
#' @importFrom parallel detectCores
.onLoad <- function(libname, pkgname) {
  
  Sys.setenv(
    '_R_CHECK_LIMIT_CORES_' = 'false',
    # otherwise ?parallel:::.check_ncores causes error when ?devtools::check the quarto vignette (up to R 4.5.1)
    
    '_R_CHECK_SYSTEM_CLOCK_' = 0
    # https://stackoverflow.com/questions/63613301/r-cmd-check-note-unable-to-verify-current-time
    # to disable "checking for future file timestamps" in R Check
  )
  
  options(
    
    mc.cores = switch(
      EXPR = .Platform$OS.type, # as of R 4.5, only two responses, 'windows' or 'unix'
      windows = 1L, 
      unix = detectCores()
    )
    # read this discussion very very carefully!  Especially Dirk's reply!!!!
    # https://github.com/Rdatatable/data.table/issues/5658
    
  )
  
}

