###################################
### clrng global options  ###
###################################

.onLoad <- function(libname, pkgname) {
  # Set global options with default values
  options(clrng.Nglobal = c(16, 8))
  
  if (!(currentDevice()$device_type %in% c('cpu', 'gpu'))) {
    options(clrng.type = 'float')
  } else {
    options(clrng.type = c('float', 'double')[1 + gpuR::deviceHasDouble()])
  }
  
}


# Create package environment
clrng_env <<- new.env(parent = emptyenv())
ns <- asNamespace('clrng')
assign("clrng_env", clrng_env, envir = ns)

# the point of this file is to create empty functions 
# corresponding to names in the assertive package.
# if the assertive package becomes available again 
# these functions will be removed
# assertive.numbers 
invisible(mapply(function(xx) assign(xx, function(...){}, pos=clrng_env), 
                 xx = 
                   c( paste0('assert_', 
                             c('all_are_in_closed_range', 'assert_all_are_positive', 'all_are_in_range(')),
                      
                      #assertive.types 
                      paste0('assert_is_', c('scalar', 'matrix', 'numeric', 'a_bool', 'a_number')),
                      
                      # assertive.properties
                      'assert_is_of_length',
                      # assertive.base
                      c('get_name_in_parent', 'assert_engine', 'assert_all_are_true')
                   )))






