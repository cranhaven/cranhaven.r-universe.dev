#' @import utils


.onAttach <- function(libname, pkgname) {
  
  desc  <- packageDescription(pkgname, libname)
  
  packageStartupMessage(
    
    'Version:  ', desc$Version, '\n',
    
    'aMNLFA is a collaborative project which aims to help researchers implement \n moderated nonlinear factor analysis (MNLFA) through a series of automated steps \n outlined by Gottfredson et al. (2019). \n',
    
    'Please note: This package generates TEMPLATES for Mplus inputs, which can and should be \n inspected, altered, and run by the user.', 
    
    '\n In addition to being presented without warranty of any kind, \n the package is provided under the assumption that everyone who uses it is  \n reading, interpreting, understanding, and altering every Mplus input and output file. \n', 
    
    'There is no one right way to implement MNLFA, and this package exists solely to \n save users time as they generate Mplus syntax according to their own judgment.',
    
    appendLF = FALSE
    
  )
}