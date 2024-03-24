
.onAttach <- function(libname, pkgname) {
  out <- paste0(
    'Please cite "plot3logit" in publications by using: \n\n',
    '   Santi F., M. M. Dickson, G. Espa, D. Giuliani (2022).',
    ' "plot3logit: Ternary Plots for Interpreting Trinomial Regression Models."',
    ' _Journal of Statistical Software, Code Snippets_, *103*(1), 1-27.',
    ' doi:10.18637/jss.v103.c01.\n\n',
    'Type `citation("', pkgname, '")` for BibTeX entry.'
  )
  
  out2 <- paste0(
    'Please, remember to cite "', pkgname, '": ',
    'run `citation("', pkgname,'")` for further info.'
  )
  
  packageStartupMessage(out)
}

