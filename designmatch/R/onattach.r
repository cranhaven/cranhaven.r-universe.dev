.onAttach <- 
  function(libname, pkgname) {
    packageStartupMessage("\nThank you for using our package! To acknowledge our work, please cite the package as: \n")
    packageStartupMessage(" Zubizarreta, Jose R., Kilcioglu, Cinar, Vielma, Juan P., and Cohn, Eric R. (2023). designmatch: Matched Samples that are Balanced and Representative by Design.")
    packageStartupMessage(" R package version 0.5.2. https://cran.r-project.org/web/packages/designmatch/ \n")
  }