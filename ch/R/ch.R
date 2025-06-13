

#' ch: some small functions
#### ch-package Description ####
#' @description It's about some functions in chem and other questions.
#' @docType package
#' @importFrom crayon blue yellow cyan
#' @name ch-package
NULL


#### Package-Loading Information ####
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    yellow("Welcome to ch package\n"),
    blue("Frequently functions about ch:\n"),
    cyan("   show_color(), ggpng(), plot_table()\n"),
    cyan("   showruler(), period_table(), symb()\n"),
    cyan("   csv2vcf(), lat_fmt(), ground_state()\n"),
    cyan("   state_1(), state_2(), sym2poly()\n")
  )
}
