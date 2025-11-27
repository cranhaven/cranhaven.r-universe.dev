.onAttach <- function(lib, pkg)  {
    packageStartupMessage("This is eHOF ",
    utils::packageDescription("eHOF", field="Version"), paste(' - build: '),
    utils::packageDate('eHOF'),
    appendLF = TRUE)
    options(eHOF.bootselectmessage = TRUE)
    options(repos = c(CRAN="http://cran.r-project.org"))
}

