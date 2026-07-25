.onAttach <- function(...) {
    packageStartupMessage(
        '*****************************************************************'
    )
    packageStartupMessage(
        '* Loaded iNZightRegression                                      *')
    packageStartupMessage(
        '*                                                               *')
    packageStartupMessage(
        '* Methods imported from \'iNZightPlots\':                         *')
    packageStartupMessage(
        '* - use `inzplot()` for diagnostic plots of model objects       *')
    packageStartupMessage(
        '* - use `inzsummary()` for a summary of model objects           *')
    packageStartupMessage(
        '*****************************************************************'
    )
}
