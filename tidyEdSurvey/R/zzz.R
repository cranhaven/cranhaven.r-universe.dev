.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("tidyEdSurvey v", utils::packageDescription("tidyEdSurvey")$Version, "\n",
                               "A package for using 'dplyr' and 'ggplot2' with student level data in an edsurvey.data.frame. To work with teacher or school level data, see ?EdSurvey::getData"))
}