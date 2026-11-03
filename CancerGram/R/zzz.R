.onAttach <- function(libname, pkgname) {
  options(CancerGram_suppress_prompt = FALSE)
  if (!is_CancerGramModel_installed())
    packageStartupMessage("To be able to use CancerGram properly, you should install 'CancerGramModel' package available via GitHub. You can do it by calling 'install_CancerGramModel()'")
}
