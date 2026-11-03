.onAttach <- function(libname, pkgname) {
  options(AmpGram_suppress_prompt = FALSE)
  if (!is_AmpGramModel_installed())
    packageStartupMessage("To be able to use AmpGram properly, you should install 'AmpGramModel' package available via GitHub. You can do it by calling 'install_AmpGramModel()'")
}
