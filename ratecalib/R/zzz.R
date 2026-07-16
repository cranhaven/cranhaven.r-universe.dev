# Startup notice. Kept ASCII per the project's English-only R-code rule; the
# full bilingual disclaimer ships in inst/DISCLAIMER.md.
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "ratecalib: calibration-weighting tool. Provided AS IS, without warranty.\n",
    "Use it for legitimate estimation with honest disclosure of methods and ",
    "target sources.\n",
    "Calibrating to arbitrary or desired targets and presenting the results as ",
    "independent findings is data fabrication and academic misconduct.\n",
    "The user is solely responsible for lawful and ethical use. ",
    "See the full disclaimer:\n",
    "  system.file(\"DISCLAIMER.md\", package = \"ratecalib\")"
  )
}
