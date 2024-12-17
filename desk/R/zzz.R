.onLoad = function(libname, pkgname){
  # packageStartupMessage("\n\n Loading DESK ... \014")
  # set custom options for the package
}

.onAttach <- function(libname, pkgname){
  packageStartupMessage(StartWelcomeMessage())
}

# desk logo in unicode: \U0001f173\U0001f174\U0001f182\U0001f17a
StartWelcomeMessage <- function(){
  paste("\n",
        "======================= desk v",utils::packageDescription("desk")$Version, " =========================\n\n",
        " Didactic Econometrics Starter Kit (desk) is released for", "\n",
        " educational purposes, accompanying the German textbooks\n\n",
        " (1) L. v.Auer (2023) \u00d6konometrie - Eine Einf\u00fchrung, 8th ed.", "\n",
        " (2) L. v.Auer, S. Hoffmann & T. Kranz (2023) \u00d6konometrie - ", "\n",
        "     Das R-Arbeitsbuch, 2nd ed.", "\n\n",
        " More information on these books:\n",
        " ", cli::style_hyperlink("https://oekonometrie-lernen.de", "https://www.uni-trier.de/index.php?id=15929"),"\n\n",
        " REMARK: Users of the 1st ed. of book (2) should install", "\n",
        " and use desk v1.0.x (pre-CRAN release) manually from:", "\n",
        " ", cli::style_hyperlink("https://github.com/OvGU-SH/desk1A/releases", "https://github.com/OvGU-SH/desk1A/releases"),"\n\n",
        "=============================================================",
        sep="")
}

packageStartupMessage(StartWelcomeMessage())

# .onUnload <- function (libpath) {
#   library.dynam.unload("mypackage", libpath)
# }
