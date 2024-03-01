.onAttach <- function(lib, pkg) {
	ver <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
        packageStartupMessage(paste(pkg, ver))
        msg <- paste("\n   This package has undergone SUBSTANTIAL revision",
                     "\n   since the release of version 0.0-6. The syntax",
                     "\n   of most of the functions has changed. (The basic",
                     "\n   function mixreg() now provides the option of using",
                     "\n   formula syntax.)",
                     "\n   ",
                     "\n   Substantial effort has been made to ensure that",
                     "\n   the \"old\" (non-formula) syntax still works.",
                     "\n   However several functions have had their names",
                     "\n   changed, and the argument names have been changed",
                     "\n   in some functions, whence \"old\" syntax will not",
                     "\n   alway work.  Several new functions and some new",
                     "\n   data sets have been added.  The names of the",
                     "\n   variables in the \"old\" data set \"aphids\" have",
                     "\n   been changed.  Users should study the documentation",
                     "\n   thoroughly.  They would also be well advised to read",
                     "\n   the vignette \"mystMix\".\n")
        packageStartupMessage(msg)
}
