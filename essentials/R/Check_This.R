if (FALSE) {
    (function() {
        odir <- getwd()
        on.exit(setwd(odir))
        setwd("~")
        pkg <- "essentials"
        command <- sprintf("Rcmd build %s", pkg)
        cat(normalizePath(getwd()), ">", command, "\n\n", sep = "")
        system(command)
        command <- sprintf("Rcmd check --as-cran %s_%s.tar.gz",
            pkg, utils::packageVersion(pkg))
        cat(normalizePath(getwd()), ">", command, "\n\n", sep = "")
        system(command)
    })()
}
