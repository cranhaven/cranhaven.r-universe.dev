makeTitle <- function(names, types, g1.level = NULL, g2.level = NULL,
                      template = NULL) {
  # creates the title text for the plot
    varnames <- names(names)
    names <- lapply(names, stringr::str_trunc, width = 60)
    names(names) <- varnames
    if (types$x == "factor") {
      # Need a different title for barplots:
      # Distribution of X by Y
        title1 <- paste0("Distribution of ", names$x)
        title2 <- ifelse(! "y" %in% varnames, "", paste0(" by ", names$y))
    } else {
        if (!"y" %in% varnames) {
            title1 <- ""
            title2 <- names$x
        } else if (types$y == "numeric") {
            title1 <- paste0(names$y, " versus ")
            title2 <- names$x
        } else {
            title1 <- names$x
            title2 <- paste0(' by ', names$y)
        }
    }

    title3 <- ifelse(
        ! "g1" %in% varnames,
        "",
        paste0(" subset by ", names$g1)
    )
    title4 <- ifelse(
        ! "g2" %in% varnames,
        "",
        ifelse(
            is.null(g2.level),
            "",
            ifelse(
                g2.level == "_MULTI",
                ifelse(
                    "g1" %in% varnames,
                    paste0(" and ", names$g2),
                    paste0(" subset by ", names$g2)
                ),
                paste0(", for ", names$g2, " = ", g2.level)
            )
        )
    )

    if ("y" %in% varnames) {
        if (types$x == "numeric" & types$y == "numeric") {
            title5 <- ifelse(
                !"sizeby" %in% varnames,
                "",
                paste0(" (size proportional to ", names$sizeby, ")")
            )
        } else {
            title5 <- ""
        }
    } else {
        title5 <- ""
    }

    if (is.null(template))
        return(paste0(title1, title2, title3, title4, title5))

    ## Replace %subset% in template by title3, title4
    title <- gsub("%subset%", paste0(title3, title4), template)

    ## Replace %sizeby% in template by title5
    title <- gsub("%sizeby%", title5, title)

    title
}
