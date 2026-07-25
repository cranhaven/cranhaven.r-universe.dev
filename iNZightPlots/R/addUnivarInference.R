addUnivarInference <- function(inflist, i, opts) {
    bs <- attr(inflist, "bootstrap")
    col1 <- ifelse(bs, opts$inf.col.comp[2], opts$inf.col.comp[1])
    col2 <- ifelse(bs, opts$inf.col.conf[2], opts$inf.col.conf[1])

    ## We can only display one type of inference (mean OR median), so take the fist:
    inflist <- inflist[[1]]

    if (is.character(inflist)) return()

    lapply(rev(c("conf", "comp")),
        function(n) {
            if (!is.null(inflist[[n]])) {
                ci <- inflist[[n]][i, c("lower", "upper")]
                grid.lines(
                    x = unit(ci, units = "native"),
                    y = unit(0.5, "npc"),
                    gp = gpar(
                        col = switch(n, 'comp' = col1, 'conf' = col2),
                        lwd = switch(n, 'comp' = 5, 'conf' = 2),
                        lineend = "butt"
                    )
                )
            }
        }
    )
}

addUnivarCompLines <- function(inflist) {
    inflist <- inflist[[1]]
    guides <- unique(c(inflist$comp[, c("lower", "upper")]))
    if (!is.null(guides))
        grid.polyline(
            x = unit(rep(guides, each = 2), "native"),
            y = rep(c(0, 1), length(guides)),
            id = rep(1:length(guides), each = 2),
            gp = gpar(lty = 3, col = "grey50", lwd = 0.5)
        )
}
