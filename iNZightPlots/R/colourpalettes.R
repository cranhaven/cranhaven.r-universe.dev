#' iNZight colour palette
#'
#' Used to obtain a colour palette of a given name.
#' A list of available palettes can be obtained by
#' `cat_palette_names()` and `cont_palette_names()`.
#'
#' @param palette the name of a palette
#' @return a colour palette function with single argument `n`
#' @author Tom Elliott
#' @export
#' @examples
#' plot(1:5, pch = 19, col = inzpalette("bright")(5))
#'
#' # for a list of palette names
#' cat_palette_names()
#' cont_palette_names()
inzpalette <- function(palette) {
    switch(palette,
        "contrast" = function(n) {
            if (n > 8)
                return(inzpar()$col.default$cat(n))
            RColorBrewer::brewer.pal(max(n, 3L), "Set2")[1:n]
        },
        "bright" = function(n) {
            if (n > 9)
                return(inzpar()$col.default$cat(n))
            RColorBrewer::brewer.pal(max(n, 3L), "Set1")[1:n]
        },
        "light" = function(n) {
            if (n > 12)
                return(inzpar()$col.default$cat(n))
            RColorBrewer::brewer.pal(max(n, 3L), "Set3")[1:n]
        },
        "viridis" = viridis::viridis,
        "magma" = viridis::magma,
        "plasma" = viridis::plasma,
        "inferno" = viridis::inferno,
        "colourblind" = inzpar()$col.default$cat,
        "rainbow" = function(n) {
            hcl( (1:n) / n * 360, c = 80, l = 50)
        },
        "blue" = function(n) {
            sequential_hcl(n,
                h = 260,
                c. = c(80, 10),
                l = c(30, 95),
                power = 0.7
            )
        },
        "green" = function(n) {
            sequential_hcl(n,
                h = 135,
                c. = c(50, 10),
                l = c(40, 95),
                power = 0.4
            )
        },
        "red" = function(n) {
            sequential_hcl(n,
                h = 10,
                c. = c(80, 10),
                l = c(30, 95),
                power = 0.7
            )
        },
        "greenyellow" = function(n) {
            terrain_hcl(n,
                h = c(130, 30),
                c. = c(65, 0),
                l = c(45, 90),
                power = c(0.5, 1.5)
            )
        },
        "redblue" = function(n) {
            terrain_hcl(n,
                h = c(0, -100),
                c. = c(80, 40),
                l = c(40, 75),
                power = c(1, 1)
            )
        },
        "terrain" = terrain_hcl,
        "heat" = heat_hcl,
        "bluewhitepink" = function(n) {
            diverge_hcl(n,
                h = c(180, 330),
                c = 59,
                l = c(75, 95),
                power = 1.5
            )
        },
        "bluewhitered" = function(n) {
            diverge_hcl(n,
                h = c(260, 0),
                c = 100,
                l = c(50, 90),
                power = 1
            )
        },
        stop("Unknown palette `", palette, "`")
    )
}

#' Emphasize a level or interval of a colour palette
#'
#' @param n the number of colours to draw from the palette
#' @param k the index of the colour to emphasize
#' @param cat logical indicator if palette is categorical or numeric
#' @param ncat the number of intervals to use for continuous palettes
#' @param fn the colour palette function to use
#' @return a colour palette, with one level emphasized (or range for numeric)
#' @author Tom Elliott
#' @export
#' @examples
#' pal <- inzpalette("bright")
#' plot(1:5, pch = 19, col = emphasize_pal_colour(5, 2, fn = pal))
emphasize_pal_colour <- function(n, k, cat = TRUE, ncat = 5, fn) {
    if (missing(fn)) {
        fn <- inzpar()$col.default[[ifelse(cat, "cat", "cont")]]
    }

    cols <- fn(n)
    if (!cat) {
        ks <- floor(seq(1, n, length = ncat + 1))
        k <- ks[k]:ks[k + 1]
    }
    cols[-k] <- shade(cols[-k], 0.7)
    cols
}

#' @describeIn inzpalette List of categorical colour palettes
#' @export
cat_palette_names <- function() {
    pals <- list()

    if (requireNamespace("RColorBrewer", quietly = TRUE)) {
        pals <- c(pals, list(
            contrast = "contrast (max 8)",
            bright = "bright (max 9)",
            light = "light (max 12)"
        ))
    }

    if (requireNamespace("viridis", quietly = TRUE))
        pals <- c(pals, viridis_palette_names())

    pals <- c(pals, list(
        colourblind = "Colourblind friendly",
        rainbow = "rainbow (hcl)"
    ))

    pals
}

#' @describeIn inzpalette List of continuous  colour palettes
#' @export
cont_palette_names <- function() {
    pals <- list()

    if (requireNamespace("viridis", quietly = TRUE))
        pals <- c(pals, viridis_palette_names())

    pals <- c(pals, list(
        rainbow = "rainbow (hcl)",
        blue = "blue",
        green = "green",
        red = "red",
        greenyellow = "green-yellow",
        redblue = "red-blue",
        terrain = "terrain",
        heat = "heat",
        bluewhitepink = "bluewhitepink",
        bluewhitered = "bluewhitered"
    ))

    pals
}

#' An incorrectly spelled function - deprecated
#'
#' This function was misspelled in earlier versions
#' and has been corrected to \code{cont_palette_names},
#' which should be used instead.
#'
#' @return a list of continuous colour palettes
#' @seealso \code{\link{cont_palette_names}}
#' @export
const_palette_names <- function() {
    warning("Function renamed to 'cont_palette_names()'")
    cont_palette_names()
}

viridis_palette_names <- function() {
    list(
        viridis = "viridis",
        magma = "magma",
        plasma = "plasma",
        inferno = "inferno"
    )
}
