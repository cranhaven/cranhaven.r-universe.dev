#' Draw sector with grid
#'
#' [sectorGrob()] return a polygon grob. [grid.sector()] draw sector.
#' For more details, please type `vignette("ggsector")`.
#'
#' @rdname sectorGrob
#'
#' @inheritParams sector_df
#' @inheritParams grid::polygonGrob
#'
#' @return polygon grob
#'
#' @examples
#' ## Draw basic grid
#'
#' # sectorGrob with units of "cm" and type of "degree"
#' grid.newpage()
#' gp <- sectorGrob(
#'     x = unit(c(3, 9, 15), "cm"),
#'     y = unit(c(5, 9, 15), "cm"),
#'     theta = c(90, 180, 270),
#'     r = 1,
#'     start = c(180, 180, 270),
#'     r_start = c(0.6, 0.3, 0),
#'     type = "degree",
#'     group = factor(1:3, levels = c(2, 3, 1)),
#'     gp = gpar(fill = c("green", "red", "blue"))
#' )
#' grid.draw(gp)
#'
#' # grid.sector with units of "npc" and type of "percent"
#' grid.newpage()
#' grid.sector(
#'     x = c(0.1, 0.5, 0.9),
#'     y = c(0.9, 0.6, 0.1),
#'     theta = c(25, 50, 90),
#'     r = .1,
#'     start = c(25, 50, 100),
#'     r_start = c(0.06, 0.03, 0),
#'     type = "percent",
#'     group = factor(1:3, levels = c(2, 3, 1)),
#'     gp = gpar(col = c("green", "red", "blue"), fill = 2:4),
#'     default.units = "npc"
#' )
#'
#' \donttest{
#' ## Draw sector with ComplexHeatmap
#'
#' # prepare data
#' library(magrittr)
#' library(ComplexHeatmap)
#' t0 <- cor(mtcars) %>%
#'     set_colnames(paste("y_", colnames(.))) %>%
#'     set_rownames(paste("x_", rownames(.)))
#' mat <- abs(t0)
#' mat[1:5, 1:5]
#'
#' # Realized by modifying the [grid::viewport()],
#' # the sector can be set with a fixed width and height
#' set.seed(1)
#' Heatmap(
#'     mat,
#'     name = "vp",
#'     rect_gp = gpar(type = "none"),
#'     cell_fun = function(j, i, x, y, width, height, fill) {
#'         grid.rect(
#'             x = x, y = y, width = width, height = height,
#'             gp = gpar(col = "grey", fill = NA)
#'         )
#'         grid.sector(
#'             theta = mat[i, j] * 100,
#'             r = 0.5,
#'             start = mat[i, j] * 100 * runif(1),
#'             r_start = mat[i, j] * 0.49 * runif(1),
#'             vp = viewport(x, y, width, height),
#'             gp = gpar(fill = fill, col = "transparent")
#'         )
#'     },
#'     width = unit(.7, "snpc"),
#'     height = unit(.7, "snpc")
#' )
#'
#' # Realized in the form of coordinates + radius.
#' # The default viewport locks the horizontal and vertical axes
#' # so that the sector does not deform, which needs to be removed here.
#' # The radius 'r' is half the min(length, width).
#' set.seed(2)
#' Heatmap(
#'     mat,
#'     name = "xy + r",
#'     rect_gp = gpar(type = "none"),
#'     cell_fun = function(j, i, x, y, width, height, fill) {
#'         grid.rect(
#'             x = x, y = y, width = width, height = height,
#'             gp = gpar(col = "grey", fill = NA)
#'         )
#'         r <- as.numeric(min(width, height)) / 2
#'         grid.sector(
#'             x,
#'             y,
#'             theta = mat[i, j] * 100,
#'             r = r,
#'             start = mat[i, j] * 100 * runif(1),
#'             r_start = mat[i, j] * r * 0.9 * runif(1),
#'             vp = NULL,
#'             gp = gpar(fill = fill, col = "transparent")
#'         )
#'     },
#'     width = unit(.7, "snpc"),
#'     height = unit(.7, "snpc")
#' )
#'
#' # layer full
#' # The input matrix needs to be extracted with pindex(mat, i, j)
#' set.seed(3)
#' Heatmap(
#'     mat,
#'     name = "layer",
#'     rect_gp = gpar(type = "none"),
#'     layer_fun = function(j, i, x, y, width, height, fill) {
#'         grid.rect(
#'             x = x, y = y, width = width, height = height,
#'             gp = gpar(col = "grey", fill = NA)
#'         )
#'         r <- as.numeric(min(width, height)) / 2
#'         grid.sector(
#'             x,
#'             y,
#'             theta = pindex(mat, i, j) * 100,
#'             r = r,
#'             start = pindex(mat, i, j) * 100 * runif(nrow(mat) * ncol(mat)),
#'             r_start = pindex(mat, i, j) * r * 0.9 * runif(nrow(mat) * ncol(mat)),
#'             vp = NULL,
#'             gp = gpar(fill = fill, col = "transparent")
#'         )
#'     },
#'     width = unit(.7, "snpc"),
#'     height = unit(.7, "snpc")
#' )
#' }
#'
#' @export
sectorGrob <- function(x = 0.5,
                       y = 0.5,
                       theta = 25,
                       r = 0.5,
                       start = 0,
                       r_start = 0,
                       type = "percent",
                       ratio = 1,
                       group,
                       default.units = "npc",
                       vp = viewport(height = unit(1, "snpc"), width = unit(1, "snpc")),
                       gp = gpar(col = "black", fill = "transparent")
                       #
) {
    # unit
    if (is.unit(x) || is.unit(y)) {
        u1 <- unitType(x) %>% unique()
        u2 <- unitType(y) %>% unique()
        if (length(u1) != 1 || length(u2) != 1 || u1 != u2) stop("The input `x` and `y` should have the same unit.")
        x <- as.numeric(x)
        y <- as.numeric(y)
        default.units <- u1
    }
    ## test unit
    unit(1, default.units)
    # get df
    df_plot <- sector_df_multiple(
        x = x,
        y = y,
        theta = theta,
        r = r,
        start = start,
        r_start = r_start,
        type = type,
        ratio = ratio,
        group = group
    )
    pg <-
        polygonGrob(
            x = df_plot$x,
            y = df_plot$y,
            id = df_plot$group,
            vp = vp,
            gp = gp,
            default.units = default.units
        )
    return(pg)
}



#' @rdname sectorGrob
#'
#' @return draw sector
#' @export
grid.sector <- function(x = 0.5,
                        y = 0.5,
                        theta = 25,
                        r = 0.5,
                        start = 0,
                        r_start = 0,
                        type = "percent",
                        ratio = 1,
                        group,
                        default.units = "npc",
                        vp = viewport(height = unit(1, "snpc"), width = unit(1, "snpc")),
                        gp = gpar(col = "black", fill = "transparent")
                        #
) {
    # unit
    if (is.unit(x) || is.unit(y)) {
        u1 <- unitType(x) %>% unique()
        u2 <- unitType(y) %>% unique()
        if (length(u1) != 1 || length(u2) != 1 || u1 != u2) stop("The input `x` and `y` should have the same unit.")
        x <- as.numeric(x)
        y <- as.numeric(y)
        default.units <- u1
    }
    ## test unit
    unit(1, default.units)
    pg <- sectorGrob(
        x = x,
        y = y,
        theta = theta,
        r = r,
        start = start,
        r_start = r_start,
        type = type,
        ratio = ratio,
        group = group,
        vp = vp,
        gp = gp,
        default.units = default.units
    )
    grid.draw(pg)
}
