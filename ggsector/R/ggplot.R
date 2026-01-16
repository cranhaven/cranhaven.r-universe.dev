### legend -----------------------------------------------

#' draw_key_sector
#'
#' @rdname draw_key_sector
#'
#' @inheritParams ggplot2::draw_key_polygon
#' @return ggplot legend
#' @export
draw_key_sector <- function(data, params, size) {
    if (getOption("debug_sector_legend", FALSE)) {
        # print(head(data))
        # print(head(size))
    }
    # grob
    sectorGrob(
        r = 0.5,
        vp = viewport(0.25, 0.25),
        gp = gpar(
            col = data$colour %||% NA,
            fill = alpha(data$fill %||% "grey20", data$alpha)
        )
    )
}


### geom  -----------------------------------------------
# Draw in vector form

#' @rdname geom_sector
#' @format NULL
#' @usage NULL
#' @export
GeomSectorPanel <- ggproto(
    "GeomSectorPanel",
    Geom,
    required_aes = c("x", "y", "theta"),
    # non_missing_aes = c("r", "start", "r_start", "type"),
    default_aes = aes(
        r = 0.45,
        start = 0,
        r_start = 0,
        type = "percent",
        ratio = NULL,
        colour = "black",
        fill = "transparent",
        alpha = NA,
        size = 1,
        linetype = 1
    ),
    setup_data = function(data, params) {
        data$width_raw <- resolution(data$x, FALSE)
        data$height_raw <- resolution(data$y, FALSE)
        # print(data)
        return(data)
    },
    draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
        ### coord transform
        # print(data)
        coords <- coord$transform(data, panel_params)
        ### Calculate the range of the radius
        # ratio
        ratio_best <- diff(panel_params$y.range) /
            diff(panel_params$x.range)
        if (is.null(coords$ratio)) {
            coords$ratio <- ratio_best
        }
        ratio_now <- unique(coords$ratio)
        if (ratio_best != ratio_now) {
            message("\nThis is not an error or warning, just a small reminder.")
            message(paste0("The calculated optimal ratio value is: ", ratio_best, "."))
            message(paste0("The ratio value currently used is: ", ratio_now, "."))
        }
        # width and height
        tmp_width <- diff(scales::rescale(
            c(0, min(coords$width_raw)),
            from = panel_params$x.range
        ))
        tmp_hw <- tmp_width
        # print(c(ratio = ratio_now, width = tmp_hw))
        # print(panel_params$x.range)
        # print(panel_params$y.range)
        ### grob
        sectorGrob(
            x = coords$x,
            y = coords$y,
            theta = coords$theta,
            r = coords$r * tmp_hw,
            start = coords$start,
            r_start = coords$r_start * tmp_hw,
            type = coords$type,
            ratio = coords$ratio,
            vp = NULL,
            gp = gpar(
                col = alpha(coords$colour, coords$alpha),
                fill = alpha(coords$fill, coords$alpha),
                lwd = coords$size,
                lty = coords$linetype
            )
        )
    },
    draw_key = draw_key_sector
)


# Draw in a single form

#' @rdname geom_sector
#' @format NULL
#' @usage NULL
#' @export
GeomSectorIndividual <- ggproto(
    "GeomSectorIndividual",
    Geom,
    required_aes = c("x", "y", "theta"),
    # non_missing_aes = c("r", "start", "r_start", "type", "size"),
    default_aes = aes(
        r = 0.45,
        start = 0,
        r_start = 0,
        type = "percent",
        # ratio = NULL,
        colour = "black",
        fill = "transparent",
        alpha = NA,
        size = 1,
        linetype = 1
    ),
    setup_data = function(data, params) {
        data$width_raw <- resolution(data$x, FALSE)
        data$height_raw <- resolution(data$y, FALSE)
        len <- nrow(data) + 1
        rank_x <- as.numeric(factor(data$x)) * len^2
        rank_y <- as.numeric(factor(data$y)) * len
        data$group <- rank_x + rank_y
        # print(data)
        return(data)
    },
    draw_group = function(data, panel_params, coord, na.rm = FALSE) {
        ### coord trans
        # print(data)
        coords <- coord$transform(data, panel_params)
        ### Calculate the range of the radius
        # ratio
        ratio_best <- diff(panel_params$y.range) /
            diff(panel_params$x.range)
        if (is.null(coords$ratio)) {
            coords$ratio <- ratio_best
        }
        ratio_now <- unique(coords$ratio)
        if (ratio_best != ratio_now) {
            message("\nThis is not an error or warning, just a small reminder.")
            message(paste0("The calculated optimal ratio value is: ", ratio_best, "."))
            message(paste0("The ratio value currently used is: ", ratio_now, "."))
        }
        # width
        tmp_width <- diff(scales::rescale(
            c(0, min(coords$width_raw)),
            from = panel_params$x.range
        ))
        tmp_height <- diff(scales::rescale(
            c(0, min(coords$height_raw)),
            from = panel_params$y.range
        ))
        if (ratio_now > 1) {
            tmp_hw <- tmp_width
        } else {
            tmp_hw <- tmp_height
        }
        # print(c(width = tmp_width, height = tmp_height, ratio = unique(coords$ratio)))
        # print(panel_params$x.range)
        # print(panel_params$y.range)
        ### grob
        sectorGrob(
            x = 0.5,
            y = 0.5,
            theta = coords$theta,
            r = coords$r,
            start = coords$start,
            r_start = coords$r_start,
            type = coords$type,
            vp = viewport(
                x = coords$x,
                y = coords$y,
                # default.units = "native",
                width = unit(tmp_hw, "snpc"),
                height = unit(tmp_hw, "snpc")
            ),
            gp = gpar(
                col = alpha(coords$colour, coords$alpha),
                fill = alpha(coords$fill, coords$alpha),
                lwd = coords$size,
                lty = coords$linetype
            )
        )
    },
    draw_key = draw_key_sector
)

### layer -----------------

#' ggplot sector
#'
#' Draw sector with ggplot2.
#'
#' When "individual=FALSE", draw very quickly with a vector form,
#' when "individual=TRUE", draw individually at a slower speed.
#'
#' The required parameters in mapping are "x", "y", "theta", and
#' the additional modifiable parameters are "r", "start", "r_start", "type", "colour", "fill", "ratio", "size" for line size, "linetype".
#'
#' When there is coord_fixed(), r = 0.5 means that the sector-shaped
#' background circle just fills the entire cell
#'
#' The `ratio` parameter is still an experimental parameter,
#' if it is not necessary, please do not set it yourself.
#' The `ratio` parameter only works when `individual = FALSE`.
#' When `ratio` is null, it will be auto calculated.
#'
#' For better display effect, please always  add `coord_fixed()`.
#'
#' For details, please check the [grid.sector()].
#'
#' For more details, please type `vignette("ggsector")`.
#'
#' @rdname geom_sector
#'
#' @inheritParams ggplot2::geom_point
#' @param individual Logical, default is FALSE.
#' When "individual=FALSE", draw very quickly with a vector form,
#' when "individual=TRUE", draw individually at a slower speed.
#' Anyway, for better presentation, please add coord_fixed().
#' @param verbose Logical, default is TRUE. Whether to display reminder information.
#' @return ggplot object
#'
#' @examples
#' ## prepare data
#' library(ggsector)
#' library(reshape2)
#' df <- cor(mtcars)[1:3, 1:5] %>%
#'     abs() %>%
#'     melt(varnames = c("x", "y"))
#'
#' ###
#' ## Note, for better display effect, please always add coord_fixed()
#' ## Note, for better display effect, please always add coord_fixed()
#' ## Note, for better display effect, please always add coord_fixed()
#'
#' ## theta
#' ggplot(df) +
#'     ## type = "percent", theta = 0-100
#'     geom_sector(
#'         aes(y, x, theta = value * 100),
#'         type = "percent",
#'         color = "blue",
#'         individual = TRUE
#'     ) +
#'     ## type = "degree", theta = 0-360
#'     geom_sector(
#'         aes(y, x, theta = value * 360),
#'         type = "degree",
#'         color = "red",
#'         alpha = 0.5,
#'         individual = TRUE
#'     ) +
#'     coord_fixed() +
#'     theme_bw() +
#'     theme(axis.title = element_blank())
#'
#' ## r
#' ggplot(df) +
#'     geom_sector(
#'         aes(y, x, theta = value * 100),
#'         r = rep(c(0.15, 0.3, 0.45), 5),
#'         fill = 2,
#'         individual = TRUE
#'     ) +
#'     coord_fixed() +
#'     theme_bw() +
#'     theme(axis.title = element_blank())
#'
#' ## start
#' ggplot(df) +
#'     geom_sector(
#'         aes(y, x, theta = value * 100),
#'         start = rep(c(60, 40, 20), 5),
#'         fill = 2,
#'         individual = TRUE
#'     ) +
#'     coord_fixed() +
#'     theme_bw() +
#'     theme(axis.title = element_blank())
#'
#' ## r_start
#' ggplot(df) +
#'     geom_sector(
#'         aes(y, x, theta = value * 100),
#'         r_start = rep(c(0.15, 0.25, 0.35), 5),
#'         fill = 2,
#'         individual = TRUE
#'     ) +
#'     coord_fixed() +
#'     theme_bw() +
#'     theme(axis.title = element_blank())
#'
#' \donttest{
#' ####################  individual ###################
#' ##########  individual with coord_fixed() ##########
#'
#' ## `individual = TRUE` + coord_fixed()
#' # x = x, y = y
#' ggplot(rbind(
#'     cbind(df, t1 = 1),
#'     cbind(df[1:9, ], t1 = 2)
#' )) +
#'     facet_wrap(~t1, ncol = 2) +
#'     geom_sector(
#'         aes(x, y),
#'         theta = 75,
#'         fill = 2,
#'         r = 0.5,
#'         individual = TRUE
#'     ) +
#'     coord_fixed() +
#'     theme_bw() +
#'     theme(axis.title = element_blank())
#'
#' # x = y, y =x
#' ggplot(rbind(
#'     cbind(df, t1 = 1),
#'     cbind(df[1:9, ], t1 = 2)
#' )) +
#'     facet_wrap(~t1, ncol = 2) +
#'     geom_sector(
#'         aes(y, x),
#'         theta = 75,
#'         fill = 2,
#'         r = 0.5,
#'         individual = TRUE
#'     ) +
#'     coord_fixed() +
#'     theme_bw() +
#'     theme(axis.title = element_blank())
#'
#' ## `individual = FALSE` + coord_fixed()
#' # x = x, y = y
#' ggplot(rbind(
#'     cbind(df, t1 = 1),
#'     cbind(df[1:9, ], t1 = 2)
#' )) +
#'     facet_wrap(~t1, ncol = 2) +
#'     geom_sector(
#'         aes(x, y),
#'         theta = 75,
#'         fill = 2,
#'         r = 0.5,
#'         individual = FALSE
#'     ) +
#'     coord_fixed() +
#'     theme_bw() +
#'     theme(axis.title = element_blank())
#'
#' # x = y, y =x
#' ggplot(rbind(
#'     cbind(df, t1 = 1),
#'     cbind(df[1:9, ], t1 = 2)
#' )) +
#'     facet_wrap(~t1, ncol = 2) +
#'     geom_sector(
#'         aes(y, x),
#'         theta = 75,
#'         fill = 2,
#'         r = 0.5,
#'         individual = TRUE
#'     ) +
#'     coord_fixed() +
#'     theme_bw() +
#'     theme(axis.title = element_blank())
#'
#' ##########  individual without coord_fixed() ##########
#' ## If you are in a special situation and cannot use coord_fixed(),
#' ## then it is recommended that you use `individual = TRUE` and
#' ## the `r` parameter to fine-tune.
#' ## Also, to reduce the radius, you need to try it manually.
#'
#' ## `individual = TRUE` without coord_fixed()
#' # x = x, y = y
#' ggplot(rbind(
#'     cbind(df, t1 = 1),
#'     cbind(df[1:9, ], t1 = 2)
#' )) +
#'     facet_wrap(~t1, ncol = 2) +
#'     geom_sector(
#'         aes(x, y),
#'         theta = 75,
#'         fill = 2,
#'         r = 0.35, ## To reduce the radius, you need to try it manually
#'         individual = TRUE
#'     ) +
#'     theme_bw() +
#'     theme(axis.title = element_blank())
#'
#' # x = y, y =x
#' ggplot(rbind(
#'     cbind(df, t1 = 1),
#'     cbind(df[1:9, ], t1 = 2)
#' )) +
#'     facet_wrap(~t1, ncol = 2) +
#'     geom_sector(
#'         aes(y, x),
#'         theta = 75,
#'         fill = 2,
#'         r = 0.25, ## To reduce the radius, you need to try it manually
#'         individual = TRUE
#'     ) +
#'     theme_bw() +
#'     theme(axis.title = element_blank())
#'
#' ## `individual = FALSE`
#' ## If you really want to use `individual = FALSE` without coord_fixed(),
#' ## you might try the experimental parameter `ratio'
#' ## You need to manually adjust the `ratio` value
#' ## to prevent sector deformation.
#' # x = x, y = y
#' ggplot(rbind(
#'     cbind(df, t1 = 1),
#'     cbind(df[1:9, ], t1 = 2)
#' )) +
#'     facet_wrap(~t1, ncol = 2) +
#'     geom_sector(
#'         aes(x, y),
#'         theta = 75,
#'         fill = 2,
#'         r = 0.5,
#'         ## You need to manually adjust the `ratio` value
#'         ## to prevent sector deformation.
#'         ratio = 1.6,
#'         individual = FALSE
#'     ) +
#'     theme_bw() +
#'     theme(axis.title = element_blank())
#'
#' # x = y, y =x
#' ggplot(rbind(
#'     cbind(df, t1 = 1),
#'     cbind(df[1:9, ], t1 = 2)
#' )) +
#'     facet_wrap(~t1, ncol = 2) +
#'     geom_sector(
#'         aes(y, x),
#'         theta = 75,
#'         fill = 2,
#'         r = 0.5,
#'         ## You need to manually adjust the `ratio` value
#'         ## to prevent sector deformation.
#'         ratio = 1.6,
#'         individual = FALSE
#'     ) +
#'     # coord_fixed() +
#'     theme_bw() +
#'     theme(axis.title = element_blank())
#' }
#'
#' @export
geom_sector <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE,
                        individual = FALSE,
                        verbose = TRUE
                        #
) {
    if (individual) {
        ly <- layer(
            data = data,
            mapping = mapping,
            stat = stat,
            geom = GeomSectorIndividual,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                na.rm = na.rm,
                ...
            )
        )
    } else {
        ly <- layer(
            data = data,
            mapping = mapping,
            stat = stat,
            geom = GeomSectorPanel,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                na.rm = na.rm,
                ...
            )
        )
    }
    if (verbose) message("For better display effect, please add `coord_fixed()`")
    return(ly)
}
