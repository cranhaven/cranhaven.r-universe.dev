#' Diagnostic Plots for Regression Models
#'
#' @section Plot types:
#' There are several plot types available:
#' * residual versus fitted
#' * scale-location
#' * residual versus leverage
#' * Cook's distance
#' * normal Q-Q
#' * histogram array
#' * forest plot
#'
#' @param x a regression model
#' @param which the type of plot to draw
#' @param show.bootstraps logical, if `TRUE` bootstrap smoothers will be shown (defaults to `TRUE` if fewer than 100,000 observations)
#' @param label.id integer for the number of extreme points to label (with row id)
#' @param col.smooth the colour of smoothers
#' @param col.bs the colour of bootstrap (smoothers)
#' @param cook.levels levels of the Cook's distance at which to draw contours.
#' @param col.cook the colour of Cook's distance contours
#' @param ... additional arguments
#' @param bs.fits a list of bootstrapped datasets
#' @param env the environment for evaluating things (e.g., bootstraps)
#' @return A ggplot object with a plot method that will show the plot in the graphics device
#'
#' @md
#' @import ggplot2
#' @import ggtext
#' @import ggrepel
#' @import patchwork
#' @author Tom Elliott
#' @export
#' @examples
#' iris_fit <- lm(Sepal.Width ~ Sepal.Length, data = iris)
#' inzplot(iris_fit)
#' inzplot(iris_fit, which = "residual", show.bootstraps = FALSE)
inzplot.lm <- function(x,
                       which = c(
                           "residual",
                           "scale",
                           "leverage",
                           "cooks",
                           "normal",
                           "hist"
                       ),
                       show.bootstraps = nrow(x$model) < 1e5,
                       label.id = 3L,
                       col.smooth = "orangered",
                       col.bs = "lightgreen",
                       cook.levels = c(0.5, 1),
                       col.cook = "pink",
                       ...,
                       bs.fits = NULL,
                       env = parent.frame()) {
    if (which[1] == "marginal") { # nocov start
        terms <- x$terms
        vars <- attr(terms, "term.labels")
        vars <- vars[attr(terms, "dataClasses")[vars] == "numeric"]
        f <- as.formula(paste("~", paste(vars, collapse = " + ")))
        p <- car::marginalModelPlots(x, f, ...)
        return(invisible(p))
    } # nocov end

    # instead, just loop over `which` and patchwork:: them together
    short.title <- length(which) > 1L
    if (show.bootstraps && is.null(bs.fits)) {
        bs.fits <- generate_bootstraps(x, env)
    }
    ps <- lapply(
        which,
        function(w) {
            switch(w,
                "residual" = ,
                "scale" = ,
                "leverage" = .inzplot_lm_scatter(x, w, show.bootstraps,
                    label.id, col.smooth, col.bs, cook.levels, col.cook,
                    short.title,
                    ...,
                    bs.fits = bs.fits,
                    env = env
                ),
                "cooks" = .inzplot_lm_cooks(x, label.id,
                    short.title,
                    ...,
                    env = env
                ),
                "normal" = .inzplot_lm_normqq(x, show.bootstraps, label.id,
                    short.title,
                    ...,
                    bs.fits = bs.fits,
                    env = env
                ),
                "hist" = .inzplot_lm_hist(x, show.bootstraps,
                    short.title,
                    ...,
                    env = env
                ),
                "forest" = .inzplot_lm_forest(x, show.bootstraps,
                    short.title,
                    ...,
                    env = env
                )
            )
        }
    )

    p <- patchwork::wrap_plots(ps)

    grDevices::dev.hold()
    on.exit(grDevices::dev.flush())
    suppressWarnings(print(p))

    invisible(p)
}

.inzplot_lm_scatter <- function(x, which, show.bootstraps, label.id,
                                col.smooth, col.bs,
                                cook.levels, col.cook,
                                short.title,
                                ...,
                                bs.fits,
                                env) {
    HEX_THRESHOLD <- 1e5

    d_fun <- function(x, which, label.id = 0L, is.bs = FALSE) {
        # point labels
        if (label.id > 0L) {
            iid <- seq_len(label.id)
        }

        labs <- character(nrow(x$model))
        switch(which,
            "residual" = {
                r <- residuals(x)
                if (label.id > 0L) {
                    l <- sort.list(abs(r), decreasing = TRUE)[iid]
                    labs[l] <- names(r)[l]
                }
                data.frame(x = predict(x), y = r, lab = labs)
            },
            "scale" = {
                r <- residuals(x)
                s <- sqrt(deviance(x) / df.residual(x))
                hii <- lm.influence(x, do.coef = FALSE)$hat
                rs <- dropInf(r / (s * sqrt(1 - hii)), hii)
                if (label.id > 0L) {
                    l <- sort.list(abs(r), decreasing = TRUE)[iid]
                    labs[l] <- names(r)[l]
                }
                data.frame(x = predict(x), y = sqrt(abs(rs)), lab = labs)
            },
            "leverage" = {
                rp <- residuals(x, "pearson")
                hii <- lm.influence(x, do.coef = FALSE)$hat
                s <- sqrt(deviance(x) / df.residual(x))
                rsp <- dropInf(rp / (s * sqrt(1 - hii)), hii)
                xx <- ifelse(hii > 1, NA, hii)
                cook <- cooks.distance(x, sd = s, res = residuals(x))
                if (label.id > 0L) {
                    l <- order(-cook)[iid]
                    labs[l] <- names(rp)[l]
                }
                d <- data.frame(x = xx, y = rsp, lab = labs)

                if (!is.bs && length(cook.levels)) {
                    r.hat <- range(hii, na.rm = TRUE)
                    attr(d, "r.hat") <- r.hat
                }
                d
            }
        )
    }
    d <- d_fun(x, which, label.id = label.id)

    if (show.bootstraps && is.null(bs.fits)) {
        bs.fits <- generate_bootstraps(x, env)
    }

    title <- switch(which,
        "residual" = "Residuals vs Fitted Values",
        "scale" = "Scale-location plot",
        "leverage" = "Residuals vs Leverage"
    )
    if (short.title) {
        subtitle <- waiver()
    } else {
        subtitle <- sprintf(
            "Linear model: %s",
            utils::capture.output(x$call$formula)
        )
        if (show.bootstraps) {
            title <- sprintf(
                "**%s** with <span style='color:%s'>fitted</span> and <span style='color:%s'>bootstrap</span> smoothers",
                title,
                col.smooth,
                col.bs
            )
        } else {
            title <- sprintf(
                "**%s** with fitted smoother",
                title
            )
        }
        if (which == "leverage" && !is.null(attr(d, "r.hat"))) {
            title <- sprintf(
                "%s, and <span style='color:%s'>Cook's contours</span>",
                title,
                col.cook
            )
        }
    }

    USE_HEX <- nrow(d) > HEX_THRESHOLD
    XL <- extendrange(range(d$x, na.rm = TRUE))
    YL <- extendrange(range(d$y, na.rm = TRUE), f = 0.08)

    p <- ggplot(d, aes(.data$x, .data$y))

    yax2 <- NULL
    if (which == "leverage" && !is.null(attr(d, "r.hat"))) {
        px <- length(coef(x))
        r.hat <- attr(d, "r.hat")
        hh <- seq.int(min(r.hat[1L], r.hat[2L] / 100), XL[2],
            length.out = 101
        )

        yax2 <- numeric()
        for (crit in cook.levels) {
            cl.h <- sqrt(crit * px * (1 - hh) / hh)
            yax2 <- c(
                yax2,
                structure(cl.h[length(cl.h)] * c(1, -1), .Names = c(crit, crit))
            )
            dx <- data.frame(x = hh, y = cl.h)
            p <- p +
                geom_path(
                    lty = 2, col = col.cook,
                    data = dx, na.rm = TRUE
                ) +
                geom_path(aes(y = -.data$y),
                    lty = 2, col = col.cook,
                    data = dx, na.rm = TRUE
                ) +
                geom_vline(xintercept = XL[2])
        }
        yax2 <- yax2[dplyr::between(yax2, YL[1], YL[2])]
    }

    if (USE_HEX) {
        p <- p + geom_hex() +
            scale_fill_gradient(low = "gray80", high = "black") +
            labs(fill = "Count")
    } else {
        p <- p + geom_point()
    }

    p <- p +
        geom_hline(yintercept = 0, lty = 3) +
        scale_x_continuous(
            switch(which,
                "residual" = "Fitted values",
                "scale" = "Fitted values",
                "leverage" = "Leverage"
            )
        ) +
        scale_y_continuous(
            switch(which,
                "residual" = "Residuals",
                "scale" = expression(sqrt(abs("Standardized residuals"))),
                "leverage" = "Residuals"
            ),
            sec.axis =
                if (!is.null(yax2) && length(yax2)) {
                    sec_axis(
                        transform = ~.,
                        name = NULL,
                        breaks = yax2,
                        labels = names(yax2)
                    )
                } else {
                    waiver()
                }
        ) +
        ggtitle(title, subtitle = subtitle) +
        theme_classic() +
        theme(
            plot.title.position = ifelse(short.title, "panel", "plot"),
            plot.title = element_markdown(size = ifelse(short.title, 11, 12))
        ) +
        coord_cartesian(xlim = XL, ylim = YL, expand = FALSE)

    if (label.id > 0L && !is.null(d$lab)) {
        p <- p +
            geom_text_repel(aes(label = .data$lab),
                data = d[d$lab != "", ]
            )
    }

    if (show.bootstraps) {
        bs.data <- do.call(
            rbind,
            lapply(
                seq_along(bs.fits),
                function(i) {
                    di <- cbind(
                        d_fun(bs.fits[[i]], which = which, is.bs = TRUE),
                        bs.index = i
                    )
                    si <- suppressWarnings(loess(y ~ x, data = di))
                    o <- order(si$x)
                    data.frame(x = si$x[o], y = si$fitted[o], bs.index = i)
                }
            )
        )

        p <- p +
            geom_path(
                aes(group = .data$bs.index),
                data = bs.data,
                colour = col.bs
            )
    }

    p <- p +
        geom_smooth(
            method = "loess",
            formula = y ~ x,
            colour = col.smooth,
            se = FALSE,
            na.rm = TRUE
        )

    p
}

.inzplot_lm_cooks <- function(x, label.id, short.title, ..., env) {
    cdx <- cooks.distance(x)
    show.mx <- order(-cdx)[1:3]
    d <- data.frame(
        obs_n = seq_along(cdx),
        cooks_distance = cdx
    )
    co <- order(-cdx)[1:3]
    d$lab <- as.character(d$obs_n)

    XL <- extendrange(c(0L, nrow(d)))
    YL <- c(0, max(cdx) * 1.08)

    ggplot(d, aes(.data$obs_n, .data$cooks_distance)) +
        geom_segment(aes(xend = .data$obs_n, y = 0, yend = .data$cooks_distance)) +
        geom_text(aes(label = .data$lab),
            data = d[co, ],
            nudge_y = 0.02 * YL[2]
        ) +
        scale_x_continuous("Observation number",
            limits = XL
        ) +
        scale_y_continuous("Cook's Distance",
            limits = YL
        ) +
        ggtitle(
            ifelse(short.title, "Cook's Distance",
                "**Cook's Distance** of ordered observations"
            ),
            subtitle = if (short.title) {
                waiver()
            } else {
                sprintf(
                    "Linear model: %s",
                    utils::capture.output(x$call$formula)
                )
            }
        ) +
        theme_classic() +
        theme(
            plot.title.position = ifelse(short.title, "panel", "plot"),
            plot.title = element_markdown(size = ifelse(short.title, 11, 12))
        ) +
        coord_cartesian(expand = FALSE)
}

.inzplot_lm_normqq <- function(x, show.bootstraps, label.id,
                               short.title,
                               ..., bs.fits = NULL, env = env) {
    r <- residuals(x)
    s <- sqrt(deviance(x) / df.residual(x))
    hii <- lm.influence(x, do.coef = FALSE)$hat
    rs <- dropInf(r / (s * sqrt(1 - hii)), hii)
    iid <- 1:3L
    labs <- character(nrow(x$model))
    l <- sort.list(abs(rs), decreasing = TRUE)[iid]
    labs[l] <- names(r)[l]

    qq <- normCheck(rs, plot = FALSE)
    d <- data.frame(x = qq$x, y = qq$y, lab = labs)

    stest <- shapiro.test(rs)
    sp <- stest$p.value
    sres <- sprintf(
        "Shapiro Wilk normality test: W = %s, P-value %s %s",
        round(stest$statistic, 4),
        ifelse(sp < 1e-4, "<", "="),
        max(round(stest$p.value, 3), 1e-4)
    )

    p <- ggplot(d, aes(.data$x, .data$y)) +
        geom_abline(slope = 1, intercept = 0)

    if (short.title) {
        title <- "Normal Q-Q"
        subtitle <- waiver()
    } else {
        title <- "**Normal Q-Q** of residuals"
        subtitle <- sprintf(
            "Linear model: %s<br>%s",
            utils::capture.output(x$call$formula),
            sres
        )
    }

    if (show.bootstraps) {
        colz <- iNZightPlots::inzpalette("rainbow")(10L)
        for (i in 1:10) {
            qqx <- normCheck(rnorm(length(rs)), plot = FALSE)
            dx <- data.frame(x = qqx$x, y = qqx$y)
            p <- p +
                geom_point(
                    colour = colz[i],
                    data = dx,
                    pch = 4L
                )
        }
        if (!short.title) {
            tx <- c("boo", "tst", "rap", " No", "rma", "l e", "rro", "rs")
            tc <- paste0("<span style='color:", colz[1:8], "'>", tx, "</span>", collapse = "")
            title <- sprintf("%s with a sample of %s", title, tc)
        }
    }

    p <- p +
        geom_point() +
        geom_text_repel(aes(label = .data$lab),
            data = d[d$lab != "", ],
            direction = "x"
        ) +
        ggtitle(title, subtitle = subtitle) +
        scale_x_continuous("Theoretical quantiles") +
        scale_y_continuous("Standardized residuals") +
        theme_classic() +
        theme(
            plot.title.position = ifelse(short.title, "panel", "plot"),
            plot.title = element_markdown(size = ifelse(short.title, 11, 12)),
            plot.subtitle = element_markdown(lineheight = 1.5)
        )
}

.inzplot_lm_hist <- function(x, short.title, ..., env = env) {
    d <- data.frame(x = residuals(x))
    mx <- mean(d$x, na.rm = TRUE)
    sx <- sd(d$x, na.rm = TRUE)
    rx <- range(d$x, na.rm = TRUE)

    h <- hist(d$x, plot = FALSE)

    xmin <- min(rx[1], mx - 3.5 * sx, h$breaks[1])
    xmax <- max(rx[2], mx + 3.5 * sx, h$breaks[length(h$breaks)])
    ymax <- max(h$density, dnorm(mx, mx, sx)) * 1.05

    d2 <- data.frame(x = seq(xmin, xmax, length.out = 1001))
    d2$y <- dnorm(d2$x, mx, sx)

    dd <- data.frame(x = h$mids, y = h$density)

    curve.col <- "orangered"
    if (short.title) {
        title <- "Histogram"
        subtitle <- waiver()
    } else {
        title <- sprintf(
            "**Histogram of residuals** with <span style='color: %s'>Normal density curve</span>",
            curve.col
        )
        subtitle <- sprintf(
            "Linear model: %s",
            utils::capture.output(x$call$formula)
        )
    }

    ggplot(dd, aes(.data$x, .data$y)) +
        geom_col(
            width = diff(h$breaks),
            fill = "light blue",
            colour = "black"
        ) +
        geom_path(aes(y = .data$y),
            data = d2,
            colour = curve.col,
            linetype = 2,
            linewidth = 1.2
        ) +
        coord_cartesian(expand = FALSE) +
        scale_x_continuous("Residuals",
            limits = extendrange(range(d$x))
        ) +
        scale_y_continuous("Density",
            limits = function(l) c(l[1], l[2] * 1.04)
        ) +
        ggtitle(title, subtitle = subtitle) +
        theme_classic() +
        theme(
            plot.title.position = ifelse(short.title, "panel", "plot"),
            plot.title = element_markdown(size = ifelse(short.title, 11, 12)),
            plot.subtitle = element_markdown(lineheight = 1.5)
        )
}

.inzplot_lm_forest <- function(x, short.title, ..., env = env) {
    if (!requireNamespace("broom.helpers", quietly = TRUE)) {
        stop("Please install suggested package: 'broom.helpers'")
    } # no cov
    GGally::ggcoef_model(x)
}


dropInf <- function(x, h) {
    if (any(isInf <- h >= 1)) {
        # warning("Not plotting observations with leverage one:\n  ",
        #        paste(which(isInf), collapse = ", "), call. = FALSE)
        x[isInf] <- NaN
    }
    x
}
