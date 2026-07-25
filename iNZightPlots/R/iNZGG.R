required_arguments <- list(
  pie = c("fill"),
  donut = c("fill"),
  column = c("x")
)

optional_args <- list(
  gg_violin = c("adjust", "alpha"),
  gg_density = c("adjust", "alpha", "alpha_densitygroup"),
  gg_lollipop = c("gg_lwd", "labels", "gg_size"),
  gg_boxplot = c("gg_lwd"),
  gg_cumcurve = c("gg_lwd"),
  gg_column = c("ordered"),
  gg_lollipop2 = c("ordered", "gg_lwd", "gg_size"),
  gg_pie = c("ordered"),
  gg_donut = c("ordered"),
  gg_column2 = c("labels"),
  gg_barcode = c("alpha", "gg_barSize"),
  gg_dotstrip = c("alpha", "gg_size"),
  gg_poppyramid = c("gg_bins"),
  gg_freqpolygon = c("gg_lwd", "gg_size"),
  gg_barcode2 = c("gg_height", "gg_width", "alpha"),
  gg_barcode3 = c("gg_height", "gg_width", "alpha"),
  gg_beeswarm = c("gg_size", "rotation"),
  gg_ridgeline = c("alpha", "alpha_densitygroup"),
  gg_gridplot = c("gg_perN"),
  gg_quasirandom = c("gg_size", "gg_swarmwidth", "gg_method"),
  gg_divergingstackedbar = c("gg_cutpoint")
)

replace_data_name <- function(expr, new_name) {
  if (is.name(expr[[2]])) {
    expr[[2]] <- rlang::sym(new_name)
  } else {
    expr[[2]] <- replace_data_name(expr[[2]], new_name)
  }

  expr
}

insert_into_first_place <- function(expr, insert_expr) {
  if (is.name(expr[[2]])) {
    expr[[2]] <- rlang::expr(!!expr[[2]] %>% !!insert_expr)
  } else {
    expr[[2]] <- insert_into_first_place(expr[[2]], insert_expr)
  }

  expr
}

add_to_group <- function(expr, vars) {
  if (expr[[3]][[1]] == "dplyr::group_by") {
    expr[[3]] <- as.call(c(as.list(expr[[3]]), vars))
  } else if (expr[[3]][[1]] == "dplyr::ungroup") {
    expr[[3]] <- as.call(list(rlang::expr(dplyr::group_by), vars))
  }

  if (is.name(expr[[2]])) {
    expr
  } else {
    expr[[2]] <- add_to_group(expr[[2]], vars)
    expr
  }
}

rotate_gridplot <- function(expr) {
  if (expr[[1]] == "waffle::waffle") {
    expr$flip <- TRUE
  }

  expr
}

apply_palette <- function(expr, palette, type) {
  viridis_names <- unname(unlist(viridis_palette_names()))
  colour_plots <- c("gg_cumcurve", "gg_lollipop", "gg_freqpolygon", "gg_barcode", "gg_dotstrip", "gg_quasirandom", "gg_lollipop2", "gg_barcode3", "gg_dotstrip")

  if (palette %in% viridis_names) {
    if (type %in% colour_plots) {
      rlang::expr(!!expr + ggplot2::scale_colour_viridis_d(option = !!palette))
    } else {
      if (type != "gg_heatmap") {
        rlang::expr(!!expr + ggplot2::scale_fill_viridis_d(option = !!palette))
      } else {
        rlang::expr(!!expr + ggplot2::scale_fill_viridis_c(option = !!palette))
      }
    }
  } else if (palette == "greyscale") {
    if (type %in% colour_plots) {
      rlang::expr(!!expr + ggplot2::scale_colour_grey())
    } else {
      if (type != "gg_heatmap") {
        rlang::expr(!!expr + ggplot2::scale_fill_grey())
      } else {
        rlang::expr(!!expr + ggplot2::scale_fill_gradient(low = "white", high = "black"))
      }
    }
  } else {
    if (type %in% colour_plots) {
      rlang::expr(!!expr + ggplot2::scale_colour_brewer(palette = !!palette))
    } else {
      if (type != "gg_heatmap") {
        rlang::expr(!!expr + ggplot2::scale_fill_brewer(palette = !!palette))
      } else {
        rlang::expr(!!expr + ggplot2::scale_fill_distiller(palette = !!palette))
      }
    }
  }
}

check_nas <- function(data, exprs, data_name, plot_args) {
  plot_varnames <- unlist(plot_args[plot_args %in% names(data)])

  if (any(vapply(data[, plot_varnames, drop = FALSE], anyNA, logical(1)))) {
    complete <- complete.cases(data[, plot_varnames])

    plot_varnames <- rlang::syms(plot_varnames)

    if (is.null(exprs$data)) {
      exprs <- list(
        data = rlang::expr(plot_data <- !!rlang::sym(data_name) %>% tidyr::drop_na(!!!plot_varnames)),
        plot = replace_data_name(exprs$plot, "plot_data")
      )

      exprs$plot <- rlang::expr(!!exprs$plot + ggplot2::labs(subtitle = !!sprintf("%d Missing Observations Removed", sum(!complete))))
    } else {
      exprs$data[[3]] <- insert_into_first_place(exprs$data[[3]], rlang::expr(tidyr::drop_na()))

      exprs$plot <- rlang::expr(!!exprs$plot + ggplot2::labs(subtitle = !!sprintf("%d Missing Observations Removed", sum(!complete))))
    }
  }

  exprs
}

count_nas <- function(data, exprs, data_name, plot_args) {
  plot_varnames <- unlist(plot_args[plot_args %in% names(data)])

  if (any(vapply(data[, plot_varnames, drop = FALSE], anyNA, logical(1)))) {
    complete <- complete.cases(data[, plot_varnames])
    exprs$plot <- rlang::expr(!!exprs$plot + ggplot2::labs(subtitle = !!sprintf("%d Missing Observations Removed", sum(!complete))))
  }

  exprs
}

iNZightPlotGG_facet <- function(data, data_name, exprs, g1, g2, g1.level, g2.level) {
  if (!is.null(g1) && length(g1) > 0) {
    if (!is.null(g1.level) && g1.level != "_MULTI") {
      if (is.null(exprs$data)) {
        exprs <- list(
          data = rlang::expr(plot_data <- !!rlang::sym(data_name) %>% dplyr::filter(!!rlang::sym(g1) == !!g1.level)),
          plot = replace_data_name(exprs$plot, "plot_data")
        )
      } else {
        exprs$data[[3]] <- insert_into_first_place(exprs$data[[3]], rlang::expr(dplyr::filter(!!rlang::sym(g1) == !!g1.level)))
        exprs$data[[3]] <- add_to_group(exprs$data[[3]], rlang::sym(g1))
      }
    } else {
      if (!is.null(exprs$data)) {
        exprs$data[[3]] <- add_to_group(exprs$data[[3]], rlang::sym(g1))
      }
    }
  }

  if (!is.null(g2) && length(g2) > 0) {
    if (!is.null(g2.level) && g2.level != "_MULTI" && g2.level != "_ALL") {
      if (is.null(exprs$data)) {
        exprs <- list(
          data = rlang::expr(plot_data <- !!rlang::sym(data_name) %>% dplyr::filter(!!rlang::sym(g2) == !!g2.level)),
          plot = replace_data_name(exprs$plot, "plot_data")
        )

        exprs$data[[3]] <- add_to_group(exprs$data[[3]], rlang::sym(g2))
      } else {
        exprs$data[[3]] <- insert_into_first_place(exprs$data[[3]], rlang::expr(dplyr::filter(!!rlang::sym(g2) == !!g2.level)))
        exprs$data[[3]] <- add_to_group(exprs$data[[3]], rlang::sym(g2))
      }
    } else {
      if (!is.null(exprs$data)) {
        exprs$data[[3]] <- add_to_group(exprs$data[[3]], rlang::sym(g2))
      }
    }
  }

  if (isTRUE(is.null(g2) || length(g2) == 0)) {
    exprs$plot <- rlang::expr(!!exprs$plot + ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(g1)), labeller = ggplot2::label_both))
  } else {
    if (!is.null(g2.level) && g2.level == "_MULTI") {
      exprs$plot <- rlang::expr(!!exprs$plot + ggplot2::facet_grid(cols = ggplot2::vars(!!rlang::sym(g1)), rows = ggplot2::vars(!!rlang::sym(g2)), labeller = ggplot2::label_both))
    } else {
      exprs$plot <- rlang::expr(!!exprs$plot + ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(g1)), labeller = ggplot2::label_both))
    }
  }

  exprs
}

iNZightPlotGG_decide <- function(data, varnames, type, extra_vars) {
  varnames <- varnames[grep("\\.level$", names(varnames), invert = TRUE)]
  varnames <- varnames[grep("g1", names(varnames), invert = TRUE)]
  varnames <- varnames[grep("g2", names(varnames), invert = TRUE)]
  non_mapped <- varnames[grep("^(x|y)$", names(varnames), invert = TRUE)]
  varnames <- varnames[grep("^(x|y)$", names(varnames))]
  varnames <- varnames[varnames != ""]
  nullVars <- vapply(data[, varnames, drop = FALSE], is.null, FUN.VALUE = logical(1))
  varnames[which(nullVars)] <- NULL

  varnames[!varnames %in% colnames(data)] <- NULL

  if (type %in% c("gg_pie", "gg_donut")) {
    names(varnames) <- replace(names(varnames), names(varnames) == "x", "fill")
  } else if (type %in% c("gg_violin", "gg_barcode", "gg_boxplot", "gg_cumcurve", "gg_column2", "gg_lollipop", "gg_dotstrip", "gg_density", "gg_barcode2", "gg_beeswarm", "gg_ridgeline", "gg_quasirandom", "gg_barcode3")) {
    if (!("y" %in% names(varnames))) {
      names(varnames) <- replace(names(varnames), names(varnames) == "x", "y")
      if (isTRUE(!is.null(extra_vars$fill_colour) && extra_vars$fill_colour != "")) {
        if (type %in% c("gg_lollipop", "gg_cumcurve", "gg_barcode", "gg_dotstrip", "gg_quasirandom", "gg_barcode3")) {
          varnames["colour"] <- extra_vars$fill_colour
        } else {
          varnames["fill"] <- extra_vars$fill_colour
        }
      } else if (type != "gg_cumcurve") {
        varnames["fill"] <- "darkgreen"
      }
    } else if (is.numeric(data[[varnames["x"]]])) {
      orig_x <- varnames["x"]
      varnames["x"] <- varnames["y"]
      varnames["y"] <- orig_x
    }

    # if (type %in% c("gg_barcode", "gg_dotstrip") && isTRUE(!is.null(extra_vars$fill_colour) && extra_vars$fill_colour != "")) {
    #   varnames["colour"] <- extra_vars$fill_colour
    # }
  } else if (type %in% c("gg_stackedbar", "gg_stackedcolumn")) {
    names(varnames) <- replace(names(varnames), names(varnames) == "x", "fill")
    if ("y" %in% names(varnames)) {
      names(varnames) <- replace(names(varnames), names(varnames) == "y", "x")
    }
  } else if (type == "gg_poppyramid") {
    if (is.numeric(data[[varnames["x"]]])) {
      names(varnames) <- replace(names(varnames), names(varnames) == "y", "fill")
    } else {
      names(varnames) <- replace(names(varnames), names(varnames) == "x", "fill")
      names(varnames) <- replace(names(varnames), names(varnames) == "y", "x")
    }
  } else if (type == "gg_spine") {
    names(varnames) <- replace(names(varnames), names(varnames) == "y", "fill")
  } else if (type == "gg_freqpolygon") {
    names(varnames) <- replace(names(varnames), names(varnames) == "y", "colour")
  } else if (type == "gg_column") {
    if ("y" %in% names(varnames)) {
      names(varnames) <- replace(names(varnames), names(varnames) == "y", "group")
    }
  }

  if (type %in% c("gg_column2", "gg_lollipop")) {
    names(varnames) <- replace(names(varnames), names(varnames) == "labels", "x")
  }

  extra_args <- Filter(Negate(is.null), extra_vars[optional_args[[type]]])

  varnames <- as.list(varnames)

  if (!is.null(extra_args) && length(extra_args) > 0) {
    varnames <- append(as.list(varnames), as.list(extra_args))
    names(varnames) <- sub("^gg_", "", names(varnames))

    if (type %in% c("gg_barcode")) {
      if ("barSize" %in% names(varnames)) {
        names(varnames) <- replace(names(varnames), names(varnames) == "barSize", "size")
      } else {
        varnames[["size"]] <- 16
      }
    }

    if (type %in% c("gg_barcode2")) {
      if ("width" %in% names(varnames)) {
        varnames[["width"]] <- as.numeric(varnames[["width"]])
        # names(non_mapped) <- replace(names(non_mapped), names(non_mapped) == "gg_width", "width")
      }

      if ("height" %in% names(varnames)) {
        varnames[["height"]] <- as.numeric(varnames[["height"]])
        # names(non_mapped) <- replace(names(non_mapped), names(non_mapped) == "gg_height", "height")
      }
    }

    if (type %in% c("gg_barcode3")) {
      if ("width" %in% names(varnames)) {
        varnames[["size"]] <- as.numeric(varnames[["width"]])
        varnames[["width"]] <- NULL
      }

      if ("height" %in% names(varnames)) {
        varnames[["radius"]] <- as.numeric(varnames[["height"]])
        varnames[["height"]] <- NULL
      }
    }

    if (type %in% c("gg_density", "gg_ridgeline")) {
      if ("x" %in% names(varnames)) {
        varnames[["alpha"]] <- NULL
        varnames[["alpha_density"]] <- NULL

        if (!is.null(varnames[["alpha_densitygroup"]])) {
          names(varnames) <- replace(names(varnames), names(varnames) == "alpha_densitygroup", "alpha")
        } else {
          varnames[["alpha"]] <- 0.6
        }
      }

      if (!is.null(varnames[["alpha"]])) {
        varnames[["alpha"]] <- as.numeric(varnames[["alpha"]])
      }
    }

    if (type %in% c("gg_quasirandom")) {
      names(varnames) <- replace(names(varnames), names(varnames) == "swarmwidth", "width")
    }

    if (type %in% c("gg_lollipop2")) {
      if (!("y" %in% names(varnames))) {
        if (isTRUE(!is.null(extra_vars$fill_colour) && extra_vars$fill_colour != "")) {
          varnames[["colour"]] <- extra_vars$fill_colour
        }
      }
    }
  }

  if (type %in% c("gg_lollipop", "gg_lollipop2", "gg_freqpolygon", "gg_dotstrip", "gg_beeswarm", "gg_quasirandom")) {
    if (!("size" %in% names(varnames))) {
      varnames[["size"]] <- 6
    }
  }

  append(varnames, as.list(non_mapped))
}

iNZightPlotGG_extraargs <- function(extra_args) {
  to.keep <- c(
    "shape" = "pch",
    "colour" = "col.pt",
    "size" = "cex",
    "alpha" = "alpha",
    "bg" = "bg",
    "adjust" = "adjust",
    "lwd" = "lwd",
    "gg_lwd" = "gg_lwd",
    "mean_indicator" = "mean_indicator"
  )

  extra_args <- extra_args[to.keep]

  changed_args <- Filter(function(x) extra_args[[x]] != inzpar()[[x]], names(extra_args))

  return_args <- extra_args[changed_args]
  names(return_args) <- names(to.keep)[match(names(return_args), to.keep)]

  return_args
}

##' @importFrom magrittr "%>%"
##' @importFrom rlang ":="
iNZightPlotGG <- function(
    data,
    type,
    data_name = "data",
    ...,
    main = NULL,
    xlab = NULL,
    ylab = NULL,
    caption = NULL,
    extra_args = c(),
    palette = "default",
    gg_theme = "grey") {
  dots <- list(...)

  if (length(extra_args) > 0) {
    rotate <- extra_args$rotation
    desc <- extra_args$desc
    overall_size <- extra_args$cex
    rotate_labels <- extra_args$rotate_labels

    extra_args$desc <- desc
  }

  plot_args <- iNZightPlotGG_decide(data, unlist(dots), type, extra_args)

  plot_exprs <- do.call(
    sprintf("iNZightPlotGG_%s", gsub("^gg_", "", type)),
    c(rlang::sym(data_name), main = main, xlab = xlab, ylab = ylab, plot_args)
  )

  if (!(type %in% c("gg_pie", "gg_donut", "gg_cumcurve"))) {
    if (type == "gg_gridplot" && isTRUE(rotate)) {
      plot_exprs$plot <- rotate_gridplot(plot_exprs$plot)
    } else {
      default_rotated <- c("gg_boxplot", "gg_violin", "gg_beeswarm", "gg_quasirandom", "gg_lollipop", "gg_column2", "gg_spine")

      if (type %in% default_rotated) {
        rotate <- if (!is.null(rotate)) !rotate else TRUE
      }

      if (isTRUE(rotate)) {
        plot_exprs$plot <- rotate(plot_exprs$plot)
      }
    }
  }

  if (length(gg_theme) > 0 && gg_theme != "grey") {
    theme_fun <- list(
      "bw" = rlang::expr(ggplot2::theme_bw()),
      "light" = rlang::expr(ggplot2::theme_light()),
      "dark" = rlang::expr(ggplot2::theme_dark()),
      "minimal" = rlang::expr(ggplot2::theme_minimal()),
      "classic" = rlang::expr(ggplot2::theme_classic()),
      "void" = rlang::expr(ggplot2::theme_void()),
      "stata" = rlang::expr(ggthemes::theme_stata()),
      "wsj" = rlang::expr(ggthemes::theme_wsj()),
      "tufte" = rlang::expr(ggthemes::theme_tufte()),
      "gdocs" = rlang::expr(ggthemes::theme_gdocs()),
      "fivethirtyeight" = rlang::expr(ggthemes::theme_fivethirtyeight()),
      "excel" = rlang::expr(ggthemes::theme_excel()),
      "economist" = rlang::expr(ggthemes::theme_economist())
    )[[gg_theme]]

    plot_exprs$plot <- rlang::expr(!!plot_exprs$plot + !!theme_fun)
  }

  plot_exprs$plot <- rlang::expr(!!plot_exprs$plot +
    ggplot2::theme(
      plot.title = ggtext::element_textbox_simple(
        margin = ggplot2::margin(0, 0, 8, 0)
      ),
      plot.title.position = "plot",
      axis.title.x = ggtext::element_textbox_simple(
        halign = 0.5,
        margin = ggplot2::margin(10, 0, 8, 0)
      )
    ))

  if (!is.null(extra_args$mean_indicator) && (
    isTRUE(extra_args$mean_indicator) || extra_args$mean_indicator %in% c("grand", "group")
  )) {
    if (type %in% c("gg_boxplot")) {
      plot_exprs$plot <- rlang::expr(
        !!plot_exprs$plot +
          ggplot2::geom_point(
            data = function(x) {
              x %>%
                dplyr::group_by(!!rlang::sym(plot_args$x), drop = FALSE) %>%
                dplyr::summarize(Mean = mean(!!rlang::sym(plot_args$y), na.rm = TRUE))
            },
            ggplot2::aes(
              x = !!rlang::sym(plot_args$x),
              y = !!rlang::sym("Mean")
            ),
            shape = 8,
            size = 2,
            colour = "black"
          )
      )
    }
    if (type %in% c("gg_density")) {
      dexpr <- rlang::expr(!!rlang::sym("x"))
      fill <- "mean"
      mean_palette <- rlang::expr(ggplot2::scale_colour_manual(values = c(mean = "black")))
      if (!is.null(plot_args$x) && extra_args$mean_indicator == "group") {
        dexpr <- rlang::expr(!!dexpr %>%
          dplyr::group_by(!!rlang::sym(plot_args$x), drop = FALSE))
        fill <- rlang::sym(plot_args$x)
        mean_palette <- NULL
      }
      dexpr <- rlang::expr(!!dexpr %>%
        dplyr::summarise(Mean = mean(!!rlang::sym(plot_args$y), na.rm = TRUE)))
      plot_exprs$plot <- rlang::expr(
        !!plot_exprs$plot +
          ggplot2::geom_vline(
            data = function(x) {
              !!dexpr
            },
            ggplot2::aes(
              xintercept = !!rlang::sym("Mean"),
              colour = !!fill
            ),
            lty = 2
          )
      )
      if (!is.null(mean_palette)) {
        plot_exprs$plot <- rlang::expr(
          !!plot_exprs$plot + !!mean_palette
        )
      }
    }
  }

  if (exists("rotate_labels") && !(type %in% c("gg_pie", "gg_donut", "gg_cumcurve", "gg_gridplot"))) {
    if (isTRUE(rotate_labels$x)) {
      plot_exprs$plot <- rlang::expr(!!plot_exprs$plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)))
    }

    if (isTRUE(rotate_labels$y)) {
      plot_exprs$plot <- rlang::expr(!!plot_exprs$plot + ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)))
    }
  }

  if (exists("overall_size") && !is.null(overall_size) && isTRUE(overall_size != 1)) {
    plot_exprs$plot <- rlang::expr(!!plot_exprs$plot + ggplot2::theme(text = ggplot2::element_text(size = !!(as.numeric(overall_size) * 11))))
  }

  if (isTRUE(!extra_args$bg %in% c("lightgrey", "#eeeeee") && type != "gg_gridplot")) {
    plot_exprs$plot <- rlang::expr(!!plot_exprs$plot + ggplot2::theme(panel.background = ggplot2::element_rect(fill = !!extra_args$bg)))
  }

  if (isTRUE(!is.null(dots$g1) && length(dots$g1) > 0)) {
    plot_exprs <- iNZightPlotGG_facet(data, data_name, plot_exprs, dots$g1, dots$g2, dots$g1.level, dots$g2.level)
  }

  if (isTRUE(!missing(palette) && !is.null(palette) && palette != "default")) {
    plot_exprs$plot <- apply_palette(plot_exprs$plot, palette, type)
  }

  if (!(type %in% c("gg_lollipop", "gg_column2"))) {
    plot_exprs <- check_nas(data, plot_exprs, data_name, unname(plot_args))
  } else {
    plot_exprs <- count_nas(data, plot_exprs, data_name, unname(plot_args))
  }

  if (isTRUE(!is.null(caption) && caption != "")) {
    plot_exprs$plot <- rlang::expr(!!plot_exprs$plot + ggplot2::labs(caption = caption))
  }

  if (type %in% c("gg_barcode3", "gg_dotstrip", "gg_ridgeline")) {
    plot_exprs$plot <- rlang::expr(
      !!plot_exprs$plot + ggplot2::scale_y_discrete(limits = rev)
    )
    # } else if (type %in% c("gg_violin", "gg_boxplot", "gg_beeswarm", "gg_quasirandom")) {
  } else if (type %in% c("gg_violin", "gg_boxplot", "gg_quasirandom") && rotate) {
    plot_exprs$plot <- rlang::expr(
      !!plot_exprs$plot + ggplot2::scale_x_discrete(limits = rev)
    )
  }

  eval_env <- rlang::env(!!rlang::sym(data_name) := data)
  eval_results <- lapply(plot_exprs, eval, envir = eval_env)

  plot_object <- eval_results[[length(eval_results)]]

  dev.hold()
  tryCatch(
    print(plot_object),
    finally = dev.flush()
  )

  attr(plot_object, "code") <- unname(unlist(lapply(plot_exprs, rlang::expr_text)))
  attr(plot_object, "code_expr") <- plot_exprs
  attr(plot_object, "data_name") <- data_name
  attr(plot_object, "plottype") <- c(type)
  attr(plot_object, "varnames") <- unlist(dots)
  attr(plot_object, "use.plotly") <- !type %in% c("gg_pie", "gg_donut", "gg_gridplot", "gg_barcode2", "gg_barcode", "gg_ridgeline")

  if (type %in% c("gg_lollipop", "gg_column2")) {
    attr(plot_object, "varnames") <- attr(plot_object, "varnames")[names(attr(plot_object, "varnames")) != "y"]
  }

  invisible(plot_object)
}

iNZightPlotGG_pie <- function(data, fill, main = sprintf("Pie Chart of %s", as.character(fill)), ordered = FALSE, ...) {
  fill <- rlang::sym(fill)

  if (ordered == "desc") {
    data_expr <- rlang::expr(
      plot_data <- !!rlang::enexpr(data) %>%
        dplyr::mutate(!!fill := forcats::fct_infreq(!!fill))
    )

    data <- rlang::sym("plot_data")
  } else if (ordered == "asc") {
    data_expr <- rlang::expr(
      plot_data <- !!rlang::enexpr(data) %>%
        dplyr::mutate(!!fill := forcats::fct_rev(forcats::fct_infreq(!!fill)))
    )

    data <- rlang::sym("plot_data")
  }

  plot_expr <- rlang::expr(
    ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = factor(1), fill = !!fill)) +
      ggplot2::geom_bar(
        ggplot2::aes(
          y = ggplot2::after_stat(!!rlang::sym("count")) /
            sum(ggplot2::after_stat(!!rlang::sym("count")))
        ),
        position = "fill"
      ) +
      ggplot2::coord_polar(theta = "y") +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      ggplot2::scale_y_reverse() +
      ggplot2::scale_x_discrete(breaks = NULL) +
      ggplot2::ggtitle(!!main) +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.text.x      = ggplot2::element_blank()
      )
  )

  if (ordered %in% c("asc", "desc")) {
    list(
      data = data_expr,
      plot = plot_expr
    )
  } else {
    list(
      plot = plot_expr
    )
  }
}

iNZightPlotGG_donut <- function(data, fill, main = sprintf("Donut Chart of %s", as.character(fill)), ordered = FALSE, ...) {
  fill <- rlang::sym(fill)

  if (ordered == "desc") {
    data_expr <- rlang::expr(
      plot_data <- !!rlang::enexpr(data) %>%
        dplyr::mutate(!!fill := forcats::fct_infreq(!!fill)) %>%
        dplyr::group_by(!!fill) %>%
        dplyr::summarise(Count = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Fraction = !!rlang::sym("Count") / sum(!!rlang::sym("Count"))) %>%
        dplyr::arrange(dplyr::desc(!!rlang::sym("Fraction"))) %>%
        dplyr::mutate(ymax = cumsum(!!rlang::sym("Fraction"))) %>%
        dplyr::mutate(ymin = dplyr::lag(!!rlang::sym("ymax"), default = 0))
    )
  } else if (ordered == "asc") {
    data_expr <- rlang::expr(
      plot_data <- !!rlang::enexpr(data) %>%
        dplyr::mutate(!!fill := forcats::fct_rev(forcats::fct_infreq(!!fill))) %>%
        dplyr::group_by(!!fill) %>%
        dplyr::summarise(Count = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Fraction = !!rlang::sym("Count") / sum(!!rlang::sym("Count"))) %>%
        dplyr::arrange(!!rlang::sym("Fraction")) %>%
        dplyr::mutate(ymax = cumsum(!!rlang::sym("Fraction"))) %>%
        dplyr::mutate(ymin = dplyr::lag(!!rlang::sym("ymax"), default = 0))
    )
  } else {
    data_expr <- rlang::expr(
      plot_data <- !!rlang::enexpr(data) %>%
        dplyr::group_by(!!fill) %>%
        dplyr::summarise(Count = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Fraction = !!rlang::sym("Count") / sum(!!rlang::sym("Count"))) %>%
        dplyr::mutate(ymax = cumsum(!!rlang::sym("Fraction"))) %>%
        dplyr::mutate(ymin = dplyr::lag(!!rlang::sym("ymax"), default = 0))
    )
  }

  plot_expr <- rlang::expr(
    ggplot2::ggplot(plot_data, ggplot2::aes(fill = !!fill, ymax = !!rlang::sym("ymax"), ymin = !!rlang::sym("ymin"), xmax = 4, xmin = 3)) +
      ggplot2::geom_rect() +
      ggplot2::coord_polar(theta = "y") +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      ggplot2::scale_x_continuous(breaks = NULL, limits = c(0, 4)) +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::ggtitle(!!main) +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.text.x      = ggplot2::element_blank()
      )
  )

  list(
    data = data_expr,
    plot = plot_expr
  )
}

iNZightPlotGG_column <- function(data, x, group, main = sprintf("Column chart of %s", as.character(x)), xlab = as.character(x), ylab = "Count", ordered = FALSE, ...) {
  x <- rlang::sym(x)

  if (ordered == "desc") {
    data_expr <- rlang::expr(
      plot_data <- !!rlang::enexpr(data) %>%
        dplyr::mutate(!!x := forcats::fct_infreq(!!x))
    )

    data <- rlang::sym("plot_data")
  } else if (ordered == "asc") {
    data_expr <- rlang::expr(
      plot_data <- !!rlang::enexpr(data) %>%
        dplyr::mutate(!!x := forcats::fct_rev(forcats::fct_infreq(!!x)))
    )

    data <- rlang::sym("plot_data")
  }

  if (missing(group)) {
    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x, fill = !!x)) +
        ggplot2::geom_bar() +
        ggplot2::labs(title = !!main, fill = stringr::str_wrap(!!xlab, 40)) +
        ggplot2::xlab(!!xlab) +
        ggplot2::ylab(!!ylab)
    )
  } else {
    group <- rlang::sym(group)

    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x, fill = !!group)) +
        ggplot2::geom_bar(position = "dodge") +
        ggplot2::labs(title = !!main) +
        ggplot2::xlab(!!xlab) +
        ggplot2::ylab(!!ylab)
    )
  }


  if (ordered %in% c("asc", "desc")) {
    list(
      data = data_expr,
      plot = plot_expr
    )
  } else {
    list(
      plot = plot_expr
    )
  }
}

rotate <- function(plot_expr) {
  check_for_function <- function(expr, fun, i = 0) {
    if (length(expr) == 1) {
      as.character(expr) == fun
    } else {
      if (rlang::call_name(expr[[3]]) == fun) {
        TRUE
      } else {
        check_for_function(expr[[2]], fun)
      }
    }
  }

  remove_function <- function(expr, fun, i = 0) {
    if (length(expr) == 1) {
      if (as.character(expr) == fun) {
        expr <- NULL
        expr
      }
    } else {
      if (rlang::call_name(expr[[3]]) == fun) {
        expr[[2]]
      } else {
        expr[[2]] <- remove_function(expr[[2]], fun)
        expr
      }
    }
  }

  if (check_for_function(plot_expr, "coord_flip")) {
    remove_function(plot_expr, "coord_flip")
  } else {
    rlang::expr(!!plot_expr + ggplot2::coord_flip())
  }
}

iNZightPlotGG_bar <- function(data, x, main = "Bar chart", ...) {
  column_plot <- iNZightPlotGG_column(data, x, main, ...)

  column_plot$plot <- rotate(column_plot$plot)

  column_plot
}

iNZightPlotGG_heatmap <- function(data, x, y, main = sprintf("Heatmap of %s and %s", as.character(x), as.character(y)), xlab = as.character(x), ylab = as.character(y), ...) {
  x <- rlang::sym(x)
  y <- rlang::sym(y)

  data_expr <- rlang::expr(
    plot_data <- !!rlang::enexpr(data) %>%
      dplyr::group_by(!!x, !!y) %>%
      dplyr::summarise(Count = dplyr::n())
  )

  plot_expr <- rlang::expr(
    ggplot2::ggplot(plot_data, ggplot2::aes(x = !!x, y = !!y)) +
      ggplot2::geom_tile(ggplot2::aes(fill = !!rlang::sym("Count"))) +
      ggplot2::labs(title = !!main) +
      ggplot2::xlab(!!xlab) +
      ggplot2::ylab(!!ylab)
  )

  list(
    data = data_expr,
    plot = plot_expr
  )
}

iNZightPlotGG_stackedcolumn <- function(data, fill, main = sprintf("Stacked column of %s", as.character(fill)), x, xlab = as.character(x), ylab = "Percent", ...) {
  fill <- rlang::sym(fill)

  if (missing(x)) {
    x <- rlang::expr(factor(1))
    was_missing <- TRUE
  } else {
    x <- rlang::sym(x)
    was_missing <- FALSE
  }

  plot_expr <- rlang::expr(
    ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x, fill = !!fill)) +
      ggplot2::geom_bar(
        ggplot2::aes(
          y = ggplot2::after_stat(!!rlang::sym("count")) /
            sum(ggplot2::after_stat(!!rlang::sym("count")))
        ),
        position = "fill"
      ) +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::labs(title = !!main, fill = stringr::str_wrap(!!as.character(fill), 40)) +
      ggplot2::xlab(!!xlab) +
      ggplot2::ylab(!!ylab)
  )

  if (isTRUE(was_missing)) {
    plot_expr <- rlang::expr(
      !!plot_expr +
        ggplot2::scale_x_discrete(breaks = NULL) +
        ggplot2::xlab("")
    )
  } else {
    plot_expr <- rlang::expr(
      !!plot_expr +
        ggplot2::xlab(!!xlab)
    )
  }

  list(
    plot = plot_expr
  )
}

iNZightPlotGG_stackedbar <- function(data, fill, main = sprintf("Stacked bar of %s", as.character(fill)), x, ...) {
  column_plot <- iNZightPlotGG_stackedcolumn(!!rlang::enexpr(data), fill, main, x, ...)

  column_plot$plot <- rotate(column_plot$plot)

  column_plot
}

iNZightPlotGG_violin <- function(data, x, y, fill = "darkgreen", main = sprintf("Distribution of %s", as.character(y)), xlab = as.character(x), ylab = as.character(y), ...) {
  y <- rlang::sym(y)

  dots <- list(...)

  if (missing(x)) {
    x <- rlang::expr(factor(1))

    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x, y = !!y)) +
        ggplot2::geom_violin(fill = !!fill, !!!dots) +
        ggplot2::labs(title = !!main, fill = stringr::str_wrap(!!as.character(fill), 40)) +
        ggplot2::xlab("") +
        ggplot2::ylab(!!ylab) +
        ggplot2::theme(
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        )
    )
  } else {
    x <- rlang::sym(x)
    fill <- rlang::sym(x)

    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x, y = !!y, fill = !!fill)) +
        ggplot2::geom_violin(!!!dots) +
        ggplot2::labs(title = !!main, fill = stringr::str_wrap(!!as.character(fill), 40)) +
        ggplot2::xlab(!!xlab) +
        ggplot2::ylab(!!ylab)
    )
  }

  list(
    plot = plot_expr
  )
}

iNZightPlotGG_barcode <- function(data, x, y, fill = "darkgreen", main = sprintf("Distribution of %s", as.character(y)), xlab = as.character(y), ylab = as.character(x), ...) {
  y <- rlang::sym(y)
  dots <- list(...)

  if (missing(x)) {
    x <- rlang::expr(factor(1))

    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!y, y = !!x)) +
        ggplot2::geom_point(shape = "|", !!!dots) +
        ggplot2::labs(title = !!main) +
        ggplot2::xlab(!!xlab) +
        ggplot2::ylab("") +
        ggplot2::theme(
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        )
    )
  } else {
    x <- rlang::sym(x)
    colour <- rlang::sym(x)

    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!y, y = !!x)) +
        ggplot2::geom_point(shape = "|", !!!dots) +
        ggplot2::labs(title = !!main) +
        ggplot2::xlab(!!xlab) +
        ggplot2::ylab(!!ylab)
    )
  }

  list(
    plot = plot_expr
  )
}

iNZightPlotGG_barcode2 <- function(data, x, y, fill = "darkgreen", main = sprintf("Distribution of %s", as.character(y)), xlab = as.character(y), ylab = as.character(x), ...) {
  y <- rlang::sym(y)
  dots <- list(...)

  if (missing(x)) {
    x <- rlang::expr(factor(1))

    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!y, y = !!x)) +
        ggplot2::geom_tile(!!!dots) +
        ggplot2::labs(title = !!main) +
        ggplot2::xlab(!!xlab) +
        ggplot2::ylab("") +
        ggplot2::theme(
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        )
    )
  } else {
    x <- rlang::sym(x)
    colour <- rlang::sym(x)

    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!y, y = !!x)) +
        ggplot2::geom_tile(!!!dots) +
        ggplot2::labs(title = !!main) +
        ggplot2::xlab(!!xlab) +
        ggplot2::ylab(!!ylab)
    )
  }

  list(
    plot = plot_expr
  )
}

iNZightPlotGG_barcode3 <- function(data, x, y, fill = "darkgreen", main = sprintf("Distribution of %s", as.character(y)), xlab = as.character(y), ylab = as.character(x), ...) {
  y <- rlang::sym(y)
  dots <- list(...)

  if (is.null(dots$radius)) {
    radius <- 0.5
    dots$radius <- 0.5
  } else {
    radius <- dots$radius
  }

  if (is.null(dots$size)) {
    dots$size <- 1
  }

  if (missing(x)) {
    x <- rlang::expr(factor(1))

    if (!is.null(dots$size)) {
      # ggplot2::geom_spoke has deprecated `size` in favor of `linewidth`
      dots$linewidth <- dots$size
      dots$size <- NULL
    }

    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!y, y = !!x)) +
        ggplot2::geom_spoke(angle = pi / 2, position = ggplot2::position_nudge(y = -!!radius / 2), !!!dots) +
        ggplot2::labs(title = !!main) +
        ggplot2::xlab(!!xlab) +
        ggplot2::ylab("") +
        ggplot2::theme(
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        )
    )
  } else {
    x <- rlang::sym(x)
    colour <- rlang::sym(x)

    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!y, y = !!x, colour = !!colour)) +
        ggplot2::geom_spoke(angle = pi / 2, position = ggplot2::position_nudge(y = -!!radius / 2), !!!dots) +
        ggplot2::labs(title = !!main, colour = stringr::str_wrap(!!as.character(colour), 40)) +
        ggplot2::xlab(!!xlab) +
        ggplot2::ylab(!!ylab)
    )
  }

  list(
    plot = plot_expr
  )
}

iNZightPlotGG_boxplot <- function(data, x, y, fill = "darkgreen", main = sprintf("Distribution of %s", as.character(y)), xlab = as.character(x), ylab = as.character(y), ...) {
  y <- rlang::sym(y)
  dots <- list(...)

  if (missing(x)) {
    x <- rlang::expr(factor(1))

    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x, y = !!y)) +
        ggplot2::geom_boxplot(fill = !!fill, !!!dots) +
        ggplot2::labs(title = !!main) +
        ggplot2::xlab("") +
        ggplot2::ylab(!!ylab) +
        ggplot2::theme(
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        )
    )
  } else {
    x <- rlang::sym(x)
    fill <- rlang::sym(x)

    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x, y = !!y, fill = !!fill)) +
        ggplot2::geom_boxplot(!!!dots) +
        ggplot2::labs(title = !!main, fill = stringr::str_wrap(!!as.character(fill), 40)) +
        ggplot2::xlab(!!xlab) +
        ggplot2::ylab(!!ylab)
    )
  }

  list(
    plot = plot_expr
  )
}

iNZightPlotGG_column2 <- function(data, x, y, main = sprintf("Distribution of %s", as.character(y)), xlab = "Index", ylab = as.character(y), desc = FALSE, labels, ...) {
  y <- rlang::sym(y)
  dots <- list(...)

  if (missing(x)) {
    if (missing(labels) || labels == "") {
      x <- rlang::expr(1:nrow(!!rlang::enexpr(data)))

      data_expr <- rlang::expr(
        plot_data <- !!rlang::enexpr(data) %>%
          dplyr::arrange(!!y)
      )
    } else {
      x <- rlang::sym(labels)

      data_expr <- rlang::expr(
        plot_data <- !!rlang::enexpr(data) %>%
          dplyr::arrange(!!y) %>%
          dplyr::mutate(!!x := forcats::fct_reorder(!!x, !!y))
      )
    }
  } else {
    x <- rlang::sym(x)

    data_expr <- rlang::expr(
      plot_data <- !!rlang::enexpr(data) %>%
        dplyr::arrange(!!y) %>%
        dplyr::mutate(!!x := forcats::fct_reorder(!!x, !!y))
    )
  }

  plot_expr <- rlang::expr(
    ggplot2::ggplot(plot_data, ggplot2::aes(x = !!x, y = !!y)) +
      ggplot2::geom_col(!!!dots) +
      ggplot2::labs(title = !!main) +
      ggplot2::xlab(!!xlab) +
      ggplot2::ylab(!!ylab)
  )

  list(
    data = data_expr,
    plot = plot_expr
  )
}

iNZightPlotGG_lollipop <- function(data, x, y, main = sprintf("Distribution of %s", as.character(y)), xlab = "Index", ylab = as.character(y), desc = FALSE, labels, ...) {
  y <- rlang::sym(y)
  dots <- list(...)

  point_dots <- dots[c("size", "colour")]
  line_dots <- dots[c("lwd", "colour")]

  point_dots <- Filter(Negate(is.null), point_dots)
  line_dots <- Filter(Negate(is.null), line_dots)

  if (missing(x)) {
    if (missing(labels) || labels == "") {
      x <- rlang::expr(1:nrow(!!rlang::enexpr(data)))

      data_expr <- rlang::expr(
        plot_data <- !!rlang::enexpr(data) %>%
          dplyr::arrange(!!y)
      )
    } else {
      x <- rlang::sym(labels)

      data_expr <- rlang::expr(
        plot_data <- !!rlang::enexpr(data) %>%
          dplyr::arrange(!!y) %>%
          dplyr::mutate(!!x := forcats::fct_reorder(!!x, !!y))
      )
    }
  } else {
    x <- rlang::sym(x)

    data_expr <- rlang::expr(
      plot_data <- !!rlang::enexpr(data) %>%
        dplyr::arrange(!!y) %>%
        dplyr::mutate(!!x := forcats::fct_reorder(!!x, !!y))
    )
  }

  plot_expr <- rlang::expr(
    ggplot2::ggplot(plot_data, ggplot2::aes(x = !!x, y = !!y)) +
      ggplot2::geom_segment(ggplot2::aes(xend = !!x, yend = 0), !!!line_dots) +
      ggplot2::geom_point(!!!point_dots) +
      ggplot2::labs(title = !!main) +
      ggplot2::xlab(!!xlab) +
      ggplot2::ylab(!!ylab)
  )

  list(
    data = data_expr,
    plot = plot_expr
  )
}

iNZightPlotGG_cumcurve <- function(data, x, y, main = sprintf("Cumulative Curve of %s", as.character(y)), xlab = as.character(y), ylab = "Cumulative Frequency", ...) {
  y <- rlang::sym(y)
  dots <- list(...)

  if (missing(x)) {
    data_expr <- rlang::expr(
      plot_data <- !!rlang::enexpr(data) %>%
        dplyr::arrange(!!y) %>%
        dplyr::mutate(Observation = 1:dplyr::n())
    )

    plot_expr <- rlang::expr(
      ggplot2::ggplot(plot_data, ggplot2::aes(x = !!y, y = !!rlang::sym("Observation"))) +
        ggplot2::geom_step(!!!dots) +
        ggplot2::labs(title = !!main) +
        ggplot2::xlab(!!xlab) +
        ggplot2::ylab(!!ylab)
    )
  } else {
    x <- rlang::sym(x)

    data_expr <- rlang::expr(
      plot_data <- !!rlang::enexpr(data) %>%
        dplyr::group_by(!!x) %>%
        dplyr::arrange(!!x, !!y) %>%
        dplyr::mutate(Observation = 1:dplyr::n())
    )

    plot_expr <- rlang::expr(
      ggplot2::ggplot(plot_data, ggplot2::aes(x = !!y, y = !!rlang::sym("Observation"), colour = !!x)) +
        ggplot2::geom_step(!!!dots) +
        ggplot2::labs(title = !!main, colour = stringr::str_wrap(!!as.character(x), 40)) +
        ggplot2::xlab(!!xlab) +
        ggplot2::ylab(!!ylab)
    )
  }

  list(
    data = data_expr,
    plot = plot_expr
  )
}

iNZightPlotGG_poppyramid <- function(data, x, fill, main = sprintf("Count of %s by %s", as.character(x), as.character(fill)), xlab = as.character(x), ylab = "Count", ...) {
  x <- rlang::sym(x)
  fill <- rlang::sym(fill)
  dots <- list(...)

  plot_expr <- rlang::expr(
    ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x, fill = !!fill)) +
      ggplot2::geom_histogram(data = subset(!!rlang::enexpr(data), !!fill == levels(!!fill)[1]), !!!dots) +
      ggplot2::geom_histogram(
        data = subset(
          !!rlang::enexpr(data),
          !!fill == levels(!!fill)[2]
        ),
        ggplot2::aes(
          y = ggplot2::after_stat(!!rlang::sym("count")) * -1
        ),
        !!!dots
      ) +
      ggplot2::labs(title = !!main, fill = stringr::str_wrap(!!as.character(fill), 40)) +
      ggplot2::xlab(!!xlab) +
      ggplot2::ylab(!!ylab) +
      ggplot2::scale_y_continuous(labels = abs)
  )

  list(
    plot = plot_expr
  )
}

iNZightPlotGG_spine <- function(data, x, fill, main = sprintf("Count of %s by %s", as.character(x), as.character(fill)), xlab = as.character(x), ylab = "Count", ...) {
  x <- rlang::sym(x)
  fill <- rlang::sym(fill)
  dots <- list(...)

  plot_expr <- rlang::expr(
    ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x, fill = !!fill)) +
      ggplot2::geom_bar(data = subset(!!rlang::enexpr(data), !!fill == levels(!!fill)[1]), !!!dots) +
      ggplot2::geom_bar(
        data = subset(
          !!rlang::enexpr(data),
          !!fill == levels(!!fill)[2]
        ),
        ggplot2::aes(
          y = !!rlang::sym("count") * -1
        ),
        !!!dots
      ) +
      ggplot2::coord_flip() +
      ggplot2::labs(title = !!main, fill = stringr::str_wrap(!!as.character(fill), 40)) +
      ggplot2::xlab(!!xlab) +
      ggplot2::ylab(!!ylab) +
      ggplot2::scale_y_continuous(labels = abs)
  )

  list(
    plot = plot_expr
  )
}

iNZightPlotGG_freqpolygon <- function(data, x, colour, main = sprintf("Count of %s by %s", as.character(x), as.character(colour)), xlab = as.character(x), ylab = "Count", ...) {
  x <- rlang::sym(x)
  colour <- rlang::sym(colour)
  dots <- list(...)

  point_dots <- dots[c("size", "colour")]
  line_dots <- dots[c("lwd", "colour")]

  point_dots <- Filter(Negate(is.null), point_dots)
  line_dots <- Filter(Negate(is.null), line_dots)

  plot_expr <- rlang::expr(
    ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x, colour = !!colour, group = !!colour)) +
      ggplot2::geom_line(stat = "count", !!!line_dots) +
      ggplot2::geom_point(stat = "count", !!!point_dots) +
      ggplot2::labs(title = !!main, colour = stringr::str_wrap(!!as.character(colour), 40)) +
      ggplot2::xlab(!!xlab) +
      ggplot2::ylab(!!ylab)
  )

  list(
    plot = plot_expr
  )
}

iNZightPlotGG_dotstrip <- function(data, x, y, fill = "darkgreen", main = sprintf("Distribution of %s", as.character(y)), xlab = as.character(y), ylab = as.character(x), ...) {
  y <- rlang::sym(y)
  dots <- list(...)

  if (missing(x)) {
    x <- rlang::expr(factor(1))

    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!y, y = !!x)) +
        ggplot2::geom_point(!!!dots) +
        ggplot2::labs(title = !!main) +
        ggplot2::xlab(!!xlab) +
        ggplot2::ylab("") +
        ggplot2::theme(
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        )
    )
  } else {
    x <- rlang::sym(x)
    colour <- rlang::sym(x)

    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!y, y = !!x, colour = !!colour)) +
        ggplot2::geom_point(!!!dots) +
        ggplot2::labs(title = !!main, colour = stringr::str_wrap(!!as.character(colour), 40)) +
        ggplot2::xlab(!!xlab) +
        ggplot2::ylab(!!ylab)
    )
  }

  list(
    plot = plot_expr
  )
}

iNZightPlotGG_density <- function(data, x, y, fill = "darkgreen", main = sprintf("Distribution of %s", as.character(y)), xlab = as.character(y), ylab = "Density", ...) {
  y <- rlang::sym(y)
  dots <- list(...)

  if (missing(x)) {
    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!y)) +
        ggplot2::geom_density(fill = !!fill, !!!dots) +
        ggplot2::labs(title = !!main, fill = stringr::str_wrap(!!as.character(fill), 40)) +
        ggplot2::xlab(!!xlab) +
        ggplot2::ylab(!!ylab)
    )
  } else {
    fill <- rlang::sym(x)

    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!y, fill = !!fill)) +
        ggplot2::geom_density(!!!dots) +
        ggplot2::labs(title = !!main, fill = stringr::str_wrap(!!as.character(fill), 40)) +
        ggplot2::xlab(!!xlab) +
        ggplot2::ylab(!!ylab)
    )
  }

  list(
    plot = plot_expr
  )
}

iNZightPlotGG_mosaic <- function(data, x, y, main = sprintf("Mosaic plot of %s and %s", as.character(x), as.character(y)), xlab = as.character(x), ylab = as.character(y), ...) {
  # library("ggmosaic")
  # mosaic plots don't work unless the package is attached

  if (!"package:ggmosaic" %in% search()) {
    stop("Please install and load the 'ggmosaic' library\n # library(ggmosaic)")
  }

  x <- rlang::sym(x)
  y <- rlang::sym(y)

  data_expr <- rlang::expr(
    plot_data <- !!rlang::enexpr(data) %>%
      dplyr::select(!!x, !!y) %>%
      dplyr::mutate(!!x := factor(!!x)) %>%
      dplyr::mutate(!!y := factor(!!y))
  )

  plot_expr <- rlang::expr(
    ggplot2::ggplot(plot_data) +
      ggmosaic::geom_mosaic(ggplot2::aes(x = ggmosaic::product(!!x), fill = !!y)) +
      ggplot2::labs(title = !!main, fill = stringr::str_wrap(!!as.character(y), 40)) +
      ggplot2::xlab(!!xlab) +
      ggplot2::ylab(!!ylab)
  )

  list(
    data = data_expr,
    plot = plot_expr
  )
}

iNZightPlotGG_lollipop2 <- function(data, x, y, main = sprintf("Count of %s", as.character(x)), xlab = as.character(x), ylab = "Count", ordered = FALSE, ...) {
  x <- rlang::sym(x)
  dots <- list(...)

  point_dots <- dots[c("size", "colour")]
  line_dots <- dots[c("lwd", "colour")]

  point_dots <- Filter(Negate(is.null), point_dots)
  line_dots <- Filter(Negate(is.null), line_dots)

  if (missing(y)) {
    if (ordered %in% c("desc", "asc")) {
      data_expr <- rlang::expr(
        plot_data <- !!rlang::enexpr(data) %>%
          dplyr::group_by(!!x) %>%
          dplyr::summarise(Count = dplyr::n()) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(!!x := forcats::fct_reorder(!!x, !!rlang::sym("Count"), .desc = !!(ordered == "desc")))
      )
    } else {
      data_expr <- rlang::expr(
        plot_data <- !!rlang::enexpr(data) %>%
          dplyr::group_by(!!x) %>%
          dplyr::summarise(Count = dplyr::n())
      )
    }

    plot_expr <- rlang::expr(
      ggplot2::ggplot(plot_data, ggplot2::aes(!!x, !!rlang::sym("Count"))) +
        ggplot2::geom_point(!!!point_dots) +
        ggplot2::geom_segment(ggplot2::aes(xend = !!x, yend = 0), !!!line_dots) +
        ggplot2::labs(title = !!main) +
        ggplot2::xlab(!!xlab) +
        ggplot2::ylab(!!ylab)
    )
  } else {
    y <- rlang::sym(y)

    if (ordered %in% c("desc", "asc")) {
      data_expr <- rlang::expr(
        plot_data <- !!rlang::enexpr(data) %>%
          dplyr::group_by(!!x, !!y) %>%
          dplyr::summarise(Count = dplyr::n()) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(!!x := forcats::fct_reorder(!!x, !!rlang::sym("Count"), .desc = !!(ordered == "desc")))
      )
    } else {
      data_expr <- rlang::expr(
        plot_data <- !!rlang::enexpr(data) %>%
          dplyr::group_by(!!x, !!y) %>%
          dplyr::summarise(Count = dplyr::n())
      )
    }

    plot_expr <- rlang::expr(
      ggplot2::ggplot(plot_data, ggplot2::aes(x = !!x, colour = !!y, y = !!rlang::sym("Count"))) +
        ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.5), !!!point_dots) +
        ggplot2::geom_linerange(ggplot2::aes(ymin = 0, ymax = !!rlang::sym("Count")), position = ggplot2::position_dodge(width = 0.5), !!!line_dots) +
        ggplot2::labs(title = !!main) +
        ggplot2::xlab(!!xlab) +
        ggplot2::ylab(!!ylab)
    )
  }

  list(
    data = data_expr,
    plot = plot_expr
  )
}

iNZightPlotGG_gridplot <- function(data, x, main = sprintf("Gridplot of %s", as.character(x)), xlab = sprintf("%s observation/square", perN), perN = 1, ...) {
  x <- rlang::sym(x)

  data_expr <- rlang::expr(
    plot_data <- !!rlang::enexpr(data) %>%
      dplyr::select(!!x) %>%
      table() %>%
      magrittr::divide_by_int(!!as.integer(perN))
  )

  plot_expr <- rlang::expr(
    waffle::waffle(plot_data, title = !!main, xlab = !!xlab)
  )

  list(
    data = data_expr,
    plot = plot_expr
  )
}

iNZightPlotGG_divergingstackedbar <- function(data, x, y, main = sprintf("Diverging stacked bar of %s by %s", as.character(y), as.character(x)), xlab = as.character(x), ylab = "Count", cutpoint = NULL, ...) {
  orig_x <- x
  x <- rlang::sym(y)

  y <- rlang::sym(orig_x)

  if (is.null(cutpoint) || cutpoint == "Default") {
    cutpoint <- rlang::expr(floor(nlevels(!!y) / 2))
  } else {
    cutpoint <- rlang::enexpr(cutpoint)
  }

  data_expr <- rlang::expr(
    plot_data <- !!rlang::enexpr(data) %>%
      dplyr::group_by(!!x, !!y) %>%
      dplyr::summarise(Count = dplyr::n())
  )

  plot_expr <- rlang::expr(
    ggplot2::ggplot(plot_data, ggplot2::aes(x = !!x, fill = !!y)) +
      ggplot2::geom_col(data = subset(plot_data, !!y %in% levels(!!y)[1:!!cutpoint]), ggplot2::aes(y = -!!rlang::sym("Count"))) +
      ggplot2::geom_col(data = subset(plot_data, !(!!y %in% levels(!!y)[1:!!cutpoint])), ggplot2::aes(y = !!rlang::sym("Count")), position = ggplot2::position_stack(reverse = TRUE)) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::coord_flip() +
      ggplot2::labs(title = !!main) +
      ggplot2::xlab(!!xlab) +
      ggplot2::ylab(!!ylab) +
      ggplot2::scale_y_continuous(labels = abs) +
      ggplot2::scale_fill_discrete(breaks = levels(plot_data[[!!as.character(y)]]))
  )

  list(
    data = data_expr,
    plot = plot_expr
  )
}

iNZightPlotGG_beeswarm <- function(data, x, y, main = sprintf("Distribution of %s", as.character(y)), xlab = as.character(x), ylab = as.character(y), rotation = FALSE, ...) {
  y <- rlang::sym(y)

  dots <- list(...)


  if (missing(x)) {
    x <- rlang::expr(factor(1))

    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x, y = !!y)) +
        ggbeeswarm::geom_beeswarm(!!!dots) +
        ggplot2::ggtitle(!!main) +
        ggplot2::xlab("") +
        ggplot2::ylab(!!ylab) +
        ggplot2::theme(
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        )
    )
  } else {
    x <- rlang::sym(x)
    if (rotation) {
      plot_expr <- rlang::expr(
        ggplot2::ggplot(
          !!rlang::enexpr(data),
          ggplot2::aes(x = !!x, y = !!y, colour = !!x)
        ) +
          ggbeeswarm::geom_beeswarm(!!!dots) +
          ggplot2::ggtitle(!!main) +
          ggplot2::xlab(!!xlab) +
          ggplot2::ylab(!!ylab)
      )
    } else {
      plot_expr <- rlang::expr(
        ggplot2::ggplot(
          !!rlang::enexpr(data),
          ggplot2::aes(x = factor(!!x, levels = rev(levels(!!x))), y = !!y, colour = !!x)
        ) +
          ggbeeswarm::geom_beeswarm(!!!dots) +
          ggplot2::ggtitle(!!main) +
          ggplot2::xlab(!!xlab) +
          ggplot2::ylab(!!ylab)
      )
    }
  }

  list(
    plot = plot_expr
  )
}

iNZightPlotGG_ridgeline <- function(data, x, y, main = sprintf("Distribution of %s", as.character(y)), xlab = as.character(y), ylab = as.character(x), ...) {
  x <- rlang::sym(x)
  y <- rlang::sym(y)

  dots <- list(...)

  plot_expr <- rlang::expr(
    ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!y, y = !!x, fill = !!x)) +
      ggridges::geom_density_ridges(!!!dots) +
      ggplot2::ggtitle(!!main) +
      ggplot2::xlab(!!xlab) +
      ggplot2::ylab(!!ylab)
  )

  list(
    plot = plot_expr
  )
}

iNZightPlotGG_quasirandom <- function(data, x, y, main = sprintf("Distribution of %s", as.character(y)), xlab = as.character(x), ylab = as.character(y), ...) {
  y <- rlang::sym(y)

  dots <- list(...)

  if (missing(x)) {
    x <- rlang::expr(factor(1))

    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x, y = !!y)) +
        ggbeeswarm::geom_quasirandom(!!!dots) +
        ggplot2::ggtitle(!!main) +
        ggplot2::xlab("") +
        ggplot2::ylab(!!ylab) +
        ggplot2::theme(
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        )
    )
  } else {
    x <- rlang::sym(x)

    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x, y = !!y, colour = !!x)) +
        ggbeeswarm::geom_quasirandom(!!!dots) +
        ggplot2::ggtitle(!!main) +
        ggplot2::xlab(!!xlab) +
        ggplot2::ylab(!!ylab)
    )
  }

  list(
    plot = plot_expr
  )
}
