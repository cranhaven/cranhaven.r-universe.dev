#' Premium ggplot2 theme for inkaR
#'
#' @param mode Character. "light" or "dark".
#' @param base_size Numeric. Base font size.
#' @export
theme_inkaR <- function(mode = c("light", "dark"), base_size = 11) {
    mode <- match.arg(mode)
    
    bg_color <- if (mode == "dark") "#1e1e1e" else "white"
    text_color <- if (mode == "dark") "#e0e0e0" else "#2c3e50"
    grid_color <- if (mode == "dark") "#333333" else "#f0f0f0"
    
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = bg_color, color = NA),
        panel.background = ggplot2::element_rect(fill = bg_color, color = NA),
        text = ggplot2::element_text(color = text_color),
        plot.title = ggplot2::element_text(face = "bold", size = 14, margin = ggplot2::margin(b = 10)),
        plot.subtitle = ggplot2::element_text(color = if (mode == "dark") "#aaaaaa" else "#7f8c8d", size = 10, margin = ggplot2::margin(b = 20)),
        panel.grid.major = ggplot2::element_line(color = grid_color),
        panel.grid.minor = ggplot2::element_blank(),
        legend.background = ggplot2::element_rect(fill = bg_color, color = NA),
        legend.text = ggplot2::element_text(color = text_color),
        axis.text = ggplot2::element_text(color = text_color)
    )
}

#' Plot INKAR Data on German Maps
#'
#' Automatically projects regional INKAR data onto administrative boundaries of Germany using
#' the `geodata` and `sf` packages. Supports Kreise (KRE) and Bundesländer (BLD) levels.
#'
#' @param data A data frame returned by `get_inkar_data()`.
#' @param variable Character. For wide-format data with multiple indicators, specify which indicator column to plot.
#' @param year Integer/Character. If the data contains multiple years, specify which year to plot. If NULL and multiple years exist, the most recent year is plotted.
#' @param mode Character. "light" (default) or "dark" theme.
#' @return A `ggplot2` object displaying the mapped data.
#' @export
plot_inkar <- function(data, variable = NULL, year = NULL, mode = c("light", "dark")) {
    mode <- match.arg(mode)
    
    if (!requireNamespace("sf", quietly = TRUE)) {
        stop("Package 'sf' is required for plotting. Install it with: install.packages('sf')")
    }
    if (!requireNamespace("geodata", quietly = TRUE)) {
        stop("Package 'geodata' is required for plotting. Install it with: install.packages('geodata')")
    }
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package 'ggplot2' is required for plotting. Install it with: install.packages('ggplot2')")
    }

    if (!is.data.frame(data) || nrow(data) == 0) {
        stop("Provided data is empty or not a valid data frame.")
    }

    # Identify language and keys
    is_en <- "region_id" %in% names(data)
    id_col <- if (is_en) "region_id" else "Kennziffer"
    agg_col <- if (is_en) "level_name" else "Aggregat"
    
    if (!id_col %in% names(data)) {
        stop("Data must contain a valid ID column (region_id or Kennziffer).")
    }

    # Identify spatial level
    level_raw <- if (agg_col %in% names(data)) unique(data[[agg_col]])[1] else "KRE"

    # Handle Time/Value column identification
    val_col <- if (is_en) "value" else "Wert"
    time_col <- if (is_en) "year" else "Zeit"
    
    # 1. Detection: Is this the NEW Analytical Wide Format? 
    # (Indicators as columns, specific time column exists)
    is_wide_analytical <- time_col %in% names(data) && !(val_col %in% names(data))
    
    if (is_wide_analytical) {
        meta_cols <- c(id_col, agg_col, time_col, 
                       if(is_en) "region_name" else "Raumeinheit",
                       "M_ID", "indicator_name", "Indikator", "unit", "Einheit", "description", "Beschreibung")
        potential_vars <- setdiff(names(data), meta_cols)
        
        if (length(potential_vars) == 0) stop("No indicator columns found in wide data.")
        
        if (is.null(variable)) {
            variable <- potential_vars[1]
            if (length(potential_vars) > 1) {
                cli::cli_alert_warning("Multi-indicator data detected. Plotting first variable: {.val {variable}}")
            }
        }
        
        if (!variable %in% potential_vars) {
            stop("Variable '", variable, "' not found. Available indicators: ", paste(potential_vars, collapse = ", "))
        }
        
        val_col <- variable
        indicator_title <- variable
        
        # Filter year
        if (!is.null(year)) {
            data <- data[data[[time_col]] == year, ]
            if (nrow(data) == 0) stop("No data found for year: ", year)
        } else {
            max_year <- max(as.integer(as.character(data[[time_col]])), na.rm = TRUE)
            data <- data[data[[time_col]] == max_year, ]
            year <- max_year
        }
    } else if (val_col %in% names(data) && time_col %in% names(data)) {
        # Long format
        if (!is.null(year)) {
            data <- data[data[[time_col]] == year, ]
            if (nrow(data) == 0) stop("No data found for year: ", year)
        } else {
            max_year <- max(as.integer(as.character(data[[time_col]])), na.rm = TRUE)
            data <- data[data[[time_col]] == max_year, ]
            year <- max_year
        }
        
        ind_name_col <- if (is_en) "indicator_name" else "Indikator"
        indicator_title <- if (ind_name_col %in% names(data)) unique(data[[ind_name_col]])[1] else "Indicator Value"
    } else {
        # Wide format - years as columns (older style)
        year_cols <- grep("^[0-9]{4}$", names(data), value = TRUE)
        if (length(year_cols) == 0) stop("No valid value/year columns found.")
        
        if (!is.null(year)) {
            val_col <- as.character(year)
            if (!val_col %in% year_cols) stop("No data found for year: ", year)
        } else {
            val_col <- max(year_cols)
            year <- val_col
        }
        indicator_title <- "Indicator Value"
    }

    unit_col <- if (is_en) "unit" else "Einheit"
    unit_str <- if (unit_col %in% names(data) && !is.na(data[[unit_col]][1])) paste0(" (", data[[unit_col]][1], ")") else ""

    # Mapping polygons
    message("Loading map geometries...")
    level_type <- if (grepl("KRE|Kreise|Districts", level_raw, ignore.case = TRUE)) 2 else 1
    map_raw <- geodata::gadm("DEU", level = level_type, path = tempdir())
    map_sf <- sf::st_as_sf(map_raw)

    join_key <- if (level_type == 2) "CC_2" else "CC_1"
    map_sf <- merge(map_sf, data, by.x = join_key, by.y = id_col, all.x = TRUE)

    # Styling colors
    fill_pal <- if (mode == "dark") "magma" else "viridis"

    p <- ggplot2::ggplot(map_sf) +
        ggplot2::geom_sf(ggplot2::aes(fill = .data[[val_col]]), color = if (mode == "dark") "grey20" else "white", linewidth = 0.05) +
        ggplot2::scale_fill_viridis_c(option = fill_pal, na.value = if (mode == "dark") "grey10" else "grey95") +
        theme_inkaR(mode = mode) +
        ggplot2::theme(axis.title = ggplot2::element_blank(), axis.text = ggplot2::element_blank(), panel.grid = ggplot2::element_blank()) +
        ggplot2::labs(
            title = indicator_title,
            subtitle = paste0("Spatial Level: ", level_raw, " | Year: ", year),
            fill = unit_str
        )

    return(p)
}
