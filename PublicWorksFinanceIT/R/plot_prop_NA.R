#' Visual representation of NAs proportion over space and time
#'
#' plot_prop_NA allows to visualize spatial proportion of NAs and temporal proportion of NAs.
#'
#' @param data Dataset of class 'data.frame'. Specify the dataset obtained from the retrieving and the \code{merge_data} functions.
#' @param variable character. Specify the name of the variable for which to obtain the spatial autocorrelation.
#' @param time Logical. By default set to \code{FALSE}. If the temporal proportion of NAs is required set the argument to \code{TRUE}.
#' @param interactive Logical. By default set to \code{FALSE}. If interactive plot set the argument to \code{TRUE}.
#' @param bar Logical. By default set to \code{FALSE}. If set to \code{TRUE} a bar plot will be showed.
#' @param map Logical. By default set to \code{FALSE}. If set to \code{TRUE} a map will be showed.
#'
#' @returns Return \code{ggplot} object representing an interactive map.
#' @import magrittr
#' @import PublicWorksFinanceIT
#' @author Lorena Ricciotti
#' @examples
#' data(OBDAPpoint)
#' plot_prop_NA(OBDAPpoint, variable = "EuFunding")
#'
#'@export
plot_prop_NA <- function(data, variable, time = FALSE, interactive = FALSE, bar = FALSE, map = FALSE){

  idx <- grep("Date|Operability|Closed", names(data))
suppressWarnings(
  suppressMessages(
      data <- data %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(idx), ~ as.Date(strptime(., format = "%Y-%m-%d")))) %>%
        dplyr::mutate(
          first_y = do.call(pmin, c(dplyr::across(dplyr::all_of(idx)), na.rm = TRUE)),
          year = format(.data$first_y, "%Y")) %>%
        dplyr::select(-.data$first_y)  %>%
        dplyr::mutate(is_na = dplyr::case_when(is.na(get(variable)) | get(variable) == 0 ~ 1,  TRUE ~ 0),
                      DEN_MUNICIPALITY = toupper(.data$DEN_MUNICIPALITY)) %>%
        dplyr::group_by(.data$COD_MUNICIPALITY, .data$DEN_MUNICIPALITY, .data$year) %>%
        dplyr::summarise(is_na = unique(.data$is_na),
                         geom = unique(.data$geom),
                         DEN_MUNICIPALITY = unique(.data$DEN_MUNICIPALITY),
                         year= unique(.data$year))
    )
  )
if(bar == FALSE){
    g <- data %>%
      dplyr::group_by(.data$COD_MUNICIPALITY) %>%
      dplyr::summarise(
        DEN_MUNICIPALITY = unique(toupper(.data$DEN_MUNICIPALITY)),
        prop_na = mean(.data$is_na)
      ) %>%
      dplyr::mutate(
        na_range = cut(round(.data$prop_na, 2),
                       breaks = c(0.00, 0.25, 0.5, 0.75, 1),
                       labels = c("0-25%", "25-50%", "50-75%", "75-100%"), include.lowest = TRUE)
      ) %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$na_range, fill = .data$na_range)) +
      ggplot2::geom_bar() +
      ggplot2::labs(
        title = "Number of Municipalities by Proportion of Missing Values",
        x = "Proportion NA Range",
        y = "Number of Municipalities",
        fill = "NA Proportion Ranges"
      ) +
      ggplot2::theme_minimal()
    print(g)

  }
if(interactive == TRUE & time == FALSE){
    g <- data %>%
    dplyr::group_by(.data$COD_MUNICIPALITY) %>%
    dplyr::summarise(
      DEN_MUNICIPALITY = unique(toupper(.data$DEN_MUNICIPALITY)),
      prop_na = mean(.data$is_na)
    ) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = stats::reorder(.data$DEN_MUNICIPALITY, .data$prop_na),
      y = .data$prop_na,
      text = paste("Municipality:", .data$DEN_MUNICIPALITY, "<br>Proportion NA:", round(.data$prop_na, 2))
    )) +
    ggplot2::geom_bar(stat = "identity", alpha = 0.8, color = "white", fill = "steelblue") +
    ggplot2::labs(
      title = "Proportion of Missing Values Across Municipalities",
      x = "Municipality",
      y = "Proportion of NA Values"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5)
    )

    print(plotly::ggplotly(g, tooltip = "text"))
  }
if(interactive == FALSE & time == TRUE){

  df <- data.frame(
    COD_MUNICIPALITY =
      rep(sort(unique(data$COD_MUNICIPALITY)),
          each = max(as.numeric(data$year), na.rm = T) -
            min(as.numeric(data$year), na.rm=T)+1),
    year = as.character(rep(min(as.numeric(data$year), na.rm=T):max(as.numeric(data$year), na.rm = T), length(unique(data$COD_MUNICIPALITY))))
  )

  df <- df %>% dplyr::left_join(data, by = c("COD_MUNICIPALITY", "year")) %>%
    dplyr::group_by(.data$COD_MUNICIPALITY) %>%
    dplyr::mutate(
      DEN_MUNICIPALITY = ifelse(is.na(.data$DEN_MUNICIPALITY), dplyr::first(stats::na.omit(.data$DEN_MUNICIPALITY)), .data$DEN_MUNICIPALITY), is_na = dplyr::case_when(is.na(.data$is_na) ~ 1, TRUE ~0 )) %>% dplyr::ungroup()

  df <- df %>%
    dplyr::group_by(.data$year) %>%
    dplyr::summarise(
      prop_na = mean(.data$is_na)
    )


  g <- ggplot2::ggplot(df, ggplot2::aes(x = as.numeric(.data$year), y = .data$prop_na)) +
    ggplot2::geom_line(color = "steelblue", size = 1) +  # Line to show the trend over time
    ggplot2::geom_point(color = "firebrick", size = 2) +  # Points for each year's proportion
    ggplot2::labs(
      title = "Proportion of Missing Values Over Time",
      x = "Year",
      y = "Proportion of NA Values"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title =  ggplot2::element_text(hjust = 0.5),
      axis.text.x =  ggplot2::element_text(angle = 45, hjust = 1)
    )
    print(g)
}
if(map == TRUE){

  data <- sf::st_as_sf(data, wkt = "geom")
  ggplot2::ggplot(data %>%
                    dplyr::group_by(.data$COD_MUNICIPALITY) %>%
                    dplyr::summarise(
                      DEN_MUNICIPALITY = unique(toupper(.data$DEN_MUNICIPALITY)),
                      prop_na = mean(.data$is_na)
                    )) +
    ggplot2::geom_sf(ggplot2::aes(geometry = .data$geom, color = .data$prop_na, fill = .data$prop_na)) +
    ggplot2::scale_color_viridis_c()+
    ggplot2::scale_fill_viridis_c()+
    ggplot2::theme_bw()+
    ggplot2::labs(color = "Proportion of NAs", fill = "Proportion of NAs")
}
}


