#' function to remove hyphens, underscores, spaces and transform to lowercase
#' @param string to modify
#' @return modified string
clean_string <- function(string) {
  mod_string <- gsub("[-| |_]", "", string)
  low <- tolower(mod_string)
  return(low)
}

#' function to search for the possible critical columns in a data.frame
#' @param colnames_df a character vector with names
#' @param crit_cols a character vector
#' @importFrom rlang is_empty
#' @importFrom purrr set_names modify
#' @return list: possible match to each critical column
guess_match <- function(colnames_df, crit_cols) {

  crit_cols_list <- vector("list", length(crit_cols)) %>% set_names(crit_cols)

  uncased <- modify(crit_cols, clean_string)
  uncased_colnames <- modify(colnames_df, clean_string)
  for (i in seq_len(length(crit_cols))) {
    indx <- NULL
    indx <- grep(uncased[i],
                 uncased_colnames, #colnames of df
                 ignore.case = TRUE)
    if (is_empty(indx)) {
      crit_cols_list[[i]] <- ""
    } else {
      crit_cols_list[[i]] <- colnames_df[indx[1]] #first match
    }
  }
  return(crit_cols_list)
}

#' function to read data from users (.csv or .xlsx files)
#' @param path path to a temp file
#' @param name filename provided by the web browser
#' @importFrom vroom vroom
#' @importFrom readxl read_excel
#' @importFrom shiny validate
#' @return data frame
load_data <- function(path, name) {
  ext <- tools::file_ext(name)
  df <- switch(ext,
               csv = vroom(path, delim = ","),
               xlsx = read_excel(path),
               validate("Invalid file; Please upload a .csv or .xlsx file"))
  return(as.data.frame(df))
}

#' Function to expand a vector of colors if needed
#' @param col_palette character palette to color the treatments
#' @param n how many colors are needed
#' @importFrom grDevices colorRampPalette
#' @return a character vector of colors
expand_palette <- function(col_palette, n) {
  if (length(col_palette) < n) {
    col_palette <- colorRampPalette(col_palette)(n)
  } else {
    col_palette
  }
}

#' Create volume plot for one-batch data
#' @param df data.frame, single-batch long format
#' @param faceting_var string
#' @param y_name string
#' @param y_var string: column name for y axis
#' @param col_palette character palette to color the treatments
#' @param p_title plot title
#' @param ... arguments passed to plot_ly
#' @return plotly object
#' @importFrom plotly arrange plot_ly %>% layout add_annotations add_trace
#' subplot
#' @importFrom rlang .data
plotly_volume <- function(df,
                         col_palette = NULL,
                         faceting_var,
                         y_name,
                         y_var,
                         p_title,
                         ...) {

  y_var <- paste0("~", y_var)
  facets <- unique(df[, faceting_var])

  plot_palette <- expand_palette(col_palette, length(facets))

  p <- lapply(facets, function(facet) {

    my_data <- df %>%
      filter(!!as.symbol(faceting_var) == facet) %>%
      arrange(.data$day) # sort by day to make sure it is plotted as time series

    if (faceting_var == "treatment") {
      color_sb <- plot_palette[which(facets == facet)]
    } else {
      color_sb <- if (unique(my_data$control) == "new") {
        "#830051"
      } else {
        "#3F4444"
      }
    }

    my_data %>%
      group_by(.data$animal_id) %>%
      plot_ly(
        x = ~day,
        y = as.formula(y_var),
        text = ~paste("Animal id:", animal_id,
                      "<br>Treatment:", treatment
        ),
        hovertemplate = "%{text} <br>Volume: %{y} <br>Day: %{x}<extra></extra>",
        showlegend = FALSE,
        alpha = 0.6,
        color = I(color_sb)
      ) %>%
      add_trace(
        type = "scatter", mode = "lines+markers") %>%
      add_annotations(
        text = facet,
        x = 0.5,
        y = 1,
        yref = "paper",
        xref = "paper",
        xanchor = "center",
        yanchor = "top",
        showarrow = FALSE,
        font = list(
          color = { if (facet == "Control") "grey"  else "black"},
          size = 15)
      )
  })

  # set margin
  m <- list(
    l = 100,
    r = 25,
    b = 50,
    t = 50
  )

  subplots <- p %>%
    subplot(shareY = TRUE, nrows = ceiling(length(p) / 3), titleY = FALSE) %>%
    layout(annotations = list(
      list(
        text = "Day",
        x = 0.5,
        y = 0,
        yshift = -35,
        yref = "paper",
        xref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE,
        font = list(size = 15)
      ),
      list(
        text = y_name,
        x = 0,
        y = 0.5,
        xshift = -70,
        xref = "paper",
        yref = "paper",
        textangle = 270,
        showarrow = FALSE,
        font = list(size = 15)
      )
    ), margin = m, title = p_title)

  return(subplots)

}



#' create a table with aggregated data: each row contains information
#' about control and treatments of a single study
#' @param df data.frame
#' @importFrom dplyr group_by summarise %>%
#' @importFrom rlang .data
#' @return data.frame
aggregate_study_info <- function(df) {
  summary_table <- df %>%
    group_by(.data$study) %>%
    summarise(
      control = ifelse("Control" %in% .data$treatment, "Control", ""),
      treatment  = paste(unique(.data$treatment[.data$treatment != "Control"]),
                         collapse = ", ")
    )
  return(summary_table)
}

#' Calculate percentage of survived animals
#' @param df data frame
#' @return data frame
#' @importFrom dplyr group_by summarise %>% mutate ungroup select left_join
#' arrange last
#' @importFrom tidyr fill
#' @importFrom rlang .data
calc_survived <- function(df) {

  dat_survival <- df %>%
    group_by(.data$study, .data$animal_id) %>% #group
    summarise(day = max(.data$day)) %>%
    group_by(.data$study) %>%
    mutate(animal_number = length(unique(.data$animal_id))) %>%
    arrange(.data$study, .data$day) %>%
    mutate(number_lost = cumsum(!duplicated(.data$animal_id))) %>%
    group_by(.data$study, .data$animal_number, .data$day) %>%
    summarise(number_lost = last(.data$number_lost)) %>%
    mutate(number_survive = .data$animal_number - .data$number_lost,
           freq_survive = .data$number_survive / .data$animal_number) %>%
    ungroup() %>%
    select(.data$study, .data$day, .data$freq_survive) %>%
    rbind(data.frame(study = unique(df$study),
                     day = 1,
                     freq_survive = 1))



  dat_full_survival <-
    expand.grid(study = unique(dat_survival$study),
                day = 1:max(dat_survival$day)) %>%
    left_join(dat_survival) %>%
    arrange(.data$study, .data$day) %>%
    fill(.data$freq_survive)

  return(dat_full_survival)
}

#' Set up a waiting screen
#' @param header text to display on loading screen
#' @return object of a class waiter
#' @importFrom waiter Waiter spin_3 transparent
set_waiter <- function(header) {

  waiting_screen <- div(
    style = "color:black;",
    spin_3(),
    h3(header),
    h4("Do not close this tab")
  )

  w <- Waiter$new(
    html = waiting_screen,
    color = transparent(.5)
  )

  return(w)
}

#' Display a popup message and reset fileInput
#' @param message_text the modal's text
#' @importFrom shinyalert useShinyalert
#' @importFrom shinyjs reset
notify_error_and_reset_input <- function(message_text) {
  shinyalert(paste(message_text),
             type = "error")
  reset("file")
}
