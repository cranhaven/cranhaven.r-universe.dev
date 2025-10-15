#' Real-Time Plate View
#'
#' Converts the real-time data into a ggplot figure. The layout is either 8x12
#' or 16x24 for 96- and 384-well plates, respectively.
#'
#' @param df Real-time dataframe
#' @param meta Dataframe containing well IDs and Sample IDs to title each facet.
#' @param plate Integer either 96 or 384 to denote microplate type.
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom tidyr replace_na
#' @importFrom tidyr separate
#' @importFrom stringr str_length
#' @importFrom reshape2 melt
#'
#' @examples
#' # This test takes >5 sec
#' \donttest{
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test2.xlsx",
#'   package = "quicR"
#' )
#'
#' # Get the real-time data.
#' df_ <- get_real(file, ordered = FALSE)[[1]] |>
#'   as.data.frame()
#'
#' sample_locations <- get_sample_locations(
#'   file,
#'   dilution_bool = TRUE,
#'   dilution_fun = function(x) -log10(x)
#' )
#'
#' plate_view(df_, sample_locations)
#' }
#'
#' @export
plate_view <- function(df, meta, plate = 96) {

  if (plate != 96 & plate != 384) {
    return("Invalid plate layout. Format should be either 96 or 384. ")
  }

  # Ensures that the input is a dataframe.
  df <- data.frame(df)

  colnames(df) <- c("Time", paste(meta[[1]], meta[[2]], sep = "."))

  # Create a template of all possible columns
  template_columns <- expand.grid(
    if (plate == 96) {
      Var1 <- LETTERS[1:8]
    } else {
      Var1 <- LETTERS[1:16]
    },
    if (plate == 96) {
      Var2 <- sprintf("%02d", 1:12)
    } else {
      Var2 <- sprintf("%02d", 1:24)
    }
  )
  template_columns <- sort(paste0(template_columns$Var1, template_columns$Var2))
  rm(Var1, Var2)

  # Add columns with NAs if they do not exist.
  for (col in template_columns) {
    if (!(col %in% meta[[1]])) {
      df[col] <- NA
    }
  }

  # Add a "Time" column. This is important for the melt function.
  # df <- cbind("Time" = rownames(df), df)

  # Combine the template_columns and sample_locations.
  template_columns <- as.data.frame(template_columns)
  colnames(template_columns) <- colnames(meta[1])

  # Create a data.frame with all the wells and IDs, even if some are missing.
  full <- meta |>
    full_join(as.data.frame(template_columns)) |>
    arrange_at(1) %>%
    suppressMessages()

  # Create the labeller function for the facet plot.
  ID_labeller <- function(variable, value) {
    i <- full[, 2][full[, 1] == value]
    ifelse(is.na(i), " ", i)
  }

  df |>
    # Melt the data to help with the faceting.
    reshape2::melt(id.vars = "Time") |>
    # Separate the wells from the IDs.
    separate("variable", c("Well", "ID"), "\\.", fill = "right") |>
    # Ensures that Time and observations are numeric.
    mutate(
      Time  = as.numeric  (.data$Time),
      value = as.numeric  (.data$value),
      ID    = as.character(.data$ID),
      Well  = as.factor   (.data$Well)
    ) |>
    mutate(ID = replace_na(.data$ID, "none")) |>
    # Create the facet plot.
    ggplot(aes(x = .data$Time, y = .data$value)) +
    geom_line() +
    labs(
      y = "RFU",
      x = "Time (h)"
    ) +
    theme_classic() +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      strip.background = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    ) +
    facet_wrap(vars(.data$Well),
      nrow = ifelse(plate == 96, 8, 16),
      ncol = ifelse(plate == 96, 12, 24),
      labeller = ID_labeller
    ) |>
    suppressWarnings()
}
