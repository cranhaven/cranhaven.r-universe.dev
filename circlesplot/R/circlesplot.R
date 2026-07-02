#' circlesplot(): Plots multiple circles with their given ratios
#'
#' @description
#' `circlesplot()` plots circles with a given diameter next to each other, so readers can observe the
#' ratio between them.
#'
#'
#' @param cp_vals Vector (numeric); provides data
#' @param cp_text Vector (characters); provides text-labels
#' @param cp_max Maximum number of circles in a row (integer)
#' @param cp_line_width Line-width of the circles (integer)
#' @param cp_title Title of the plot (String)
#' @param cp_color Vector of hex-colors for each circle
#' @param cp_title_size Size of the title (numeric or integer)
#' @param cp_sort String; specifies if values should be sorted ('asc', 'desc'; default: 'none')
#' @param cp_tight_spacing Number (numeric); specifies spacing between rows (default: 1.0, possible: 1.0 - 2.0; 2.0 smallest distance)
#' @param cp_shape String; specifies the shape (default: 'circle'; possible: 'square')
#'
#' @importFrom graphics par text
#' @importFrom plotrix draw.circle
#' @importFrom grDevices recordPlot
#' @importFrom graphics rect
#' @return Returns object of class 'recordedPlot'. Can be used for saving the plot to a variable and replay it again (See https://benst099.github.io/circlesplot/articles/cp_vignette.html).
#' @export circlesplot
#'
#' @examples
#'
#' \donttest{
#' library('plotrix')
#' colors = c('#D1BBD7', '#AE76A3', '#882E72', '#1965B0', '#5289C7', '#7BAFDE', '#4EB265', '#90C987')
#' values = c(5,5,4,5,5,5,2,1)
#' text = c('8','7','6','5','4','3','2','1')
#' circlesplot(cp_vals=values, cp_text=text, cp_max=3L, cp_title="Some title", cp_color=colors)
#' }
#'
#' # Proportions among planets
#' library('plotrix')
#' colors = c('#D1BBD7', '#AE76A3', '#882E72', '#1965B0', '#5289C7', '#7BAFDE', '#4EB265', '#90C987')
#' planets = c('Mercury','Venus','Earth','Mars','Jupiter','Saturn','Uranus','Neptune')
#' diameter = c(4879.4,12103.6,12756.3,6792.4,142984,120536,51118,49528)
#' circlesplot(cp_vals=diameter, cp_text=planets, cp_max=3L, cp_title="Planets", cp_color=colors)
#'
#' # For coloring, you can also use viridis package:
#' library("viridis")
#' values = c(5,5,4,5,5,5,2,1)
#' text = c('8','7','6','5','4','3','2','1')
#' circlesplot(cp_vals=values, cp_text=text, cp_max=4L, cp_title="Some title", cp_color=viridis(8))
#'
circlesplot <- function(cp_vals=NULL, cp_text=NULL, cp_max=10L, cp_line_width=2L, cp_title="", cp_color=NULL, cp_title_size=1.5, cp_sort='none', cp_tight_spacing=1, cp_shape='circle') {

  .check_params(cp_vals, cp_text, cp_max, cp_line_width, cp_title, cp_color, cp_title_size, cp_sort, cp_tight_spacing, cp_shape)

  if (is.null(cp_color)) {
    cp_color <- rep("#FFFFFF", times=length(cp_vals))
    df <- data.frame(cp_vals, cp_text, cp_color)
  }
  df <- data.frame(cp_vals, cp_text, cp_color)

  if(cp_sort == 'desc') {
    df <- df[order(df$cp_vals, decreasing = TRUE),] # descending
  } else if (cp_sort == 'asc') {
    df <- df[order(df$cp_vals, decreasing = FALSE),] # ascending
  }

  return(.plot_circlesplot(df, cp_line_width, cp_title, cp_max, cp_title_size, cp_tight_spacing, cp_shape))
}

.plot_circlesplot <- function(df, cp_line_width, cp_title, cp_max, cp_title_size, cp_tight_spacing, cp_shape) {

  diameter <- max(df$cp_vals)
  count <- 0
  x_pos <- 0
  y_pos <- 5
  y_pos_text <- y_pos -(diameter*1.5 + 3)
  color_pos <- 1

  par_old <- par(no.readonly = TRUE, fig=c(0, 1, 0, 1))

  plot(0, 0, type = "n", xlim = c(- (2 + diameter), (cp_max * diameter) * 2), ylim = c( - (2.5* (ceiling(length(df$cp_vals) / cp_max) * diameter)), diameter *2), axes=FALSE, asp=1, xlab="", ylab="")

  if(cp_max %% 2 == 0) {
    text((cp_max / 2) * (x_pos + diameter * 2 + 1) - ((x_pos + diameter * 2 + 1) / 2), diameter*2 + y_pos, cp_title, cex=cp_title_size)
  } else {
    text((floor(cp_max / 2)) * (x_pos + diameter * 2 + 1), diameter*2 + y_pos, cp_title, cex=cp_title_size)
  }

  on.exit(par(par_old), add = TRUE)

  if (cp_tight_spacing != 1 || cp_tight_spacing != 1L) {

    spacing <- diameter / cp_tight_spacing
    y_pos_text <- y_pos_text + diameter / 2

    if(cp_shape == 'circle') {
      for (item in df$cp_vals) {

        if(count >= cp_max) {
          x_pos <- 0
          y_pos <- y_pos -(3 * spacing + spacing)
          count <- 0
          y_pos_text <- y_pos -(spacing + 3)
        }
        draw.circle(x_pos, y_pos, item, lwd=cp_line_width, col = df$cp_color[color_pos])
        text(x_pos, y_pos_text, df$cp_text[color_pos])
        x_pos <- x_pos + diameter * 2 + 1
        count <- count + 1
        color_pos <- color_pos + 1
      }
    } else {
      for (item in df$cp_vals) {

        if(count >= cp_max) {
          x_pos <- 0
          y_pos <- y_pos -(3 * spacing + spacing)
          count <- 0
          y_pos_text <- y_pos -(spacing + 3)
        }
        rect(x_pos-item, y_pos-item , x_pos+item, y_pos+item, col= df$cp_color[color_pos], lwd=cp_line_width)
        text(x_pos, y_pos_text, df$cp_text[color_pos])
        x_pos <- x_pos + diameter * 2 + 1
        count <- count + 1
        color_pos <- color_pos + 1
      }
    }
  } else {
    if(cp_shape == 'circle') {
      for (item in df$cp_vals) {

        if(count >= cp_max) {

          x_pos <- 0
          y_pos <- y_pos -(3 * diameter + diameter)
          count <- 0
          y_pos_text <- y_pos -(diameter*1.5 + 3)
        }
        draw.circle(x_pos, y_pos, item, lwd=cp_line_width, col = df$cp_color[color_pos])
        text(x_pos, y_pos_text, df$cp_text[color_pos])
        x_pos <- x_pos + diameter * 2 + 1
        count <- count + 1
        color_pos <- color_pos + 1
      }
    } else {
      for (item in df$cp_vals) {

        if(count >= cp_max) {

          x_pos <- 0
          y_pos <- y_pos -(3 * diameter + diameter)
          count <- 0
          y_pos_text <- y_pos -(diameter*1.5 + 3)
        }
        rect(x_pos-item, y_pos-item , x_pos+item, y_pos+item, col= df$cp_color[color_pos], lwd=cp_line_width)
        text(x_pos, y_pos_text, df$cp_text[color_pos])
        x_pos <- x_pos + diameter * 2 + 1
        count <- count + 1
        color_pos <- color_pos + 1
      }
    }
  }

  cplot <- recordPlot()
  return(cplot)
}

.check_params <- function(cp_vals, cp_text, cp_max, cp_line_width, cp_title, cp_color, cp_title_size, cp_sort, cp_tight_spacing, cp_shape) {

  if (!inherits(cp_max, "integer")) {
    stop("[Error][circlesplot][Error in Parameter(s)]: Parameter 'cp_max' should be integer!")
  }
  if (cp_max <= 0L) {
    stop("[Error][circlesplot][Error in Parameter(s)]: Parameter 'cp_max' should be greater than zero!")
  }
  if (!inherits(cp_line_width, "integer")) {
    stop("[Error][circlesplot][Error in Parameter(s)]: Parameter 'cp_line_width' should be integer!")
  }
  if (cp_line_width <= 0L) {
    stop("[Error][circlesplot][Error in Parameter(s)]: Parameter 'cp_line_width' should be greater than zero!")
  }
  if (is.null(cp_vals) == TRUE) {
    stop("[Error][circlesplot][Error in Parameter(s)]: Vector 'cp_vals' has to be provided!")
  }
  if (is.null(cp_text) == TRUE) {
    stop("[Error][circlesplot][Error in Parameter(s)]: Vector 'cp_text' has to be provided!")
  }
  if (length(cp_vals) != length(cp_text)) {
    stop("[Error][circlesplot][Error in Parameter(s)]: Vector 'cp_vals' and 'cp_text' should have same length!")
  }
  if (is.numeric(cp_vals) != TRUE) {
    stop("[Error][circlesplot][Error in Parameter(s)]: Vector 'cp_vals' should contain numericals!")
  }
  if (is.character(cp_text) != TRUE) {
    stop("[Error][circlesplot][Error in Parameter(s)]: Vector 'cp_text' should contain characters!")
  }
  if (is.null(cp_color) != TRUE) {
    if (length(cp_color) != length(cp_vals)) {
      stop("[Error][circlesplot][Error in Parameter(s)]: Vector 'cp_color' should have same length as 'cp_vals'!")
    }
  }
  if (!inherits(cp_title_size, "numeric") && !inherits(cp_title_size, "integer")) {
    stop("[Error][circlesplot][Error in Parameter(s)]: Parameter 'cp_title_size' should be numeric / integer!")
  }
  if (cp_title_size < 1) {
    stop("[Error][circlesplot][Error in Parameter(s)]: Parameter 'cp_title_size' should be at least 1!")
  }
  if (cp_sort != 'none' && cp_sort != 'desc' && cp_sort != 'asc') {
    stop("[Error][circlesplot][Error in Parameter(s)]: Parameter 'cp_sort' should be either 'none','desc' or 'asc'!")
  }
  if (!inherits(cp_tight_spacing, "integer") && !inherits(cp_tight_spacing, "numeric")) {
    stop("[Error][circlesplot][Error in Parameter(s)]: Parameter 'cp_tight_spacing' should be integer or numeric!")
  }
  if (cp_tight_spacing < 1.0 || cp_tight_spacing > 2.0) {
    stop("[Error][circlesplot][Error in Parameter(s)]: Parameter 'cp_tight_spacing' should be between 1.0 and 2.0!")
  }
  if (!inherits(cp_shape, "character")) {
    stop("[Error][circlesplot][Error in Parameter(s)]: Parameter 'cp_shape' should be character!")
  }
  if (cp_shape != 'circle' && cp_shape != 'square') {
    stop("[Error][circlesplot][Error in Parameter(s)]: Parameter 'cp_shape' should be either 'circle' or 'square'!")
  }
}
