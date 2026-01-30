# (C) Philippe J.S. De Brouwer -- 2021
# licensed under GNU Affero General Public License v3.0

#' Uses ggplot2 to produce a gauge plot in RAG colour
#'
#' This function produces one or more gauge plots coloured in red (R), amber (A) or green (G) for a value between 0 and 1.
#' @param df tibble, a tibble with columns "value" and "label" (value = the values between 0 and 1;  - label = text to show e.g. paste("group", colnames(t)))
#' @param breaks numeric vector with the lower limit, the border between green and amber, the border between amber and red, and the upper limit
#' @param ncol numeric, the number of columns to produce
#' @param nbrSize numeric, the font size for the label
#' @keywords gauge plot
#' @returns ggplot object
#' @export
#' @importFrom tibble tibble
#' @examples
#' d <- div_fake_team()
#' tbl_gender_div <- table(d$gender, d$grade) %>%
#'    apply(2, diversity, prior = c(50.2, 49.8)) %>%
#'    tibble(value = ., label = paste("Grade", names(.)))
#' div_gauge_plot(tbl_gender_div, ncol = 2, nbrSize = 4)


div_gauge_plot <- function(df,
                           breaks  = c(0, 0.80, 0.95, 1),
                           ncol    = NULL,
                           nbrSize = 6.0) {

  # Global variables:
  value <- label <- NULL
  # Local variables:
  clr <- NULL

  xGrey  = "#c1c1c1"
  xRed   = "#D2222D"
  xAmber = "#FFBF00"
  xGreen = "#238823"

  get_color <- function(x) {
    if (x <= breaks[2]) {return("red")}
    if (x <= breaks[3]) {return("orange")}
    return("green")
  }

d <- tibble(df, clr = sapply(df$value, get_color))

ggplot(d, aes(fill = clr, ymax = value, ymin = 0, xmax = 2, xmin = 1)) +
  geom_rect(aes(ymax=1, ymin=0, xmax=2, xmin=1), fill =xGrey) +
  geom_rect() +
  coord_polar(theta = "y",start=-pi/2) + xlim(c(0, 2)) + ylim(c(0,2)) +
  geom_text(aes(x = 0.25, y = 0.5, label = round(value,2), colour=clr),
            family = "sans", size=nbrSize, fontface='bold') +
  geom_text(aes(x=0.4, y=1.5, label = label), fontface='bold', family="sans", size=4.0) +
  facet_wrap(~label, ncol = ncol) +
  theme_void() +
  scale_fill_manual(values = c("red"=xRed, "orange"=xAmber, "green"=xGreen)) +
  scale_colour_manual(values = c("red"=xRed, "orange"=xAmber, "green"=xGreen)) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  guides(fill=FALSE) +
  guides(colour=FALSE)
}
