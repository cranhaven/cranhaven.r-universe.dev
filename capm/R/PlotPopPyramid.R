#' Population PlotPopPyramid
#' @description Displays two opposed horizontal barplots (pyramid).
#' @param dat \code{\link{data.frame}}.
#' @param age.col \code{dat} column that has a \code{\link{numeric}} \code{\link{vector}} representing ages or stage categories.
#' @param sex.col \code{dat} column that has two unique values representing the sex of individuals (see Details).
#' @param str.col \code{dat} column that has two unique values representing the reproductive status of individuals (see Details).
#' @param str.tip string with the category of \code{str.col} to place at tip of the bars.
#' @param x.label string to be used as a label for the x axis. If undefined, \code{x.label} is equal to "Total" (see Details).
#' @param stage.label a string to be used as a label for the ages or stage categories. If undefined, \code{stage.label} is equal to "Years" (see Details).
#' @param legend.label a string to be used as a label for the legend. If undefined, \code{legend.label} is equal to "Sterilized".
#' @param inner.color any valid specification of a color. When \code{str.col} is not \code{NULL}, \code{inner.color} is the color of inner bars.
#' @param outer.color any valid way specification of a color. When \code{str.col} is \code{NULL}, \code{outer.color} is the default color. When \code{str.col} is not \code{NULL}, \code{outer.color} is the outer color of bars.
#' @param label.size string to define the font size for labels.
#' @details \code{PlotPopPyramid} is mainly intended for companion animals population pyramids, although it can display other types of opposed bar charts.
#' 
#' The bars to the left of the x axis correspond to \code{sort(unique(dat[, sex.col]))[1]}. If \code{str.col} is not \code{NULL}, bars will be stacked, with \code{sort(unique(dat[, str.col]))[1]} as their base.
#' 
#' On the top of the plot, it is displayed the total number of observations of each \code{dat[, sex.col]} unique value. This unique values are used as \code{\link{labels}}.
#' 
#' The legend \code{\link{labels}} are equal to the \code{dat[, str.col]} unique values.
#' 
#' Font size of saved plots is usually different to the font size seen in graphic browsers. Before changing font sizes, see the final result in saved (or preview) plots.
#'  
#' Other details of the plot can be modifyed using appropriate functions from \code{ggplot2} package (see examples).
#'  
#' @note In companion animals population surveys, some age categories might be empty. One difference between \code{PlotPopPyramid} and \code{pryramid.plot} is that the first does not drop empty age categories.
#' @return Two opposed horizontal barplots.
#' @references Baquero, O. S., Marconcin, S., Rocha, A., & Garcia, R. D. C. M. (2018). Companion animal demography and population management in Pinhais, Brazil. Preventive Veterinary Medicine.
#' 
#' \url{http://oswaldosantos.github.io/capm}
#' @export
#' @examples 
#' data(dogs)
#' 
#' PlotPopPyramid(dogs,
#'                age.col = "age",
#'                sex.col = "sex",
#'                str.col = "sterilized")
#'
#' PlotPopPyramid(dogs,
#'                age.col = "age",
#'                sex.col = "sex")
#' 
#' ## Merge age categories
#' pp_age <- cut(c(dogs$age, dogs$age3),
#'                breaks = c(0, 1, 3, 5, 7, 9, 11, 13, 15,
#'                           max(c(dogs$age, dogs$age3), na.rm = TRUE)),
#'                labels = c("<1", "1-3", "3-5", "5-7", "7-9",
#'                           "9-11", "11-13", "13-15", ">15"),
#'                include.lowest = TRUE)
#' pp_sex <- c(dogs$sex, dogs$sex3)
#' pp_ster <- c(dogs$sterilized, dogs$sterilized3)
#' pp <- data.frame(age = pp_age, sex = pp_sex, sterilized = pp_ster)
#' 
#' PlotPopPyramid(pp,
#'               age.col = "age",
#'               sex.col = "sex",
#'               str.col = "sterilized")
#' 
#' PlotPopPyramid(pp,
#'               age.col = "age",
#'               sex.col = "sex")
#' 
PlotPopPyramid <-  function (dat = NULL, age.col = NULL, sex.col = NULL, str.col = NULL, str.tip = NULL, x.label = "Count", stage.label = "Years", legend.label = "Sterilized", inner.color = "LightBlue", outer.color = "DarkRed", label.size = 13) {
  
  # Workaround to the "no visible binding for global variable" note.
  sex <- age <- ster <- ..count.. <- NULL
  
  if (is.character(age.col)) {
    age.col <- which(names(dat) == age.col)
  }
  if (is.character(sex.col)) {
    sex.col <- which(names(dat) == sex.col)
  }
  if (!is.null(str.col)) {
    str.col <- which(names(dat) == str.col)
    names(dat)[str.col] <- "ster"
    if (!is.null(str.tip)) {
      dat$ster <- relevel(factor(dat$ster), str.tip)
    }
    dat <- dat[!is.na(dat$ster), ]
  }
  names(dat)[age.col] <- "age"
  names(dat)[sex.col] <- "sex"
  
  dat <- dat[!is.na(dat$age) & !is.na(dat$sex), ]
  
  n_sex <- dat %>%
    group_by(sex) %>%
    summarise(n = n())
  
  n_sex_age <- dat %>%
    group_by(age, sex) %>%
    summarise(n = n()) %>%
    arrange(desc(n))
  n_sex_age <- n_sex_age[1, 3][[1]]
  n_sex_age <- (n_sex_age %/% 10) * 10 + 10
  count_ticks <- seq(-n_sex_age, n_sex_age, length.out = 11)
  
  if (!is.null(str.col)) {
    gg_base <- ggplot(dat, aes(age, fill = ster)) +
      scale_fill_manual(name = legend.label,
                        values = c(outer.color, inner.color)) +
      theme_minimal() +
      theme(legend.position = c(1, .9),
            legend.justification = c(1, 1),
            legend.title = element_text(face = "plain", size = label.size),
            legend.text = element_text(size = label.size))
  } else {
    gg_base <- ggplot(dat, aes(age, fill = "a")) +
      scale_fill_manual(name = legend.label,
                        values = inner.color) +
      theme_minimal() +
      theme(legend.position = "none")
  }
  if(is.numeric(dat$age)) {
    res <- gg_base +
      geom_bar(data = subset(dat, sex == sort(unique(dat$sex))[1])) +
      geom_bar(data = subset(dat, sex == sort(unique(dat$sex))[2]),
               aes(y = ..count.. * (-1))) + 
      scale_y_continuous(name = x.label,
                         breaks = count_ticks,
                         labels = abs(count_ticks),
                         limits = c(-n_sex_age, n_sex_age)) +
      scale_x_continuous(name = stage.label,
                         breaks = unique(dat$age)) +
      geom_hline(yintercept = 0, color = "gray") +
      theme(plot.margin = unit(c(0.5, 1, 0.5, 0.5), "lines"), 
            axis.ticks.length = unit(0, "lines"),
            axis.text.y = element_text(size = label.size),
            axis.title.y = element_text(size = label.size),
            axis.text.x = element_text(size = label.size),
            axis.title.x = element_text(size = label.size),
            panel.grid.minor.y = element_blank()) +
      annotate("text",
               x = rev(sort(unique(dat$age)))[1],
               y = count_ticks[c(3, 9)],
               label = paste(sort(unique(dat$sex)), "(total) =", n_sex$n)) +
      coord_flip()
  } else {
    res <- gg_base +
      geom_bar(data = subset(dat, sex == sort(unique(dat$sex))[1])) +
      geom_bar(data = subset(dat, sex == sort(unique(dat$sex))[2]),
               aes(y = ..count.. * (-1))) + 
      scale_y_continuous(name = x.label,
                         breaks = count_ticks,
                         labels = abs(count_ticks),
                         limits = c(-n_sex_age, n_sex_age)) +
      scale_x_discrete(name = stage.label, limits = levels(factor(dat$age))) +
      geom_hline(yintercept = 0, color = "gray") +
      theme(plot.margin = unit(c(0.5, 1, 0.5, 0.5), "lines"), 
            axis.ticks.length = unit(0, "lines"),
            axis.text.y = element_text(size = label.size),
            axis.title.y = element_text(size = label.size),
            axis.text.x = element_text(size = label.size),
            axis.title.x = element_text(size = label.size),
            panel.grid.minor.y = element_blank()) +
      annotate("text",
               x = rev(sort(unique(dat$age)))[1],
               y = count_ticks[c(3, 9)],
               label = paste(sort(unique(dat$sex)), "(total) =", n_sex$n)) +
      coord_flip()
  }
  return(res)
}
