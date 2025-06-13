#' @description use ggplot2 to draw the periodic table.
#' @title Draw the Periodic table
#' @export  period_table
#' @return  A ggplot2 object.
#' @importFrom  ggplot2 ggplot geom_text geom_rect  geom_label
#' geom_linerange  annotate theme_void  scale_y_reverse
#' scale_fill_discrete  scale_colour_discrete ggtitle
#' guides  element_text aes  theme  guide_legend
#' @author Chai


period_table <- function() {
  per_table()
}
per_table <- function() {
  ele <- c(
    "H ", "He",
    "Li", "Be", "B ", "C ", "N ", "O ", "F ", "Ne",
    "Na", "Mg", "Al", "Si", "P ", "S ", "Cl", "Ar",
    "K ", "Ca", "Sc", "Ti", "V ", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn", "Ga", "Ge", "As", "Se", "Br", "Kr",
    "Rb", "Sr", "Y ", "Zr", "Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn", "Sb", "Te", "I ", "Xe",
    "Cs", "Ba", "Hf", "Ta", "W ", "Re", "Os", "Ir", "Pt", "Au", "Hg", "TI", "Pb", "Bi", "Po", "At", "Rn",
    "Fr", "Ra", "Rf", "Db", "Sg", "Bh", "Hs", "Mt", "Ds", "Rg", "Cn", "Nh", "FI", "Mc", "Lv", "Ts", "Og"
  )
  num <- c(1:56, 72:88, 104:118)
  x <- c(
    1, 18,
    1:2, 13:18,
    1:2, 13:18,
    rep(1:18, 2),
    rep(c(1:2, 4:18), 2)
  )
  y <- c(
    1, 1,
    rep(2, 8), rep(3, 8),
    rep(4:5, each = 18),
    rep(6:7, each = 17)
  )

  f <- c(
    1, 2, 3, 4, 5, rep(1, 3), 6, 2,
    3, 4, 7, 5, 1, 1, 6, 2,
    3, 4, rep(8, 10), 7, 5, 5, 1, 6, 2,
    3, 4, rep(8, 10), 7, 7, 5, 5, 6, 2,
    3, 4, rep(8, 9), 7, 7, 7, 5, 6, 2,
    3, 4, rep(8, 9), 7, 7, 7, 7, 6, 2
  )
  f <- factor(f, labels = c(
    "nonmetal", "noble gas", "alkali metal",
    "alkali earth metal", "metalloid", "halogen",
    "metal", "transition metal"
  ))
  d2 <- data.frame(
    ele, num, x, y, f
  )
  roman_num <- c(
    "\u2160", "\u2161", "\u2162", "\u2163",
    "\u2164", "\u2165", "\u2166"
  )
  l <- c(roman_num, 1:18)
  d3 <- data.frame(
    x = c(rep(0.7, 7), 1:18 + 0.5),
    y = c(1:7 + 0.5, rep(8.1, 18)),
    l
  )


  ggplot(d2) +
    geom_rect(aes(
      xmin = x, ymin = y,
      xmax = x + 0.96, ymax = y + 0.96,
      fill = f, colour = f
    ),
    alpha = 0.45,
    size = 0.1
    ) +
    geom_text(aes(x + 0.2, y + 0.5, label = ele),
      fontface = "bold",
      hjust = 0
    ) +
    geom_text(aes(x = x + 0.05, y = y + 0.2, label = num),
      vjust = 0.5, hjust = 0,
      size = 3
    ) +
    geom_text(aes(x = x, y = y, label = l),
      d3,
      family = "sans",
      hjust = 0.5, vjust = 0.5
    ) +
    annotate("text",
      y = 6:7 + 0.6, x = c(3, 3) + 0.5,
      label = c("LA", "AC"),
      hjust = 0.5, vjust = 0.5
    ) +
    scale_y_reverse() +
    theme_void() +
    scale_fill_discrete(NULL,
      type = list(c(
        "#F1D4AF",
        "#EAEAEA", "#A6CEE3", "#1F78B4",
        "#E08D49", "#999D9A", "#D93B43",
        "#599D7A"
      ))
    ) +
    scale_colour_discrete(NULL,
      type = list(c(
        "#F1D4AF",
        "#EAEAEA", "#A6CEE3", "#1F78B4",
        "#E08D49", "#999D9A", "#D93B43",
        "#599D7A"
      ))
    ) +
    ggtitle("  Periodic Table") +
    guides(fill = guide_legend(
      nrow = 1
    )) +
    theme(
      legend.position = c(0.5, 0.9),
      plot.title = element_text(
        hjust = 0.05, vjust = -0.3
      )
    ) -> p1
  return(p1)
}
