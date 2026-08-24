# -------------------------------
# pacotes usados diretamente
# -------------------------------

# stats
#' @importFrom stats sd aggregate shapiro.test ks.test t.test wilcox.test var.test
#' @importFrom stats TukeyHSD complete.cases setNames na.omit

# utils
#' @importFrom utils globalVariables combn

# dplyr / purrr / tidyverse helpers
#' @importFrom dplyr %>%
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble as_tibble
#' @importFrom tidyr pivot_longer
#' @importFrom scales hue_pal

# pacotes específicos para analise
#' @importFrom car leveneTest
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_jitter geom_violin
#' @importFrom ggplot2 geom_point geom_segment annotate labs theme_minimal
#' @importFrom ggplot2 scale_fill_manual element_text
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggExtra ggMarginal
NULL
