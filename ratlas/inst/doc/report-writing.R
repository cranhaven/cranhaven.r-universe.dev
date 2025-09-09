## ----chunk-options, include=FALSE---------------------------------------------
if (requireNamespace("pkgdown", quietly = TRUE) && pkgdown::in_pkgdown()) {
  tiny_width <- small_width <- med_width <- 7
  large_width <- 8
} else {
  tiny_width <- small_width <- med_width <- 5
  large_width <- 5.5
}

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.asp = 0.618,
  fig.width = small_width,
  fig.align = "center",
  out.width = "90%"
)

if (capabilities("cairo") && Sys.info()[['sysname']] != "Darwin") {
  knitr::opts_chunk$set(
    dev = "png",
    dev.args = list(type = "cairo")
  )
}

## ----setup, include = FALSE---------------------------------------------------
library(ratlas)
library(dplyr)
library(ggplot2)
library(english)
library(knitr)
library(kableExtra)
library(here)
library(tibble)
library(stringr)

## ----setup-tinytex, eval = FALSE----------------------------------------------
# install.packages("tinytex")
# tinytex::install_tinytex()

## ----create-report, echo = FALSE, out.width = "80%", fig.subcap = c("Create a new project.", "Create project in a new directory.",  "Choose the type of report."), fig.cap = "Process for creating a new report with {ratlas}."----
include_graphics(c(here("vignettes", "images", "01-new-project.png"),
                   here("vignettes", "images", "02-new-directory.png"),
                   here("vignettes", "images", "03-techreport.png"),
                   here("vignettes", "images", "04-name-report.png")))

## ----name-report, echo = FALSE, out.width = "80%", fig.cap = "Open the R Markdown file."----
include_graphics(here("vignettes", "images", "05-open-rmd.png"))

## ----knit-doc, echo = FALSE, out.width = "80%", fig.cap = "(ref:knit-doc-cap)"----
include_graphics(here("vignettes", "images", "06-knit-doc.png"))

## ----yaml-header, echo = FALSE, out.width = "80%", fig.cap = "The default YAML header for a technical report."----
include_graphics(here("vignettes", "images", "07-yaml.png"))

## ----setup-chunk, echo = FALSE, fig.cap = "(ref:setup-chunk-cap)"-------------
include_graphics(here("vignettes", "images", "08-setup.png"))

## ----fake-data----------------------------------------------------------------
my_data <- tribble(
  ~label,     ~count,  ~percent,  ~correlation, ~pvalue,
  "Group 1",    103L,      0.92,        -0.895,   0.120,
  "Group 2",   1064L,      32.1,         0.051,   0.030,
  "Group 3",  79205L,      95.4,         0.927,   0.001
)
my_data

## ----fmt-table----------------------------------------------------------------
my_data %>%
  fmt_table(corr_dig = 2, prop_dig = 2)

## ----example-table------------------------------------------------------------
my_data %>%
  fmt_table(corr_dig = 2, prop_dig = 2) %>%
  kable(align = c("l", rep("c", 4)), booktabs = TRUE, linesep = "",
        escape = FALSE,
        col.names = c("Group", "Students (*n*)", "\\%", "*r*", "*p*-value"),
        caption = "An example table") %>%
 kable_styling(latex_options = "HOLD_position")

## ----example-figure, fig.cap = "An example figure."---------------------------
ggplot(iris, aes(x = Petal.Length, y = Sepal.Length, color = Species)) +
  geom_point() +
  labs(x = "Petal Length", y = "Sepal Length") +
  scale_color_okabeito() +
  theme_atlas()

## ----cite-style, echo = FALSE-------------------------------------------------
tribble(
                   ~code,             ~renders,
         "`[@R-ratlas]`",        "[@R-ratlas]",
           "`@R-ratlas`",          "@R-ratlas",
       "`[LCDM; @lcdm]`",      "[LCDM; @lcdm]",
            "`[-@lcdm]`",           "[-@lcdm]",
      "`[@lcdm, pp. 3]`",     "[@lcdm, pp. 3]",
        "`@lcdm [p. 3]`",       "@lcdm [p. 3]",
  "`[@lcdm; @R-ratlas]`", "[@lcdm; @R-ratlas]"
  
) %>%
  kable(align = c("l", "l"), booktabs = TRUE, linesep = "",
        col.names = c("Code", "Renders as..."),
        caption = "Citation Styles") %>%
  kable_styling()

## ----echo = FALSE-------------------------------------------------------------
my_data <- select(mtcars, mpg, disp, cyl) %>% as_tibble()
my_new_data <- sample_n(my_data, size = nrow(mtcars) * 1.5, replace = TRUE)

## -----------------------------------------------------------------------------
my_data

lm_mod <- lm(mpg ~ disp + cyl, data = my_data)
coef(lm_mod)

## -----------------------------------------------------------------------------
my_new_data

## -----------------------------------------------------------------------------
lm_mod <- lm(mpg ~ disp + cyl, data = my_new_data)
coef(lm_mod)

## ----write-packages, include = FALSE------------------------------------------
if (!file.exists("bib/packages.bib")) file.create("bib/packages.bib")
suppressWarnings(
  knitr::write_bib(c(.packages()), "bib/packages.bib")
)

# Correct capitalization in packages
readLines("bib/packages.bib") %>%
  str_replace_all("ratlas:", "{ratlas}:") %>%
  writeLines("bib/packages.bib")

