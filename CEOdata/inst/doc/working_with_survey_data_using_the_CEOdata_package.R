## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
library(CEOdata)

## ----message = FALSE, echo = TRUE, eval = FALSE-------------------------------
#  library(CEOdata)
#  d <- CEOdata()

## ----message = FALSE, echo = FALSE, eval = TRUE-------------------------------
library(knitr)
library(CEOdata)
d <- CEOdata()
# If there is an internet problem, do not run the remaining of the chunks.
if (is.null(d)) {
  print("here")
  knitr::opts_chunk$set(eval = FALSE)
} else {
  knitr::opts_chunk$set(eval = TRUE)
}

## ---- message = FALSE, warning = FALSE----------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

## -----------------------------------------------------------------------------
d |>
  count(SEXE)

## ----prop-females, fig.width = 8, fig.height = 4, fig.cap = 'Proportion of females in the different Barometers.'----
d |>
  group_by(BOP_NUM) |>
  summarize(propFemales = length(which(SEXE == "Dona")) / n()) |>
  ggplot(aes(x = BOP_NUM, y = propFemales, group = 1)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  expand_limits(y = c(0, 1))

## ----tags, fig.width = 6, fig.height = 6, fig.cap = 'Prevalence of topics covered.'----
tags <- CEOmeta() |>
  separate_rows(Descriptors, sep = ";") |>
  mutate(tag = factor(stringr::str_trim(Descriptors))) |>
  select(REO, tag)

tags |>
  group_by(tag) |>
  count() |>
  filter(n > 5) |>
  ggplot(aes(x = n, y = reorder(tag, n))) +
    geom_point() +
    ylab("Topic")

## ----fieldwork, fig.width = 8, fig.height = 10, fig.cap = 'Fieldwork periods.'----
CEOmeta() |>
  filter(`Dia inici treball de camp` > "2018-01-01") |>
  ggplot(aes(xmin = `Dia inici treball de camp`,
             xmax = `Dia final treball de camp`,
             y = reorder(REO, `Dia final treball de camp`),
             color = microdata_available)) +
  geom_linerange() +
  xlab("Date") + ylab("Surveys with fieldwork") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

## -----------------------------------------------------------------------------
survey.data <- d |>
  mutate(Female = ifelse(SEXE == "Dona", 1, 0),
         Age = EDAT,
         # Pass NA correctly
         Income = ifelse(INGRESSOS_1_15 %in% c("No ho sap", "No contesta"), 
                         NA,
                         INGRESSOS_1_15),
         Date = Data,
         # Reorganize factor labels
         `Place of birth` = factor(case_when(
            LLOC_NAIX == "Catalunya" ~ "Catalonia",
            LLOC_NAIX %in% c("No ho sap", "No contesta") ~ as.character(NA),
            TRUE ~ "Outside Catalonia")),
         # Convert into numerical (integer)
         `Interest in politics` = case_when(
            INTERES_POL == "Gens" ~ 0L,
            INTERES_POL == "Poc" ~ 1L,
            INTERES_POL == "Bastant" ~ 2L,
            INTERES_POL == "Molt" ~ 3L,
            TRUE ~ as.integer(NA)),
         # Convert into numeric (double) and properly address missing values
         `Satisfaction with democracy` = ifelse(
            SATIS_DEMOCRACIA %in% c("No ho sap", "No contesta"),
            NA,
            as.numeric(SATIS_DEMOCRACIA))) |>
  # Center income to the median
  mutate(Income = Income - median(Income, na.rm = TRUE)) |>
  # Pick only specific variables
  select(Date, Female, Age, Income,
         `Place of birth`, `Interest in politics`, 
         `Satisfaction with democracy`)



## ----eval = FALSE-------------------------------------------------------------
#  save(survey.data, file = "my_cleaned_dataset.RData")

## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  library(vtable)
#  st(survey.data)

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
if (exists("survey.data")) {
  if (!is.null(survey.data)) {
    vtable::st(survey.data, out = "kable")
  }
}

## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  library(compareGroups)
#  createTable(compareGroups(Female ~ . -Date, data = survey.data))

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
if (exists("survey.data")) {
  if (!is.null(survey.data)) {
    library(compareGroups)
    createTable(compareGroups(Female ~ . -Date, data = survey.data))
  }
}

