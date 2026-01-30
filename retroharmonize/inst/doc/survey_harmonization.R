## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(retroharmonize)

## ----simple-example-def-------------------------------------------------------
library(labelled)
survey_1 <- data.frame(
  sex = labelled(c(1, 1, 0, NA_real_), c(Male = 1, Female = 0))
)
attr(survey_1, "id") <- "Survey 1"

survey_2 <- data.frame(
  gender = labelled(c(1, 3, 9, 1, 2), c(male = 1, female = 2, other = 3, declined = 9))
)
attr(survey_2, "id") <- "Survey 2"

## ----ex1----------------------------------------------------------------------
library(dplyr, quietly = TRUE)
survey_1 %>%
  mutate(
    sex_numeric = as_numeric(.data$sex),
    sex_factor = as_factor(.data$sex)
  )

## ----example2-----------------------------------------------------------------
survey_2 %>%
  mutate(
    gender_numeric = as_numeric(.data$gender),
    gender_factor = as_factor(.data$gender)
  )

## ----manually-joined----------------------------------------------------------
survey_joined <- data.frame(
  id = c(1, 2, 3, 4, 1, 2, 3, 4, 5),
  survey = c(rep(1, 4), rep(2, 5)),
  gender = labelled(c(1, 1, 0, 9, 1, 3, 9, 1, 0), c(male = 1, female = 0, other = 3, declined = 9))
)

survey_joined %>%
  mutate(
    id = paste0("survey_", .data$survey, "_", .data$id),
    gender_numeric = c(1, 1, 0, NA_real_, 1, 3, NA_real_, 1, 0),
    gender_factor = as_factor(.data$gender),
    is_female = ifelse(.data$gender_numeric == 0, 1, 0)
  )

## -----------------------------------------------------------------------------
library(dplyr)

survey_1 %>%
  mutate(
    survey = 1,
    sex_numeric = as_numeric(.data$sex),
    sex_factor = as_factor(.data$sex)
  ) %>%
  full_join(
    survey_2 %>%
      mutate(
        survey = 2,
        gender_numeric = as_numeric(.data$gender),
        gender_factor = as_factor(.data$gender)
      )
  )

## ----naive-join---------------------------------------------------------------
library(dplyr)

survey_var_harmonized <- survey_1 %>%
  rename(gender = .data$sex) %>%
  mutate(
    survey = 1,
    gender_numeric = as_numeric(.data$gender),
    gender_factor = as_factor(.data$gender)
  ) %>%
  full_join(
    survey_2 %>%
      mutate(
        survey = 2,
        gender_numeric = as_numeric(.data$gender),
        gender_factor = as_factor(.data$gender)
      ),
    by = c("gender", "survey", "gender_numeric", "gender_factor")
  )

## -----------------------------------------------------------------------------
summary(survey_var_harmonized)

## -----------------------------------------------------------------------------
survey_joined %>%
  mutate(
    id = paste0("survey_", .data$survey, "_", .data$id),
    gender_numeric = c(1, 1, 0, NA_real_, 1, 3, NA_real_, 1, 0),
    gender_factor = as_factor(.data$gender),
    female_ratio = ifelse(.data$gender_numeric == 0, 1, 0)
  ) %>%
  summary()

