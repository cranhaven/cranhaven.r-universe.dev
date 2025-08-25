## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE, 
  message = FALSE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("tntpr")

## ----load_packages------------------------------------------------------------
# NOTE: We are not simply loading `tidyverse` in this vignette due to how vignettes
# are built to create the documentation website. In other contexts, however, we
# would simply use `library(tidyverse)` instead of loading many of the packages individually.
library(tntpr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(forcats)
library(ggalt)
library(ggridges)

## -----------------------------------------------------------------------------
# base plot we will reuse in this section
plt <- ggplot(ggplot2::mpg, aes(displ, hwy)) +
  geom_point() +
  labs(
    title = "Cars with higher displacement\nhave a lower MPG",
    subtitle = "Displacement vs. MPG",
    caption = "Data from ggplot's mpg dataset",
    x = "Engine Displ.",
    y = "MPG"
  )

plt +
  tntp_style()

## -----------------------------------------------------------------------------
plt +
  tntp_style(show_axis_titles = TRUE, family = "sans", base_size = 20) +
  theme(plot.subtitle = ggplot2::element_text(family = "serif", size = 25))

## -----------------------------------------------------------------------------
plt +
  tntp_style(show_axis_titles = TRUE) +
  theme(panel.border = element_rect(color = "black", fill = NA))

## -----------------------------------------------------------------------------
tntp_colors("green", "mint")

## -----------------------------------------------------------------------------
show_tntp_colors("green", "moss", "mint")

## -----------------------------------------------------------------------------
# Note: the cex_label parameter is used to adjust the relative font size
show_tntp_colors(cex_label = 0.7)

## -----------------------------------------------------------------------------
tntp_palette("likert_6")

## -----------------------------------------------------------------------------
show_tntp_palette("likert_6", reverse = TRUE)

## -----------------------------------------------------------------------------
show_tntp_palette()

## ----message = FALSE----------------------------------------------------------
# load fake data into global environment
# remove all salaries of 0
county_data <- tntpr::fake_county |> 
  filter(t_salary > 0)

avg_salary <- county_data |> 
  filter(t_salary != 0) |> 
  group_by(school_year) |> 
  summarize(avg_salary = mean(t_salary, na.rm = TRUE), .groups = "drop")

## -----------------------------------------------------------------------------
base_font_size <- 16

## ----message = FALSE----------------------------------------------------------
#Make plot
ggplot(avg_salary, aes(x = school_year, y = avg_salary)) +
  geom_line(colour = tntp_colors("green"), linewidth = 1) +
  scale_y_continuous(labels = scales::dollar, limits = c(0, 5000)) +
  labs(
    title="Average Teacher Salaries",
    subtitle = "Teacher salaries remained constant between 2012 and 2015"
  ) +
    tntp_style(base_size = base_font_size)

## -----------------------------------------------------------------------------
#Prepare data
school_salary <- county_data |> 
  filter(t_salary != 0) |> 
  group_by(school_year, school_name) |> 
  summarize(avg_salary = mean(t_salary, na.rm = TRUE), .groups = "drop")

# create list of school names so we can easily filter data set for the number of schools we want
school_names <- unique(school_salary$school_name)

# only plot two schools
line_plot_schools <- school_salary |> 
  filter(school_name %in% school_names[1:3])


## -----------------------------------------------------------------------------
ggplot(line_plot_schools, aes(x = school_year, y = avg_salary, color = school_name)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::dollar, limits = c(0, 5000)) +
  scale_colour_manual(values = tntp_palette("colorful")) +
  labs(
    title="Average Teacher Salaries",
    subtitle = "Teacher salaries remained constant between 2012 and 2015"
  ) +
  tntp_style(base_size = base_font_size)

## -----------------------------------------------------------------------------
#Prepare data
bar_df <- school_salary |>
  filter(
    school_year == 2015,
    school_name %in% school_names[1:5]
  ) |> 
  # add line breaks for better plotting
  mutate(school_name = str_wrap(school_name, 7))

ggplot(bar_df, aes(x = school_name, y = avg_salary)) +
  geom_bar(stat="identity", 
           position="identity", 
           fill= tntp_colors("gold")) +
  scale_y_continuous(labels = scales::dollar, limits = c(0, 5000)) +
  labs(
    title="Acacia had higher average salaries in 2015",
    subtitle = "Average teacher salaries in 2015 by school"
  ) +
  tntp_style(base_size = base_font_size)

## -----------------------------------------------------------------------------

#prepare data
stacked_df <- county_data |> 
  filter(
    school_year == 2015,
    school_name %in% school_names[1:5]
  ) |> 
  mutate(t_salary_cut = cut(t_salary, 
                    breaks = c(0, 2500, 3500, 4500, 10000),
                    labels = c("under $2.5k", "$2.5k-$3.5k", "$3.5k-$4.5k", "$4.5k+"))) |>
  group_by(school_name, t_salary_cut) |>
  summarise(n_cut_school = n(), .groups = "drop_last") |> 
  mutate(
    n_cut_salary = sum(n_cut_school, na.rm = TRUE),
    perc_in_each_cut = n_cut_school / n_cut_salary
  )  |> 
  # add line breaks for better plotting
  mutate(school_name = str_wrap(school_name, 7))

#set order of stacks by changing factor levels
stacked_df$t_salary_cut = factor(stacked_df$t_salary_cut, levels = rev(levels(stacked_df$t_salary_cut)))

## -----------------------------------------------------------------------------
ggplot(
  data = stacked_df, 
  aes(x = school_name, y = perc_in_each_cut, fill = t_salary_cut)
) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_fill_manual(values = tntp_palette("greens")) +
  labs(title = "Most teachers earn between $2.5K and $4.5K",
       subtitle = "Percentage of teachers by salary range") +
  theme(legend.position = "top", 
        legend.justification = "left") +
  guides(fill = guide_legend(reverse = TRUE)) +
  tntp_style(base_size = base_font_size)

## -----------------------------------------------------------------------------
# only plot the lowest and highest earning groups
earnings_to_keep <- levels(stacked_df$t_salary_cut)[c(4,1)]

# map bar colors to values
colors_to_use <- tntp_colors("yellow", "green") |> 
  set_names(earnings_to_keep)

dodged_df <- stacked_df |> 
  # only plot the lowest and highest earning groups
  filter(t_salary_cut %in% !!earnings_to_keep) |> 
  # switch order of factors so that the lowest earnings plot first
  mutate(t_salary_cut = factor(t_salary_cut, levels = earnings_to_keep)) 

ggplot(dodged_df, aes(x = school_name, y = perc_in_each_cut, fill = t_salary_cut)) +
    geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
    scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
    scale_fill_manual(values = colors_to_use) +
    labs(title = "More teachers earn over $4.5k than under $2.5k",
         subtitle = "Percentage of teachers by salary range") +
    tntp_style(base_size = base_font_size)

## -----------------------------------------------------------------------------
# create data set showing school test scores and average school salaries
scores_salary <- county_data |> 
  group_by(school_name) |> 
  summarize(
    avg_test_score = mean(sch_ela_avg + sch_math_avg, na.rm=FALSE),
    avg_salary = mean(t_salary, na.rm = FALSE),
    enrollment = mean(sch_enroll_2015, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  mutate(perc_rank_test_score = percent_rank(avg_test_score)) |> 
  drop_na()

## -----------------------------------------------------------------------------
ggplot(scores_salary, aes(avg_salary, perc_rank_test_score)) +
  geom_point(color = tntp_colors("tangerine"), alpha = 0.9, size = 3) +
  labs(
    title = "Schools with higher salaries do not have higher test scores",
    subtitle = "Relationship between school test scores and salaries",
    x = "Average school salary",
    y = "Percentile rank test score"
  ) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::percent) +
  tntp_style(base_size = base_font_size, show_axis_titles = TRUE)


## -----------------------------------------------------------------------------
dumbbell_df <- dodged_df |> 
  pivot_wider(id_cols = "school_name", names_from = "t_salary_cut", values_from = "n_cut_school") |> 
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

ggplot(dumbbell_df, aes(x = `under $2.5k`, xend = `$4.5k+`, y = fct_reorder(school_name, `under $2.5k`)), group = school_names) + 
  geom_dumbbell(
    color = tntp_colors("light_grey"),
    size = 3,
    colour_x = tntp_colors("gold"),
    colour_xend = tntp_colors("green"),
    show.legend = TRUE
  ) +
  labs(title = "More teachers earn over $4.5k than under $2.5k",
       subtitle = "Number of teachers by salary range",
       x = "Number of teachers earnign a given salary") + 
  tntp_style(base_size = base_font_size)

## ----message = FALSE----------------------------------------------------------
# number of teachers per school
number_teachers_school <- county_data |>
  count(school_year, school_name)

ggplot(number_teachers_school, aes(n)) +
  geom_histogram(binwidth = 5, colour = "white", fill = tntp_colors('navy')) +
  labs(
    title = "Schools have a wide distribution in the number of teachers",
    subtitle = "Total number of teachers per school",
    x = "Number of teacher in school",
    y = "Count"
  ) +
  tntp_style(base_size = base_font_size)

## -----------------------------------------------------------------------------
ggplot(county_data, aes(x = t_salary, y = school_year, group = school_year, fill = factor(school_year))) +
  geom_density_ridges(quantile_lines = TRUE, quantiles = 4) +
  scale_x_continuous(labels = scales::dollar) +
  labs(title = "Income distrubutions for teachers has remained constant",
       subtitle = "Income distrubution and quantiles for teachers") +
  tntp_style(base_size = base_font_size) + 
  theme(legend.position = "none") + 
  scale_fill_manual(values = tntp_palette("colorful"))

## -----------------------------------------------------------------------------
teacher_survey <- tntpr::teacher_survey

# the y-axis will contain text of an entire survey question
# we want to place line breaks in this text so plots look better
axis_line_breaks <- 40

# scales in HE questions, in order starting with the strongest
agree_disagree_scale <- rev(c("Strongly Agree", "Agree", "Somewhat Agree", "Somewhat Disagree", "Disagree", "Strongly Disagree"))

# put survey into long form and clean up question names
teacher_survey_he <- teacher_survey |> 
  select(-timing) |> 
  pivot_longer(cols = everything(), names_to = "question", values_to = "response")

# calculate percentage of responses to each high expectations question
teacher_survey_he_perc <- teacher_survey_he |> 
  drop_na("response") |>
  # calculate the number of responses for each response option
  count(question, response, name = "n_response") |>
  # calculate the number of responses for each question
  group_by(question) |>
  mutate(n_question = sum(n_response)) |>
  ungroup() |>
  # calculate percentages
  mutate(
    # calculate percentages
    percent = n_response / n_question,
    # make a column that is text of the percent for plotting
    percent_pretty = scales::percent(percent, accuracy = 1)
  )

# calculate percentage of strongly agree and agree
teacher_survey_he_perc <- teacher_survey_he_perc |> 
  mutate(scale_strength = ifelse(response %in% !!agree_disagree_scale[c(5,6)], "Strong response", "Weak response")) |>
  group_by(question, scale_strength) |>
  mutate(strong_response_percent = sum(percent)) |>
  ungroup() |> 
  mutate(
    strong_response_percent = ifelse(response == "Agree", strong_response_percent, NA),
    # create line breaks for questions ,which will make plots look better
    question = str_wrap(question, axis_line_breaks),
    response = factor(response, levels = agree_disagree_scale)
  )

## -----------------------------------------------------------------------------
ggplot(teacher_survey_he_perc, aes(percent, question, fill = response)) +
  geom_col() +
  geom_text(
    aes(label = scales::percent(strong_response_percent, accuracy = 1), x = strong_response_percent),
    color = "white", fontface = "bold", family = "Halyard Display", size = 5, hjust = 1.05
  ) +
  scale_x_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_fill_manual(values = tntp_palette("top2_6"), drop = FALSE) +
  guides(fill=guide_legend(nrow=2, byrow=TRUE, reverse = TRUE))  +
  labs(title = "High Expectations Survey Responses") +
  tntp_style(base_size = base_font_size)

## ----eval=FALSE---------------------------------------------------------------
#  geom_col(position = "diverge")

## -----------------------------------------------------------------------------
ggplot(teacher_survey_he_perc, aes(x = percent, y = question, fill = fct_rev(response))) +
  geom_col(position = position_diverge()) +
  scale_fill_manual(
    values = tntp_palette("likert_6"), 
    drop = FALSE,
    breaks = agree_disagree_scale,
    labels = agree_disagree_scale
  ) +
  geom_vline(aes(xintercept = 0), linetype = 1, linewidth = 1.2, alpha = .7) +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, .25), labels = \(x) scales::percent(abs(x))) +
  labs(title = "High Expectations Survey Responses") +
  tntp_style(base_size = base_font_size)


## -----------------------------------------------------------------------------
ggplot(teacher_survey_he_perc, aes(x = percent, y = question, fill = fct_rev(response))) +
  geom_col(position = position_diverge(break_after = "Agree")) +
  geom_text(aes(label = ifelse(percent > 0.1, percent_pretty, "")), 
            position = position_diverge(break_after = "Agree", vjust = 0.5),
            family = "Halyard Display", size = 3) + 
  scale_fill_manual(
    values = tntp_palette("likert_6"), 
    drop = FALSE,
    breaks = agree_disagree_scale,
    labels = agree_disagree_scale
  ) +
  geom_vline(aes(xintercept = 0), linetype = 1, linewidth = 1.2, alpha = .7) +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, .25), labels = \(x) scales::percent(abs(x))) +
  labs(title = "High Expectations Survey Responses") +
  tntp_style(base_size = base_font_size)


