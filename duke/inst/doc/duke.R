## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.asp = 0.618
)

## ----setup, warning = FALSE---------------------------------------------------

library(duke)
library(palmerpenguins)
library(dplyr)
library(ggplot2)
library(ggmosaic)

## ----students, warning = FALSE------------------------------------------------
student_names <- c("Jack", "Annie", "Paul", "Aidan", "Jake", "Josh", "Grace", "Suzy", "Beth", "Taylor", "Tanner", "Lisa", "Jimmy", "Larry", "Patricia", "Laura", "Yasmin", "Tim")
student_grades <- c("A+", "B", "A+", "C", "D", "A+", "E", "C", "B-", "B-", "D", "A-", "B+", "A-", "A-", "D", "B", "E")

students <- tibble(student = student_names, grade = student_grades)

## ----scatter c, warning = FALSE-----------------------------------------------
plot <- ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(color = body_mass_g)) +
  labs(
    title = "Bill Length vs. Bill Depth", 
    x = "Bill Length (mm)", 
    y = "Bill Depth (mm)"
  )

plot +
  scale_duke_continuous() +
  theme_duke()

plot +
  geom_point(aes(shape = species)) +
  scale_duke_continuous() +
  theme_duke()

plot +
  scale_duke_continuous() +
  theme_minimal()

## ----scatter d, warning = FALSE-----------------------------------------------
plot1 <- ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point(size = 2) +
  labs(title = "Bill Length vs. Bill Depth", x = "Bill Length (mm)", y = "Bill Depth (mm)")

ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(color = species)) +
  labs(
    title = "Bill Length vs. Bill Depth", 
    subtitle = "This is the subtitle", 
    caption = "All text is in Duke Royal Blue", 
    x = "Bill Length (mm)", 
    y = "Bill Depth (mm)"
  ) +
  facet_wrap(~species) +
  theme_duke() +
  scale_duke_color_discrete()

plot1 +
  theme_duke() +
  scale_duke_color_discrete()

plot1 +
  scale_duke_color_discrete() +
  theme_minimal()

## ----bar, warning = FALSE-----------------------------------------------------
plot2 <- ggplot(penguins, aes(x = species, fill = species)) +
  geom_bar() +
  labs(title = "Distribution of Penguin Species", x = "Species", y = "Count")

m_penguins <- penguins %>%
  dplyr::filter(sex == "male")

plot2.1 <- ggplot(m_penguins, aes(x = sex, fill = sex)) +
  geom_bar()

plot2.1 +
  scale_duke_fill_discrete() +
  theme_duke()


# 8-category plot
plot2.2 <- ggplot(students, aes(x = grade, fill = grade)) +
  geom_bar()

plot2.2 +
  scale_duke_fill_discrete() +
  theme_duke()

# 7-category plot
plot2.3 <- students %>%
  slice(-13) %>%
  ggplot(aes(x = grade, fill = grade)) +
  geom_bar()

plot2.3 +
  scale_duke_fill_discrete() +
  theme_duke()

# 6-category plot
plot2.4 <- students %>%
  slice(-c(9, 10, 13)) %>%
  ggplot(aes(x = grade, fill = grade)) +
  geom_bar()

plot2.4 +
  scale_duke_fill_discrete() +
  theme_duke()

# 5-category plot
plot2.4 <- students %>%
  slice(-c(9, 10, 13, 7, 18)) %>%
  ggplot(aes(x = grade, fill = grade)) +
  geom_bar()

plot2.4 +
  scale_duke_fill_discrete() +
  theme_duke()

# 4-category plot
plot2.5 <- students %>%
  slice(-c(9, 10, 13, 7, 18, 4, 8)) %>%
  ggplot(aes(x = grade, fill = grade)) +
  geom_bar()

plot2.5 +
  scale_duke_fill_discrete() +
  theme_duke()

## ----hist, warning = FALSE----------------------------------------------------
plot3 <- ggplot2::ggplot(penguins, aes(body_mass_g)) +
  geom_histogram(ggplot2::aes(fill = species), alpha = 0.8) +
  labs(title = "Distribution of Penguin Body Mass", caption = "(Colors used) \n Duke Royal Blue, Duke Navy Blue, Copper", x = "Body Mass (g)", y = "Count")

plot3 +
  scale_duke_fill_discrete() +
  theme_duke()

plot3 +
  scale_duke_fill_discrete() +
  theme_minimal()

## ----box, warning = FALSE-----------------------------------------------------
plot4 <- ggplot2::ggplot(penguins, ggplot2::aes(sex, body_mass_g)) +
  ggplot2::geom_boxplot() +
  ggplot2::labs(title = "Comparison of Body Mass By Sex", x = "Sex", y = "Body Mass (g)")

plot4 +
  theme_duke()

plot4 +
  theme_minimal()

## ----density, warning = FALSE-------------------------------------------------
plot5 <- ggplot2::ggplot(penguins, ggplot2::aes(bill_depth_mm)) +
  ggplot2::geom_density(ggplot2::aes(fill = species)) +
  ggplot2::labs(title = "Density of Penguin Bill Depth", x = "Bill Depth (mm)", y = "Densiy")

plot5 +
  scale_duke_fill_discrete() +
  theme_duke()

plot5 +
  scale_duke_fill_discrete() +
  theme_minimal()

## ----jitter d, warning = FALSE------------------------------------------------
plot6 <- ggplot2::ggplot(penguins, ggplot2::aes(year, body_mass_g)) +
  ggplot2::geom_jitter(ggplot2::aes(color = species)) +
  ggplot2::labs(title = "Comparison of Body Mass By Year", x = "Year", y = "Body Mass (g)")

plot6 +
  scale_duke_color_discrete() +
  theme_duke()

plot6 +
  scale_duke_color_discrete() +
  theme_minimal()

## ----jitter c, warning = FALSE------------------------------------------------
plot6.1 <- ggplot2::ggplot(penguins, ggplot2::aes(year, body_mass_g)) +
  ggplot2::geom_jitter(ggplot2::aes(color = bill_length_mm)) +
  ggplot2::labs(title = "Comparison of Body Mass By Year", x = "Year", y = "Body Mass (g)")

plot6.1 +
  scale_duke_continuous() +
  theme_duke()

plot6.1 +
  scale_duke_continuous() +
  theme_minimal()

## ----line plot, warning = FALSE-----------------------------------------------
yearly_avg <- penguins %>%
  filter(!is.na(bill_length_mm)) %>%
  group_by(island, year) %>%
  summarize(island, year, mean = mean(bill_length_mm)) %>%
  distinct(island, year, .keep_all = T)

lineplot <- ggplot(data = yearly_avg, aes(x = as.factor(year), y = mean, group = island)) +
  geom_line(aes(color = island), linewidth = 1) +
  geom_point(aes(color = island)) +
  labs(title = "Average Island Bill Length(mm) Per Year", x = "Year", y = "Average Bill Length (mm)") +
  theme_duke() +
  scale_duke_color_discrete()
lineplot

# with point shape and line pattern
lineplot.2 <- ggplot(data = yearly_avg, aes(x = as.factor(year), y = mean, group = island)) +
  geom_line(aes(color = island, linetype = island), linewidth = 1) +
  geom_point(aes(color = island, shape = island)) +
  labs(title = "Average Island Bill Length(mm) Per Year", x = "Year", y = "Average Bill Length (mm)") +
  theme_duke() +
  scale_duke_color_discrete()
lineplot.2

## ----mosaic plot, warning = FALSE---------------------------------------------
plot7 <- ggplot(data = penguins) +
  ggmosaic::geom_mosaic(aes(x = ggmosaic::product(sex, island), fill = sex)) +
  labs(title = "Penguin Sex vs. Island", x = "Island", y = "Penguin Sex", caption = "(Color from top to bottom) \n Granite, Duke Navy Blue, Duke Royal Blue")

plot7 +
  scale_duke_fill_discrete() +
  theme_duke()

plot7 +
  scale_duke_fill_discrete() +
  theme_minimal()

## ----label, warning = FALSE---------------------------------------------------
plot8 <- ggplot2::ggplot(penguins, ggplot2::aes(bill_length_mm, bill_depth_mm, fill = island)) +
  ggplot2::geom_label(aes(label = island)) +
  ggplot2::labs(title = "Bill Length/Depth by Island", x = "Bill Length (mm)", y = "Bill Depth (mm)")

plot8 +
  scale_duke_fill_discrete() +
  theme_duke()

plot8 +
  scale_duke_fill_discrete() +
  theme_minimal()

## ----quantile, warning = FALSE------------------------------------------------
plot9 <- ggplot2::ggplot(penguins, ggplot2::aes(bill_length_mm, bill_depth_mm, color = species)) +
  ggplot2::geom_quantile() +
  ggplot2::labs(title = "Bill Length/Depth Quantiles", x = "Bill Length (mm)", y = "Bill Depth (mm)")

plot9 +
  scale_duke_color_discrete() +
  theme_duke()

plot9 +
  scale_duke_color_discrete() +
  theme_minimal()

## ----area, warning = FALSE----------------------------------------------------
plot10 <- ggplot2::ggplot(penguins, ggplot2::aes(body_mass_g, fill = species)) +
  ggplot2::geom_area(stat = "bin") +
  ggplot2::labs(title = "Area of Body Mass Index", x = "BMI")

plot10 +
  scale_duke_fill_discrete() +
  theme_duke()

plot10 +
  scale_duke_fill_discrete() +
  theme_minimal()

## ----dotpot, warning = FALSE--------------------------------------------------
plot11 <- ggplot2::ggplot(penguins, ggplot2::aes(body_mass_g)) +
  ggplot2::geom_dotplot(aes(fill = species)) +
  ggplot2::labs(title = "Dotplot of BMI", x = "BMI")

plot11 +
  scale_duke_fill_discrete() +
  theme_duke()

plot11 +
  scale_duke_fill_discrete() +
  theme_minimal()

## ----freqplot, warning = FALSE------------------------------------------------
plot12 <- ggplot2::ggplot(penguins, ggplot2::aes(body_mass_g)) +
  ggplot2::geom_freqpoly(aes(color = species)) +
  ggplot2::labs(title = "Frequency Polynomial Plot of BMI", x = "BMI")

plot12 +
  scale_duke_color_discrete() +
  theme_duke()

plot12 +
  scale_duke_color_discrete() +
  theme_minimal()

## ----colplot, warning = FALSE-------------------------------------------------
plot13 <- ggplot2::ggplot(penguins, ggplot2::aes(species, body_mass_g, color = species)) +
  ggplot2::geom_col() +
  ggplot2::labs(title = "BMI By Species of BMI", x = "Species", y = "BMI")

plot13 +
  scale_duke_color_discrete() +
  theme_duke()

plot13 +
  scale_duke_color_discrete() +
  theme_minimal()

## ----violinplot, warning = FALSE----------------------------------------------
plot14 <- ggplot2::ggplot(penguins, ggplot2::aes(species, body_mass_g, fill = species)) +
  geom_violin(scale = "area") +
  ggplot2::labs(title = "BMI By Species", x = "Species", y = "BMI")

plot14 +
  scale_duke_fill_discrete() +
  theme_duke()

plot14 +
  scale_duke_fill_discrete() +
  theme_minimal()

## ----countplot, warning = FALSE-----------------------------------------------
plot15 <- ggplot2::ggplot(penguins, ggplot2::aes(species, island, color = species)) +
  geom_count() +
  ggplot2::labs(title = "Species Count by Island", x = "Species", y = "Island")

plot15 +
  scale_duke_color_discrete() +
  theme_duke()

plot15 +
  scale_duke_color_discrete() +
  theme_minimal()

## ----stepplot, warning = FALSE------------------------------------------------
plot16 <- ggplot2::ggplot(penguins, ggplot2::aes(year, body_mass_g, color = species)) +
  geom_step()
ggplot2::labs(title = "BMI By Year", x = "Year", y = "BMI")

plot16 +
  scale_duke_color_discrete() +
  theme_duke()

plot16 +
  scale_duke_color_discrete() +
  theme_minimal()

## ----coordpol, warning = FALSE------------------------------------------------
plot1 +
  facet_wrap(~species) +
  theme_bw() +
  scale_duke_color_discrete()

