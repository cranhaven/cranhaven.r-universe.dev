## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE, message=FALSE---------------------------------------
library(mpactr)
library(tidyverse)

## -----------------------------------------------------------------------------
samplelist <- read_csv(example_path("PTY087I2_samplelist.csv"))

## -----------------------------------------------------------------------------
metadata <- read_csv(example_path("PTY087I2_extractmetadata.csv"))

## -----------------------------------------------------------------------------
samples <- read_csv(example_path("PTY087I2_dataset.csv"), skip = 2) %>%
  colnames() %>%
  str_subset(., "200826")

meta_data <- samplelist %>%
  left_join(metadata, by = "Sample_Code") %>%
  filter(Injection %in% samples)

## -----------------------------------------------------------------------------
data <- import_data(peak_table = example_path("PTY087I2_dataset.csv"),
  meta_data = meta_data,
  format = "Progenesis"
)

## -----------------------------------------------------------------------------
data

## -----------------------------------------------------------------------------
get_raw_data(data)[1:5, 1:8]

## -----------------------------------------------------------------------------
get_peak_table(data)[1:5, 1:8]

## -----------------------------------------------------------------------------
get_meta_data(data)[1:5, ]

## -----------------------------------------------------------------------------
data2 <- import_data(peak_table = example_path("PTY087I2_dataset.csv"),
  meta_data = meta_data,
  format = "Progenesis"
)

get_peak_table(data2)[, 1:5]

## -----------------------------------------------------------------------------
data2_mispicked <- filter_mispicked_ions(data2,
  ringwin = 0.5,
  isowin = 0.01, trwin = 0.005,
  max_iso_shift = 3, merge_peaks = TRUE,
  merge_method = "sum",
  copy_object = FALSE
)

get_peak_table(data2_mispicked)[, 1:5]

## -----------------------------------------------------------------------------
get_peak_table(data2)[, 1:5]

## -----------------------------------------------------------------------------
data_mispicked <- filter_mispicked_ions(data,
  ringwin = 0.5,
  isowin = 0.01, trwin = 0.005,
  max_iso_shift = 3, merge_peaks = TRUE,
  merge_method = "sum",
  copy_object = TRUE
)

## -----------------------------------------------------------------------------
head(get_similar_ions(data_mispicked))

## -----------------------------------------------------------------------------
data_blank <- filter_group(data,
  group_threshold = 0.01,
  group_to_remove = "Blanks", remove_ions = TRUE,
  copy_object = TRUE
)

## -----------------------------------------------------------------------------
data_media_blank <- filter_group(data,
  group_threshold = 0.01,
  group_to_remove = "Media", remove_ions = TRUE,
  copy_object = TRUE
)

## -----------------------------------------------------------------------------
data_rep <- filter_cv(data,
  cv_threshold = 0.2,
  copy_object = TRUE
)

## -----------------------------------------------------------------------------
cv <- get_cv_data(data_rep) %>%
  pivot_longer(cols = c("cv"),
               names_to = "param",
               values_to = "cv") %>%
  nest(.by = param) %>%
  mutate(
    data = map(data, arrange, cv),
    data = map(data, mutate, index = 0:(length(cv) - 1)),
    data = map(data, mutate, index_scale = index * 100 / length(cv))
  )

head(cv)

## ----echo=FALSE---------------------------------------------------------------
head(cv$data[[1]])

## -----------------------------------------------------------------------------
cv_thresh_percent <- cv %>%
  filter(param == "cv") %>%
  unnest(cols = data) %>%
  mutate(diff_cv_thresh = abs(cv - 0.2)) %>%
  slice_min(diff_cv_thresh, n = 1) %>%
  pull(index_scale)

cv_thresh_percent

## -----------------------------------------------------------------------------
cv %>%
  unnest(cols = data) %>%
  mutate(param = factor(param, levels = c("cv"),
                        labels = c("cv"))) %>%
  ggplot() +
  aes(x = cv, y = index_scale, group = param, color = param) +
  geom_line(linewidth = 2) +
  geom_vline(xintercept = 0.2,
             color = "darkgrey",
             linetype = "dashed",
             linewidth = 1) +
  geom_hline(yintercept = cv_thresh_percent,
             color = "darkgrey",
             linewidth = 1) +
  labs(x = "CV",
       y = "Percentage of Features",
       param = "Statistic") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 15),
    legend.position = "inside",
    legend.position.inside = c(.90, .08),
    legend.title = element_blank(),
    legend.text = element_text(size = 15)
  )

## -----------------------------------------------------------------------------
data_insource <- filter_insource_ions(data,
  cluster_threshold = 0.95,
  copy_object = TRUE
)

## -----------------------------------------------------------------------------
data <- import_data(peak_table = example_path("PTY087I2_dataset.csv"),
  meta_data = meta_data,
  format = "Progenesis"
)

data_filtered <- filter_mispicked_ions(data, merge_method = "sum") |>
  filter_group(group_to_remove = "Blanks") |>
  filter_cv(cv_threshold = 0.2) |>
  filter_group(group_to_remove = "Media")

## -----------------------------------------------------------------------------
mispicked_summary <- filter_summary(data_filtered, filter = "mispicked")

## -----------------------------------------------------------------------------
head(mispicked_summary$failed_ions, 100)

## -----------------------------------------------------------------------------
head(mispicked_summary$passed_ions, 100)

## ----error=TRUE---------------------------------------------------------------
try({
filter_summary(data_filtered, filter = "insource")
})

## ----eval = FALSE-------------------------------------------------------------
# filter_summary(data_filtered, filter = "group", group = "Blanks")

## -----------------------------------------------------------------------------
head(qc_summary(data_filtered)[order(compounds), ])

## -----------------------------------------------------------------------------
library(ggplot2)
library(treemapify)

ion_counts <- qc_summary(data_filtered)[, .(count = .N), by = status][
  , percent := (count / sum(count) * 100)
]

## -----------------------------------------------------------------------------
tm <- ggplot(ion_counts) +
  aes(area = percent, fill = status) +
  geom_treemap() +
  geom_treemap_text(aes(
    label = paste(status, count, paste0("(", round(percent, 2), "%)"),
                  sep = "\n"),
    fontface = c("bold")
  ))

tm

## -----------------------------------------------------------------------------
tm <- ggplot(ion_counts) +
  aes(area = percent, fill = status) +
  geom_treemap() +
  geom_treemap_text(aes(
    label = paste(status, paste0("(", round(percent, 2), "%)"), sep = "\n"),
    fontface = c("bold")
  ))

tm

## -----------------------------------------------------------------------------
tm +
  scale_fill_brewer(palette = "Greens") +
  theme(legend.position = "none")

## -----------------------------------------------------------------------------
plot_qc_tree(data_filtered)

