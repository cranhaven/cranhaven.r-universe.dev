## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(mpactr)
library(viridis)
library(plotly)
library(data.table)
library(tidyverse)
library(Hmisc)
library(corrplot)
library(ggdendro)
library(ggtext)

## -----------------------------------------------------------------------------
data <- import_data(example_path("cultures_peak_table.csv"),
  example_path("cultures_metadata.csv"),
  format = "Progenesis"
)

## -----------------------------------------------------------------------------
data_filtered <- data |>
  filter_mispicked_ions(merge_peaks = TRUE, merge_method = "sum") |>
  filter_group(group_to_remove = "Solvent_Blank") |>
  filter_group(group_to_remove = "Media") |>
  filter_cv(cv_threshold = 0.2)

## -----------------------------------------------------------------------------
plot_qc_tree(data_filtered)

## -----------------------------------------------------------------------------
get_raw_data(data_filtered) %>%
  select(Compound, mz, rt) %>%
  head()

## -----------------------------------------------------------------------------
qc_summary(data_filtered) %>%
  head()

## -----------------------------------------------------------------------------
get_raw_data(data_filtered) %>%
  mutate(Compound = as.character(Compound)) %>%
  select(Compound, mz, rt) %>%
  left_join(qc_summary(data_filtered),
    by = join_by("Compound" == "compounds")
  ) %>%
  head()

## -----------------------------------------------------------------------------
get_raw_data(data_filtered) %>%
  mutate(Compound = as.character(Compound)) %>%
  select(Compound, mz, rt) %>%
  left_join(qc_summary(data_filtered),
    by = join_by("Compound" == "compounds")
  ) %>%
  ggplot() +
  aes(x = rt, y = mz, color = status) +
  geom_point() +
  viridis::scale_color_viridis(discrete = TRUE) +
  labs(
    x = "Retention time (min)",
    y = "m/z",
    color = "Ion Status"
  ) +
  theme_bw() +
  theme(legend.position = "top")

## -----------------------------------------------------------------------------
feature_plot <- get_raw_data(data_filtered) %>%
  mutate(Compound = as.character(Compound)) %>%
  select(Compound, mz, rt) %>%
  left_join(qc_summary(data_filtered),
    by = join_by("Compound" == "compounds")
  ) %>%
  ggplot() +
  aes(
    x = rt, y = mz, color = status,
    text = paste0("Compound: ", Compound)
  ) +
  geom_point() +
  viridis::scale_color_viridis(discrete = TRUE) +
  labs(
    x = "Retention time (min)",
    y = "m/z",
    color = "Ion Status"
  ) +
  theme_bw() +
  theme(legend.position = "top")

ggplotly(feature_plot)

## -----------------------------------------------------------------------------
ft <- get_peak_table(data_filtered)

ft[1:5, 1:7]

## -----------------------------------------------------------------------------
counts <- ft %>%
  select(Compound, all_of(get_meta_data(data_filtered)$Injection)) %>%
  column_to_rownames(var = "Compound") %>%
  select(where(~ sum(.x) != 0))

counts[1:5, 1:2]

## -----------------------------------------------------------------------------
counts_cor <- rcorr(as.matrix(counts), type = "spearman")

## -----------------------------------------------------------------------------
counts_cor$r[, 1]

## ----warning=FALSE------------------------------------------------------------
corrplot(counts_cor$r,
  type = "lower",
  method = "square",
  order = "hclust",
  col = COL2("BrBG", 10),
  tl.col = "black",
  tl.cex = .5
)

## -----------------------------------------------------------------------------
meta <- get_meta_data(data_filtered)

counts %>%
  rownames_to_column(var = "Compound") %>%
  pivot_longer(
    cols = starts_with("102623"),
    names_to = "Injection",
    values_to = "Intensity"
  ) %>%
  head()

## -----------------------------------------------------------------------------
counts %>%
  rownames_to_column(var = "Compound") %>%
  pivot_longer(
    cols = starts_with("102623"),
    names_to = "Injection",
    values_to = "Intensity"
  ) %>%
  left_join(meta, by = "Injection") %>%
  head()

## -----------------------------------------------------------------------------
counts %>%
  rownames_to_column(var = "Compound") %>%
  pivot_longer(
    cols = starts_with("102623"),
    names_to = "Injection",
    values_to = "Intensity"
  ) %>%
  left_join(meta, by = "Injection") %>%
  summarise(
    mean_intensity = mean(Intensity),
    .by = c(Compound, Sample_Code)
  ) %>%
  head()

## ----warning=FALSE------------------------------------------------------------
sample_counts <- counts %>%
  rownames_to_column(var = "Compound") %>%
  pivot_longer(
    cols = starts_with("102623"),
    names_to = "Injection",
    values_to = "Intensity"
  ) %>%
  left_join(meta, by = "Injection") %>%
  summarise(
    mean_intensity = mean(Intensity),
    .by = c(Compound, Sample_Code)
  ) %>%
  pivot_wider(
    names_from = Sample_Code,
    values_from = mean_intensity
  ) %>%
  column_to_rownames(var = "Compound")

sample_counts[1:5, 1:5]

## -----------------------------------------------------------------------------
sample_counts_cor <- rcorr(as.matrix(sample_counts), type = "spearman")

## -----------------------------------------------------------------------------
corrplot(sample_counts_cor$r,
  type = "lower",
  method = "square",
  order = "alphabet",
  col = COL2("BrBG", 10),
  tl.col = "black"
)

## ----warning=FALSE------------------------------------------------------------
group_counts <- counts %>%
  rownames_to_column(var = "Compound") %>%
  pivot_longer(
    cols = starts_with("102623"),
    names_to = "Injection",
    values_to = "Intensity"
  ) %>%
  left_join(meta, by = "Injection") %>%
  summarise(
    mean_intensity = mean(Intensity),
    .by = c(Compound, Biological_Group)
  ) %>%
  pivot_wider(
    names_from = Biological_Group,
    values_from = mean_intensity
  ) %>%
  column_to_rownames(var = "Compound")

## -----------------------------------------------------------------------------
group_counts_cor <- rcorr(as.matrix(group_counts), type = "spearman")

## -----------------------------------------------------------------------------
corrplot(group_counts_cor$r,
  type = "lower",
  method = "square",
  order = "alphabet",
  col = COL2("BrBG", 10),
  tl.col = "black"
)

## -----------------------------------------------------------------------------
dist <- stats::dist(t(counts), method = "euclidian")
cluster <- stats::hclust(dist, method = "complete")

## -----------------------------------------------------------------------------
dendro <- as.dendrogram(cluster)

## -----------------------------------------------------------------------------
den_data <- dendro_data(dendro, type = "rectangle")

## -----------------------------------------------------------------------------
ggplot(segment(den_data)) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_text(
    data = den_data$labels,
    aes(x = x, y = y, label = label),
    size = 3,
    hjust = "outward"
  ) +
  coord_cartesian(ylim = c((min(segment(den_data)$y) + 10),
                           (max(segment(den_data)$y)))) +
  coord_flip() +
  scale_y_reverse(expand = c(.5, 0)) +
  theme_dendro()

## -----------------------------------------------------------------------------
get_group_averages(data_filtered) %>%
  filter(Biological_Group == "Coculture" |
           Biological_Group == "ANG18") %>%
  select(Compound, Biological_Group, average) %>%
  pivot_wider(names_from = Biological_Group, values_from = average) %>%
  mutate(fc = Coculture / ANG18) %>%
  head()

## -----------------------------------------------------------------------------
get_group_averages(data_filtered) %>%
  filter(Biological_Group == "Coculture" |
           Biological_Group == "ANG18") %>%
  select(Compound, Biological_Group, average) %>%
  pivot_wider(names_from = Biological_Group, values_from = average) %>%
  mutate(nonzero_compound = if_else(Coculture == 0 & ANG18 == 0,
                                    FALSE,
                                    TRUE)) %>%
  filter(nonzero_compound == TRUE) %>%
  mutate(fc = Coculture / ANG18) %>%
  head()

## -----------------------------------------------------------------------------
get_group_averages(data_filtered) %>%
  filter(Biological_Group == "Coculture" |
           Biological_Group == "ANG18") %>%
  select(Compound, Biological_Group, average) %>%
  head()

## -----------------------------------------------------------------------------
get_group_averages(data_filtered) %>%
  filter(Biological_Group == "Coculture" |
           Biological_Group == "ANG18") %>%
  select(Compound, Biological_Group, average) %>%
  head()

## -----------------------------------------------------------------------------
get_group_averages(data_filtered) %>%
  filter(Biological_Group == "Coculture" |
           Biological_Group == "ANG18") %>%
  select(Compound, Biological_Group, average) %>%
  mutate(average = average + 0.001) %>%
  pivot_wider(names_from = Biological_Group, values_from = average) %>%
  head()

## -----------------------------------------------------------------------------
get_group_averages(data_filtered) %>%
  filter(Biological_Group == "Coculture" |
           Biological_Group == "ANG18") %>%
  select(Compound, Biological_Group, average) %>%
  mutate(average = average + 0.001) %>%
  pivot_wider(names_from = Biological_Group, values_from = average) %>%
  mutate(nonzero_compound = if_else(Coculture == 0.001 & ANG18 == 0.001,
                                    FALSE,
                                    TRUE)) %>%
  filter(nonzero_compound == TRUE) %>%
  head()

## -----------------------------------------------------------------------------
get_group_averages(data_filtered) %>%
  filter(Biological_Group == "Coculture" |
           Biological_Group == "ANG18") %>%
  select(Compound, Biological_Group, average) %>%
  mutate(average = average + 0.001) %>%
  pivot_wider(names_from = Biological_Group, values_from = average) %>%
  mutate(nonzero_compound = if_else(Coculture == 0.001 & ANG18 == 0.001,
                                    FALSE,
                                    TRUE)) %>%
  filter(nonzero_compound == TRUE) %>%
  mutate(fc = Coculture / ANG18) %>%
  head()

## -----------------------------------------------------------------------------
foldchanges <- get_group_averages(data_filtered) %>%
  filter(Biological_Group == "Coculture" |
           Biological_Group == "ANG18") %>%
  select(Compound, Biological_Group, average) %>%
  mutate(average = average + 0.001) %>%
  pivot_wider(names_from = Biological_Group, values_from = average) %>%
  mutate(nonzero_compound = if_else(Coculture == 0.001 & ANG18 == 0.001,
                                    FALSE,
                                    TRUE)) %>%
  filter(nonzero_compound == TRUE) %>%
  mutate(fc = Coculture / ANG18,
         logfc = log2(fc))

head(foldchanges)

## -----------------------------------------------------------------------------
fc_plotting <- foldchanges %>%
  left_join(select(ft, Compound, mz, rt), by = "Compound")

plot_ly(fc_plotting,
  x = ~logfc, y = ~rt, z = ~mz,
  marker = list(color = ~logfc, showscale = TRUE)
) %>%
  add_markers() %>%
  layout(
    scene = list(
      xaxis = list(title = "log2 Fold Change"),
      yaxis = list(title = "Retention Time (min)"),
      zaxis = list(title = "m/z")
    ),
    annotations = list(
      x = 1.13,
      y = 1.05,
      text = "log2 Fold Change",
      xref = "paper",
      yref = "paper",
      showarrow = FALSE
    )
  )

## -----------------------------------------------------------------------------
# Satterwaite
calc_samplesize_ws <- function(sd1, n1, sd2, n2) {
  s1 <- sd1 / (n1^0.5)
  s2 <- sd2 / (n2^0.5)

  n <- (s1^2 / n1 + s2^2 / n2)^2
  d1 <- s1^4 / ((n1^2) * (n1 - 1))
  d2 <- s2^4 / ((n2^2) * (n2 - 1))

  d1[which(!is.finite(d1))] <- 0
  d2[which(!is.finite(d2))] <- 0

  d <- d1 + d2

  return(n / d)
}

my_comp <- c("Coculture", "ANG18")

stats <- get_group_averages(data_filtered) %>%
  mutate(
    combRSD = sqrt(techRSD^2 + BiolRSD^2),
    combASD = combRSD * average,
    combASD = if_else(is.na(combASD), 0, combASD),
    BiolRSD = if_else(is.na(BiolRSD), 0, BiolRSD),
    techRSD = if_else(is.na(techRSD), 0, techRSD),
    neff = calc_samplesize_ws(techRSD, techn, BiolRSD, Bioln) + 1
  ) %>%
  filter(Biological_Group %in% my_comp)

head(stats)

## -----------------------------------------------------------------------------
denom <- stats %>%
  summarise(den = combASD^2 / (neff),
            .by = c("Compound", "Biological_Group")) %>%
  mutate(den = if_else(!is.finite(den), 0, den)) %>%
  summarise(denom = sqrt(sum(den)), .by = c("Compound"))

t_test <- stats %>%
  select(Compound, Biological_Group, average) %>%
  pivot_wider(names_from = Biological_Group, values_from = average) %>%
  mutate(numerator = (Coculture - ANG18)) %>% # experimental - control
  left_join(denom, by = "Compound") %>%
  mutate(t = abs(numerator / denom))

head(t_test)

## -----------------------------------------------------------------------------
df <- stats %>%
  select(Compound, Biological_Group, neff) %>%
  mutate(neff = if_else(!is.finite(neff), 0, neff)) %>%
  pivot_wider(names_from = Biological_Group, values_from = neff) %>%
  mutate(deg = Coculture + ANG18 - 2) %>%
  select(Compound, deg)

head(df)

## -----------------------------------------------------------------------------
t <- t_test %>%
  left_join(df, by = "Compound") %>%
  mutate(
    p = (1 - pt(t, deg)) * 2,
    logp = log10(p),
    neg_logp = -logp
  ) %>%
  select(Compound, t, deg, p, logp, neg_logp)

head(t)

## -----------------------------------------------------------------------------
num_ions <- t %>%
  filter(p <= 1) %>%
  count() %>%
  pull()

fc <- foldchanges %>%
  left_join(t, by = "Compound") %>%
  arrange(p) %>%
  mutate(
    qval = seq_len(length(p)),
    qval = p * num_ions / qval
  ) %>%
  arrange(desc(p))

min_qval <- 1

for (i in seq_len(nrow(fc))) {
  if (!is.finite(fc$qval[i])) {
    next
  }

  if (fc$qval[i] < min_qval) {
    min_qval <- fc$qval[i]
  } else {
    fc$qval[i] <- min_qval
  }
}

fc2 <- fc %>%
  mutate(neg_logq = -log10(qval))

## -----------------------------------------------------------------------------
fc2 %>%
  ggplot() +
  aes(x = logfc, y = neg_logp) +
  geom_point() +
  labs(x = "log2 Fold Change",
       y = "-Log~10~ *P*",
       color = "Differential Abundance") +
  theme_bw()

## -----------------------------------------------------------------------------
fc2 %>%
  ggplot() +
  aes(x = logfc, y = neg_logp) +
  geom_point() +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
  labs(x = "log2 Fold Change",
       y = "-Log~10~ *P*",
       color = "Differential Abundance") +
  theme_bw()

## -----------------------------------------------------------------------------
fc2 %>%
  ggplot() +
  aes(x = logfc, y = neg_logp) +
  geom_point() +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
  labs(x = "log2 Fold Change",
       y = "-Log~10~ *P*",
       color = "Differential Abundance") +
  geom_vline(xintercept = log2(1.5), linetype = "dashed") +
  geom_vline(xintercept = -log2(1.5), linetype = "dashed") +
  theme_bw()

## -----------------------------------------------------------------------------
fc2 %>%
  mutate(
    sig = case_when(
      p > 0.05 ~ "not_sig",
      p <= 0.05 & logfc > log2(1.5) ~ "Increased",
      p <= 0.05 & logfc < -log2(1.5) ~ "Decreased",
      TRUE ~ "Inconclusive"
    ),
    sig = factor(sig,
      levels = c("Increased", "Decreased", "Inconclusive", "not_sig"),
      labels = c("Increased", "Decreased", "Inconclusive", "Not significant")
    )
  ) %>%
  select(Compound, ANG18, Coculture, fc, logfc, p, sig) %>%
  head()

## -----------------------------------------------------------------------------
fc2 %>%
  mutate(
    sig = case_when(
      p > 0.05 ~ "not_sig",
      p <= 0.05 & logfc > log2(1.5) ~ "Increased",
      p <= 0.05 & logfc < -log2(1.5) ~ "Decreased",
      TRUE ~ "Inconclusive"
    ),
    sig = factor(sig,
      levels = c("Increased", "Decreased", "Inconclusive", "not_sig"),
      labels = c("Increased", "Decreased", "Inconclusive", "Not significant")
    )
  ) %>%
  ggplot() +
  aes(x = logfc, y = neg_logp, color = sig) +
  geom_point() +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
  geom_vline(xintercept = log2(1.5), linetype = "dashed") +
  geom_vline(xintercept = -log2(1.5), linetype = "dashed") +
  scale_color_manual(values = c("red", "blue", "grey", "black")) +
  labs(
    x = "log2 Fold Change",
    y = "-Log~10~ *P*",
    color = "Differential Abundance"
  ) +
  theme_bw()

## -----------------------------------------------------------------------------
fc2 %>%
  mutate(
    sig = case_when(
      p > 0.05 ~ "not_sig",
      p <= 0.05 & logfc > log2(1.5) ~ "Increased",
      p <= 0.05 & logfc < -log2(1.5) ~ "Decreased",
      TRUE ~ "Inconclusive"
    ),
    sig = factor(sig,
      levels = c("Increased", "Decreased", "Inconclusive", "not_sig"),
      labels = c("Increased", "Decreased", "Inconclusive", "Not significant")
    )
  ) %>%
  ggplot() +
  aes(x = logfc, y = neg_logp, color = sig) +
  geom_point() +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
  geom_vline(xintercept = log2(1.5), linetype = "dashed") +
  geom_vline(xintercept = -log2(1.5), linetype = "dashed") +
  scale_color_manual(values = c("red", "blue", "grey", "black")) +
  labs(
    x = "log2 Fold Change",
    y = "-Log~10~ *P*",
    color = "Differential Abundance"
  ) +
  theme_bw() +
  theme(
    axis.title = element_markdown(size = 20),
    axis.text = element_text(size = 15)
  )

## -----------------------------------------------------------------------------
volcano <- fc2 %>%
  mutate(
    sig = case_when(
      p > 0.05 ~ "not_sig",
      p <= 0.05 & logfc > log2(1.5) ~ "Increased",
      p <= 0.05 & logfc < -log2(1.5) ~ "Decreased",
      TRUE ~ "Inconclusive"
    ),
    sig = factor(sig,
      levels = c("Increased", "Decreased", "Inconclusive", "not_sig"),
      labels = c("Increased", "Decreased", "Inconclusive", "Not significant")
    )
  ) %>%
  ggplot() +
  aes(
    x = logfc, y = neg_logp, color = sig,
    text = paste0("Compound: ", Compound)
  ) +
  geom_point() +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
  geom_vline(xintercept = log2(1.5), linetype = "dashed") +
  geom_vline(xintercept = -log2(1.5), linetype = "dashed") +
  scale_color_manual(values = c("red", "blue", "grey", "black")) +
  labs(
    x = "log2 Fold Change",
    y = "-Log~10~ *P*",
    color = "Differential Abundance"
  ) +
  theme_bw() +
  theme(
    axis.title = element_markdown(size = 20),
    axis.text = element_text(size = 15)
  )

ggplotly(volcano)

## -----------------------------------------------------------------------------
fc2 %>%
  mutate(
    sig = case_when(
      qval > 0.05 ~ "not_sig",
      qval <= 0.05 & logfc > log2(1.5) ~ "Increased",
      qval <= 0.05 & logfc < -log2(1.5) ~ "Decreased",
      TRUE ~ "Inconclusive"
    ),
    sig = factor(sig,
      levels = c("Increased", "Decreased", "Inconclusive", "not_sig"),
      labels = c("Increased", "Decreased", "Inconclusive", "Not significant")
    )
  ) %>%
  ggplot() +
  aes(x = logfc, y = neg_logq, color = sig) +
  geom_point() +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
  geom_vline(xintercept = log2(1.5), linetype = "dashed") +
  geom_vline(xintercept = -log2(1.5), linetype = "dashed") +
  scale_color_manual(values = c("red", "blue", "grey", "black")) +
  labs(
    x = "log2 Fold Change",
    y = "-Log~10~ q-value",
    color = "Differential Abundance"
  ) +
  theme_bw() +
  theme(
    axis.title = element_markdown(size = 20),
    axis.text = element_text(size = 15)
  )

