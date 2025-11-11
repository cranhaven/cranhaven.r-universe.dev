## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(peruflorads43)
library(dplyr)
library(tibble)
library(ggplot2)
library(scales)
library(gt)

## -----------------------------------------------------------------------------
threatenedperu <- get_threatened_database(type = "original")


## -----------------------------------------------------------------------------

threatenedperu |>
  dplyr::mutate(compara = scientific_name != accepted_name) |> 
  dplyr::mutate(
    # Calcula la distancia solo cuando los nombres difieren
    string_distance = ifelse(
      compara,
      stringdist::stringdist(scientific_name, accepted_name, method = "lv"), # 'lv' = Levenshtein
      0
    )
  ) |>
  dplyr::filter(string_distance > 0 & string_distance < 3) |> 
  dplyr::select(scientific_name, accepted_name, string_distance) |> 
  gt::gt()

## -----------------------------------------------------------------------------
# Nombres aceptados
threatenedperu |>
filter(taxonomic_status == "Accepted") |>
select(scientific_name, accepted_name) |>
slice_head(n = 5)

#Nombres sinónimos

threatenedperu |>
filter(taxonomic_status == "Synonym") |>
select(scientific_name, accepted_name) |>
slice_head(n = 5)

#Casos sin opinión

threatenedperu |>
filter(taxonomic_status == "No opinion") |>
select(scientific_name, accepted_name) |>
slice_head(n = 5)

## -----------------------------------------------------------------------------
status_summary <- threatenedperu |>
count(taxonomic_status) |>
mutate(pct = round(100 * n / sum(n), 1), 
       label = paste0(pct, "% (", n, ")"))

gt::gt(status_summary)

## ----fig.width=20, fig.height=10, out.width='90%', fig.align='center'---------
ggplot(status_summary, aes(
  x = reorder(taxonomic_status, -pct),
  y = pct,
  fill = taxonomic_status
)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = label), vjust = -0.4, size = 10, fontface = "bold") +
  scale_fill_manual(
    values = c(
      "Accepted" = "#4CAF50",   # verde para nombres válidos
      "Synonym" = "#2196F3",    # azul para sinónimos
      "No opinion" = "#FFC107"  # amarillo para indeterminados
    )
  ) +
  labs(
    title = "Proporción de nombres por estado taxonómico",
    x = "Estatus taxonómico",
    y = "Porcentaje (%)"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 22, margin = margin(t = 10)),
    axis.title.y = element_text(size = 22,margin = margin(r = 10)),
    axis.text = element_text(size = 22, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

## -----------------------------------------------------------------------------
category_summary <- threatenedperu |>
count(threat_category, .drop = FALSE) |>
mutate(
    threat_category = factor(threat_category, levels = c("CR","EN","VU","NT")),
    pct   = round(100 * n / sum(n), 1),
    label = paste0(n, " (", pct, "%)")
  )

gt::gt(category_summary)


## ----fig.width=20, fig.height=10, out.width='90%', fig.align='center'---------
ggplot(category_summary,
       aes(x = forcats::fct_reorder(threat_category, n),
                                    y = n,
           fill = threat_category)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = label), vjust = -0.35, size = 10, fontface = "bold") +
  scale_fill_manual(values = c(
    "CR" = "#D32F2F",        
    "EN" = "#F57C00",        
    "VU" = "#FBC02D",        
    "NT" = "#388E3C"        
  )) +
  labs(
    title = "Distribución de especies por categoría de amenaza (IUCN)",
    x = "Categoría IUCN", y = "Número de especies"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
    axis.text  = element_text(size = 22, color = "black"),
    axis.title = element_text(size = 22, color = "black"),
    panel.grid.major.x = element_blank()
  )

## -----------------------------------------------------------------------------
threatenedperu |>
filter(taxonomic_status == "Synonym") |>
select(scientific_name, accepted_name, accepted_family, threat_category) |>
  head(n=20) |> 
  gt::gt()

## -----------------------------------------------------------------------------
threatenedperu |>
summarise(
total = n(),
iguales = sum(scientific_name == accepted_name, na.rm = TRUE),
porcentaje_iguales = round(100 * iguales / total, 1)
)

## -----------------------------------------------------------------------------
threatenedperu |>
  dplyr::count(taxonomic_status) |>
  dplyr::mutate(porcentaje = scales::percent(n / sum(n), accuracy = 0.1)) |> 
  gt::gt()
  

