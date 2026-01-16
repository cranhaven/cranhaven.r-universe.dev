## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(perumammals)
library(dplyr)
library(ggplot2)
library(tidyr)

## ----load-data----------------------------------------------------------------
data("peru_mammals")

glimpse(peru_mammals)


## ----orders-diversity, fig.width=8, fig.height=6, out.width="100%", fig.align="center", fig.alt="."----
order_summary <- pm_list_orders() |> 
  dplyr::arrange(dplyr::desc(n_species)) |> 
  dplyr::mutate(percentage = round(n_species / sum(n_species) * 100, 1))

order_summary

ggplot(order_summary, 
       aes(x = reorder(order, n_species),
           y = n_species)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = n_species),
            hjust = -0.2, size = 3) +
  coord_flip() +
  labs(
    title = "Mammalian Diversity in Peru by Order",
    subtitle = "Based on Pacheco et al. (2021)",
    x = "Order",
    y = "Number of Species"
  ) +
  theme_minimal()

## ----endemics-----------------------------------------------------------------
pm_list_endemic(include_rate = TRUE)

## ----ecoregions, fig.width=10, fig.height=12, out.width="100%", fig.align="center", fig.alt="."----

# Obtener datos de ecoregiones
ecoregion_diversity <- pm_list_ecoregions(include_endemic = TRUE)

# Preparar datos para el gráfico tipo waffle
waffle_data <- ecoregion_diversity |> 
  select(ecoregion_label, pct_endemic) |> 
  mutate(
    total_pct = paste0(round(pct_endemic, 1), "% endemic"),
    ecoregion_full = paste0(ecoregion_label, "\n", total_pct)
  ) |> 
  rowwise() |> 
  mutate(
    grid_data = list(
      expand.grid(x = 1:10, y = 1:10) |> 
        mutate(
          id = row_number(),
          type = ifelse(id <= round(pct_endemic), "Endemic", "Common")
        )
    )
  ) |> 
  unnest(grid_data)

# Crear el gráfico
ggplot(waffle_data, aes(x = x, y = y, fill = type)) +
  geom_tile(color = "white", linewidth = 0.4) +
  facet_wrap(~ecoregion_full, ncol = 2) +
  scale_fill_manual(
    name = NULL,
    values = c("Common" = "#CBD5E1", "Endemic" = "#E07A5F"),
    labels = c("Common species", "Endemic species")
  ) +
  coord_equal() +
  labs(
    title = "Mammal Endemism by Ecoregion in Peru",
    subtitle = "Each square represents 1% of species. The percentage indicates the proportion of endemic species"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      face = "bold", 
      size = 16, 
      hjust = 0.5, 
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      hjust = 0.5, 
      size = 10, 
      color = "gray30", 
      margin = margin(b = 20)
    ),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.7, "cm"),
    legend.margin = margin(t = 15),
    strip.text = element_text(
      size = 9,
      face = "bold", 
      lineheight = 1.1,
      margin = margin(t = 5, b = 5)  # Aumentar márgenes
    ),
    strip.clip = "off",  # Permitir que el texto se extienda fuera del panel
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing.x = unit(2, "lines"),  # Aumentar espacio horizontal
    panel.spacing.y = unit(2.5, "lines"),  # Aumentar espacio vertical
    plot.margin = margin(15, 15, 15, 15)
  )


## ----fuzzy-matching-----------------------------------------------------------
species_list <- c(
  "Tremarctos ornatos",
  "Leopardus pardalis",
  "Odocoileus virginanus",
  "Lagothrix flavicauda",
  "Alouatta seniculus",
  "Puma concolor"
)

validated <- validate_peru_mammals(
  species_list,
  quiet = TRUE
)

validated %>%
  select(Orig.Name, Matched.Name, Match.Level, valid_rank)

## ----conservation-example-----------------------------------------------------
endemic_primates <- peru_mammals %>%
  filter(order == "Primates", endemic == TRUE) %>%
  select(genus, species, common_name)

endemic_primates

## ----data-cleaning-example----------------------------------------------------
field_data <- data.frame(
  site = rep(c("Site_A", "Site_B", "Site_C"), each = 3),
  species = c(
    "Tremarctos ornatos", "Mazama rufina", "Odocoileus virginianus",
    "Leopardus pardalis", "Puma concolor", "Leopardus jacobita",
    "Lagothrix flavicauda", "Ateles belzebuth", "Alouatta seniculus"
  ),
  count = c(2, 1, 3, 1, 2, 1, 5, 3, 2)
)

cleaned_data <- field_data %>%
  left_join(
    validate_peru_mammals(field_data$species),
    by = c("species" = "Orig.Name")
  ) %>%
  select(site, original = species, validated = Matched.Name, count, Match.Level)

cleaned_data

