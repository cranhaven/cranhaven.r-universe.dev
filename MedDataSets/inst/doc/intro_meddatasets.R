## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  error = TRUE,  # Continue even if there are errors
  warning = TRUE  # Show warnings
)

## ----setup--------------------------------------------------------------------
library(MedDataSets)

library(ggplot2)

library(dplyr)


## ----tooth,eval=TRUE, message=FALSE, warning=FALSE----------------------------
# Example: Visualizing tooth growth data
ggplot(ToothGrowth_df, aes(x = dose, y = len, color = supp)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Tooth Growth by Supplement Type and Dose",
       x = "Dose",
       y = "Tooth Length",
       color = "Supplement Type") +
  theme_minimal()


## ----transplant,eval=TRUE, message=FALSE, warning=FALSE-----------------------
ggplot(transplant_tbl_df, aes(x = outcome)) +
  geom_bar(fill = "steelblue", alpha = 0.8) +
  labs(title = "Transplant Outcomes",
       x = "Outcome",
       y = "Count") +
  theme_minimal()


## ----mdeath_ts,eval=TRUE, message=FALSE, warning=FALSE------------------------
# Crear un gráfico de serie de tiempo utilizando ggplot2
# Convertir 'mdeaths_ts' en un data frame
mdeaths_df <- data.frame(
  month = time(mdeaths_ts),  # Extrae las fechas de la serie de tiempo
  deaths = as.numeric(mdeaths_ts)  # Convierte la serie de tiempo a numérico
)

# Crear gráfico
ggplot(mdeaths_df, aes(x = month, y = deaths)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Muertes Masculinas Respiratorias Mensuales (1974-1980)",
       x = "Mes",
       y = "Número de Muertes") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1974, 1980, by = 1), 
                     labels = seq(1974, 1980, by = 1)) +
  geom_point(color = "red", size = 1.5, alpha = 0.5)  # Añadir puntos para cada mes



