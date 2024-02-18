## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup--------------------------------------------------------------------
#  library(threeBrain)
#  subject_code <- "N27"
#  subject_path <- "~/Downloads/N27"
#  brain <- freesurfer_brain2(subject_path, subject_code)

## -----------------------------------------------------------------------------
#  electrode_table <- `~/Downloads/N27/electrodes.csv`
#  brain$set_electrodes(electrode_table)
#  brain$plot()

## -----------------------------------------------------------------------------
#  electrode_table <- `~/Downloads/N27/electrodes.csv`
#  value_table <- `~/Downloads/N27/values.csv`
#  brain$set_electrodes(electrode_table)
#  brain$set_electrode_values(value_table)
#  brain$plot()

## -----------------------------------------------------------------------------
#  pal <- c(
#    colorRampPalette(c("red", "#FFC6C6"))(8),
#    colorRampPalette(c("#FFC6C6", "white"))(56)
#  )
#  brain$plot(
#    value_ranges = list( "p_value" = c(0, 1) ),
#    palettes = list( "p_value" = pal )
#  )

