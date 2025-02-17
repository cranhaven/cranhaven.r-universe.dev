## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  dpi = 50,
  out.width = "70%"
)

## -----------------------------------------------------------------------------
library(PvSTATEM)

plate_filepath <- system.file("extdata", "CovidOISExPONTENT.csv", package = "PvSTATEM", mustWork = TRUE) # get the filepath of the csv dataset

layout_filepath <- system.file("extdata", "CovidOISExPONTENT_layout.xlsx", package = "PvSTATEM", mustWork = TRUE)


plate <- read_luminex_data(plate_filepath, layout_filepath) # read the data

plate

## -----------------------------------------------------------------------------
example_dir <- tempdir(check = TRUE) # create a temporary directory to store the output
df <- process_plate(plate, output_dir = example_dir)
colnames(df)

## -----------------------------------------------------------------------------
df[1:5, 1:5]

## -----------------------------------------------------------------------------
plate$summary()

plate$summary(include_names = TRUE) # more detailed summary

plate$sample_names
plate$analyte_names

## -----------------------------------------------------------------------------
summary(plate)

## -----------------------------------------------------------------------------
plot_standard_curve_analyte(plate, analyte_name = "OC43_S")

plate$blank_adjustment()

print(plate$blank_adjusted)

plot_standard_curve_analyte(plate, analyte_name = "OC43_S")

## -----------------------------------------------------------------------------
plot_standard_curve_analyte(plate, analyte_name = "RBD_wuhan", data_type = "Mean")
plot_standard_curve_analyte(plate, analyte_name = "RBD_wuhan", data_type = "Avg Net MFI")

## -----------------------------------------------------------------------------
plot_standard_curve_analyte(plate, analyte_name = "ME")
plot_standard_curve_analyte(plate, analyte_name = "ME", log_scale = "all")

## -----------------------------------------------------------------------------
plot_mfi_for_analyte(plate, analyte_name = "OC43_S")

plot_mfi_for_analyte(plate, analyte_name = "Spike_6P")

## -----------------------------------------------------------------------------
model <- create_standard_curve_model_analyte(plate, analyte_name = "OC43_S")

model

## -----------------------------------------------------------------------------
plot_standard_curve_analyte_with_model(plate, model, log_scale = c("all"))
plot_standard_curve_analyte_with_model(plate, model, log_scale = c("all"), plot_asymptote = FALSE)

## -----------------------------------------------------------------------------
mfi_values <- plate$data$Median$OC43_S
head(mfi_values)

predicted_rau <- predict(model, mfi_values)

head(predicted_rau)

## -----------------------------------------------------------------------------
model <- create_standard_curve_model_analyte(plate, analyte_name = "Spike_6P")
plot_standard_curve_analyte_with_model(plate, model, log_scale = c("all"))

## -----------------------------------------------------------------------------
plot_standard_curve_analyte_with_model(plate, model, log_scale = c("all"), over_max_extrapolation = 100000)

## -----------------------------------------------------------------------------
nmfi_values <- get_nmfi(plate)

# process plate with nMFI normalisation

df <- process_plate(plate, output_dir = example_dir, normalisation_type = "nMFI")
df[1:5, 1:5]

