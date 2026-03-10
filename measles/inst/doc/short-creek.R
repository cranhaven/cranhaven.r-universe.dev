## -----------------------------------------------------------------------------
#| label: load
library(measles)


## -----------------------------------------------------------------------------
#| label: data
data(short_creek, package = "measles")
data(short_creek_matrix, package = "measles")


## -----------------------------------------------------------------------------
#| label: data-peek
short_creek


## -----------------------------------------------------------------------------
#| label: data-peak-matrix
# Looking at the first 5 columns
short_creek_matrix[, 1:5] |>
  round(2)

# Looking at it as a heatmap
image(short_creek_matrix)


## -----------------------------------------------------------------------------
#| label: create-model
N <- sum(short_creek$agepops)

measles_model <- ModelMeaslesMixing(
  n                            = N,
  prevalence                   = 1 / N,
  contact_rate                 = 15 / 0.9 / 4,
  transmission_rate            = 0.9,
  vax_efficacy                 = 0.97,
  contact_matrix               = short_creek_matrix,
  hospitalization_rate         = 0.1,
  hospitalization_period       = 7,
  days_undetected              = 2,
  quarantine_period            = 21,
  quarantine_willingness       = 0.9,
  isolation_willingness        = 0.9,
  isolation_period             = 4,
  prop_vaccinated              = 0.95,
  contact_tracing_success_rate = 0.8,
  contact_tracing_days_prior   = 4
)


## -----------------------------------------------------------------------------
#| label: distribute-virus
# Adding the entities to the model
add_entities_from_dataframe(
  model = measles_model,
  entities = short_creek,
  col_name = "age_labels",
  col_number = "agepops",
  as_proportion = FALSE
)


## -----------------------------------------------------------------------------
#| label: distribute-tool
# Creating the distribution function
dist_fun <- distribute_tool_to_entities(
  prevalence = short_creek$vacc_rate,
  as_proportion = TRUE
)

# Setting the distribution function
measles_model |>
  get_tool(0) |>
  set_distribution_tool(dist_fun)


## -----------------------------------------------------------------------------
#| label: running-model
measles_model |>
  run_multiple(
    ndays = 100,
    nsims = 100,
    seed  = 8812,
    saver = make_saver("outbreak_size", "hospitalizations"),
    nthreads = 2L
  )


## -----------------------------------------------------------------------------
#| label: summarize
summary(measles_model)


## -----------------------------------------------------------------------------
#| label: run_multiple_get_results
# Extracting the results
ans <- measles_model |>
  run_multiple_get_results(
    freader = data.table::fread
  )

# Taking a look at the structure
str(ans)


## -----------------------------------------------------------------------------
#| label: data.table
# Converting into data.table format for convenience
library(data.table)
outbreak_size <- ans$outbreak_size |> as.data.table()
hospitalizations <- ans$hospitalizations |> as.data.table()


## -----------------------------------------------------------------------------
#| label: timeline-outbreak-size
# Aggregating to get the final
with(
  outbreak_size,
  {
    plot(
      x = date,
      y = outbreak_size,
      col = adjustcolor("blue", alpha.f = .2),
      pch = 19,
      ylab = "Cases",
      xlab = "Day",
      main = "Measles outbreak size in Short Creek",
      sub = "Mixing model with age and school data (100 simulations)"
    )
})


## -----------------------------------------------------------------------------
#| label: hist-outbreak-size
# Aggregating to get the final
with(
  outbreak_size[date == max(date)],
  {
    hist(
      outbreak_size,
      main = "Measles outbreak size in Short Creek",
      sub = "Mixing model with age and school data (100 simulations)",
      breaks = 20
    )
})


## -----------------------------------------------------------------------------
#| label: hospitalizations
hosp_tot <- hospitalizations[, .(total = sum(count)), by = .(sim_num, tool_id)]
hosp_tot[, status := fifelse(tool_id == -1, "Unvax", "Vax")]
hosp_tot[, tool_id := NULL]


## -----------------------------------------------------------------------------
#| label: summary-hosp
hosp_tot <- merge(
  hosp_tot[status == "Unvax", .(sim_num, unvax = total)],
  hosp_tot[status == "Vax", .(sim_num, vax = total)],
  all = TRUE
)

# Filling zeros
hosp_tot[, unvax := fcoalesce(unvax, 0L)]
hosp_tot[, vax := fcoalesce(vax, 0L)]

hosp_tot[, .(vax, unvax)] |>
  as.matrix() |>
  boxplot(
    ylab = "Count",
    xlab = "Status",
    main = "Hospitalization per vaccination status",
    sub  = "Mixing model with age and school data (100 simulations)"
  )

