## ----loadPackage, message = FALSE, warning=FALSE------------------------------
library(antaresViz)

## ----loadData, message = FALSE------------------------------------------------
load("data_for_antaresViz_vignette_extralight.Rdata")

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics(path = "figures/plot_resized.gif")

## ----barplot, eval = FALSE----------------------------------------------------
#  plot(
#    data_annual_synthesis,
#    table = "districts",
#    variable = "LOLD",
#    type = "barplot",
#    elements = c("00_a", "00_b", "00_c", "00_d",
#                 "00_e", "00_f", "00_g", "00_h", "00_i"),
#    interactive = FALSE,
#    width = "100%",
#    height = 400
#  )

## ----ts, eval=FALSE-----------------------------------------------------------
#  plot(
#    data_hourly_allmc,
#    table = "areas",
#    variable = "LOAD",
#    type = "ts",
#    elements = "23_b",
#    confInt = 0.95,
#    dateRange = c("2018-01-08", "2018-01-14"),
#    width = "100%",
#    height = 400,
#    interactive = FALSE
#  )

## ----density, eval=FALSE------------------------------------------------------
#  plot(
#    data_hourly_synthesis,
#    table = "areas",
#    variable = "WIND",
#    type = "density",
#    elements = c("01_a", "02_a"),
#    interactive = FALSE,
#    width = "100%",
#    height = 400
#  )

## ----heatmap, eval=FALSE------------------------------------------------------
#  plot(
#    data_hourly_synthesis_1year,
#    table = "links",
#    variable = "CONG. PROB +",
#    type = "heatmap",
#    elements = "25_c - 26_d",
#    interactive = FALSE,
#    width = "100%",
#    height = 400,
#    main = "Congestion probability"
#  )

## ----prodStack, eval = FALSE--------------------------------------------------
#  prodStack(
#    data_hourly_synthesis,
#    stack = "eco2mix",
#    areas = "37_h",
#    dateRange = c("2018-01-08", "2018-01-21"),
#    main = "Production stack",
#    unit = "GWh",
#    interactive = FALSE,
#    width = "100%",
#    height = 500
#  )

## ----eval=FALSE---------------------------------------------------------------
#  https://www.rte-france.com/eco2mix/la-production-delectricite-par-filiere

## ----exchangeStack, eval = FALSE----------------------------------------------
#  exchangesStack(
#    data_hourly_synthesis,
#    area = "37_h",
#    dateRange = c("2018-01-08", "2018-01-21"),
#    main = "Import/Export of area 37_h",
#    unit = "GWh",
#    interactive = FALSE,
#    width = "100%",
#    height = 500
#  )

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics(path = "figures/maplyout_resized.gif")

## ----plotmap_co2, eval = FALSE------------------------------------------------
#  plotMap(
#    data_annual_filtered, map_layout,
#    showLabels = TRUE,
#    sizeAreaVar = "CO2 EMIS.",
#    interactive = FALSE,
#    labelAreaVar = "CO2 EMIS.",
#    colAreaVar = "CO2 EMIS.",
#    type = "avg",
#    options = plotMapOptions(
#      areaDefaultSize = 30,
#      labelMaxSize = 8,
#      labelMinSize = 14,
#      areaColorScaleOpts = colorScaleOptions(
#        zeroCol = "white",
#        posCol = "red2"
#      )
#    ),
#    width = "100%",
#    height = 600
#  )

## ----plotmap_flows, eval = FALSE----------------------------------------------
#  plotMap(
#    data_annual_synthesis,
#    map_layout,
#    showLabels = TRUE,
#    colAreaVar = "BALANCE",
#    interactive = FALSE,
#    labelAreaVar = "BALANCE",
#    sizeLinkVar = "FLOW LIN.",
#    type = "avg",
#    options = plotMapOptions(
#      areaDefaultSize = 30,
#      labelMaxSize = 10,
#      labelMinSize = 8,
#      areaColorScaleOpts = colorScaleOptions(
#        negCol = "tomato3",
#        zeroCol = "white",
#        posCol = "blue3"
#      )
#    ),
#    width = "100%",
#    height = 600
#  )

## ----plotmap_mix, eval = FALSE------------------------------------------------
#  data_annual_synthesis$areas <- data_annual_synthesis$areas[, `:=`(
#    THERMAL = NUCLEAR + LIGNITE + COAL + GAS + OIL + `MIX. FUEL` + `MISC. DTG`, HYDRO =`H. ROR` + `H. STOR`
#  )]
#  
#  plotMap(
#    data_annual_synthesis,
#    map_layout,
#    interactive = FALSE,
#    sizeAreaVars = c("HYDRO", "SOLAR", "WIND", "THERMAL"),
#    popupAreaVars = c("HYDRO", "SOLAR", "WIND", "THERMAL"),
#    areaChartType = "pie", type = "avg",
#    options = plotMapOptions(
#      areaDefaultSize = 25
#    ),
#    width = "100%",
#    height = 600
#  )

