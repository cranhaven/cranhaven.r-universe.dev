#' This list needs to be updated regularly as the npm package changes.
#' Therefore, it is good to have it as a dataset bundled for:
#' 1. users to refer to when they learn about the package and the TGVE
#' 2. internal package use rather than updating code without updating(1)
#'
#' as of 1.5.0
apis = c(tooltipColumns="tgver.tooltipColumns",
         defaultURL="tgver.defaultURL",
         geographyURL="tgver.geographyURL",
         geographyColumn="tgver.geographyColumn",
         column="tgver.column",
         data="tgver.data",
         layerName="tgver.layerName",
         dark="tgver.dark",
         viewport="tgver.viewport",
         hideChartGenerator="tgver.hideChartGenerator",
         hideCharts="tgver.hideCharts",
         hideSidebar="tgver.hideSidebar",
         select="tgver.select")
usethis::use_data(apis, internal = TRUE, overwrite = TRUE)
