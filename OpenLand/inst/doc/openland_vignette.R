## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include=FALSE------------------------------------------------------------
dataset_available <- tryCatch({
  # downloading the SaoLourencoBasin multi-layer raster and make it available into R
  url <- "https://zenodo.org/record/3685230/files/SaoLourencoBasin.rda?download=1"
  temp <- tempfile()
  download.file(url, temp, mode = "wb") # downloading the SaoLourencoBasin dataset
  load(temp)
}, error = function(e) {
  return(FALSE)
})

## ----include=FALSE, eval=isFALSE(dataset_available)---------------------------
#  knitr::knit_exit("## The complete rendering of this document depends on the availability of the dataset in zenodo. Please try at another time.")

## -----------------------------------------------------------------------------
# first we load the OpenLand package
library(OpenLand)

# The SaoLourencoBasin dataset
SaoLourencoBasin


## ----eval=FALSE, fig.height=5, fig.width=7, include=FALSE---------------------
#  #sp::spplot(SaoLourencoBasin)

## -----------------------------------------------------------------------------

# SL_2002_2014 <- contingencyTable(input_raster = SaoLourencoBasin, pixelresolution = 30)

SL_2002_2014


## ----echo=TRUE----------------------------------------------------------------
## editing the category name
SL_2002_2014$tb_legend$categoryName <- factor(c("Ap", "FF", "SA", "SG", "aa", "SF", 
                                          "Agua", "Iu", "Ac", "R", "Im"),
                                  levels = c("FF", "SF", "SA", "SG", "aa", "Ap", 
                                         "Ac", "Im", "Iu", "Agua", "R"))

## add the color by the same order of the legend,
## it can be the color name (eg. "black") or the HEX value (eg. #000000)
SL_2002_2014$tb_legend$color <- c("#FFE4B5", "#228B22", "#00FF00", "#CAFF70", 
                                  "#EE6363", "#00CD00", "#436EEE", "#FFAEB9", 
                                  "#FFA54F", "#68228B", "#636363")

## now we have
SL_2002_2014$tb_legend


## ----eval=FALSE, include=FALSE------------------------------------------------
#  knitr::kable(
#  caseStudy$lulc_Mulstistep[1:10, ]
#  )

## -----------------------------------------------------------------------------
testSL <- intensityAnalysis(dataset = SL_2002_2014,
                            category_n = "Ap", category_m = "SG")

# it returns a list with 6 objects
names(testSL)


## -----------------------------------------------------------------------------
# showing the objects from the intensity analysis for our illustrative case
testSL


## ----fig.width=7, fig.height=4------------------------------------------------

plot(testSL$interval_lvl,
     labels = c(leftlabel = "Interval Change Area (%)",
                rightlabel = "Annual Change Area (%)"),
     marginplot = c(-8, 0), labs = c("Changes", "Uniform Rate"), 
     leg_curv = c(x = 2/10, y = 3/10))


## ----fig.width=7, fig.height=4------------------------------------------------

plot(testSL$category_lvlGain,
     labels = c(leftlabel = bquote("Gain Area (" ~ km^2 ~ ")"),
                rightlabel = "Intensity Gain (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
     leg_curv = c(x = 5/10, y = 5/10))



## ----fig.width=7, fig.height=4------------------------------------------------

plot(testSL$category_lvlLoss,
     labels = c(leftlabel = bquote("Loss Area (" ~ km^2 ~ ")"),
                rightlabel = "Loss Intensity (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
     leg_curv = c(x = 5/10, y = 5/10))


## ----fig.width=7, fig.height=4------------------------------------------------

plot(testSL$transition_lvlGain_n,
     labels = c(leftlabel = bquote("Gain of Ap (" ~ km^2 ~ ")"),
                rightlabel = "Intensity Gain of Ap (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
     leg_curv = c(x = 5/10, y = 5/10))


## ----fig.width=7, fig.height=4------------------------------------------------

plot(testSL$transition_lvlLoss_m,
     labels = c(leftlabel = bquote("Loss of SG (" ~ km^2 ~ ")"),
                rightlabel = "Intensity Loss of SG (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
     leg_curv = c(x = 1/10, y = 5/10))
  


## ----fig.width=7, fig.height=4------------------------------------------------

netgrossplot(dataset = SL_2002_2014$lulc_Multistep,
             legendtable = SL_2002_2014$tb_legend,
             xlab = "LUC Category",
             ylab = bquote("Area (" ~ km^2 ~ ")"),
             changesLabel = c(GC = "Gross changes", NG = "Net Gain", NL = "Net Loss"),
             color = c(GC = "gray70", NG = "#006400", NL = "#EE2C2C")
             )


## ----fig.width=7, fig.height=6------------------------------------------------

chordDiagramLand(dataset = SL_2002_2014$lulc_Onestep,
                 legendtable = SL_2002_2014$tb_legend)


## ----fig.width=8, fig.height=4------------------------------------------------

sankeyLand(dataset = SL_2002_2014$lulc_Multistep,
           legendtable = SL_2002_2014$tb_legend)



## ----fig.width=8, fig.height=4------------------------------------------------

sankeyLand(dataset = SL_2002_2014$lulc_Onestep,
           legendtable = SL_2002_2014$tb_legend)


## ----fig.width=7, fig.height=4------------------------------------------------

barplotLand(dataset = SL_2002_2014$lulc_Multistep, 
          legendtable = SL_2002_2014$tb_legend,
          xlab = "Year",
          ylab = bquote("Area (" ~ km^2~ ")"),
          area_km2 = TRUE)


## -----------------------------------------------------------------------------

# testacc <- acc_changes(SaoLourencoBasin)
# 
# testacc


## ----fig.height=7, fig.width=7------------------------------------------------

# tmap_options(max.raster = c(plot = 41711112, view = 41711112))

# acc_map <- tmap::tm_shape(testacc[[1]]) +
#   tmap::tm_raster(
#     style = "cat",
#     labels = c(
#       paste0(testacc[[2]]$PxValue[1], " Change", " (", round(testacc[[2]]$Percent[1], 2), "%", ")"),
#       paste0(testacc[[2]]$PxValue[2], " Change", " (", round(testacc[[2]]$Percent[2], 2), "%", ")"),
#       paste0(testacc[[2]]$PxValue[3], " Changes", " (", round(testacc[[2]]$Percent[3], 2), "%", ")")
#     ),
#     palette = c("#757575", "#FFD700", "#CD0000"),
#     title = "Changes in the interval \n2002 - 2014"
#   ) +
#   tmap::tm_legend(
#     position = c(0.01, 0.2),
#     legend.title.size = 1.2,
#     legend.title.fontface = "bold",
#     legend.text.size = 0.8
#   ) +
#   tmap::tm_compass(type = "arrow",
#                    position = c("right", "top"),
#                    size = 3) +
#   tmap::tm_scale_bar(
#     breaks = c(seq(0, 40, 10)),
#     position = c(0.76, 0.001),
#     text.size = 0.6
#   ) +
#   tmap::tm_credits(
#     paste0(
#       "Case of Study site",
#       "\nAccumulate changes from 2002 to 2014",
#       "\nData create with OpenLand package",
#       "\nLULC derived from Embrapa Pantanal, Instituto SOS Pantanal, and WWF-Brasil 2015."
#     ),
#     size = 0.7,
#     position = c(0.01, -0, 01)
#   ) +
#   tmap::tm_graticules(
#     n.x = 6,
#     n.y = 6,
#     lines = FALSE,
#     #alpha = 0.1
#     labels.rot = c(0, 90)
#   ) +
#   tmap::tm_layout(inner.margins = c(0.02, 0.02, 0.02, 0.02))
#
#
#
#
# tmap::tmap_save(acc_map,
#                 filename = "vignettes/acc_mymap.png",
#                 width = 7,
#                 height = 7)



## ----mymap, echo=FALSE, fig.cap='Accumulated changes in pixels in the interval 2002 - 2014 at four time points (2002, 2008, 2010, 2012, 2014)', out.width='100%'----

knitr::include_graphics("acc_mymap.png")


