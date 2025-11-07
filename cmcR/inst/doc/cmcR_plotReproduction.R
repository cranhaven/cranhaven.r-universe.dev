## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>",
eval=FALSE
)

## ----setup,echo=TRUE,message=FALSE,warning=FALSE------------------------------
#  library(cmcR)
#  library(magrittr)
#  library(ggplot2)
#  library(purrr)
#  library(dplyr)
#  library(tidyr)
#  library(x3ptools)
#  library(rgl)

## -----------------------------------------------------------------------------
#  fadul1.1_id <- "DownloadMeasurement/2d9cc51f-6f66-40a0-973a-a9292dbee36d"
#  # Same source comparison
#  fadul1.2_id <- "DownloadMeasurement/cb296c98-39f5-46eb-abff-320a2f5568e8"
#  
#  # Code to download breech face impressions:
#  nbtrd_url <- "https://tsapps.nist.gov/NRBTD/Studies/CartridgeMeasurement"
#  
#  fadul1.1_raw <- read_x3p(file.path(nbtrd_url,fadul1.1_id))
#  fadul1.2_raw <- read_x3p(file.path(nbtrd_url,fadul1.2_id))

## ---- eval=FALSE--------------------------------------------------------------
#  #apply lowpass filter to reduce noise in scan:
#  surface1 <- fadul1.1_raw %>%
#    cmcR::preProcess_gaussFilter(wavelength = 16,filtertype = "lp")
#  surface2 <- fadul1.2_raw %>%
#    cmcR::preProcess_gaussFilter(wavelength = 16,filtertype = "lp")
#  
#  params <- rgl::r3dDefaults
#  
#  zoom = .7
#  size = c(300,300)
#  
#  params$windowRect <- c(40, 125, 40 + size[1], 125 + size[2])
#  params$userMatrix <- diag(c(1, 1, 1, 1))
#  params$zoom <- zoom
#  
#  #opens blank "canvas" upon which we can add lights, surfaces, etc.
#  open3d(params = params)
#  
#  #removes any previously declared lights in scene
#  rgl.pop("lights")
#  
#  #set-up two lights for scene -- a lot of experimentation possible here
#  light3d(x = -1,y = 1,z = 2,viewpoint.rel = TRUE,ambient = "white",diffuse = "white",specular = "white")
#  light3d(x = 0,y = 0,z = 10,ambient = "grey60",diffuse = "grey50",specular = "grey60",viewpoint.rel = TRUE)
#  
#  #setup surface visualization
#  multiply <- 1 #x3ptools::image_x3p default to exaggerate relief
#  z <- multiply * surface1$surface.matrix # Exaggerate the relief
#  yidx <- ncol(z):1
#  y <- fadul1.1_raw$header.info$incrementY * yidx
#  x <- fadul1.1_raw$header.info$incrementX * (1:nrow(z))
#  
#  # emission, specular, ambient affect how the surface interacts with lights --
#  # again, a lot of possible experimentation
#  surface3d(x, y, z, back = "filled",emission = "grey30",specular = "grey50",ambient = "grey10")
#  
#  x3ptools::x3p_snapshot(file = "bfScanImages/fadul1-1.png")
#  
#  rgl.close()

## ----include=FALSE,eval=FALSE-------------------------------------------------
#  #opens blank "canvas" upon which we can add lights, surfaces, etc.
#  open3d(params = params)
#  
#  #removes any previously declared lights in scene
#  rgl.pop("lights")
#  
#  #set-up two lights for scene -- a lot of experimentation possible here
#  light3d(x = -1,y = 1,z = 2,viewpoint.rel = TRUE,ambient = "white",diffuse = "white",specular = "white")
#  light3d(x = 0,y = 0,z = 10,ambient = "grey60",diffuse = "grey50",specular = "grey60",viewpoint.rel = TRUE)
#  
#  #setup surface visualization
#  multiply <- 1 #x3ptools::image_x3p default to exaggerate relief
#  z <- multiply * surface2$surface.matrix # Exaggerate the relief
#  yidx <- ncol(z):1
#  y <- fadul1.2_raw$header.info$incrementY * yidx
#  x <- fadul1.2_raw$header.info$incrementX * (1:nrow(z))
#  
#  # emission, specular, ambient affect how the surface interacts with lights --
#  # again, a lot of possible experimentation
#  surface3d(x, y, z, back = "filled",emission = "grey30",specular = "grey50",ambient = "grey10")
#  
#  x3ptools::x3p_snapshot(file = "bfScanImages/fadul1-2.png")
#  
#  rgl.close()

## -----------------------------------------------------------------------------
#  plt <- magick::image_append(c(magick::image_read("bfScanImages/fadul1-1.png"),
#                         magick::image_read("bfScanImages/fadul1-2.png")))
#  
#  # magick::image_write(path = "unprocessedScans.png",image = plt)

## ----echo=FALSE,fig.align="center",out.width=600,eval=TRUE--------------------
knitr::include_graphics("https://github.com/jzemmels/vignetteImages/blob/main/unprocessedScans.png?raw=true")

## -----------------------------------------------------------------------------
#  data("fadul1.1_processed","fadul1.2_processed")

## -----------------------------------------------------------------------------
#  #Download a non-matching cartridge case to Fadul 1-1 and Fadul 1-2
#  
#  fadul2.1_raw <- x3ptools::read_x3p("https://tsapps.nist.gov/NRBTD/Studies/CartridgeMeasurement/DownloadMeasurement/8ae0b86d-210a-41fd-ad75-8212f9522f96")
#  
#  fadul2.1_processed <- fadul2.1_raw %>%
#    preProcess_crop(region = "exterior",
#                    radiusOffset = -30) %>%
#    preProcess_crop(region = "interior",
#                    radiusOffset = 200) %>%
#    preProcess_removeTrend(statistic = "quantile",
#                                   tau = .5,
#                                   method = "fn") %>%
#    preProcess_gaussFilter() %>%
#    x3ptools::sample_x3p()

## ----include=FALSE,eval=FALSE-------------------------------------------------
#  #if we want to include more pairs in the comparison
#  fadul2.2 <- x3ptools::read_x3p("https://tsapps.nist.gov/NRBTD/Studies/CartridgeMeasurement/DownloadMeasurement/702956c6-4d7d-4cc5-be62-219b788dc7b0")

## -----------------------------------------------------------------------------
#  plt <- cmcR::x3pListPlot(x3pList = list("Fadul 1-1" = fadul1.1_processed,
#                                   "Fadul 1-2" = fadul1.2_processed,
#                                   "Fadul 2-1" = fadul2.1_processed),
#                    type = "faceted",
#                    rotate = 90,
#                    legend.quantiles = c(0,.01,.2,.5,.8,.99,1))
#  
#  # ggsave("processedScans.png",plot = plt)

## ----echo=FALSE,fig.align="center",out.width=600,eval=TRUE--------------------
knitr::include_graphics("https://github.com/jzemmels/vignetteImages/blob/main/processedScans.png?raw=true")

## -----------------------------------------------------------------------------
#  kmComparisonFeatures <- purrr::map_dfr(seq(-30,30,by = 3),
#                                         ~ comparison_allTogether(reference = fadul1.1_processed,
#                                                                  target = fadul1.2_processed,
#  
#                                                                  theta = .))
#  
#  kmComparisonFeatures_rev <- purrr::map_dfr(seq(-30,30,by = 3),
#                                             ~ comparison_allTogether(reference = fadul1.2_processed,
#                                                                      target = fadul1.1_processed,
#                                                                      theta = .))
#  
#  kmComparison_allCMCs <- kmComparisonFeatures %>%
#    mutate(originalMethodClassif = decision_CMC(cellIndex = cellIndex,
#                                                x = x,
#                                                y = y,
#                                                theta = theta,
#                                                corr = pairwiseCompCor,
#                                                xThresh = 20,
#                                                thetaThresh = 6,
#                                                corrThresh = .5),
#           highCMCClassif = decision_CMC(cellIndex = cellIndex,
#                                                x = x,
#                                                y = y,
#                                                theta = theta,
#                                                corr = pairwiseCompCor,
#                                                xThresh = 20,
#                                                thetaThresh = 6,
#                                                corrThresh = .5,
#                                                tau = 1))
#  
#  kmComparison_allCMCs_rev <- kmComparisonFeatures_rev %>%
#    mutate(originalMethodClassif = decision_CMC(cellIndex = cellIndex,
#                                                x = x,
#                                                y = y,
#                                                theta = theta,
#                                                corr = pairwiseCompCor,
#                                                xThresh = 20,
#                                                thetaThresh = 6,
#                                                corrThresh = .5),
#           highCMCClassif = decision_CMC(cellIndex = cellIndex,
#                                                x = x,
#                                                y = y,
#                                                theta = theta,
#                                                corr = pairwiseCompCor,
#                                                xThresh = 20,
#                                                thetaThresh = 6,
#                                                corrThresh = .5,
#                                                tau = 1))
#  
#  knmComparisonFeatures <- purrr::map_dfr(seq(-30,30,by = 3),
#                                         ~ comparison_allTogether(reference = fadul1.1_processed,
#                                                                  target = fadul2.1_processed,
#  
#                                                                  theta = .))
#  
#  knmComparisonFeatures_rev <- purrr::map_dfr(seq(-30,30,by = 3),
#                                             ~ comparison_allTogether(reference = fadul2.1_processed,
#                                                                      target = fadul1.1_processed,
#                                                                      theta = .))
#  
#  knmComparison_allCMCs <- knmComparisonFeatures %>%
#    mutate(originalMethodClassif = decision_CMC(cellIndex = cellIndex,
#                                                x = x,
#                                                y = y,
#                                                theta = theta,
#                                                corr = pairwiseCompCor,
#                                                xThresh = 20,
#                                                thetaThresh = 6,
#                                                corrThresh = .5),
#           highCMCClassif = decision_CMC(cellIndex = cellIndex,
#                                                x = x,
#                                                y = y,
#                                                theta = theta,
#                                                corr = pairwiseCompCor,
#                                                xThresh = 20,
#                                                thetaThresh = 6,
#                                                corrThresh = .5,
#                                                tau = 1))
#  
#  knmComparison_allCMCs_rev <- knmComparisonFeatures_rev %>%
#    mutate(originalMethodClassif = decision_CMC(cellIndex = cellIndex,
#                                                x = x,
#                                                y = y,
#                                                theta = theta,
#                                                corr = pairwiseCompCor,
#                                                xThresh = 20,
#                                                thetaThresh = 6,
#                                                corrThresh = .5),
#           highCMCClassif = decision_CMC(cellIndex = cellIndex,
#                                                x = x,
#                                                y = y,
#                                                theta = theta,
#                                                corr = pairwiseCompCor,
#                                                xThresh = 20,
#                                                thetaThresh = 6,
#                                                corrThresh = .5,
#                                                tau = 1))

## -----------------------------------------------------------------------------
#  kmCMCPlot <- cmcR::cmcPlot(reference = fadul1.1_processed,
#                              target = fadul1.2_processed,
#                              reference_v_target_CMCs = kmComparison_allCMCs,
#                              target_v_reference_CMCs = kmComparison_allCMCs_rev,
#                              type = "faceted",
#                              x3pNames = c("Fadul 1-1","Fadul 2-1"),
#                              legend.quantiles = c(0,.01,.2,.5,.8,.99,1),
#                              cell.colors = c("#a60b00","#1b03a3"),
#                              cell.alpha = .15,
#                              na.value = "gray80") %>%
#    map(~ . + theme(strip.text = element_blank()))
#  
#  kmLegend_originalCMC <- cowplot::get_legend(kmCMCPlot$originalMethodCMCs_reference_v_target +
#                                     theme(legend.direction = "horizontal"))
#  
#  km_originalCMC_reference_v_target <- kmCMCPlot$originalMethodCMCs_reference_v_target +
#    theme(legend.position = "none",
#          plot.margin=unit(c(-.05,-.5,-.05,-.5), "cm"),
#          plot.title = element_blank())
#  
#  km_originalCMC_target_v_reference <- kmCMCPlot$originalMethodCMCs_target_v_reference +
#    theme(legend.position = "none",
#          plot.margin=unit(c(-.05,-.5,-.05,-.5), "cm"),
#          plot.title = element_blank())
#  
#  km_originalCMCPlot_bothDirections <- ggplot(data.frame(a = 1)) +
#    theme_void() +
#    coord_cartesian(xlim = c(1,10),
#                    ylim = c(1,11),
#                    expand = FALSE) +
#    annotation_custom(ggplotGrob(km_originalCMC_reference_v_target),
#                      xmin = 1,xmax = 10,ymin = 6.2,ymax = 11) +
#    annotation_custom(ggplotGrob(km_originalCMC_target_v_reference),
#                      xmin = 1,xmax = 10,ymin = 2,ymax = 6.2) +
#    annotation_custom(kmLegend_originalCMC,
#                      xmin = 1,xmax = 10,ymin = 1.45,ymax = 1.45) +
#    annotate("text",x = 3.85,y = 8.15,size = 5,label = "Fadul 1-1") +
#    annotate("text",x = 3.85,y = 4,size = 5,label = "Fadul 1-1") +
#    annotate("text",x = 7.05,y = 8.15,size = 5,label = "Fadul 1-2") +
#    annotate("text",x = 7.05,y = 4,size = 5,label = "Fadul 1-2")
#  
#  # ggsave("kmOriginalmethodCMCs.png",km_originalCMCPlot_bothDirections)

## ----echo=FALSE,fig.align="center",out.width=600,eval=TRUE--------------------
knitr::include_graphics("https://github.com/jzemmels/vignetteImages/blob/main/kmOriginalmethodCMCs.png?raw=true")

## -----------------------------------------------------------------------------
#  kmLegend_highCMC <- cowplot::get_legend(kmCMCPlot$highCMC_reference_v_target +
#                                     theme(legend.direction = "horizontal"))
#  
#  km_highCMC_reference_v_target <- kmCMCPlot$highCMC_reference_v_target +
#    theme(legend.position = "none",
#          plot.margin=unit(c(-.05,-.5,-.05,-.5), "cm"),
#          plot.title = element_blank())
#  
#  km_highCMC_target_v_reference <- kmCMCPlot$highCMC_target_v_reference +
#    theme(legend.position = "none",
#          plot.margin=unit(c(-.05,-.5,-.05,-.5), "cm"),
#          plot.title = element_blank())
#  
#  km_highCMCCMCPlot_bothDirections <- ggplot(data.frame(a = 1)) +
#    theme_void() +
#    coord_cartesian(xlim = c(1,10),
#                    ylim = c(1,11),
#                    expand = FALSE) +
#    annotation_custom(ggplotGrob(km_highCMC_reference_v_target),
#                      xmin = 1.1,xmax = 10,ymin = 6.55,ymax = 11) +
#    annotation_custom(ggplotGrob(km_highCMC_target_v_reference),
#                      xmin = 1,xmax = 10,ymin = 2,ymax = 6.55) +
#    annotation_custom(kmLegend_highCMC,
#                      xmin = 1,xmax = 10,ymin = 1.45,ymax = 1.45) +
#    annotate("text",x = 3.65,y = 8.65,size = 5,label = "Fadul 1-1") +
#    annotate("text",x = 3.65,y = 4,size = 5,label = "Fadul 1-1") +
#    annotate("text",x = 7.25,y = 8.65,size = 5,label = "Fadul 1-2") +
#    annotate("text",x = 7.25,y = 4,size = 5,label = "Fadul 1-2")
#  
#  # ggsave("kmHighCMCs.png",km_highCMCCMCPlot_bothDirections)

## ----echo=FALSE,fig.align="center",out.width=600,eval=TRUE--------------------
knitr::include_graphics("https://github.com/jzemmels/vignetteImages/blob/main/kmHighCMCs.png?raw=true")

## -----------------------------------------------------------------------------
#  knmCMCPlot <- cmcR::cmcPlot(reference = fadul1.1_processed,
#                              target = fadul2.1_processed,
#                              reference_v_target_CMCs = knmComparison_allCMCs,
#                              target_v_reference_CMCs = knmComparison_allCMCs_rev,
#                              type = "faceted",
#                              x3pNames = c("Fadul 1-1","Fadul 2-1"),
#                              legend.quantiles = c(0,.01,.2,.5,.8,.99,1),
#                              cell.colors = c("#a60b00","#1b03a3"),
#                              cell.alpha = .15,
#                              na.value = "gray80") %>%
#    map(~ . + theme(strip.text = element_blank()))
#  
#  knmLegend <- cowplot::get_legend(knmCMCPlot$originalMethodCMCs_reference_v_target +
#                                     theme(legend.direction = "horizontal"))
#  
#  knm_reference_v_target <- knmCMCPlot$originalMethodCMCs_reference_v_target +
#    theme(legend.position = "none",
#          plot.margin=unit(c(-.05,-.5,-.05,-.5), "cm"),
#          plot.title = element_blank())
#  
#  knm_target_v_reference <- knmCMCPlot$originalMethodCMCs_target_v_reference +
#    theme(legend.position = "none",
#          plot.margin=unit(c(-.05,-.5,-.05,-.5), "cm"),
#          plot.title = element_blank())
#  
#  knm_cmcPlot_bothDirections <- ggplot(data.frame(a = 1)) +
#    theme_void() +
#    coord_cartesian(xlim = c(1,10),
#                    ylim = c(1,11),
#                    expand = FALSE) +
#    annotation_custom(ggplotGrob(knm_reference_v_target),
#                      xmin = 1,xmax = 10,ymin = 6.5,ymax = 11) +
#    annotation_custom(ggplotGrob(knm_target_v_reference),
#                      xmin = 1,xmax = 10,ymin = 2,ymax = 6.5) +
#    annotation_custom(knmLegend,
#                      xmin = 1,xmax = 10,ymin = 1.45,ymax = 1.45) +
#    annotate("text",x = 3.75,y = 8.65,size = 5,label = "Fadul 1-1") +
#    annotate("text",x = 3.75,y = 4,size = 5,label = "Fadul 1-1") +
#    annotate("text",x = 7.45,y = 8.65,size = 5,label = "Fadul 2-1") +
#    annotate("text",x = 7.45,y = 4,size = 5,label = "Fadul 2-1")
#  
#  # ggsave("knmOriginalMethodCMCs.png",knm_cmcPlot_bothDirections)

## ----echo=FALSE,fig.align="center",out.width=600,eval=TRUE--------------------
knitr::include_graphics("https://github.com/jzemmels/vignetteImages/blob/main/knmOriginalMethodCMCs.png?raw=true")

