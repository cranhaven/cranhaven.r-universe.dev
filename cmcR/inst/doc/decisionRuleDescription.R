## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval=FALSE
)

## ----setup,message=FALSE,warning=TRUE-----------------------------------------
#  library(cmcR)
#  library(dplyr)
#  library(ggplot2)
#  library(purrr)
#  library(tidyr)
#  library(gridExtra)

## -----------------------------------------------------------------------------
#  data("fadul1.1_processed")
#  data("fadul1.2_processed")

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

## ---- echo=FALSE, fig.align = 'center'----------------------------------------
#  plt <- cmcR::x3pListPlot(list("Fadul 1-1" = fadul1.1_processed,
#                         "Fadul 1-2" = fadul1.2_processed,
#                         "Fadul 2-1" = fadul2.1_processed))
#  
#  # ggsave("derivatives/cmcPlot.png")

## ---- echo=FALSE,eval=TRUE,out.width=600,fig.align="center"-------------------
knitr::include_graphics("https://github.com/jzemmels/vignetteImages/blob/main/cmcPlot.png?raw=true")

## ----include=FALSE------------------------------------------------------------
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

## ----include=FALSE------------------------------------------------------------
#  knmCMCPlot <- cmcR::cmcPlot(fadul1.1_processed,
#                              fadul2.1_processed,
#                              reference_v_target_CMCs = knmComparison_allCMCs,
#                              target_v_reference_CMCs = knmComparison_allCMCs_rev,
#                              type = "faceted",
#                              x3pNames = c("Fadul 1-1","Fadul 2-1"),
#                              legend.quantiles = c(0,.01,.2,.5,.8,.99,1),
#                              cell.colors = c("#a60b00","#1b03a3"),
#                              cell.alpha = .15,
#                              na.value = "gray80")
#  
#  kmCMCPlot <- cmcR::cmcPlot(fadul1.1_processed,
#                             fadul1.2_processed,
#                             reference_v_target_CMCs = kmComparison_allCMCs,
#                             target_v_reference_CMCs = knmComparison_allCMCs_rev,
#                             x3pNames = c("Fadul 1-1","Fadul 1-2"),
#                             legend.quantiles = c(0,.01,.2,.5,.8,.99,1),
#                             cell.colors = c("#a60b00","#1b03a3"),
#                             cell.alpha = .15,
#                             na.value = "gray80")
#  
#  knmCMCPlot_list_comparison1to2 <- cmcR::cmcPlot(fadul1.1_processed,
#                                                  fadul2.1_processed,
#                                                  reference_v_target_CMCs = knmComparison_allCMCs,
#                                                  target_v_reference_CMCs = knmComparison_allCMCs_rev,
#                                                  type = "list",
#                                                  x3pNames = c("Fadul 1-1","Fadul 2-1"),
#                                                  legend.quantiles = c(0,.01,.2,.5,.8,.99,1),
#                                                  cell.colors = c("black","black"),
#                                                  cell.alpha = .15,
#                                                  na.value = "gray90")
#  
#  kmCMCPlot_list_comparison1to2 <- cmcR::cmcPlot(fadul1.1_processed,
#                                                 fadul1.2_processed,
#                                                 reference_v_target_CMCs = kmComparison_allCMCs,
#                                                 target_v_reference_CMCs = knmComparison_allCMCs_rev,
#                                                 x3pNames = c("Fadul 1-1","Fadul 1-2"),
#                                                 legend.quantiles = c(0,.01,.2,.5,.8,.99,1),
#                                                 cell.colors = c("black","black"),
#                                                 cell.alpha = .15,
#                                                 na.value = "gray90",
#                                                 type = "list")

## ----include=FALSE------------------------------------------------------------
#  kmComparison_xData <- kmComparisonFeatures %>%
#    mutate(direction = "comparison_1to2") %>%
#    group_by(cellIndex) %>%
#    filter(pairwiseCompCor == max(pairwiseCompCor)) %>%
#    ungroup() %>%
#    mutate(med_x = median(x),
#           med_y = median(y)) %>%
#    mutate(x_threshMinus = med_x - 20,
#           x_threshPlus = med_x + 20,
#           y_threshMinus = med_y - 20,
#           y_threshPlus = med_y + 20) %>%
#    mutate(Classification = factor(ifelse(abs(x - med_x) <= 20,"Congruent","Not Congruent"),
#                                   levels = c("Not Congruent","Congruent")))
#  
#  kmComparison_yData <- kmComparisonFeatures %>%
#    mutate(direction = "comparison_1to2") %>%
#    group_by(cellIndex) %>%
#    filter(pairwiseCompCor == max(pairwiseCompCor)) %>%
#    ungroup() %>%
#    mutate(med_x = median(x),
#           med_y = median(y)) %>%
#    mutate(x_threshMinus = med_x - 20,
#           x_threshPlus = med_x + 20,
#           y_threshMinus = med_y - 20,
#           y_threshPlus = med_y + 20) %>%
#    mutate(Classification = factor(ifelse(abs(y - med_y) <= 20,"Congruent","Not Congruent"),
#                                   levels = c("Not Congruent","Congruent")))
#  
#  kmComparison_pairwiseCompCorData <- kmComparisonFeatures %>%
#    mutate(direction = "comparison_1to2") %>%
#    group_by(cellIndex) %>%
#    filter(pairwiseCompCor == max(pairwiseCompCor)) %>%
#    ungroup() %>%
#    mutate(pairwiseCompCor_thresh = .5) %>%
#    mutate(Classification = factor(ifelse(pairwiseCompCor >= .5,"Congruent","Not Congruent"),
#                                   levels = c("Not Congruent","Congruent")))
#  
#  kmComparison_thetaData <- kmComparisonFeatures %>%
#    mutate(direction = "comparison_1to2") %>%
#    group_by(cellIndex) %>%
#    filter(pairwiseCompCor == max(pairwiseCompCor)) %>%
#    ungroup() %>%
#    mutate(med_theta = median(theta)) %>%
#    mutate(theta_threshMinus = med_theta - 6,
#           theta_threshPlus = med_theta + 6) %>%
#    mutate(Classification = ifelse(abs(theta - med_theta) <= 6, "Congruent","Not Congruent")) %>%
#    mutate(Classification = factor(Classification,levels = c("Not Congruent","Congruent")))
#  
#  knmComparison_xData <- knmComparisonFeatures %>%
#    mutate(direction = "comparison_1to2") %>%
#    group_by(cellIndex) %>%
#    filter(pairwiseCompCor == max(pairwiseCompCor)) %>%
#    ungroup() %>%
#    mutate(med_x = median(x),
#           med_y = median(y)) %>%
#    mutate(x_threshMinus = med_x - 20,
#           x_threshPlus = med_x + 20,
#           y_threshMinus = med_y - 20,
#           y_threshPlus = med_y + 20) %>%
#    mutate(Classification = factor(ifelse(abs(x - med_x) <= 20,"Congruent","Not Congruent"),
#                                   levels = c("Not Congruent","Congruent")))
#  
#  knmComparison_yData <- knmComparisonFeatures %>%
#    mutate(direction = "comparison_1to2") %>%
#    group_by(cellIndex) %>%
#    filter(pairwiseCompCor == max(pairwiseCompCor)) %>%
#    ungroup() %>%
#    mutate(med_x = median(x),
#           med_y = median(y)) %>%
#    mutate(x_threshMinus = med_x - 20,
#           x_threshPlus = med_x + 20,
#           y_threshMinus = med_y - 20,
#           y_threshPlus = med_y + 20) %>%
#    mutate(Classification = factor(ifelse(abs(y - med_y) <= 20,"Congruent","Not Congruent"),
#                                   levels = c("Not Congruent","Congruent")))
#  
#  knmComparison_pairwiseCompCorData <- knmComparisonFeatures %>%
#    mutate(direction = "comparison_1to2") %>%
#    group_by(cellIndex) %>%
#    filter(pairwiseCompCor == max(pairwiseCompCor)) %>%
#    ungroup() %>%
#    mutate(pairwiseCompCor_thresh = .5) %>%
#    mutate(Classification = factor(ifelse(pairwiseCompCor >= .5,"Congruent","Not Congruent"),
#                                   levels = c("Not Congruent","Congruent")))
#  
#  knmComparison_thetaData <- knmComparisonFeatures %>%
#    mutate(direction = "comparison_1to2") %>%
#    group_by(cellIndex) %>%
#    filter(pairwiseCompCor == max(pairwiseCompCor)) %>%
#    ungroup() %>%
#    mutate(med_theta = median(theta)) %>%
#    mutate(theta_threshMinus = med_theta - 6,
#           theta_threshPlus = med_theta + 6) %>%
#    mutate(Classification = ifelse(abs(theta - med_theta) <= 6, "Congruent","Not Congruent")) %>%
#    mutate(Classification = factor(Classification,levels = c("Not Congruent","Congruent")))

## ----include=FALSE,fig.height=1-----------------------------------------------
#  kmComparison_xData_plot <- kmComparison_xData %>%
#    ggplot() +
#    geom_histogram(aes(x = x,
#                       fill = Classification),
#                   alpha = .7,
#                   binwidth = 1
#                   ) +
#    scale_fill_manual(values = c("#a60b00","#1b03a3")) +
#    geom_ribbon(mapping = aes(y = seq(0,3.5,length.out = 27),
#                              xmin = med_x - 20.5,
#                              xmax = med_x + 20.5),
#                fill = "#7570b3",
#                alpha = .3) +
#    theme_bw() +
#    scale_x_continuous(expression(paste("Estimated horizontal translation ",Delta,"x")),
#                       breaks = seq(-100,100,by = 50),
#                       limits = c(-101,101),
#                       oob = scales::oob_keep
#                       ) +
#    scale_y_continuous(limits = c(0,2.5)) +
#    ylab("# Cell Pairs") +
#    theme(legend.position = c(1, 1),
#          legend.justification = c(1, 1),
#          legend.direction = "horizontal",
#          legend.background = element_blank(),
#          legend.text = element_text(size = 7),
#          legend.title = element_text(size = 7),
#          axis.text = element_text(size = 4),
#          axis.title.x = element_text(size = 6),
#          axis.title.y = element_text(size = 4),
#          plot.margin=unit(c(0,.1,0,.1), "cm"),
#          legend.key.size = unit(0.3, "cm"))
#  
#  pltLegend_horizontal <- cowplot::get_legend(kmComparison_xData_plot)
#  
#  kmComparison_xData_plot <- kmComparison_xData_plot +
#    theme(legend.position = c(1, 1),
#          legend.justification = c(1, 1),
#          legend.direction = "vertical",
#          legend.background = element_blank(),
#          legend.text = element_text(size = 7),
#          legend.title = element_blank(),
#          axis.text = element_text(size = 4),
#          axis.title.x = element_text(size = 6),
#          axis.title.y = element_text(size = 4),
#          plot.margin=unit(c(0,.1,0,.1), "cm"),
#          legend.key.size = unit(0.3, "cm"))
#  
#  pltLegend_vertical_leftAligned <- cowplot::get_legend(kmComparison_xData_plot)
#  
#  kmComparison_xData_plot <- kmComparison_xData_plot +
#    theme(legend.position = c(1, 1),
#          legend.justification = c(1, 1),
#          legend.direction = "vertical",
#          legend.background = element_blank(),
#          legend.text = element_text(size = 7),
#          legend.title = element_blank(),
#          axis.text = element_text(size = 4),
#          axis.title.x = element_text(size = 6),
#          axis.title.y = element_text(size = 4),
#          plot.margin=unit(c(0,.1,0,.1), "cm"),
#          legend.key.size = unit(0.3, "cm")) +
#    guides(fill = guide_legend(label.position = "left",label.hjust = 1))
#  
#  pltLegend_vertical_rightAligned <- cowplot::get_legend(kmComparison_xData_plot)
#  
#  kmComparison_xData_plot <- kmComparison_xData_plot +
#    theme(legend.position = "none")
#  
#  knmComparison_xData_plot <- knmComparison_xData %>%
#    ggplot() +
#    geom_histogram(aes(x = x,
#                       fill = Classification),
#                   alpha = .7,
#                   binwidth = 1
#                   ) +
#    scale_fill_manual(values = c("#a60b00","#1b03a3")) +
#    geom_ribbon(mapping = aes(y = seq(0,3.5,length.out = 29),
#                              xmin = med_x - 20,
#                              xmax = med_x + 20),
#                fill = "#7570b3",
#                alpha = .3) +
#    theme_bw() +
#    scale_x_continuous(expression(paste("Estimated horizontal translation ",Delta,"x")),
#                       breaks = seq(-100,100,by = 50),
#                       limits = c(-101,101),na.value = 0
#                       ) +
#    scale_y_continuous(limits = c(0,2.5)) +
#    ylab("# Cell Pairs") +
#    theme(legend.position = "none",
#          axis.text = element_text(size = 4),
#          axis.title.x = element_text(size = 6), axis.title.y = element_text(size = 4)
#          ,plot.margin=unit(c(0,.1,0,.1), "cm")
#          )
#  
#  kmComparison_yData_plot <- kmComparison_yData %>%
#    ggplot() +
#    geom_histogram(aes(x = y,
#                       fill = Classification),
#                   alpha = .7,
#                   binwidth = 1
#                   ) +
#    scale_fill_manual(values = c("#a60b00","#1b03a3")) +
#    geom_ribbon(mapping = aes(y = seq(0,3.5,length.out = 27),
#                              xmin = med_y - 20,
#                              xmax = med_y + 20),
#                fill = "#7570b3",
#                alpha = .3) +
#    theme_bw() +
#    scale_x_continuous(expression(paste("Estimated horizontal translation ",Delta,"y")),
#                       breaks = seq(-100,100,by = 50),
#                       limits = c(-101,101),na.value = 0
#                       ) +
#    scale_y_continuous(limits = c(0,2.5)) +
#    ylab("# Cell Pairs") +
#    theme(legend.position = "none",
#          axis.text = element_text(size = 4),
#          axis.title.x = element_text(size = 6), axis.title.y = element_text(size = 4)
#          ,plot.margin=unit(c(0,.1,0,.1), "cm")
#          )
#  
#  knmComparison_yData_plot <- knmComparison_yData %>%
#    ggplot() +
#    geom_histogram(aes(x = y,
#                       fill = Classification),
#                   alpha = .7,
#                   binwidth = 1
#                   ) +
#    scale_fill_manual(values = c("#a60b00","#1b03a3")) +
#    geom_ribbon(mapping = aes(y = seq(0,3.5,length.out = 29),
#                              xmin = med_y - 20.5,
#                              xmax = med_y + 20.5),
#                fill = "#7570b3",
#                alpha = .3) +
#    theme_bw() +
#    scale_x_continuous(expression(paste("Estimated horizontal translation ",Delta,"y")),
#                       breaks = seq(-100,100,by = 50),
#                       limits = c(-101,101),oob = scales::squish
#                       ) +
#    scale_y_continuous(limits = c(0,2.5)) +
#    ylab("# Cell Pairs") +
#    theme(legend.position = "none",
#          axis.text = element_text(size = 4),
#          axis.title.x = element_text(size = 6), axis.title.y = element_text(size = 4)
#          ,plot.margin=unit(c(0,.1,0,.1), "cm")
#          )
#  
#  kmComparison_thetaData_plot <- kmComparison_thetaData %>%
#    ggplot() +
#    geom_bar(aes(x = theta,
#                 fill = Classification),
#             alpha = .7) +
#    scale_fill_manual(values = c("#a60b00","#1b03a3")) +
#    geom_ribbon(aes(y = seq(0,12,length.out = 27),
#                    xmin = med_theta - 7.5,
#                    xmax = med_theta + 7.5),
#                fill = "#7570b3",
#                alpha = .3) +
#    theme_bw() +
#    scale_x_continuous(expression(paste("Estimated rotation angle ", theta)),
#                       breaks = seq(-30,30,by = 15),
#                       limits = c(-32,32)) +
#    scale_y_continuous(limits = c(0,7),
#                       breaks = seq(0,7,by = 1)) +
#    ylab("# Cell Pairs") +
#    theme(legend.position = "none",
#          axis.text = element_text(size = 4),
#          axis.title.x = element_text(size = 6), axis.title.y = element_text(size = 4)
#          ,plot.margin=unit(c(0,.1,0,.1), "cm")
#          )
#  
#  knmComparison_thetaData_plot <- knmComparison_thetaData %>%
#    ggplot() +
#    geom_bar(aes(x = theta,
#                 fill = Classification),
#             alpha = .7) +
#    scale_fill_manual(values = c("#a60b00","#1b03a3")) +
#    geom_ribbon(aes(y = seq(0,12,length.out = 29),
#                    xmin = med_theta - 6,
#                    xmax = med_theta + 6),
#                fill = "#7570b3",
#                alpha = .3) +
#    theme_bw() +
#    scale_x_continuous(expression(paste("Estimated rotation angle ", theta)),
#                       breaks = seq(-30,30,by = 15),
#                       limits = c(-32,32)) +
#    scale_y_continuous(limits = c(0,7),
#                       breaks = seq(0,7,by = 1)) +
#    ylab("# Cell Pairs") +
#    theme(legend.position = "none",
#          axis.text = element_text(size = 4),
#          axis.title.x = element_text(size = 6), axis.title.y = element_text(size = 4)
#          ,plot.margin=unit(c(0,.1,0,.1), "cm")
#          )
#  
#  kmComparison_pairwiseCompCorData_plot <- kmComparison_pairwiseCompCorData %>%
#    ggplot(aes(x = pairwiseCompCor,fill = Classification)) +
#    geom_histogram( binwidth = .01) +
#    scale_fill_manual(values = c("#a60b00","#1b03a3")) +
#    geom_vline(aes(xintercept = pairwiseCompCor_thresh),
#               colour = "#7570b3") +
#    geom_ribbon(mapping = aes(y = seq(-1,4,length.out = 27),
#                              xmin = pairwiseCompCor_thresh,
#                              xmax = 1),
#                fill = "#7570b3",
#                alpha = .3) +
#    theme_bw() +
#    scale_x_continuous(expression(paste("Estimated CCF"[max])),
#                       breaks = seq(0,1,by = .5)) +
#    coord_cartesian(xlim = c(0,1),
#                    ylim = c(0,3.5),
#                    expand = FALSE) +
#    ylab("# Cell Pairs") +
#    theme(legend.position = "none",
#          axis.text = element_text(size = 4),
#          axis.title.x = element_text(size = 6), axis.title.y = element_text(size = 4)
#          ,plot.margin=unit(c(0,.1,0,.1), "cm")
#          )
#  
#  knmComparison_pairwiseCompCorData_plot <- knmComparison_pairwiseCompCorData %>%
#    ggplot(aes(x = pairwiseCompCor,fill = Classification)) +
#    geom_histogram(binwidth = .01) +
#    scale_fill_manual(values = c("#a60b00","#1b03a3")) +
#    geom_vline(aes(xintercept = pairwiseCompCor_thresh),
#               colour = "#7570b3") +
#    geom_ribbon(mapping = aes(y = seq(0,4,length.out = 29),
#                              xmin = pairwiseCompCor_thresh,
#                              xmax = 1),
#                fill = "#7570b3",
#                alpha = .3) +
#    theme_bw() +
#    scale_x_continuous(expression(paste("Estimated CCF"[max])),
#                       breaks = seq(0,1,by = .5)) +
#    coord_cartesian(xlim = c(0,1),
#                    ylim = c(0,3.5),
#                    expand = FALSE) +
#    ylab("# Cell Pairs") +
#    theme(legend.position = "none",
#          axis.text = element_text(size = 4),
#          axis.title.x = element_text(size = 6), axis.title.y = element_text(size = 4)
#          ,plot.margin=unit(c(0,.1,0,.1), "cm")
#          )

## ----include=FALSE------------------------------------------------------------
#  kmCMCPlot_list_comparison1to2$originalMethodCMCs_reference_v_target$`Fadul 1-1`$layers <- kmCMCPlot_list_comparison1to2$originalMethodCMCs_reference_v_target$`Fadul 1-1`$layers[-3]
#  kmCMCPlot_list_comparison1to2$originalMethodCMCs_reference_v_target$`Fadul 1-2`$layers <- kmCMCPlot_list_comparison1to2$originalMethodCMCs_reference_v_target$`Fadul 1-2`$layers[-3]
#  knmCMCPlot_list_comparison1to2$originalMethodCMCs_reference_v_target$`Fadul 2-1`$layers <- knmCMCPlot_list_comparison1to2$originalMethodCMCs_reference_v_target$`Fadul 2-1`$layers[-3]
#  
#  kmCMCPlot_list_comparison1to2$originalMethodCMCs_reference_v_target$`Fadul 1-1` <- kmCMCPlot_list_comparison1to2$originalMethodCMCs_reference_v_target$`Fadul 1-1` +
#    ggplot2::theme(legend.position = "none",
#                   plot.margin=unit(c(0,-.5,0,-.5), "cm"),
#                   plot.title = element_blank()) +
#    coord_fixed(xlim = c(-400,3600), ylim = c(-100,3800)) +
#    annotate(geom = "text",x = 3300/2,y = 3100/2,label = "Fadul 1-1",size = 4)
#  
#  kmCMCPlot_list_comparison1to2$originalMethodCMCs_reference_v_target$`Fadul 1-2` <- kmCMCPlot_list_comparison1to2$originalMethodCMCs_reference_v_target$`Fadul 1-2` +
#    ggplot2::theme(legend.position = "none",
#                   plot.margin=unit(c(0,-.5,0,-.5), "cm"),
#                   plot.title = element_blank()) +
#    coord_fixed(xlim = c(-400,3600), ylim = c(-100,3800)) +
#    annotate(geom = "text",x = 3300/2,y = 3100/2,label = "Fadul 1-2",size = 4)
#  
#  knmCMCPlot_list_comparison1to2$originalMethodCMCs_reference_v_target$`Fadul 2-1` <- knmCMCPlot_list_comparison1to2$originalMethodCMCs_reference_v_target$`Fadul 2-1` +
#    ggplot2::theme(legend.position = "none",
#                   plot.margin=unit(c(0,-.5,0,-.5), "cm"),
#                   plot.title = element_blank()) +
#    coord_fixed(xlim = c(-400,3600), ylim = c(-100,3800)) +
#    annotate(geom = "text",x = 3300/2,y = 3100/2,label = "Fadul 2-1",size = 4)

## ----warning=TRUE,message=FALSE,echo=FALSE,fig.width = 8, fig.align = 'center'----
#  horizontalTranslation_label <- ggplot() +
#    annotate("text",x = 1,y = 1,size = 2.5,label = "  Horizontal \n Translation") +
#    theme_void()
#  
#  verticalTranslation_label <- ggplot() +
#    annotate("text",x = 1,y = 1,size = 2.5,label = "  Vertical \n Translation") +
#    theme_void()
#  
#  rotation_label <- ggplot() +
#    annotate("text",x = 1,y = 1,size = 2.5,label = "Rotation") +
#    theme_void()
#  
#  pairwiseCompCor_label <- ggplot() +
#    annotate("text",x = 1,y = 1,size = 2.5,label = expression(paste("CCF"[max]))) +
#    theme_void()
#  
#  blank_label <- ggplot() +
#    annotate("text",x = 1,y = 1,size = 2,label = " ") +
#    theme_void()
#  
#  # Some internal call to is.na within grid.arrange is throwing a warning for the
#  # pairwiseCompCor_label since it contains an expression. Since this isn't
#  # anything wrong with vignette, we'll just suppress it
#  suppressWarnings({
#    plt <- gridExtra::arrangeGrob(kmCMCPlot_list_comparison1to2$originalMethodCMCs_reference_v_target$`Fadul 1-1`,
#                                   kmCMCPlot_list_comparison1to2$originalMethodCMCs_reference_v_target$`Fadul 1-2`,
#                                   knmCMCPlot_list_comparison1to2$originalMethodCMCs_reference_v_target$`Fadul 2-1`,
#                                   horizontalTranslation_label,
#                                   kmComparison_xData_plot,
#                                   knmComparison_xData_plot,
#                                   verticalTranslation_label,
#                                   kmComparison_yData_plot,
#                                   knmComparison_yData_plot,
#                                   rotation_label,
#                                   kmComparison_thetaData_plot,
#                                   knmComparison_thetaData_plot,
#                                   pairwiseCompCor_label,
#                                   kmComparison_pairwiseCompCorData_plot,
#                                   knmComparison_pairwiseCompCorData_plot,
#                                   blank_label,
#                                   blank_label,
#                                   pltLegend_horizontal,
#                                   heights = unit(c(7,4,4,4,4,2),units = "null"),
#                                   widths = unit(c(1,1.5,1.5),units = "null"))
#  
#    # ggsave("derivatives/originalMethodResults.png",plt)
#  })

## ---- echo=FALSE,eval=TRUE,out.width=600,fig.align="center"-------------------
knitr::include_graphics("https://github.com/jzemmels/vignetteImages/blob/main/originalMethodResults.png?raw=true")

## ----include=FALSE------------------------------------------------------------
#  #PC plots look best when the values are rescaled to be between 0 and 1. We want
#  #to rescale x, y, and theta, but not the CCF (which is already between 0 and 1,
#  #effectively). This code the rescaling desired
#  
#  medianData_comparison1to2 <- kmComparisonFeatures %>%
#    mutate(direction = "comparison_1to2") %>%
#    group_by(cellIndex) %>%
#    filter(pairwiseCompCor == max(pairwiseCompCor)) %>%
#    ungroup() %>%
#    select(-c(cellIndex,fft_ccf,pairwiseCompCor)) %>%
#    summarise(x = median(x),
#              y = median(y),
#              theta = median(theta),
#              pairwiseCompCor = .75,
#              cellNum = 100)
#  
#  yminData_comparison1to2 <- medianData_comparison1to2 %>%
#    mutate(x = x - 20,
#           y = y - 20,
#           theta = theta - 6,
#           pairwiseCompCor = .5,
#           cellNum = 99)
#  
#  ymaxData_comparison1to2 <- medianData_comparison1to2 %>%
#    mutate(x = x + 20,
#           y = y + 20,
#           theta = theta + 6,
#           pairwiseCompCor = 1,
#           cellNum = 101)
#  
#  congruentData_comparison1to2 <- kmComparisonFeatures %>%
#    mutate(direction = "comparison_1to2") %>%
#    group_by(cellIndex) %>%
#    filter(pairwiseCompCor == max(pairwiseCompCor)) %>%
#    ungroup() %>%
#    select(-c(cellIndex,fft_ccf,pairwiseCompCor)) %>%
#    mutate(cellNum = 1:nrow(.)) %>%
#    bind_rows(yminData_comparison1to2,
#              medianData_comparison1to2,
#              ymaxData_comparison1to2) %>%
#    select(-pairwiseCompCor) %>%
#    pivot_longer(cols = c(x,y,theta),
#                 names_to = "parameter") %>%
#    group_by(parameter) %>%
#    mutate(value = scales::rescale(value)) %>%
#    bind_rows(kmComparisonFeatures %>%
#                mutate(direction = "comparison_1to2") %>%
#                group_by(cellIndex) %>%
#                filter(pairwiseCompCor == max(pairwiseCompCor)) %>%
#                ungroup() %>%
#                select(-c(cellIndex,fft_ccf)) %>%
#                mutate(cellNum = 1:nrow(.)) %>%
#                bind_rows(yminData_comparison1to2,
#                          medianData_comparison1to2,
#                          ymaxData_comparison1to2) %>%
#                select(-c(x,y,theta)) %>%
#                mutate(parameter = "pairwiseCompCor") %>%
#                rename(value = pairwiseCompCor)) %>%
#    arrange(cellNum)
#  
#  extremaData_comparison1to2 <- congruentData_comparison1to2 %>%
#    filter(cellNum %in% c(99,100,101)) %>%
#    ungroup()  %>%
#    select(-c(cellNum)) %>%
#    mutate(name = rep(c("ymin","ymed","ymax"),each = 4)) %>%
#    pivot_wider(id_cols = parameter,
#                names_from = name,
#                values_from = value)
#  
#  medianData_comparison2to1 <- kmComparisonFeatures_rev %>%
#    mutate(direction = "comparison_2to1") %>%
#    group_by(cellIndex) %>%
#    filter(pairwiseCompCor == max(pairwiseCompCor)) %>%
#    ungroup() %>%
#    select(-c(cellIndex,fft_ccf,pairwiseCompCor)) %>%
#    summarise(x = median(x),
#              y = median(y),
#              theta = median(theta),
#              pairwiseCompCor = .75,
#                          cellNum = 100)
#  
#  yminData_comparison2to1 <- medianData_comparison2to1 %>%
#    mutate(x = x - 20,
#           y = y - 20,
#           theta = theta - 6,
#           pairwiseCompCor = .5,
#           cellNum = 99)
#  
#  ymaxData_comparison2to1 <- medianData_comparison2to1 %>%
#    mutate(x = x + 20,
#           y = y + 20,
#           theta = theta + 6,
#           pairwiseCompCor = 1,
#           cellNum = 101)
#  
#  congruentData_comparison2to1 <- kmComparisonFeatures_rev %>%
#    mutate(direction = "comparison_2to1") %>%
#    group_by(cellIndex) %>%
#    filter(pairwiseCompCor == max(pairwiseCompCor)) %>%
#    ungroup() %>%
#    select(-c(cellIndex,fft_ccf,pairwiseCompCor)) %>%
#    mutate(cellNum = 1:nrow(.)) %>%
#    bind_rows(yminData_comparison2to1,
#              medianData_comparison2to1,
#              ymaxData_comparison2to1) %>%
#    select(-pairwiseCompCor) %>%
#    pivot_longer(cols = c(x,y,theta),
#                 names_to = "parameter") %>%
#    group_by(parameter) %>%
#    mutate(value = scales::rescale(value)) %>%
#    bind_rows(kmComparisonFeatures %>%
#                mutate(direction = "comparison_2to1") %>%
#                group_by(cellIndex) %>%
#                filter(pairwiseCompCor == max(pairwiseCompCor)) %>%
#                ungroup() %>%
#                select(-c(cellIndex,fft_ccf)) %>%
#                mutate(cellNum = 1:nrow(.)) %>%
#                bind_rows(yminData_comparison2to1,
#                          medianData_comparison2to1,
#                          ymaxData_comparison2to1) %>%
#                select(-c(x,y,theta)) %>%
#                mutate(parameter = "pairwiseCompCor") %>%
#                rename(value = pairwiseCompCor)) %>%
#    arrange(cellNum)
#  
#  extremaData_comparison2to1 <- congruentData_comparison2to1 %>%
#    filter(cellNum %in% c(99,100,101)) %>%
#    ungroup()  %>%
#    select(-c(cellNum)) %>%
#    mutate(name = rep(c("ymin","ymed","ymax"),each = 4)) %>%
#    pivot_wider(id_cols = parameter,
#                names_from = name,
#                values_from = value)

## ---- include=FALSE-----------------------------------------------------------
#  originalMethod_comparison1to2_pcp <- congruentData_comparison1to2 %>%
#     filter(cellNum < 90) %>%
#    ungroup() %>%
#    pivot_wider(id_cols = c(cellNum,parameter),
#                names_from = parameter,
#                values_from = value) %>%
#    mutate(Classification = case_when(x >= extremaData_comparison1to2[1,"ymin"][[1]] & x <= extremaData_comparison1to2[1,"ymax"][[1]] &
#                                        y >= extremaData_comparison1to2[2,"ymin"][[1]] & y <= extremaData_comparison1to2[2,"ymax"][[1]] &
#                                        theta >= extremaData_comparison1to2[3,"ymin"][[1]] & theta <= extremaData_comparison1to2[3,"ymax"][[1]] &
#                                        pairwiseCompCor >= extremaData_comparison1to2[4,"ymin"][[1]] & pairwiseCompCor <= extremaData_comparison1to2[4,"ymax"][[1]] ~ "Congruent",
#                                      TRUE ~ "Not Congruent"))  %>%
#    pivot_longer(cols = c(x,y,theta,pairwiseCompCor),
#                 names_to = "parameter") %>%
#    mutate(parameter = factor(parameter,c("x","y","theta","pairwiseCompCor")),
#           Classification = factor(Classification,levels = c("Congruent","Not Congruent"))) %>%
#    ggplot() +
#    scale_colour_manual(values = c("#1b03a3","#a60b00")) +
#    geom_line(aes(x = parameter,y = value,group = cellNum,colour = Classification),
#              size = .1) +
#    geom_tile(data = extremaData_comparison1to2,
#              aes(x = parameter,
#                  y = ymed,
#                  width = .25,
#                  height = ymax - ymin),
#              fill = "#7570b3",
#              colour = "black",
#              alpha = .4) +
#    theme_bw() +
#    theme(axis.text.y = element_blank(),
#          axis.title.y = element_blank(),
#          axis.title.x = element_blank(),
#          axis.ticks = element_blank(),
#          legend.position = "none",
#          plot.margin = unit(c(.55,0,0,0), "cm")) +
#    scale_x_discrete(labels = c(expression(paste(Delta, "x")),
#                                expression(paste(Delta, "y")),
#                                expression(theta),
#                                expression("CCF"[max])))
#  
#  originalMethod_comparison2to1_pcp <- congruentData_comparison2to1 %>%
#    filter(cellNum < 90) %>%
#    ungroup() %>%
#    pivot_wider(id_cols = c(cellNum,parameter),
#                names_from = parameter,
#                values_from = value) %>%
#    mutate(Classification = case_when(x >= extremaData_comparison2to1[1,"ymin"][[1]] & x <= extremaData_comparison2to1[1,"ymax"][[1]] &
#                                        y >= extremaData_comparison2to1[2,"ymin"][[1]] & y <= extremaData_comparison2to1[2,"ymax"][[1]] &
#                                        theta >= extremaData_comparison2to1[3,"ymin"][[1]] & theta <= extremaData_comparison2to1[3,"ymax"][[1]] &
#                                        pairwiseCompCor >= extremaData_comparison2to1[4,"ymin"][[1]] & pairwiseCompCor <= extremaData_comparison2to1[4,"ymax"][[1]] ~ "Congruent",
#                                      TRUE ~ "Not Congruent"))  %>%
#    pivot_longer(cols = c(x,y,theta,pairwiseCompCor),
#                 names_to = "parameter") %>%
#    mutate(parameter = factor(parameter,c("x","y","theta","pairwiseCompCor")),
#           Classification = factor(Classification,levels = c("Congruent","Not Congruent"))) %>%
#    ggplot() +
#    scale_colour_manual(values = c("#1b03a3","#a60b00")) +
#    geom_line(aes(x = parameter,y = value,group = cellNum,colour = Classification),
#              size = .1) +
#    geom_tile(data = extremaData_comparison2to1,
#              aes(x = parameter,
#                  y = ymed,
#                  width = .25,
#                  height = ymax - ymin),
#              fill = "#7570b3",
#              colour = "black",
#              alpha = .4) +
#    theme_bw() +
#    theme(axis.text.y = element_blank(),
#          axis.title.y = element_blank(),
#          axis.title.x = element_blank(),
#          axis.ticks = element_blank(),
#          legend.position = "none",
#          plot.margin = unit(c(.55,0,0,0), "cm")) +
#    scale_x_discrete(labels = c(expression(paste(Delta, "x")),
#                                expression(paste(Delta, "y")),
#                                expression(theta),
#                                expression("CCF"[max])))

## ---- include=FALSE-----------------------------------------------------------
#  kmCMCPlot_list_comparison1to2$originalMethodCMCs_reference_v_target$`Fadul 1-1` <- kmCMCPlot_list_comparison1to2$originalMethodCMCs_reference_v_target$`Fadul 1-1` +
#    ggplot2::theme(plot.margin=unit(c(0,-.5,0,-.5), "cm"))
#  
#  kmCMCPlot_list_comparison1to2$originalMethodCMCs_reference_v_target$`Fadul 1-2` <- kmCMCPlot_list_comparison1to2$originalMethodCMCs_reference_v_target$`Fadul 1-2` +
#    ggplot2::theme(plot.margin=unit(c(0,-.5,0,-.5), "cm"))
#  
#  knmCMCPlot_list_comparison1to2$originalMethodCMCs_reference_v_target$`Fadul 2-1` <- knmCMCPlot_list_comparison1to2$originalMethodCMCs_reference_v_target$`Fadul 2-1` +
#    ggplot2::theme(plot.margin=unit(c(0,-.5,0,-.5), "cm"))
#  
#  knmCMCPlot_list_comparison2to1 <- cmcR::cmcPlot(fadul2.1_processed,
#                                                  fadul1.1_processed,
#                                                  reference_v_target_CMCs = knmComparison_allCMCs_rev,
#                                                  target_v_reference_CMCs = knmComparison_allCMCs,
#                                                  type = "list",
#                                                  x3pNames = c("Fadul 2-1","Fadul 1-1"),
#                                                  legend.quantiles = c(0,.01,.2,.5,.8,.99,1),
#                                                  cell.alpha = .15,
#                                                  na.value = "gray90")
#  
#  kmCMCPlot_list_comparison2to1 <- cmcR::cmcPlot(fadul1.2_processed,
#                                                 fadul1.1_processed,
#                                                 reference_v_target_CMCs = kmComparison_allCMCs_rev,
#                                                 target_v_reference_CMCs = kmComparison_allCMCs,
#                                                 x3pNames = c("Fadul 1-1","Fadul 1-2"),
#                                                 legend.quantiles = c(0,.01,.2,.5,.8,.99,1),
#                                                 cell.colors = c("black","black"),
#                                                 cell.alpha = .15,
#                                                 na.value = "gray90",
#                                                 type = "list")
#  
#  kmCMCPlot_list_comparison1to2$originalMethodCMCs_target_v_reference$`Fadul 1-1`$layers <- kmCMCPlot_list_comparison1to2$originalMethodCMCs_target_v_reference$`Fadul 1-1`$layers[-3]
#  kmCMCPlot_list_comparison1to2$originalMethodCMCs_target_v_reference$`Fadul 1-2`$layers <- kmCMCPlot_list_comparison1to2$originalMethodCMCs_target_v_reference$`Fadul 1-2`$layers[-3]
#  knmCMCPlot_list_comparison1to2$originalMethodCMCs_target_v_reference$`Fadul 2-1`$layers <- knmCMCPlot_list_comparison1to2$originalMethodCMCs_target_v_reference$`Fadul 2-1`$layers[-3]
#  
#  kmCMCPlot_list_comparison1to2$originalMethodCMCs_target_v_reference$`Fadul 1-1` <- kmCMCPlot_list_comparison1to2$originalMethodCMCs_target_v_reference$`Fadul 1-1` +
#    ggplot2::theme(legend.position = "none",
#                   plot.margin=unit(c(0,-.5,0,-.5), "cm"),
#                   plot.title = element_blank()) +
#    coord_fixed(xlim = c(-400,3600), ylim = c(-100,3800)) +
#    annotate(geom = "text",x = 3300/2,y = 3100/2,label = "Fadul 1-1",size = 4)
#  
#  kmCMCPlot_list_comparison1to2$originalMethodCMCs_target_v_reference$`Fadul 1-2` <- kmCMCPlot_list_comparison1to2$originalMethodCMCs_target_v_reference$`Fadul 1-2`+
#    ggplot2::theme(legend.position = "none",
#                   plot.margin=unit(c(0,-.5,0,-.5), "cm"),
#                   plot.title = element_blank()) +
#    coord_fixed(xlim = c(-400,3600), ylim = c(-100,3800)) +
#    annotate(geom = "text",x = 3300/2,y = 3100/2,label = "Fadul 1-2",size = 4)
#  
#  knmCMCPlot_list_comparison1to2$originalMethodCMCs_target_v_reference$`Fadul 2-1` <- knmCMCPlot_list_comparison1to2$originalMethodCMCs_target_v_reference$`Fadul 2-1` +
#    ggplot2::theme(legend.position = "none",
#                   plot.margin=unit(c(0,-.5,0,-.5), "cm"),
#                   plot.title = element_blank()) +
#    coord_fixed(xlim = c(-400,3600), ylim = c(-100,3800)) +
#    annotate(geom = "text",x = 3300/2,y = 3100/2,label = "Fadul 2-1",size = 4)

## ----warning=TRUE,message=FALSE,echo=FALSE,fig.width = 7, fig.align = 'center'----
#  
#  reference_label <- ggplot() +
#    annotate("text",x = 1,y = 1,size = 4.5,label = expression(paste(underline("Reference")))) +
#    theme_void()
#  
#  target_label <- ggplot() +
#    annotate("text",x = 1,y = 1,size = 4.5,label = expression(paste(underline("Target")))) +
#    theme_void()
#  
#  originalMethod_label <- ggplot() +
#    annotate("text",x = 1,y = 1,size = 4.5,label = expression(paste(underline("Original Method Classif.")))) +
#    theme_void()
#  
#  originalCMC_count_label <- ggplot() +
#    annotate("text",x = 1,y = 1,size = 4.5,label = expression(paste(underline("CMC Count")))) +
#    theme_void()
#  
#  originalCMC_comparison1to2_label <- ggplot() +
#    annotate("text",x = 1,y = 1,size = 4,label = "19 CMCs") +
#    theme_void()
#  
#  originalCMC_comparison2to1_label <- ggplot() +
#    annotate("text",x = 1,y = 1,size = 4,label = "18 CMCs") +
#    theme_void()
#  # Some internal call to is.na within grid.arrange is throwing a warning for the
#  # pairwiseCompCor_label since it contains an expression. Since this isn't
#  # anything wrong with vignette, we'll just suppress it
#  suppressWarnings({
#    plt <- gridExtra::arrangeGrob(reference_label,
#                                  target_label,
#                                  originalMethod_label,
#                                  originalCMC_count_label,
#                                  kmCMCPlot_list_comparison1to2$originalMethodCMCs_reference_v_target$`Fadul 1-1`,
#                                  kmCMCPlot_list_comparison1to2$originalMethodCMCs_reference_v_target$`Fadul 1-2`,
#                                  originalMethod_comparison1to2_pcp,
#                                  originalCMC_comparison1to2_label,
#                                  kmCMCPlot_list_comparison1to2$originalMethodCMCs_target_v_reference$`Fadul 1-2`,
#                                  kmCMCPlot_list_comparison1to2$originalMethodCMCs_target_v_reference$`Fadul 1-1`,
#                                  originalMethod_comparison2to1_pcp,
#                                  originalCMC_comparison2to1_label,
#                                  blank_label,
#                                  blank_label,
#                                  pltLegend_horizontal,
#                                  blank_label,
#                                  heights = unit(c(2,9,9,2),units = "null"),
#                                  widths = unit(c(2,2,2,1),"null")
#    )
#  
#    # ggsave("derivatives/originalMethodPCP.png",plt)
#  })

## ---- echo=FALSE,eval=TRUE,out.width=600,fig.align="center"-------------------
knitr::include_graphics("https://github.com/jzemmels/vignetteImages/blob/main/originalMethodPCP.png?raw=true")

## ----include=FALSE------------------------------------------------------------
#  theta_rescaled <- kmComparisonFeatures %>%
#    select(theta) %>%
#    distinct() %>%
#    mutate(thetaScaled = scales::rescale(theta))
#  
#  highCMC_medianData_comparison1to2 <- kmComparisonFeatures %>%
#    filter(theta %in% c(-24,-27)) %>%
#    group_by(theta) %>%
#    summarise(x = median(x),
#              y = median(y)) %>%
#    ungroup() %>%
#    mutate(pairwiseCompCor = c(.75,.75),
#           cellNum = c(100,103))
#  
#  highCMC_yminData_comparison1to2 <- highCMC_medianData_comparison1to2 %>%
#    mutate(x = x - 20,
#           y = y - 20,
#           pairwiseCompCor = c(.5,.5),
#           cellNum = c(99,102))
#  
#  highCMC_ymaxData_comparison1to2 <- highCMC_medianData_comparison1to2 %>%
#    mutate(x = x + 20,
#           y = y + 20,
#           pairwiseCompCor = c(1,1),
#           cellNum = c(101,103))
#  
#  highCMC_congruentData_comparison1to2 <- kmComparisonFeatures %>%
#    filter(theta %in% c(-24,-27)) %>%
#    select(-c(cellIndex,fft_ccf)) %>%
#    mutate(cellNum = 1:nrow(.)) %>%
#    bind_rows(highCMC_yminData_comparison1to2,
#              highCMC_medianData_comparison1to2,
#              highCMC_ymaxData_comparison1to2) %>%
#    select(-pairwiseCompCor) %>%
#    pivot_longer(cols = c(x,y),
#                 names_to = "parameter") %>%
#    group_by(parameter) %>%
#    mutate(scaledValue = scales::rescale(value)) %>%
#    bind_rows(kmComparisonFeatures %>%
#                filter(theta %in% c(-24,-27)) %>%
#                select(-c(cellIndex,fft_ccf)) %>%
#                mutate(cellNum = 1:nrow(.)) %>%
#                bind_rows(highCMC_yminData_comparison1to2,
#                          highCMC_medianData_comparison1to2,
#                          highCMC_ymaxData_comparison1to2) %>%
#                select(-c(x,y)) %>%
#                mutate(parameter = "pairwiseCompCor") %>%
#                rename(value = pairwiseCompCor) %>%
#                mutate(scaledValue = value)) %>%
#    arrange(cellNum)
#  
#  highCMC_extremaData_comparison1to2 <- highCMC_congruentData_comparison1to2 %>%
#    filter(cellNum > 90) %>%
#    select(-c(cellNum,value)) %>%
#    arrange(theta,parameter) %>%
#    ungroup() %>%
#    mutate(name = rep(c("ymin","ymed","ymax"),times = 6)) %>%
#    pivot_wider(id_cols = c(parameter,theta,scaledValue),
#                names_from = name,
#                values_from = scaledValue)
#  
#  highCMC_comparison1to2_congruentCells <- highCMC_congruentData_comparison1to2 %>%
#    filter(cellNum < 90) %>%
#    left_join(highCMC_extremaData_comparison1to2,by = c("parameter","theta")) %>%
#    mutate(Classification = ifelse(scaledValue >= ymin & scaledValue <= ymax,"Congruent","Not Congruent")) %>%
#    group_by(cellNum,theta) %>%
#    summarise(Classification = ifelse(all(Classification == "Congruent"),"Congruent","Not Congruent"))

## ---- include=FALSE-----------------------------------------------------------
#  highCMC_medianData_comparison1to2_lowCMC <- kmComparisonFeatures %>%
#    filter(theta %in% c(27,30)) %>%
#    group_by(theta) %>%
#    summarise(x = median(x),
#              y = median(y)) %>%
#    ungroup() %>%
#    mutate(pairwiseCompCor = c(.75,.75),
#           cellNum = c(100,103))
#  
#  highCMC_yminData_comparison1to2_lowCMC <- highCMC_medianData_comparison1to2_lowCMC %>%
#    mutate(x = x - 20,
#           y = y - 20,
#           pairwiseCompCor = c(.5,.5),
#           cellNum = c(99,102))
#  
#  highCMC_ymaxData_comparison1to2_lowCMC <- highCMC_medianData_comparison1to2_lowCMC %>%
#    mutate(x = x + 20,
#           y = y + 20,
#           pairwiseCompCor = c(1,1),
#           cellNum = c(101,103))
#  
#  highCMC_congruentData_comparison1to2_lowCMC <- kmComparisonFeatures %>%
#    filter(theta %in% c(27,30)) %>%
#    select(-c(cellIndex,fft_ccf)) %>%
#    mutate(cellNum = 1:nrow(.)) %>%
#    bind_rows(highCMC_yminData_comparison1to2_lowCMC,
#              highCMC_medianData_comparison1to2_lowCMC,
#              highCMC_ymaxData_comparison1to2_lowCMC) %>%
#    select(-pairwiseCompCor) %>%
#    pivot_longer(cols = c(x,y),
#                 names_to = "parameter") %>%
#    group_by(parameter) %>%
#    mutate(scaledValue = scales::rescale(value)) %>%
#    bind_rows( kmComparisonFeatures %>%
#                 filter(theta %in% c(27,30)) %>%
#                 select(-c(cellIndex,fft_ccf)) %>%
#                 mutate(cellNum = 1:nrow(.)) %>%
#                 bind_rows(highCMC_yminData_comparison1to2_lowCMC,
#                           highCMC_medianData_comparison1to2_lowCMC,
#                           highCMC_ymaxData_comparison1to2_lowCMC) %>%
#                 select(-c(x,y)) %>%
#                 mutate(parameter = "pairwiseCompCor") %>%
#                 rename(value = pairwiseCompCor) %>%
#                 mutate(scaledValue = value)) %>%
#    arrange(cellNum)
#  
#  highCMC_extremaData_comparison1to2_lowCMC <- highCMC_congruentData_comparison1to2_lowCMC  %>%
#    filter(cellNum > 90) %>%
#    select(-c(cellNum,value)) %>%
#    arrange(theta,parameter) %>%
#    ungroup() %>%
#    mutate(name = rep(c("ymin","ymed","ymax"),times = 6)) %>%
#    pivot_wider(id_cols = c(parameter,theta,scaledValue),
#                names_from = name,
#                values_from = scaledValue)
#  
#  highCMC_comparison1to2_congruentCells_lowCMC <- highCMC_congruentData_comparison1to2_lowCMC %>%
#    filter(cellNum < 90) %>%
#    left_join(highCMC_extremaData_comparison1to2_lowCMC,by = c("parameter","theta")) %>%
#    mutate(Classification = ifelse(scaledValue >= ymin & scaledValue <= ymax,"Congruent","Not Congruent"),
#           theta = factor(theta,levels = c(27,30))) %>%
#    group_by(cellNum,theta) %>%
#    summarise(Classification = ifelse(all(Classification == "Congruent"),"Congruent","Not Congruent"))

## ---- include=FALSE-----------------------------------------------------------
#  #For some reason the extrema aren't calculated correctly making the delta x
#  #rectangle look longer than the delta y rectangle. I'm just going to fix it here
#  highCMC_extremaData_comparison1to2 <- highCMC_extremaData_comparison1to2 %>%
#                mutate(ymin = ifelse(parameter == "x",ymax - .297,ymin))
#  
#  highCMC_comparison1to2_thetaNeg24 <- highCMC_congruentData_comparison1to2 %>%
#    filter(cellNum < 90 & theta == -24) %>%
#    left_join(theta_rescaled,by = "theta") %>%
#    pivot_wider(id_cols = c(cellNum,theta,parameter,thetaScaled),names_from = parameter,values_from = scaledValue) %>%
#    pivot_longer(cols = c(x,y,pairwiseCompCor,thetaScaled),
#                 names_to = "parameter",
#                 values_to = "scaledValue") %>%
#    left_join(highCMC_comparison1to2_congruentCells,by = c("cellNum","theta")) %>%
#    mutate(group = paste0(cellNum,theta),
#           parameter = factor(parameter,c("x","y","thetaScaled","pairwiseCompCor")),
#           Classification = factor(Classification,levels = c("Congruent","Not Congruent")),
#           theta = factor(theta,levels = c(-27,-24))) %>%
#    filter(parameter != "thetaScaled") %>%
#    ggplot() +
#    scale_colour_manual(values = c("#1b03a3","#a60b00")) +
#    geom_line(aes(x = parameter,
#                  y = scaledValue,
#                  group = group,
#                  colour = Classification
#                  # linetype = Classification,
#                  # colour = theta
#                  ),
#              size = .1) +
#    geom_tile(data = highCMC_extremaData_comparison1to2 %>%
#                filter(theta == -24),
#              aes(x = parameter,
#                  y = ymed,
#                  width = .25,
#                  height = ymax - ymin
#                  # ,fill = theta
#                  ),
#              fill = "#7570b3",
#              colour = "black",
#              alpha = .4) +
#    theme_bw() +
#    theme(axis.text.y = element_blank(),
#          axis.title.y = element_blank(),
#          axis.title.x = element_blank(),
#          axis.ticks = element_blank(),
#          legend.position = "none",
#          plot.margin = unit(c(.55,0,0,0), "cm")) +
#    scale_x_discrete(labels = c(expression(paste(Delta, "x")),
#                                expression(paste(Delta, "y")),
#                                # expression(theta),
#                                expression("CCF"[max])))
#  
#  highCMC_comparison1to2_theta30 <- highCMC_congruentData_comparison1to2_lowCMC %>%
#    filter(cellNum < 90 & theta == 30) %>%
#    left_join(theta_rescaled,by = "theta") %>%
#    pivot_wider(id_cols = c(cellNum,theta,parameter,thetaScaled),names_from = parameter,values_from = scaledValue) %>%
#    pivot_longer(cols = c(x,y,pairwiseCompCor,thetaScaled),
#                 names_to = "parameter",
#                 values_to = "scaledValue") %>%
#    mutate(theta = factor(theta,levels = c(27,30))) %>%
#    left_join(highCMC_comparison1to2_congruentCells_lowCMC %>%
#                mutate(theta = factor(theta,levels = c(27,30))),
#              by = c("cellNum","theta")) %>%
#    mutate(group = paste0(cellNum,theta),
#           parameter = factor(parameter,c("x","y","thetaScaled","pairwiseCompCor")),
#           Classification = factor(Classification,levels = c("Congruent","Not Congruent"))) %>%
#    filter(parameter != "thetaScaled") %>%
#    ggplot() +
#    scale_colour_manual(values = c("#1b03a3","#a60b00")) +
#    geom_line(aes(x = parameter,
#                  y = scaledValue,
#                  group = group,
#                  colour = Classification
#                  # linetype = Classification,
#                  # colour = theta
#                  ),
#              size = .1) +
#    geom_tile(data = highCMC_extremaData_comparison1to2_lowCMC %>%
#                filter(theta == 30),
#              aes(x = parameter,
#                  y = ymed,
#                  width = .25,
#                  height = ymax - ymin
#                  # ,fill = theta
#                  ),
#              fill = "#7570b3",
#              colour = "black",
#              alpha = .4) +
#    theme_bw() +
#    theme(axis.text.y = element_blank(),
#          axis.title.y = element_blank(),
#          axis.title.x = element_blank(),
#          axis.ticks = element_blank(),
#          legend.position = "none",
#          plot.margin = unit(c(.55,0,0,0), "cm")) +
#    scale_x_discrete(labels = c(expression(paste(Delta, "x")),
#                                expression(paste(Delta, "y")),
#                                # expression(theta),
#                                expression("CCF"[max])))

## ----warning=TRUE,message=FALSE,echo=FALSE,fig.width = 7, fig.align = 'center'----
#  
#  highCMC_comparison1to2_cmcTheta <- bind_rows(kmComparisonFeatures %>%
#              mutate(direction = "Fadul 1-1 vs. Fadul 1-2",
#                     cmcThetaDistribClassif = cmcR::decision_highCMC_cmcThetaDistrib(cellIndex,x,y,theta,pairwiseCompCor)) %>%
#              cmcR::decision_highCMC_identifyHighCMCThetas(),
#            kmComparisonFeatures_rev %>%
#              mutate(direction = "Fadul 1-2 vs. Fadul 1-1",
#                     cmcThetaDistribClassif = cmcR::decision_highCMC_cmcThetaDistrib(cellIndex,x,y,theta,pairwiseCompCor)) %>%
#              cmcR::decision_highCMC_identifyHighCMCThetas()) %>%
#    filter(cmcThetaDistribClassif == "CMC Candidate" & direction == "Fadul 1-1 vs. Fadul 1-2") %>%
#    ggplot(aes(x = theta,
#               fill = thetaCMCIdentif)) +
#    geom_bar(stat = "count",alpha = .7) +
#    facet_wrap(~ direction,ncol = 1) +
#    theme_bw() +
#    coord_cartesian(xlim = c(-30,30),
#                    ylim = c(0,NA)) +
#    ggplot2::scale_fill_manual(values = c("black","gray50")) +
#    scale_x_continuous(expression(paste("Rotation angle ", theta)),
#                       breaks = seq(-30,30,by = 15),
#                       limits = c(-32,32)) +
#    guides(fill = guide_legend(title = "High CMC Classif.")) +
#    theme(legend.position = "bottom")
#  
#  thetaNeg24_label <- ggplot() +
#    annotate("text",x = 1,y = 1,size = 4,label = expression(paste(theta," = -24"))) +
#    theme_void()
#  
#  theta30_label <- ggplot() +
#    annotate("text",x = 1,y = 1,size = 4,label = expression(paste(theta," = 30"))) +
#    theme_void()
#  # Some internal call to is.na within grid.arrange is throwing a warning for the
#  # pairwiseCompCor_label since it contains an expression. Since this isn't
#  # anything wrong with vignette, we'll just suppress it
#  suppressWarnings({
#    plt <- ggplot(data.frame(a = 1)) +
#      theme_void() +
#      coord_cartesian(xlim = c(1,30),
#                      ylim = c(1,30),
#                      expand = FALSE) +
#      annotation_custom(ggplotGrob(highCMC_comparison1to2_cmcTheta),xmin = 6,xmax = 24,ymin = 1,ymax = 30) +
#      annotation_custom(ggplotGrob(highCMC_comparison1to2_thetaNeg24),xmin = 1,xmax = 6,ymin = 4,ymax = 14) +
#      annotation_custom(ggplotGrob(thetaNeg24_label),xmin = 1,xmax = 6,ymin = 10.5,ymax = 16) +
#      annotation_custom(pltLegend_vertical_leftAligned,xmin = 2.5,xmax = 3,ymin = 2,ymax = 3) +
#      annotation_custom(ggplotGrob(highCMC_comparison1to2_theta30),xmin = 24,xmax = 29,ymin = 4,ymax = 14) +
#      annotation_custom(ggplotGrob(theta30_label),xmin = 24,xmax = 29,ymin = 10.5,ymax = 16) +
#      annotation_custom(pltLegend_vertical_rightAligned,xmin = 27,xmax = 27.75,ymin = 2,ymax = 3) +
#      geom_path(data = data.frame(x = c(6,10),y = c(9,9)),
#                aes(x = x, y = y), size = 1, arrow = arrow(length = unit(.02, "npc"), type = "closed")) +
#      geom_path(data = data.frame(x = c(23,24.1),y = c(9,9)),
#                aes(x = x, y = y), size = 1, arrow = arrow(length = unit(.02, "npc"), type = "closed",ends = "first"))
#  
#    # ggsave("derivatives/highCMCDistribution.png",plt)
#  })

## ----echo=FALSE,eval=TRUE,out.width=600,fig.align="center"--------------------
knitr::include_graphics("https://github.com/jzemmels/vignetteImages/blob/main/highCMCDistribution.png?raw=true")

