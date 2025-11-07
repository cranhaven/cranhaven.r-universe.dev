## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  rmarkdown.html_vignette.check_title = FALSE
)

## ----setup, include = FALSE---------------------------------------------------
library(plantTracker)
library(ggplot2)
library(sf)
# library(devtools)
# load_all()

## ----echo = FALSE-------------------------------------------------------------
exampleDat <- grasslandData[grasslandData$Site == "AZ" & 
                              grasslandData$Quad == "SG2" ,]

dataShow <- head(exampleDat[, !names(exampleDat) %in%
                                  c("Clone", "Seedling", "Stems", "Basal",
                                    "sp_code_4", "Area")])
rownames(dataShow) <- NULL
(dataShow)
#knitr::kable(dataShow, caption = "**Table 1.1**: *Example `dat` data.frame*" )

## ----echo = FALSE,  fig.width=7.5, fig.align = 'center', fig.cap = "**Figure 1.1**: *Spatial map of a subset of example `dat` data set*"----
exampleDat <- grasslandData[grasslandData$Site == "AZ" & 
                              grasslandData$Quad == "SG2" & 
                              grasslandData$Year %in% c(1922:1927), ]
ggplot(data = exampleDat) +
  geom_sf(aes(color = Species, fill = Species)) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 0), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 0, xend = 1, y = 1, yend = 1), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 1), size = .5, 
               lineend = "round", color = "grey30") + 
  xlab("quadrat horizontal edge (m)") +
  ylab("quadrat vertical edge (m)") +
  theme_classic() +
  facet_wrap(~Year) +
  theme(axis.line = element_blank(), 
        legend.text = element_text(face = "italic"),
        plot.margin = margin(1,0,1,0)) 

## ----echo = FALSE,  fig.show = 'hold', fig.width=7, fig.align = 'left', fig.cap = "**Figure 3.1**: *The value of 'buffGenet' used in the `trackSpp()` function can make a big difference in genetID assignments. These examples move from no genet grouping on the left, where every polygon has its own genetID, to grouping any ramets together that are less than 10 cm apart on the right. Colors and numbers indicate different genetIDs. Buffers are drawn around ramets that belong to the same genet.*", warning=FALSE----

figDat <- exampleDat[exampleDat$Species == "Heteropogon contortus" & exampleDat$Year == 1922, c("Species", "geometry")]

figDat$genetID <- as.factor(c(1:nrow(figDat)))
figDat$buffGenet <- "'clonal'=FALSE"

figDat_2 <- figDat
figDat_2$genetID <- as.factor(
  c(1,2,3,4,5,6,6,6,7,8,9,10,11,12,13, 14, 14, 15, 16, 17, 18, 19, 20, 21)
  #groupByGenet(figDat, buffGenet = .01)
  )
figDat_2$buffGenet <-  "'buffGenet'=.01"
figDat_2AG <- aggregate(x = figDat_2, 
          by = list(Species = figDat_2$Species,
            genetID = figDat_2$genetID,
            buffGenet = figDat_2$buffGenet),
          FUN = mean,
          do_union = TRUE) 
figDat_2AG <- st_buffer(figDat_2AG[,c("Species", "genetID","buffGenet","geometry")],.02)
figDat_2AG$buffGenet <- "'buffGenet'=.01"
figDat_2AG$Species <- "grouped"

figDat_3 <- figDat
figDat_3$genetID <- as.factor(
  c(1,  1,  2,  2,  2,  2,  2,  2,  3,  4,5,6,6,7,8,9,9,3,10,10,11,2,4, 4 )
  #groupByGenet(figDat, buffGenet = .05)
  )
figDat_3$buffGenet <- "'buffGenet'=.05"
figDat_3AG <- aggregate(x = figDat_3, 
          by = list(Species = figDat_3$Species,
            genetID = figDat_3$genetID,
            buffGenet = figDat_2$buffGenet),
          FUN = mean,
          do_union = TRUE)
figDat_3AG <- st_buffer(figDat_3AG[,c("Species", "genetID","buffGenet","geometry")],.05)
figDat_3AG$Species <- "grouped"
figDat_3AG$buffGenet <- "'buffGenet'=.05"
figDat <- rbind(figDat, figDat_2, figDat_3, figDat_2AG, figDat_3AG)

figDat$buffGenet <- factor(figDat$buffGenet, levels = c("'clonal'=FALSE", "'buffGenet'=.01", "'buffGenet'=.05"), ordered = TRUE)

ggplot(figDat) +
  geom_sf(data = figDat[figDat$Species == "grouped",], aes(fill = genetID), alpha = .2, color = "grey80") +
  geom_sf(data = figDat[figDat$Species == "Heteropogon contortus",], aes(fill = genetID)) + 
  #geom_sf_text(aes(label = genetID), nudge_x = .05) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 0), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 0, xend = 1, y = 1, yend = 1), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 1), size = .5, 
               lineend = "round", color = "grey30") + 
  xlab("quadrat horizontal edge (m)") +
  ylab("quadrat vertical edge (m)") +
  facet_wrap(~buffGenet) +
  theme_classic() +
  scale_alpha_continuous(guide = "none") +
  scale_fill_discrete(aes(), guide = "none") +
  theme(axis.line = element_blank(), 
        plot.margin = margin(1,0,1,0)) 

## ----echo = FALSE, warning= FALSE, fig.width=6, fig.align = 'left', fig.cap = "**Figure 3.2**: *With a 10 cm buffer, these polygons in 1922 and 1923 overlap and will be identified by trackSpp() as the **same** individual and receive the same trackID*."----

exampleDat <- grasslandData[grasslandData$Site == "AZ" & 
                              grasslandData$Quad == "SG2" & 
                              grasslandData$Year %in% c(1922,1923), ]
exampleDatIDsTemp <- exampleDat[exampleDat$Species == "Bouteloua rothrockii",]
exampleDatIDsTemp <- exampleDatIDsTemp[round(exampleDatIDsTemp$Area,7) %in% round(c( 0.0005471808, 0.0005321236),7),]
 
exampleDatIDsTemp$ghost <- "observation from current year"
exampleBuffed <- st_buffer(exampleDatIDsTemp[round(exampleDatIDsTemp$Area, 7) ==0.0005472,], dist = .10)
exampleBuffed$Year <- 1922
exampleBuffed$ghost <- "10 cm buffer"
exampleBuffedNext <- exampleBuffed
exampleBuffedNext$Year <- 1923
ghost <- exampleDatIDsTemp[round(exampleDatIDsTemp$Area, 7) == 0.0005472,]
ghost$Year <- 1923
ghost$ghost <- "polygon location in previous year"
exampleDatIDs <- rbind(exampleDatIDsTemp, ghost, exampleBuffed, exampleBuffedNext )


ggplot(data = exampleDatIDs
       ) +
  geom_sf(aes(fill = ghost, alpha = ghost, color = ghost, lty = ghost)) +
  geom_segment(aes(x = .6, xend = 1, y = .4, yend = .4), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = .6, xend = 1, y = .8, yend = .8), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = .6, xend = .6, y = .4, yend = .8), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 1, xend = 1, y = .4, yend = .8), size = .5, 
               lineend = "round", color = "grey30")+ 
  xlab("quadrat horizontal edge (m)") +
  ylab("quadrat vertical edge (m)") +
  #labs(title = Year) +
  facet_wrap( ~ Year) +
  theme_classic() +
  theme(axis.line = element_blank(), 
        legend.text = element_text(face = "italic"),
        #plot.margin = margin(1,0,1,0),
        legend.title = element_blank()) +
  scale_fill_discrete(type = c("#E69F00", "#009E73", "#A6A6A6")) +
 scale_color_discrete(type = c("#E69F00", "#009E73", "#A6A6A6")) +
  scale_alpha_discrete(range = c(0, 1, .5)) +
  scale_linetype_manual(values=c("twodash", "solid", "dotted"))

## ----echo = FALSE, warning= FALSE, fig.width=6, fig.align = 'left', fig.cap = "**Figure 3.3**: *With a 5 cm buffer, these polygons in 1922 and 1923 overlap and will be identified by trackSpp() as **different** individuals and receive different trackIDs.*"----
exampleDat <- grasslandData[grasslandData$Site == "AZ" & 
                              grasslandData$Quad == "SG2" , ]
exampleDatIDsTemp <- exampleDat[exampleDat$Species == "Bouteloua rothrockii" & 
                              exampleDat$Year %in% c(1922,1923),]
exampleDatIDsTemp <- exampleDatIDsTemp[round(exampleDatIDsTemp$Area,7) %in% round(c( 0.0005471808, 0.0005321236),7),]
 
exampleDatIDsTemp$ghost <- "observation from current year"
exampleBuffed <- st_buffer(exampleDatIDsTemp[round(exampleDatIDsTemp$Area, 7) ==0.0005472,], dist = .045)
exampleBuffed$Year <- 1922
exampleBuffed$ghost <- "5 cm buffer"
exampleBuffedNext <- exampleBuffed
exampleBuffedNext$Year <- 1923
ghost <- exampleDatIDsTemp[round(exampleDatIDsTemp$Area, 7) == 0.0005472,]
ghost$Year <- 1923
ghost$ghost <- "polygon location in previous year"
exampleDatIDs <- rbind(exampleDatIDsTemp, ghost, exampleBuffed, exampleBuffedNext )


ggplot(data = exampleDatIDs
       ) +
  geom_sf(aes(fill = ghost, alpha = ghost, color = ghost, lty = ghost)) +
  geom_segment(aes(x = .6, xend = 1, y = .4, yend = .4), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = .6, xend = 1, y = .8, yend = .8), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = .6, xend = .6, y = .4, yend = .8), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 1, xend = 1, y = .4, yend = .8), size = .5, 
               lineend = "round", color = "grey30")+ 
  xlab("quadrat horizontal edge (m)") +
  ylab("quadrat vertical edge (m)") +
  #labs(title = Year) +
  facet_wrap( ~ Year) +
  theme_classic() +
  theme(axis.line = element_blank(), 
        legend.text = element_text(face = "italic"),
        #plot.margin = margin(1,0,1,0),
        legend.title = element_blank()) +
  scale_fill_discrete(type = c("#E69F00", "#009E73", "#A6A6A6")) +
 scale_color_discrete(type = c("#E69F00", "#009E73", "#A6A6A6")) +
  scale_alpha_discrete(range = c(0, 1, .5)) +
  scale_linetype_manual(values=c("twodash", "solid", "dotted"))

## ----echo = FALSE, fig.width=7, fig.align = 'left', fig.cap = "**Figure 3.4**: *A visualization of the 'dormancy' scenario described above. The observation in 1922 has no overlap with any observation in 1923 (panels 1 and 2). However, if 'dorm' is greater than or equal to 1, we can save the 1922 observation as a 'ghost' (illustrated with a dotted border in panel 2). When compared to observations in 1924, there is an overlap! If 'dorm' = 1 (or more), then the observation in 1922 will get a '1' in the 'survives_tplus1' column. If 'dorm' = 0, then the observation in 1922 will get a '0' for survival, and the observation in 1924 will be a new recruit.*"----
exampleSmall <- exampleDat[exampleDat$Species == "Heteropogon contortus" & exampleDat$Year %in% c(1922, 1923, 1924),]

#117621 (1922); 117735 (1923); 117791 (1924)
exampleSmall <- exampleSmall[rownames(exampleSmall) %in% c(117621, 117735, 117791),] 
exampleSmall <- exampleSmall[rownames(exampleSmall) != 117735,]
exampleSmall <- rbind(exampleSmall, exampleSmall[1,])
exampleSmall[3,"Year"] <- 1923
exampleSmall$ghost <- c("not ghost", "not ghost", "ghost")

ggplot(data = exampleSmall) +
  geom_sf(data = exampleSmall[exampleSmall$ghost == "not ghost",] ,aes(), color = "red", fill = "red", alpha = .5) +
  geom_sf(data = exampleSmall[exampleSmall$ghost == "ghost",] ,aes(), color = "red", lty = 3, lwd = 1, fill = NA, alpha = .5) +
  geom_segment(aes(x = .5, xend = .8, y = .85, yend = .85), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = .5, xend = .8, y = 1, yend = 1), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = .5, xend = .5, y = .85, yend = 1), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = .8, xend = .8, y = 0.85, yend = 1), size = .5, 
               lineend = "round", color = "grey30") + 
  xlab("quadrat horizontal edge (m)") +
  ylab("quadrat vertical edge (m)") +
  xlim(c(.5,.8)) +
  ylim(c(.85,1.0)) +
  #labs(title = Year) +
  facet_wrap( ~ Year) +
  theme_classic() +
  theme(axis.line = element_blank(), 
        legend.text = element_text(face = "italic"),
        plot.margin = margin(1,0,1,0),
        legend.title = element_blank()) 

## ----echo = FALSE, fig.width=7, fig.align = 'left', fig.cap = "**Figure 3.5**: *Here are the data for *Heteropogon contortus* in 1922 and 1923. A 5 cm buffer is shown around each genet in 1922. Data from both years have been grouped by genet using 'buffGenet' = .01*", warning = FALSE----
# comparing data from 1922 to 1923 (overlapping), then show actual trackID assignments
exampleSmall <- exampleDat[exampleDat$Species == "Heteropogon contortus" & exampleDat$Year %in% c(1922, 1923), c("Species", "Year", "geometry")]

exampleSmall$genetID <- NA

exampleSmall_22 <- exampleSmall[exampleSmall$Year == 1922,]
exampleSmall_22$genetID <- c(1,2,3,4,5,6,6,6,7,8,9,10,11,12,13,14,14,15,16,17,18,19,20,21)

exampleSmall_22 <- aggregate(x = exampleSmall_22, 
          by = list(Species = exampleSmall_22$Species,
            genetID = exampleSmall_22$genetID),
          FUN = mean,
          do_union = TRUE) 
exampleSmall_22 <- exampleSmall_22[,c("Species", "genetID", "Year", "geometry")]
exampleSmall_22$buff <- "no"
exampleSmall_22_buff <- st_buffer(exampleSmall_22, .05)
exampleSmall_22_buff$buff <- "yes"


exampleSmall_23 <- exampleSmall[exampleSmall$Year == 1923,]
exampleSmall_23$genetID <- c(1,2,3,4,5,6,7,8,9,10,11,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
exampleSmall_23 <- aggregate(x = exampleSmall_23, 
          by = list(Species = exampleSmall_23$Species,
            genetID = exampleSmall_23$genetID),
          FUN = mean,
          do_union = TRUE) 
exampleSmall_23 <- exampleSmall_23[,c("Species", "genetID", "Year", "geometry")]
exampleSmall_23$buff <- "no"

exampleSmall <- rbind(exampleSmall_22, exampleSmall_22_buff, exampleSmall_23)

ggplot(data = exampleSmall) +
  geom_sf(data = exampleSmall[exampleSmall$buff == "yes",], aes(color = as.factor(Year), fill = as.factor(Year)), alpha = .2, lty = 3) +
  geom_sf(data = exampleSmall[exampleSmall$buff == "no",], aes(color = as.factor(Year), fill = as.factor(Year)), alpha = .7) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 0), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 0, xend = 1, y = 1, yend = 1), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 1), size = .5, 
               lineend = "round", color = "grey30") + 
  xlab("quadrat horizontal edge (m)") +
  ylab("quadrat vertical edge (m)") +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  #labs(title = Year) +
  facet_wrap( ~ Year) +
  theme_classic() +
  theme(axis.line = element_blank(), 
        legend.text = element_text(face = "italic"),
        plot.margin = margin(1,0,1,0),
        legend.title = element_blank()) 

## ----echo = FALSE, fig.width=4.5, fig.align = 'left', fig.cap = "**Figure 3.6**: *Here are the buffered data for Heteropogon contortus from 1922, overlapped with the data from 1923.*", warning = FALSE----
ggplot(data = exampleSmall) +
  geom_sf(data = exampleSmall[exampleSmall$buff == "yes",], aes(color = as.factor(Year), fill = as.factor(Year)), alpha = .2, lty = 3) +
  geom_sf(data = exampleSmall[exampleSmall$buff == "no" & exampleSmall$Year == 1923,], aes(color = as.factor(Year), fill = as.factor(Year)), alpha = .7) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 0), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 0, xend = 1, y = 1, yend = 1), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 1), size = .5, 
               lineend = "round", color = "grey30") + 
  xlab("quadrat horizontal edge (m)") +
  ylab("quadrat vertical edge (m)") +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  #labs(title = Year) +
  #facet_wrap( ~ Year) +
  theme_classic() +
  theme(axis.line = element_blank(), 
        legend.text = element_text(face = "italic"),
        plot.margin = margin(1,0,1,0),
        legend.title = element_blank()) 

## ----echo = FALSE, fig.width=7, fig.align = 'left', fig.cap = "**Figure 3.7**: *Here are the trackID assignments for these two years of data. Each trackID has a different color and a different number.*", warning = FALSE----

exampleSmall$Site <- "AZs"
exampleSmall$Quad <- "SG2"
exampleOut <- plantTracker::trackSpp(dat = exampleSmall[exampleSmall$buff == "no",], inv = list("SG2" = c(1922, 1923)), buff = .05, clonal = TRUE, dorm = 1, buffGenet = .01, aggByGenet = TRUE, printMessages = FALSE)

labels <- data.frame(trackID = unique(exampleOut$trackID), 
                     trackID_new = c(1:length(unique(exampleOut$trackID))))
exampleOut$trackID_new <- labels$trackID_new[match( exampleOut$trackID, labels$trackID)]

ggplot(data = exampleOut) +
  geom_sf(aes(color = trackID, fill = trackID), alpha = .9) +
  geom_sf_text(aes(label = trackID_new), nudge_x = .02, nudge_y = -.02) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 0), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 0, xend = 1, y = 1, yend = 1), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 1), size = .5, 
               lineend = "round", color = "grey30") + 
  xlab("quadrat horizontal edge (m)") +
  ylab("quadrat vertical edge (m)") +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  scale_fill_discrete(guide = "none") +
  scale_color_discrete(guide = "none") +
  #labs(title = Year) +
  facet_wrap( ~ Year) +
  theme_classic() +
  theme(axis.line = element_blank(), 
        legend.text = element_text(face = "italic"),
        plot.margin = margin(1,0,1,0),
        legend.title = element_blank()) 

## ----echo = FALSE, fig.width=7, fig.align = 'left', fig.cap = "**Figure 3.8**: *Here is a visualization of how the observations are broken into 'parents', 'ghosts', 'children', and 'orphans'.*", warning = FALSE----
# break d.f into parents, ghosts, children, and orphans
exampleOut$status <- NA
exampleOut[exampleOut$Year == 1922 & 
             is.na(exampleOut$survives_tplus1) == FALSE, "status"] <- "parents"
exampleOut[exampleOut$Year == 1922 & 
             is.na(exampleOut$survives_tplus1) == TRUE, "status"] <- "ghosts"
exampleOut[exampleOut$Year == 1923 & 
             is.na(exampleOut$recruit), "status"] <- "children"
exampleOut[exampleOut$Year == 1923 & 
             exampleOut$recruit == 1 & !is.na(exampleOut$recruit), "status"] <- "orphan"

ggplot(data = exampleOut) +
  geom_sf(aes(fill = status)) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 0), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 0, xend = 1, y = 1, yend = 1), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 1), size = .5, 
               lineend = "round", color = "grey30") + 
  xlab("quadrat horizontal edge (m)") +
  ylab("quadrat vertical edge (m)") +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  #labs(title = Year) +
  facet_wrap( ~ Year) +
  theme_classic() +
  theme(axis.line = element_blank(), 
        legend.text = element_text(face = "italic"),
        plot.margin = margin(1,0,1,0),
        legend.title = element_blank()) 


## ----echo = FALSE, fig.width=7, fig.align = 'left', fig.cap = "**Figure 5.1**: *Here are the trackID assignments over 4 years of data. Each trackID is shown as a different color and has a different number.*", warning = FALSE----
exampleSmall <- exampleDat[exampleDat$Species == "Heteropogon contortus" &
                           exampleDat$Year %in% c(1922:1925),]
exampleSmall$Site <- "AZs"
exampleSmall$Quad <- "SG2"

## trim the dataset to be smaller
## make a bounding box
pl = list(rbind(c(0,0), c(.5,0), c(.5,.7), c(0,.7), c(0,0)))
box <- st_polygon(pl)
exampleSmall <-suppressWarnings(st_intersection(exampleSmall, box))

## get trackIDs 
exampleOut <- suppressMessages(plantTracker::trackSpp(dat = exampleSmall, inv = list("SG2" = c(1922:1925)), buff = .05, clonal = TRUE, dorm = 1, buffGenet = .01, aggByGenet = TRUE, printMessages = FALSE))

labels <- data.frame(trackID = unique(exampleOut$trackID), 
                     trackID_new = c(1:length(unique(exampleOut$trackID))))
exampleOut$trackID_new <- labels$trackID_new[match( exampleOut$trackID, labels$trackID)]

ggplot(data = exampleOut) +
  geom_sf(aes(color = trackID, fill = trackID), alpha = .9) +
  geom_sf_text(aes(label = trackID_new), nudge_x = .04) +
  geom_segment(aes(x = 0, xend = .5, y = 0, yend = 0), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 0, xend = .5, y = .7, yend = .7), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = .7), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = .5, xend = .5, y = 0, yend = .7), size = .5, 
               lineend = "round", color = "grey30") + 
  xlab("quadrat horizontal edge (m)") +
  ylab("quadrat vertical edge (m)") +
  xlim(c(0,.5)) +
  ylim(c(0,.7)) +
  scale_fill_discrete(guide = "none") +
  scale_color_discrete(guide = "none") +
  #labs(title = Year) +
  facet_wrap(~ Year, ncol = 4) +
  theme_classic() +
  theme(axis.line = element_blank(), 
        legend.text = element_text(face = "italic"),
        plot.margin = margin(1,0,1,0),
        legend.title = element_blank()) 

## ----echo = FALSE, fig.width=7, fig.align = 'left', fig.cap = "**Figure 5.2**: *Here are the trackID assignments over 4 years of data. Each trackID is shown as a different color and has a different number.*", warning = FALSE----
## get trackIDs 
exampleOut <- suppressMessages(plantTracker::trackSpp(dat = exampleSmall, inv = list("SG2" = c(1922:1925)), buff = .05, clonal = TRUE, dorm = 1, buffGenet = .05, aggByGenet = TRUE, printMessages = FALSE))

labels <- data.frame(trackID = unique(exampleOut$trackID), 
                     trackID_new = c(1:length(unique(exampleOut$trackID))))
exampleOut$trackID_new <- labels$trackID_new[match( exampleOut$trackID, labels$trackID)]

ggplot(data = exampleOut) +
  geom_sf(aes(color = trackID, fill = trackID), alpha = .9) +
  geom_sf_text(aes(label = trackID_new), nudge_x = .04) +
  geom_segment(aes(x = 0, xend = .5, y = 0, yend = 0), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 0, xend = .5, y = .7, yend = .7), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = .7), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = .5, xend = .5, y = 0, yend = .7), size = .5, 
               lineend = "round", color = "grey30") + 
  xlab("quadrat horizontal edge (m)") +
  ylab("quadrat vertical edge (m)") +
  xlim(c(0,.5)) +
  ylim(c(0,.7)) +
  scale_fill_discrete(guide = "none") +
  scale_color_discrete(guide = "none") +
  #labs(title = Year) +
  facet_wrap(~ Year, ncol = 4) +
  theme_classic() +
  theme(axis.line = element_blank(), 
        legend.text = element_text(face = "italic"),
        plot.margin = margin(1,0,1,0),
        legend.title = element_blank()) 

## ----echo = FALSE, fig.width=7, fig.align = 'left', fig.cap = "**Figure 5.3**: *Here are the trackID assignments over 4 years of data. Each trackID is shown as a different color and has a different number.*", warning = FALSE----

## get trackIDs 
exampleOut <- suppressMessages(plantTracker::trackSpp(dat = exampleSmall, inv = list("SG2" = c(1922:1925)), buff = .05, clonal = FALSE, dorm = 1, aggByGenet = TRUE, printMessages = FALSE))

labels <- data.frame(trackID = unique(exampleOut$trackID), 
                     trackID_new = c(1:length(unique(exampleOut$trackID))))
exampleOut$trackID_new <- labels$trackID_new[match( exampleOut$trackID, labels$trackID)]

ggplot(data = exampleOut) +
  geom_sf(aes(color = trackID, fill = trackID), alpha = .9) +
  geom_sf_text(aes(label = trackID_new), nudge_x = .04) +
  geom_segment(aes(x = 0, xend = .5, y = 0, yend = 0), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 0, xend = .5, y = .7, yend = .7), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = .7), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = .5, xend = .5, y = 0, yend = .7), size = .5, 
               lineend = "round", color = "grey30") + 
  xlab("quadrat horizontal edge (m)") +
  ylab("quadrat vertical edge (m)") +
  xlim(c(0,.5)) +
  ylim(c(0,.7)) +
  scale_fill_discrete(guide = "none") +
  scale_color_discrete(guide = "none") +
  #labs(title = Year) +
  facet_wrap(~ Year, ncol = 4) +
  theme_classic() +
  theme(axis.line = element_blank(), 
        legend.text = element_text(face = "italic"),
        plot.margin = margin(1,0,1,0),
        legend.title = element_blank()) 

