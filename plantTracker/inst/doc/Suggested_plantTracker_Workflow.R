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
dataShow <- head(grasslandData[, !names(grasslandData) %in%
                                  c("Clone", "Seedling", "Stems", "Basal",
                                    "sp_code_4", "Area")])
rownames(dataShow) <- NULL
(dataShow)
#knitr::kable(dataShow, caption = "**Table 1.1**: Example `dat` data.frame" )

## ---- echo = FALSE,  fig.width=8, fig.align = 'center', fig.cap = "**Figure 1.1** *: Spatial map of a subset of example 'dat' dataset*"----
exampleDat <- grasslandData[grasslandData$Site == "AZ" & 
                              grasslandData$Quad == "SG2" & 
                              grasslandData$Year == 1922, ]
dat <- grasslandData[grasslandData$Site == "AZ" & 
                              grasslandData$Quad == "SG2",]
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
  theme(axis.line = element_blank(), 
        legend.text = element_text(face = "italic"),
        plot.margin = margin(1,0,1,0)) 

## ---- echo=FALSE, include=FALSE, eval = FALSE---------------------------------
#  # get the working directory name
#  wdName <- c("/Users/Alice/Dropbox/Grad School/Research/Trait Project/Data/ChartQuadDatasets/AZs_downloaded data/")

## ---- warning=FALSE, eval = FALSE---------------------------------------------
#  #  save a character vector of the file names in the file that contains the
#  # shapefiles (in this case, called "CO_shapefiles"), each of which is a quadrat
#  # note: 'wdName' is a character string indicating the path of the directory
#  # containing the 'AZ_shapefiles' folder
#  quadNames <- list.files(paste0(wdName,"AZ_shapefiles/"))
#  # trim the quadrats down to 2, for the sake of runtime in this example
#  quadNames <- quadNames[quadNames %in% c("SG2", "SG4")]
#  
#  # now we'll loop through the quadrat folders to download the data
#  for (i in 1:2){#length(quadNames)) {
#    # get the names of the quadrat for this iteration of the loop
#    quadNow <- quadNames[i]
#    #  get a character vector of the unique quad/Year combinations of data in
#    # this folder that contain polygon data
#    quadYears <- quadYears <-  unlist(strsplit(list.files(
#      paste0(wdName, "AZ_shapefiles/",quadNow,"/"),
#      pattern = ".shp$"), split = ".shp"))
#    # loop through each of the years in this quadrat
#    for (j in 1:length(quadYears)) {
#      # save the name of this quadYear combo
#      quadYearNow <- quadYears[j]
#      # read in the shapefile for this quad/year combo as an sf data frame
#      # using the 'st_read()' function from the sf package
#      shapeNow <- sf::st_read(dsn = paste0(wdName,"AZ_shapefiles/",quadNow),
#                              #  the 'dsn' argument is the folder that
#                              # contains the shapefile files--in this case,
#                              # the folder for this quadrat
#                              layer = quadYearNow) # the 'layer' argument has the
#      # name of the shapefile, without the filetype extension! This is because each
#      # shapefile consists of at least three separate files, each of which has a
#      # unique filetype extension.
#      # the shapefiles in this dataset do not have all of the metadata we
#      # need, and have some we don't need, so we'll remove what we don't need and
#      # add columns for 'site', 'quad', and 'year'
#      shapeNow$Site <- "AZs"
#      shapeNow$Quad <- quadNow
#      # get the Year for the shapefile name--in this case it is the last for
#      # numbers of the name
#      shapeNow$Year <- as.numeric(strsplit(quadYearNow, split = "_")[[1]][2]) + 1900
#      # determine if the 'current' quad/year contains point data or polygon data
#      if (grepl(quadYearNow, pattern = "C")) { # if quadYearNow has point data
#        # remove the columns we don't need
#        shapeNow <- shapeNow[,!(names(shapeNow)
#                                %in% c("Clone", "Seedling", "Area", "Length", "X", "Y"))]
#        # reformat the point into a a very small polygon
#        # (a circle w/ a radius of .003 m)
#        shapeNow <- sf::st_buffer(x = shapeNow, dist = .003)
#        # add a column indicating that this observation was originally
#        # mapped as a point
#        shapeNow$type <- "point"
#      } else { # if quadYearNow has polygon data
#        # remove the columns we don't need
#        shapeNow <- shapeNow[,!(names(shapeNow) %in% c("Seedling", "Canopy_cov", "X", "Y", "area"))]
#        # add a column indicating that this observation was originally
#        # mapped as a polygon
#        shapeNow$type <- "polygon"
#      }
#      # now we'll save this sf data frame
#      if (i == 1 & j == 1) { # if this is the first year in the first quadrat
#        dat <- shapeNow
#      } else { # if this isn't the first year in the first quadrat, simply rbind
#        # the shapeNow sf data frame onto the previous data
#        dat <- rbind(dat, shapeNow)
#      }
#    }
#  }
#  
#  # Now, all of the spatial data are in one sf data frame!
#  # for the sake of this example, we'll remove data for some species and years in order to make the example run faster (and to make this 'dat' data.frame identical to the "grasslandData" dataset included in this R pakcage).
#  dat <- dat[dat$Species %in% c("Heteropogon contortus", "Bouteloua rothrockii", "Ambrosia artemisiifolia", "Calliandra eriophylla", "Bouteloua gracilis", "Hesperostipa comata", "Sphaeralcea coccinea", "Allium textile"),]
#  dat <- dat[  (dat$Quad %in% c("SG2", "SG4") &
#               dat$Year %in% c(1922:1927)),]

## -----------------------------------------------------------------------------
# We use the function "st_buffer()" to add a buffer of our chosen radius (.01) around each point observation, which will transform each observation into a circle of the "polygon" format with a radius of .01. 
dat_1 <- st_buffer(x = dat[st_is(x = dat, type = "POINT"),], dist = .01)
dat_2 <- dat[!st_is(x = dat, type = "POINT"),]
dat <- rbind(dat_1, dat_2)

## -----------------------------------------------------------------------------
dat <- grasslandData[grasslandData$Site == "AZ",]

## ----echo=FALSE, fig.align='center'-------------------------------------------
(inv <- grasslandInventory[c( "SG2", "SG4")])

## ---- echo = FALSE------------------------------------------------------------
data.frame("quad1" = c(2000, 2001, NA, 2003, 2004, 2005, 2006, 2007),
           "quad2" = c(2000:2007), 
           "quad3" = c(2000, NA, 2002, 2003, 2004, 2005, 2006, 2007))

## -----------------------------------------------------------------------------
quadInv_DF <- data.frame("quad1" = c(2000, 2001, NA, 2003, 2004, 2005, 2006, 2007),
           "quad2" = c(2000:2007), 
           "quad3" = c(2000, NA, 2002, 2003, 2004, 2005, 2006, 2007))

# use the 'as.list()' function to transform your data frame into a named list
quadInv_list <- as.list(quadInv_DF)
# we still need to remove the 'NA' values, which we can do using the 
# 'lapply()' function
(quadInv_list <- lapply(X = quadInv_list, FUN = function(x) x[is.na(x) == FALSE]))

## ----echo = FALSE, fig.width=8, fig.height = 3, fig.align = 'center', fig.cap = "**Figure 2.1**: *A visualization of the 'dormancy' scenario described above.*"----

exampleSmall <- exampleDat[unique(c(30, 42, 44, 25,  2, 61, 59, 52, 45,  8, 42, 37, 60, 34, 45, 38, 26, 49, 52, 46)),]

exampleSmall$FocalInd <- 'other individual'
exampleSmall[10,'FocalInd'] <- 'focal individual'
exampleSmall$Year <- "Year 1"
exampleSmall_2 <- exampleSmall[c(1:9,11:nrow(exampleSmall)),]
exampleSmall_2$Year <- "Year 2"
exampleSmall_3 <- exampleSmall
exampleSmall_3$Year <- "Year 3"

exampleSmall <- rbind(exampleSmall, exampleSmall_2, exampleSmall_3)

ggplot(data = exampleSmall) +
  geom_sf(aes(color = FocalInd, fill = FocalInd)) +
  geom_segment(aes(x = 0, xend = .6, y = 0, yend = 0), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 0, xend = .6, y = .6, yend = .6), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = .6), size = .5, 
               lineend = "round", color = "grey30") + 
  geom_segment(aes(x = .6, xend = .6, y = 0, yend = .6), size = .5, 
               lineend = "round", color = "grey30") + 
  xlab("quadrat horizontal edge (m)") +
  ylab("quadrat vertical edge (m)") +
  xlim(c(0,.6)) +
  ylim(c(0,.6)) +
  #labs(title = Year) +
  facet_wrap( ~ Year) +
  theme_classic() +
  theme(axis.line = element_blank(), 
        legend.text = element_text(face = "italic"),
        plot.margin = margin(1,0,1,0),
        legend.title = element_blank()) +
  scale_fill_discrete(type = c("#E69F00", "#A6A6A6", "#009E73"))  +
  scale_color_discrete(type = c("#E69F00", "#A6A6A6", "#009E73"))

## ----echo = FALSE-------------------------------------------------------------
dormDF <- data.frame("Species" = c("tree A", "shrub B", "tree C", "forb D", "forb E", "forb F"),
           "dorm" = c(0,0,0,1,2,1))
(dormDF)
# knitr::kable(dormDF, caption = "**Table 2.1**: Example 'dorm' data.frame", )

## ----echo = FALSE, warning= FALSE, fig.width=8, fig.height = 3, fig.align = 'center', fig.cap = "**Figure 2.2**: *With a 10 cm buffer, these polygons in 1922 and 1923 overlap and will be identified by trackSpp() as the **same** individual and receive the same trackID.*"----

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

## ----echo = FALSE, warning= FALSE, fig.width=8, fig.height = 3, fig.align = 'center', fig.cap = "**Figure 2.3**: *With a 3 cm buffer, these polygons in 1922 and 1923 don't quite overlap, so will be identified by trackSpp() as **different** individuals and receive different trackIDs.*"----

exampleBuffed <- st_buffer(exampleDatIDsTemp[round(exampleDatIDsTemp$Area, 7) ==0.0005472,], dist = .03)
exampleBuffed$Year <- 1922
exampleBuffed$ghost <- "3 cm buffer"
exampleBuffedNext <- exampleBuffed
exampleBuffedNext$Year <- 1923
ghost <- exampleDatIDsTemp[round(exampleDatIDsTemp$Area, 7) ==0.0005472,]
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
               lineend = "round", color = "grey30") + 
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

## -----------------------------------------------------------------------------
datTrackSpp <- plantTracker::trackSpp(dat = dat, inv = inv,
         dorm = 1,
         buff = .05,
         buffGenet = .005,
         clonal = data.frame("Species" = c("Heteropogon contortus",
                                           "Bouteloua rothrockii",
                                           "Ambrosia artemisiifolia",
                                           "Calliandra eriophylla"),
                             "clonal" = c(TRUE,TRUE,FALSE,FALSE)),
         aggByGenet = TRUE,
         printMessages = FALSE
         )

## ---- echo = FALSE------------------------------------------------------------
temp <- datTrackSpp
names(temp)[c(7,9,11)] <- c("basalArea", "survives_t+1", "size_t+1")
(temp)
#knitr::kable(head(temp[100:120,]))

## ----echo = FALSE, fig.width=7, fig.align = 'center', fig.cap = "**Figure 3.1**: *This individual outlined in pink is a focal individual, and the pale pink shows a 10 cm buffer around it.*"----
datComp <- datTrackSpp[datTrackSpp$Year == 1922,]

ggplot(datComp) +
  geom_sf(data = 
            sf::st_buffer(datComp[datComp$trackID == "HETCON_1922_6",], .1), 
          aes(), color = "#CC79A7", fill = "#CC79A7", alpha = .3, lty = 2) +
  geom_sf(aes(fill = Species)) +
   geom_sf(data = datComp[datComp$trackID == "HETCON_1922_6",], 
          aes(), color = "#CC79A7", fill = "#CC79A7", alpha = 0, lwd = 1.5) +
  xlim(c(0,.5)) +
  ylim(c(.2,.7)) +
  xlab("quadrat horizontal edge (m)") +
  ylab("quadrat vertical edge (m)") +
  theme_classic() +
  theme(legend.text = element_text(face = "italic"),
        plot.margin = margin(1,0,1,0),
        legend.title = element_blank()) +
  scale_fill_discrete(type = c( "#009E73", "#F0E442", "#E69F00",  "#56B4E9","#A6A6A6")) +
 scale_color_discrete(type = c( "#009E73", "#F0E442","#E69F00",  "#56B4E9", "#A6A6A6")) 

## ----echo = FALSE, warnings = FALSE, include=FALSE----------------------------
suppressWarnings(st_intersects(st_buffer(datComp[datComp$trackID == "HETCON_1922_6",], .1), datComp))
countMethod <- datComp[c(3,5,63,67,70),]
areaMethodTemp <- suppressWarnings(suppressMessages(st_intersection(st_buffer(
  datComp[datComp$trackID == "HETCON_1922_6",], .1), datComp)))
areaMethod <- areaMethodTemp[st_is(areaMethodTemp, c("POLYGON", "MULTIPOLYGON")),]


## ----echo = FALSE, warnings = FALSE, fig.width=7, fig.align = 'center', fig.cap = "**Figure 3.2**: *The 10cm buffer around the focal individual overlaps with 5 other unique individuals of two species. These overlapping individuals are outlined in dark grey. Using the 'count' method in `getNeighbors()`, we would get an intraspecific competition value of 3, and an interspecific competition value of 5.*"----
ggplot(datComp) +
  geom_sf(data = 
            suppressWarnings(sf::st_buffer(datComp[datComp$trackID == "HETCON_1922_6",], .1)), 
          aes(), color = "#CC79A7", fill = "#CC79A7", alpha = .3, lty = 2) +
  geom_sf(aes(fill = Species)) +
  geom_sf(data = datComp[datComp$trackID == "HETCON_1922_6",], 
          aes(), color = "#CC79A7", fill = "#CC79A7", alpha = 0, lwd = 1.5) +
  geom_sf(data = countMethod, aes(), alpha = 0, lwd = 1.5) +
  geom_sf_label(data = countMethod, label = c(1,2,3,4,5), nudge_x = .035, 
                label.padding = unit(.15, "lines"),
                label.size = unit(.05,"mm")) +
  xlim(c(0,.5)) +
  ylim(c(.2,.7)) +
  xlab("quadrat horizontal edge (m)") +
  ylab("quadrat vertical edge (m)") +
  theme_classic() +
  theme(legend.text = element_text(face = "italic"),
        plot.margin = margin(1,0,1,0),
        legend.title = element_blank()) +
  scale_fill_discrete(type = c( "#009E73", "#F0E442", "#E69F00",  "#56B4E9","#A6A6A6")) +
 scale_color_discrete(type = c( "#009E73", "#F0E442","#E69F00",  "#56B4E9", "#A6A6A6")) 

## ----echo = FALSE, warnings = FALSE, fig.width=7, fig.align = 'center', fig.cap = "**Figure 3.3**: *The 10cm buffer around the focal individual overlaps with 5 other unique individuals of two species. The overlapping area is shaded in grey. Using the 'area' method in `getNeighbors()`, we would get an intraspecific competition metric of 0.0454, and an interspecific competition metric of 0.0462.*"----
areaMethodTemp <- suppressWarnings(st_intersection(st_buffer(
  datComp[datComp$trackID == "HETCON_1922_6",], .1), datComp))
areaMethod <- areaMethodTemp[areaMethodTemp$trackID.1 != "HETCON_1922_6",]

ggplot(datComp) +
  geom_sf(data = 
            suppressWarnings(sf::st_buffer(datComp[datComp$trackID == "HETCON_1922_6",], .1)), 
          aes(), color = "#CC79A7", fill = "#CC79A7", alpha = .3, lty = 2) +
  geom_sf(aes(fill = Species)) +
  geom_sf(data = datComp[datComp$trackID == "HETCON_1922_6",], 
          aes(), color = "#CC79A7", fill = "#CC79A7", alpha = 0, lwd = 1.5) +
  geom_sf(data = areaMethod, aes(), fill = "#A6A6A6", color = "grey50", alpha = 0.7, lwd = 1.5) +
  xlim(c(0,.5)) +
  ylim(c(.2,.7)) +
  xlab("quadrat horizontal edge (m)") +
  ylab("quadrat vertical edge (m)") +
  theme_classic() +
  theme(legend.text = element_text(face = "italic"),
        plot.margin = margin(1,0,1,0),
        legend.title = element_blank()) +
  scale_fill_discrete(type = c( "#009E73", "#F0E442", "#E69F00",  "#56B4E9","#A6A6A6")) +
 scale_color_discrete(type = c( "#009E73", "#F0E442","#E69F00",  "#56B4E9", "#A6A6A6")) 

## basal area of focal individual = 0.004981431 
## area of buffer 0.06953875 - 0.004981431 =  0.06455732
## area of interspecific competitors  2.504885e-05 + 9.549092e-04 + 1.952117e-03 + 2.461883e-05 + 2.461883e-05 = 0.002981313
## area of intraspecific competitors 2.504885e-05 + 9.549092e-04 + 1.952117e-03 = 0.002932075
## proportion of interspecific competitors 0.002981313 / 0.06455732 = 0.04618087
## proportion of intraspecific competitors 0.002932075 / 0.06455732 = 0.04541816

## -----------------------------------------------------------------------------
datNeighbors <- plantTracker::getNeighbors(dat = datTrackSpp,
             buff = .15,
             method = "area",
             compType = "allSpp")

## ---- echo = FALSE------------------------------------------------------------
temp <- datNeighbors
rownames(temp) <- NULL
names(temp)[c(8,10,12)] <- c("basalArea", "survives_t+1", "size_t+1")
(temp)
#knitr::kable(head(temp[100:110,]), row.names = FALSE)

## -----------------------------------------------------------------------------
# save the output of the getNeighbors() function
datNeighbors_bySpp <- plantTracker::getNeighbors(dat = datTrackSpp,
             buff = .15, method = "area", compType = "allSpp", output = "bySpecies")

# determine all of the possible species that can occupy the buffer zone
compSpp <- unique(datTrackSpp$Species)

temp <- lapply(X = datNeighbors_bySpp$neighbors_area,  FUN = function(x) {
  tmp <- unlist(x)
  tmp2 <- tmp[compSpp]
  }
)

for (i in 1:length(temp)) {
  # fix the column names
  names(temp[[i]]) <- compSpp
  # save the data in a matrix
  if (i == 1) {
    datOut <- temp[[i]]
  } else {
    datOut <- rbind(datOut, temp[[i]])
  }
}
# make the rownames of the matrix correspond to the trackID of the focal individual
rownames(datOut) <- datNeighbors_bySpp$trackID

# show the first few rows of the datOut data frame: 
datOut[1:5,]

