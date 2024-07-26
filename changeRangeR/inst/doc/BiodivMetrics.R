## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = F)

## ----results='hide'-----------------------------------------------------------
library(raster)
library(picante)
library(phylobase)
library(sf)
library(dplyr)
library(changeRangeR)
library(ape)

## -----------------------------------------------------------------------------
# Load binary maps/SDMs
primRas <- raster::stack(list.files(path = paste0(system.file(package="changeRangeR"), "/extdata/DemoData/SDM/colPrimates_binary"), pattern = "\\.tif$", full.names = T))
# Because of species' name resolutions in the phylogeny we use here, we need to rename some of the species in this rasterStack
names(primRas) <- gsub("*_veg_coarse", "", names(primRas))
# Drop models that are resolved into one group; Cebus_albifrons
primRas <- dropLayer(primRas, c("Cebus_versicolor", "Cebus_leucocephalus", "Cebus_malitiosus", "Cebus_albifrons"))
oldnames <- names(primRas)
namesToChange <- c("Cebus_all_albifrons", "Cheracebus_lugens", "Cheracebus_medemi", "Lagothrix_lagothricha", "Leontocebus_fuscus", "Plecturocebus_caquetensis", "Plecturocebus_discolor", "Plecturocebus_ornatus", "Saimiri_cassiquiarensis")
newNames <- c("Cebus_albifrons", "Callicebus_lugens", "Callicebus_medemi", "Lagothrix_lagotricha", "Leontocebus_fuscicollis", "Callicebus_caquetensis", "Callicebus_discolor", "Callicebus_ornatus", "Saimiri_sciureus")
oldnames[oldnames %in% namesToChange] <- newNames
names(primRas) <- oldnames

## -----------------------------------------------------------------------------
SR <- sum(primRas, na.rm = T)
plot(SR)

## -----------------------------------------------------------------------------
# Load binary maps/SDMs
primRas <- stack(list.files(path = paste0(system.file(package="changeRangeR"), "/extdata/DemoData/SDM/colPrimates_binary"), pattern = "\\.tif$", full.names = T))
# Because of species' name resolutions in the phylogeny we use here, we need to rename some of the species in this rasterStack
names(primRas) <- gsub("*_veg_coarse", "", names(primRas))
# Drop models that are resolved into one group; Cebus_albifrons
primRas <- dropLayer(primRas, c("Cebus_versicolor", "Cebus_leucocephalus", "Cebus_malitiosus", "Cebus_albifrons"))
oldnames <- names(primRas)
namesToChange <- c("Cebus_all_albifrons", "Cheracebus_lugens", "Cheracebus_medemi", "Lagothrix_lagothricha", "Leontocebus_fuscus", "Plecturocebus_caquetensis", "Plecturocebus_discolor", "Plecturocebus_ornatus", "Saimiri_cassiquiarensis")
newNames <- c("Cebus_albifrons", "Callicebus_lugens", "Callicebus_medemi", "Lagothrix_lagotricha", "Leontocebus_fuscicollis", "Callicebus_caquetensis", "Callicebus_discolor", "Callicebus_ornatus", "Saimiri_sciureus")
oldnames[oldnames %in% namesToChange] <- newNames
names(primRas) <- oldnames

## -----------------------------------------------------------------------------
e <- extent(c(-75, -70, 0, 2.5))
primRas <- crop(primRas, e)

## -----------------------------------------------------------------------------
primTree <- read.nexus(paste0(system.file(package="changeRangeR"), "/extdata/DemoData/phyloTree/output.nex"))

## -----------------------------------------------------------------------------
# set all cells that have NA values to 0
for (i in 1:nlayers(primRas)){
  primRas[[i]][is.na(primRas[[i]])] <- 0
}

## -----------------------------------------------------------------------------
# convert raster to dataframe
commDat <- as.data.frame(primRas)
# remove rows that are NA
commDat <- na.omit(commDat)
row.names(commDat) <- 1:nrow(commDat)

## -----------------------------------------------------------------------------
user_phylogeny <- primTree[1]$tree_6532
phydiv <- pd(samp = commDat, tree = user_phylogeny, include.root = TRUE)

## -----------------------------------------------------------------------------
commDat$pd <- phydiv$PD

## -----------------------------------------------------------------------------
pd_ras <- sum(primRas, na.rm=T)
pd_ras[!is.na(pd_ras)] <- commDat$pd
pd_ras[pd_ras == 0] <- NA
plot(pd_ras)

## -----------------------------------------------------------------------------
# Load binary maps/SDMs
primRas <- stack(list.files(path = paste0(system.file(package="changeRangeR"), "/extdata/DemoData/SDM/colPrimates_binary"), pattern = "\\.tif$", full.names = T))
# Because of species' name resolutions in the phylogeny we use here, we need to rename some of the species in this rasterStack
names(primRas) <- gsub("*_veg_coarse", "", names(primRas))
# Drop models that are resolved into one group; Cebus_albifrons
primRas <- dropLayer(primRas, c("Cebus_versicolor", "Cebus_leucocephalus", "Cebus_malitiosus", "Cebus_albifrons"))
oldnames <- names(primRas)
namesToChange <- c("Cebus_all_albifrons", "Cheracebus_lugens", "Cheracebus_medemi", "Lagothrix_lagothricha", "Leontocebus_fuscus", "Plecturocebus_caquetensis", "Plecturocebus_discolor", "Plecturocebus_ornatus", "Saimiri_cassiquiarensis")
newNames <- c("Cebus_albifrons", "Callicebus_lugens", "Callicebus_medemi", "Lagothrix_lagotricha", "Leontocebus_fuscicollis", "Callicebus_caquetensis", "Callicebus_discolor", "Callicebus_ornatus", "Saimiri_sciureus")
oldnames[oldnames %in% namesToChange] <- newNames
names(primRas) <- oldnames

## -----------------------------------------------------------------------------
e <- extent(c(-75, -70, 0, 2.5))
primRas <- crop(primRas, e)

## -----------------------------------------------------------------------------
primTree <- read.nexus(paste0(system.file(package="changeRangeR"), "/extdata/DemoData/phyloTree/output.nex"))

## -----------------------------------------------------------------------------
## Convert the binary primate SDMs to a point data.frame, removing the X and Y data. 
Allxy <- rasterToPoints(primRas)
sites <- as.data.frame(Allxy[,1:ncol(Allxy)])
## Change all NA values to 0
sites[is.na(sites)] <- 0

## -----------------------------------------------------------------------------
pEprimates <- calc_PE(phylo.tree = primTree[[1]], sites_x_tips = sites, presence = "presence")

## -----------------------------------------------------------------------------
## cbind the pixel centroids with PE values and convert to a raster
PExyz <- cbind(Allxy[,1:2], pEprimates$PE)
PE.ras <- rasterFromXYZ(PExyz)
PE.ras[PE.ras == 0] <- NA
plot(PE.ras)

## -----------------------------------------------------------------------------
# Load binary maps/SDMs
primRas <- stack(list.files(path = paste0(system.file(package="changeRangeR"), "/extdata/DemoData/SDM/colPrimates_binary"), pattern = "\\.tif$", full.names = T))
# Because of species' name resolutions in the phylogeny we use here, we need to rename some of the species in this rasterStack
names(primRas) <- gsub("*_veg_coarse", "", names(primRas))
# Drop models that are resolved into one group; Cebus_albifrons
primRas <- dropLayer(primRas, c("Cebus_versicolor", "Cebus_leucocephalus", "Cebus_malitiosus", "Cebus_albifrons"))
oldnames <- names(primRas)
namesToChange <- c("Cebus_all_albifrons", "Cheracebus_lugens", "Cheracebus_medemi", "Lagothrix_lagothricha", "Leontocebus_fuscus", "Plecturocebus_caquetensis", "Plecturocebus_discolor", "Plecturocebus_ornatus", "Saimiri_cassiquiarensis")
newNames <- c("Cebus_albifrons", "Callicebus_lugens", "Callicebus_medemi", "Lagothrix_lagotricha", "Leontocebus_fuscicollis", "Callicebus_caquetensis", "Callicebus_discolor", "Callicebus_ornatus", "Saimiri_sciureus")
oldnames[oldnames %in% namesToChange] <- newNames
names(primRas) <- oldnames

## -----------------------------------------------------------------------------
e <- extent(c(-75, -70, 0, 2.5))
primRas <- crop(primRas, e)

## -----------------------------------------------------------------------------
sp.End <- SpeciesEndemism(primRas)
plot(sp.End)

## -----------------------------------------------------------------------------
# Load binary maps/SDMs
primRas <- stack(list.files(path = paste0(system.file(package="changeRangeR"), "/extdata/DemoData/SDM/colPrimates_binary"), pattern = "\\.tif$", full.names = T))
# Because of species' name resolutions in the phylogeny we use here, we need to rename some of the species in this rasterStack
names(primRas) <- gsub("*_veg_coarse", "", names(primRas))
# Drop models that are resolved into one group; Cebus_albifrons
primRas <- dropLayer(primRas, c("Cebus_versicolor", "Cebus_leucocephalus", "Cebus_malitiosus", "Cebus_albifrons"))
oldnames <- names(primRas)
namesToChange <- c("Cebus_all_albifrons", "Cheracebus_lugens", "Cheracebus_medemi", "Lagothrix_lagothricha", "Leontocebus_fuscus", "Plecturocebus_caquetensis", "Plecturocebus_discolor", "Plecturocebus_ornatus", "Saimiri_cassiquiarensis")
newNames <- c("Cebus_albifrons", "Callicebus_lugens", "Callicebus_medemi", "Lagothrix_lagotricha", "Leontocebus_fuscicollis", "Callicebus_caquetensis", "Callicebus_discolor", "Callicebus_ornatus", "Saimiri_sciureus")
oldnames[oldnames %in% namesToChange] <- newNames
names(primRas) <- oldnames

## -----------------------------------------------------------------------------
SR <- sum(primRas, na.rm = T)
SR[SR == 0] <- NA

## -----------------------------------------------------------------------------
thresholdValue <- quantile(SR, probs = 0.5)

## -----------------------------------------------------------------------------
SR[SR < thresholdValue] <- NA
plot(SR)

## -----------------------------------------------------------------------------
PAs <- readRDS(file.path(system.file(package="changeRangeR"), "extdata/DemoData/shapefiles", "WDPA_COL_olinguito_simp.rds"))
# View the fields
colnames(PAs)
# Pick the field you are interested in
field <- "DESIG_ENG"
category <- "All"
ratio.Overlap <- ratioOverlap(r = SR, shp = PAs, field = field, category = category, subfield = FALSE)
print(ratio.Overlap)

## -----------------------------------------------------------------------------
SR <- sum(primRas, na.rm = T)
SR[SR == 0] <- NA

## -----------------------------------------------------------------------------
PAs <- readRDS(file.path(system.file(package="changeRangeR"), "extdata/DemoData/shapefiles", "WDPA_COL_olinguito_simp.rds"))
# View the fields
colnames(PAs)
# Pick the field you are interested in
field <- "DESIG_ENG"
category <- "All"
# Mask species richness by protected areas to find richness within protected areas
rmask <- raster::mask(SR, PAs)
# Take a look
plot(SR)
plot(PAs, add = T)

comp <- complementarity(ras1 = SR, ras1mask = rmask)
print(comp)

