## ----setup, echo = FALSE, include=FALSE---------------------------------------
library(knitr)
knitr::opts_chunk$set(collapse = TRUE,message=FALSE, warning = FALSE,
                      comment = "#>")

## ----eval = FALSE-------------------------------------------------------------
#  install.packages("phyloraster")

## ----eval = FALSE-------------------------------------------------------------
#  devtools::install_github("gabferreira/phyloraster")

## ----warning = FALSE, message = FALSE-----------------------------------------
library(phyloraster)
library(terra)
library(ape)
library(phylobase)


## -----------------------------------------------------------------------------
data <- load.data.rosauer()
head(data$presab[,1:7])

## ----fig.height = 5, fig.width = 5, fig.align = 'center'----------------------
plot(data$raster[[1]], cex = 0.65)

## -----------------------------------------------------------------------------
data$tree

## ----fig.height = 5, fig.width = 5, fig.align = 'center'----------------------
plot(data$tree, cex = 0.65)

## -----------------------------------------------------------------------------
data <- load.data.rosauer()
r <- df2rast(x = data$presab, 
             CRS = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
class(r)

## ----fig.height = 5, fig.width = 5, fig.align = 'center'----------------------
plot(r)

## -----------------------------------------------------------------------------
shp <- terra::vect(system.file("extdata", "shps_iucn_spps_rosauer.shp", 
                               package = "phyloraster"))

## ----fig.height = 5, fig.width = 5, fig.align = 'center'----------------------
colors <- rainbow(length(unique(shp$BINOMIAL)),
                  alpha = 0.5)
position <- match(shp$BINOMIAL,
                  unique(shp$BINOMIAL))
colors <- colors[position]
plot(shp, col = colors, lty = 0,
     main = "Spatial polygons")
library(maps)
maps::map(add = TRUE)

## ----message = F--------------------------------------------------------------
r2 <- shp2rast(shp, sps.col = "BINOMIAL", ymask = FALSE, background = 0, 
               resolution = 0.5)
r2
plot(r2[[9]])

## ----message = F--------------------------------------------------------------
library(terra)

shp <- terra::vect(system.file("extdata", "shps_iucn_spps_rosauer.shp",
                               package="phyloraster"))

# create a polygon to use as mask with an extent
e <- terra::ext(113, 123, -43.64, -33.90)
p <- terra::as.polygons(e, crs="")
# cut by the total extension of the polygons
coun.crop <- terra::crop(p, 
                         terra::ext(shp)) 
coun.rast <- terra::rasterize(coun.crop,
                              terra::rast(terra::ext(shp), resolution = 0.5))

# rasterizing with the mask of the polygon
shp.t <- shp2rast(shp, y = coun.rast, sps.col = "BINOMIAL", ymask = TRUE)
plot(shp.t[[1]])

## -----------------------------------------------------------------------------
data <- load.data.rosauer()
names(data$raster) == data$tree$tip.label

## -----------------------------------------------------------------------------
ras <- terra::rast(system.file("extdata", "rast.presab.tif", 
                               package = "phyloraster"))
tree <- ape::read.tree(system.file("extdata", "tree.nex", 
                                   package = "phyloraster"))
dataprep <- phylo.pres(x = ras, tree = tree)

## -----------------------------------------------------------------------------
names(dataprep$x) == tree$tip.label

## -----------------------------------------------------------------------------
ras <- terra::rast(system.file("extdata", "rast.presab.tif", 
                               package = "phyloraster"))
tree <- ape::read.tree(system.file("extdata", "tree.nex", 
                                   package = "phyloraster"))

# Using the full tree
dataprep_full <- phylo.pres(x = ras, tree = tree, full_tree_metr = TRUE)

# Using the prunned tree
dataprep_sub <- phylo.pres(x = ras, tree = tree, full_tree_metr = FALSE) 

## ----fig.height = 5, fig.width = 4, fig.align = 'center', warning= FALSE, echo = FALSE----
knitr::include_graphics("figs/tree.jpg")

## ----warning= FALSE-----------------------------------------------------------
ras <- terra::rast(system.file("extdata", "rast.presab.tif", 
                               package = "phyloraster"))
sr <- rast.sr(x = ras)
sr

## ----plot, fig.height = 5, fig.width = 7, fig.align = 'center', warning= FALSE----
plot(sr, main = "Species richness")

## ----warning= FALSE-----------------------------------------------------------
ras <- terra::rast(system.file("extdata", "rast.presab.tif", 
                               package = "phyloraster"))
wer <- rast.we(x = ras)

## ----fig.height = 5, fig.width = 7, fig.align = 'center', warning= FALSE------
wer$WE
plot(wer$WE, main ="Weigthed Endemism")

## ----pdr, warning= FALSE------------------------------------------------------
ras <- terra::rast(system.file("extdata", "rast.presab.tif", 
                               package = "phyloraster"))
tree <- ape::read.tree(system.file("extdata", "tree.nex", 
                                   package = "phyloraster"))
dataprep <- phylo.pres(x = ras, tree = tree, full_tree_metr = TRUE)

pdr <- rast.pd(x = dataprep$x, dataprep$tree)

## ----pdr-plot, fig.height = 5, fig.width = 7, fig.align = 'center', warning= FALSE----
plot(pdr$PD, main = "Phylogenetic diversity")

## ----warning= FALSE-----------------------------------------------------------
ras <- terra::rast(system.file("extdata", "rast.presab.tif", 
                               package = "phyloraster"))
tree <- ape::read.tree(system.file("extdata", "tree.nex", 
                                   package = "phyloraster"))
dataprep <- phylo.pres(x = ras, tree = tree, full_tree_metr = TRUE)

per <- rast.pe(x = dataprep$x, dataprep$tree)
per

## ----per-plot, fig.height = 5, fig.width = 7, fig.align = 'center', warning= FALSE----
plot(per$PE, main = "Phylogenetic Endemism")

## ----warning= FALSE-----------------------------------------------------------
x <- terra::rast(system.file("extdata", "rast.presab.tif", 
                             package="phyloraster"))
# phylogenetic tree
tree <- ape::read.tree(system.file("extdata", "tree.nex", 
                                   package="phyloraster"))
dataprep <- phylo.pres(x = ras, tree = tree, full_tree_metr = TRUE)

ed <- rast.ed(dataprep$x, dataprep$tree)
ed

## ----edr-plot, fig.height = 5, fig.width = 7, fig.align = 'center', warning= TRUE----
terra::plot(ed, main = "Evolutionary Distinctiveness")

## ----warning= FALSE-----------------------------------------------------------
tree <- ape::read.tree(system.file("extdata", "tree.nex", 
                                   package="phyloraster"))

ed <- phyloraster::species.ed(tree)
head(ed)

## ----sr-plot------------------------------------------------------------------
# load the data
x <- terra::rast(system.file("extdata", "rast.presab.tif", 
                             package="phyloraster"))
# richness
riq.pres <- rast.sr(x)
plot(riq.pres)

## ----srf-plot-----------------------------------------------------------------
# load the data
x <- terra::rast(system.file("extdata", "rast.presab.tif", 
                             package="phyloraster"))
# richness future
riq.fut <- rast.sr(x[[c(1:15)]]) # imagine we lost some species in the future
terra::plot(riq.fut)

## ----dg-----------------------------------------------------------------------
dg <- delta.grid(riq.pres, riq.fut)
plot(dg)

## -----------------------------------------------------------------------------
library(SESraster)
ras <- terra::rast(system.file("extdata", "rast.presab.tif", 
                               package = "phyloraster"))
tree <- ape::read.tree(system.file("extdata", "tree.nex", 
                                   package = "phyloraster"))
dataprep <- phylo.pres(ras, tree, full_tree_metr = TRUE)

t <- rast.pd.ses(dataprep$x, edge.path = dataprep$edge.path, 
                 branch.length = dataprep$branch.length, aleats = 10)

## ----pds-plot, fig.height = 5, fig.width = 7, fig.align = 'center', warning= FALSE----
plot(t)

## -----------------------------------------------------------------------------
ras <- terra::rast(system.file("extdata", "rast.presab.tif",
                               package="phyloraster"))
tree <- ape::read.tree(system.file("extdata", "tree.nex",
                                   package="phyloraster"))
data <- phylo.pres(ras, tree, full_tree_metr = TRUE)

## -----------------------------------------------------------------------------
ses <- rast.pe.ses(x = data$x, tree = data$tree, aleats = 30, metric = "all")
names(ses)

## -----------------------------------------------------------------------------
# CANAPE
canape <- canape.rast(ses$p.upper.PE, ses$p.upper.PE.alt,
ses$p.upper.RPE, ses$p.lower.RPE)
terra::plot(canape)

