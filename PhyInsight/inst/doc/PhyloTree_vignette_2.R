## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(PhyInsight)

## ---- results = "hide"--------------------------------------------------------
# query the data using the taxon name
specdata <- querySpecData("Nepenthes")

# subset results that only have nucleotides from the matK region
specdata <- subset(specdata, markercode == "matK")

# get one observation per species
specdata <- getSpeciesRepr(specdata)


## ---- results = "hide"--------------------------------------------------------
# generate a DNA bin
DNABin <- genDNABin(specdata)

# use the DNA bin to create a DNA string set
DNAStringset <- genDNAStringSet(DNABin)

# automatically manipulate the DNA strings 
DNAStringSet_manip <- ManipStringSet(DNAStringset)


## -----------------------------------------------------------------------------
# use the function to remove unsuitable strings and store into an object
BadStringsRemoved <- rmBadStrings_1(
  DNAStringSet = DNAStringSet_manip,
  specimen_dataframe = specdata
)


## -----------------------------------------------------------------------------
DNAStringSet_new <- BadStringsRemoved[[1]]
specdata_new <- BadStringsRemoved[[2]]


## ---- fig.show='hold', fig.width=16, fig.height=14----------------------------
# automatically generate a phylo tree
PhyloTree <- genPhytree(DNAStringSet_new)

# change the label names
PhyloTree$tip.label <- specdata_new$species_name

# plot the phylo tree
plot(
  PhyloTree,
  label.offset = 0.0001,
  cex = 1
)


## -----------------------------------------------------------------------------
# use the function to remove unsuitable strings and remove outliers
BadStringsRemoved <- rmBadStrings_1(
  DNAStringSet = DNAStringSet_manip,
  specimen_dataframe = specdata,
  
  rmOutliers = TRUE
)


## -----------------------------------------------------------------------------
DNAStringSet_new <- BadStringsRemoved[[1]]
specdata_new <- BadStringsRemoved[[2]]


## ---- fig.show='hold', fig.width=16, fig.height=14----------------------------
# automatically generate a phylo tree
PhyloTree <- genPhytree(DNAStringSet_new)

# change the label names
PhyloTree$tip.label <- specdata_new$species_name

# plot the phylo tree
plot(
  PhyloTree,
  label.offset = 0.0001,
  cex = 1
)


## -----------------------------------------------------------------------------
# use the function to remove unsuitable strings and remove outliers
BadStringsRemoved <- rmBadStrings_1(
  DNAStringSet = DNAStringSet_manip,
  specimen_dataframe = specdata,
  
  rmOutliers = TRUE,
  
  max_Z_score = 2
)


## -----------------------------------------------------------------------------
DNAStringSet_new <- BadStringsRemoved[[1]]
specdata_new <- BadStringsRemoved[[2]]


## ---- fig.show='hold', fig.width=16, fig.height=14----------------------------
# automatically generate a phylo tree
PhyloTree <- genPhytree(DNAStringSet_new)

# change the label names
PhyloTree$tip.label <- specdata_new$species_name

# plot the phylo tree
plot(
  PhyloTree,
  label.offset = 0.0001,
  cex = 1
)


