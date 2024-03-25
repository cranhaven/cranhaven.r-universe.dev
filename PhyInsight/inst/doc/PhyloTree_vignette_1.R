## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(PhyInsight)

## -----------------------------------------------------------------------------

# query the data using the taxon name
specdata <- querySpecData("Ursidae")

# check the number of DNA marker codes
sort(table(specdata$markercode))


## ---- results = "hide"--------------------------------------------------------

# subset results that only have nucleotides from the COI-5P region
specdata <- subset(specdata, markercode == "COI-5P")

# get one observation per species
specdata <- getSpeciesRepr(specdata)


## ---- results = "hide"--------------------------------------------------------

# generate a DNA bin
DNABin <- genDNABin(specdata)

# use the DNA bin to create a DNA string set
DNAStringset <- genDNAStringSet(DNABin)

# automatically manipulate the DNA strings 
DNAStringSet <- ManipStringSet(DNAStringset)


## -----------------------------------------------------------------------------

# automatically generate a phylo tree
PhyloTree <- genPhytree(DNAStringSet)


# change the label names
PhyloTree$tip.label <- paste(specdata$genus_name, "|", specdata$species_name)


## ---- fig.show='hold', fig.width=16, fig.height=14----------------------------
# plot the phylo tree
plot(
  PhyloTree,
  label.offset = 0.001,
  cex = 1.5
)


