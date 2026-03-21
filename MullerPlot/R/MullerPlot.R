#' MullerPlot: A package for generating Muller plot from population/abundance/frequency dynamics and
#' parental/geneaolgy/phylogeny relation.
#'
#' @description This package generates Muller plot from parental/genealogy/phylogeny relation and
#' population/abundance/frequency dynamics. Muller plots are plots which combine information about
#' succession of different OTUs (genotypes, phenotypes, species, ...) and information about dynamics
#' of their abundances (populations or frequencies) over time. They are powerful and fascinating
#' tools to visualize evolutionary dynamics. They may be employed also in study of diversity and
#' its dynamics, i.e. how diversity emerges and how changes over time. They called Muller plots
#' in honor of Hermann Joseph Muller which used them to explain his idea of Muller's ratchet
#' (H.J. Muller, The American Naturalist 66.703 (1932): 118-138).
#'
#' A big difference between Muller plots and normal box plots of abundances is that a Muller plot
#' depicts not only the relative abundances but also succession of OTUs based on their
#' genealogy/phylogeny/parental relation. In a Muller plot, horizontal axis is time/generations
#' and vertical axis represents relative abundances of OTUs at the corresponding times/generations.
#' Different OTUs are usually shown with polygons with different colors and each OTU originates
#' somewhere in the middle of its parent area in order to illustrate their succession in evolutionary
#' process.
#'
#' To generate a Muller plot one needs the genealogy/phylogeny/parental relation of OTUs and
#' their abundances over time.
#'
#' MullerPlot package has the tools to generate Muller plots which clearly depict the origin of
#' successors of OTUs.
#' @docType package
#' @name MullerPlot
NULL

