# qlcVisualize 0.4

* complete rewrite of weightedMap
* catch erratic and unexplicable failing of concaveman in weightedMap
* use two rounds of cartogram to prevent errors with complex maps
* added second round of voronoi in weightedMap to regularize result
* adding a worldwide polygon that can be used as a window for all glottolog languages
* adding function addContour in preparation for rewrite of levelMap
* various bugfixes

# qlcVisualize 0.3

* completely rework vmap approach: new approach is weightedMap.
* vmap, voronoi, mapsToOwin, gadmToOwin, hullToOwin are deprecated
* renamed function limage to factorMap, the name limage is deprecated
* function lmap is renamed to levelMap
* shortened functions calls are available: lmap, fmap, wmap

# qlcVisualize 0.2.1

* bugfixes to get the package back on CRAN
* trying to adapt to the ongoing changes in spatial methods/formats in R

# qlcVisualize 0.2

* selectively make pies in lmap only for those points that are multi-valued
* adding options to normalize data in lmap
* adding option 'ignore.others' in lmap
* reorder plotting in lmap
* allow for single row or column in limage
* bugfixes in vmap and hullToOwin

# qlcVisualize 0.1.0

* initial alpha version on CRAN

