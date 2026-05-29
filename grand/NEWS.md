---
title: "NEWS"
output: html_document
---

## grand 0.9.1

* functions renamed in snake_case, to match naming conventions in `igraph`

* restructured `grand()` as a wrapper that provides access to both interactive "interview" to add information, and to statement generation, which are now internal functions

* eliminated support for table generation in `grand_table()` and for reporting topological statistics because these are not well-defined yet

* eliminated example data to reduce package size; examples now use `igraph` generated data

* interactive GRAND information entry allows users to update existing information

* corrected usage of \link{} in documentation

## grand 0.9.0

* initial release
