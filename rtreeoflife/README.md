
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rtreeoflife

<!-- badges: start -->

![CRAN status](https://www.r-pkg.org/badges/version/rtreeoflife)
[![R-CMD-check](https://github.com/PaulESantos/rtreeoflife/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PaulESantos/rtreeoflife/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
![](https://cranlogs.r-pkg.org/badges/grand-total/rtreeoflife?color=blue)
![](https://cranlogs.r-pkg.org/badges/last-week/rtreeoflife?color=blue)
<!-- badges: end -->

`rtreeoflife` provides programmatic access to species records and
selected FASTA files associated with the Royal Botanic Gardens, Kew Tree
of Life Explorer. The package is designed for selective access: users
search or match species first, then download only the FASTA files needed
for an analysis.

The package includes `tol_species`, a normalized species/specimen index
derived from the Tree of Life species list. This makes common searches
available without mirroring the full Kew release locally.

## Installation

Install the released version from CRAN with:

``` r
install.packages("rtreeoflife")
```

Install the development version from GitHub with:

``` r
pak::pak("PaulESantos/rtreeoflife")
```

## Data Access Model

Kew release files are hosted at:

``` r
library(rtreeoflife)
tol_release_url()
```

The full release contains manifests, tree files, rendered tree assets,
and many FASTA files. Downloading the complete repository can be slow
and storage intensive, so the recommended workflow is:

1.  Search or match species in the bundled index.
2.  Resolve FASTA URLs for the selected records.
3.  Download to a temporary session directory.
4.  Manipulate the parsed FASTA data as tidy list-columns.
5.  Export FASTA files permanently only when needed.

## Example

This example searches three species, downloads only the available FASTA
files, summarises the recovered sequences, visualises the result with
`ggplot2`, builds an exploratory tree for one shared gene, and exports
FASTA files when the user decides to keep them.

``` r
library(dplyr)
library(ggplot2)

targets <- c(
  "Cnestis ferruginea",
  "Agelaea pentagyna",
  "Manotes expansa"
)

# Search the bundled species index.
matches <- tol_match_species(
  targets,
  multiple = "best"
)

matches |>
  select(
    requested_name,
    matched_name,
    match_type,
    has_data,
    sequence_id,
    no_of_genes_recovered,
    fasta_file_url
  )

# Keep only records with FASTA availability.
selected <- matches |>
  filter(has_data)

# Download selected FASTA files to a temporary directory.
# Increase timeout/retries for slow network connections.
download_plan <- tol_download_fasta(
  selected,
  timeout = 1200,
  retries = 5
)

# Parse FASTA files into a tidy list-column.
plan_nested <- download_plan |>
  tol_attach_fasta()

# Convert nested FASTA records to long tidy data.
fasta_long <- plan_nested |>
  tol_fasta_long()

fasta_long |>
  select(sequence_id, scientific_name, gene_id, width)

# Summarise and plot recovered FASTA content.
fasta_summary <- plan_nested |>
  tol_fasta_summary()

tol_plot_fasta_summary(fasta_summary)

# Identify shared genes and build an exploratory tree.
common_genes <- plan_nested |>
  tol_common_genes(min_records = 2)

tree_result <- plan_nested |>
  tol_build_gene_tree(
    gene_id = common_genes$gene_id[[1]],
    min_records = 2
  )

tol_plot_tree(tree_result)

# Export FASTA files permanently only if they should be retained.
exported <- download_plan |>
  tol_export_fasta(
    dest_dir = "raw-data/fasta/by_recovery",
    manifest_path = "raw-data/fasta/fasta_export_manifest.csv",
    overwrite = FALSE
  )

exported |>
  select(scientific_name, local_path, export_status)
```

## Species Search

Use exact taxonomic filters when the target group is known:

``` r
saxifraga <- tol_search_species(
  genus = "Saxifraga"
)

tol_plot_gene_recovery(saxifraga)
```

Use `tol_match_species()` when starting from a vector of scientific
names:

``` r
tol_match_species(
  c("Cnestis ferruginea", "Agelaea pentagyna", "Manotes expansa"),
  multiple = "best"
)
```

## Persistent Downloads

By default, `tol_download_fasta()` stores files in a temporary session
directory. This keeps exploratory workflows lightweight:

``` r
plan <- tol_download_fasta(
  genus = "Saxifraga",
  specific_epithet = "fortunei"
)

plan |>
  tol_attach_fasta()
```

To keep files permanently, use `tol_export_fasta()`:

``` r
plan |>
  tol_export_fasta(
    dest_dir = "raw-data/fasta/by_recovery"
  )
```

## Release Utilities

Lower level helpers are available for metadata and small scoped
downloads:

``` r
tol_known_bundles()

manifest_files <- tol_download_bundle("manifests")

species_tree <- tol_download_bundle("species_tree")

sequence_manifest <- tol_manifest(manifest_files[[1]])
```

For ordinary FASTA access, prefer the species-index workflow above
instead of downloading complete release directories.

## Citation

To cite the package and the original Kew Tree of Life Explorer data
source, use:

``` r
citation("rtreeoflife")
```

When using Kew Tree of Life Explorer data, cite the original publication
and indicate the data release used in the analysis.
