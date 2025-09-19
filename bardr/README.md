# bardr: Shakespeare's Complete Works for R

Have you ever felt that the R programming language suffered from a critical
lack of the Bard? Well, worry no more.

The bardr package provides the complete text of William Shakespeare's
complete works as native R files which have already been substantially
pre-processed to make them easy to use.

The complete works are contained in both a list and a tidy data frame format,
and each individual work has also been separated and stored as a character
vector.

## Quick-Start Guide

Getting started with **bardr** is easy! Just install the package (currently
by using `devtools::install_github()`, hopefully the package will be on CRAN
soon, though). Then, you can access any of the included data sources like so:
`works <- bardr::all_works_df`
or any of the other files.

## Data Sources with all works

* `all_works_df`: a tidy dataframe containing all works.
* `all_works_list`: a named list containing all works.

## Data source names for specific works

Every work included in the Project Gutenberg collection of Shakespeare's
Complete Works has a dedicated individual data source. The naming convention is
snake case (all_lower_case_with_underscores) and no punctuation.
