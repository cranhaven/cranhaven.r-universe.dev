# Purpose

The goal of tidytidbits is to complement the environment provided by the tidyverse packages with a set of higher-level
functions within the scope of data analysis and plotting.

# Installation

Install from CRAN:
```r
install.packages("tidytidbits")
```

Or install the latest git version from bitbucket:
```r
devtools::install_bitbucket("mwiesweg/tidytidbits")
```

# Components

The following areas are covered:

* methods designed for use in a dplyr pipeline
    * formatting columns: format_numbers_at, format_p_values_at,  and the corresponding generic tools as_formatted_number, as_formatted_p_value, as_percentage_label
    * adding data analysis results as new columns: count_by, add_prop_test
    * cross tabulation in a pipeline: cross_tabulate
    * lumping rows of a data frame by a count: lump_rows, and the corresponding generic tool lump
    * appending and prepepnding in a pipe with different semantics: append_object, prepend_object
    * lower-level pipeline tools: add_summary, add_summary_by, execute_if, execute_in_pipeline, interlude
* convenience methos to rename and reorder a factor: rename_reorder_factor, rename_factor, order_factor_by
* vectorised and non-vectorised methods implementing usefuls notions about NA and logical values: all_or_all_na, any_or_all_na, are_true, equal_including_na, falsy, invalid, true_or_na, truthy, valid
* vectorised methods dealing with data frame columns potentially containing NA values: first_non_nas, first_not, first_not_na, first_which_not_na, which_non_na
* a vectorised lookup method for use with dictionary-like vectors: lookup and its type-specific variants
* some accessors for use with purrr's pluck or the additionally provided pluck_vector: name_contains, named, value_contains
* generic tools: invert_value_and_names, str_locate_match
* dealing with sequential duplicates: sequential_duplicates, replace_sequential_duplicates
* for plotting:
    * conveniently saving a plot from a pipe: save_pdf, save_png
    * convenience methods for safe directory generation: prepare_path, prepare_directory
    * convenience method giving frequently used paper sizes: dinA_format, dinA_height, dinA_width
    * convenience method to create a named palette: named_palette and for creating a qualitative palette with many entries: iwanthue_palette
* a notion of having a method which provides its local variables for reuse in other methods: local_variables and the corresponding source_variables
* a python-style tuple-assignment implementation `g(a, b) %=% function_returning_vector_of_two_elements()`
* some low-level methods utilizing rlang features: eval_unquoted, expression_list, print_deparsed, quosure_list, symbol_as_quosure, symbol_string_list, syntactically_safe

