# genular 1.0.1

- Improved vignette documentation and examples for all functions.

# Genular R API Package 1.0.0

## Initial Release

### New Features
- Implemented `cells_suggest()` function for retrieving cell matches based on query values.
- Added `cells_search()` function to search for cells and retrieve associated gene data, including expression profiles.
- Introduced `cells_to_gene_signature()` function to fetch genes associated with specific cells and filter them based on fold change thresholds.
- Implemented seamless API integration with Genular, including automatic handling of query formatting, cell lineage searches, and result aggregation.
- Integrated R functions for processing gene expression data, including:
  - `dplyr`-based data transformations for filtering and summarizing results.
  - `tidyr`-based pivoting functions to reshape data for further analysis.

### Other Additions
- Included documentation and examples for each function.
- Created input validation and debugging options for all API-related functions.
- Added the ability to configure API endpoints and user-specific options (e.g., timeout, API keys).
- Provided support for fetching large datasets (up to 50,000 entries) through API pagination.

### Coming Soon
- More detailed error handling for API requests.
- Advanced filtering options for gene searches.
- Support for additional organism types.

