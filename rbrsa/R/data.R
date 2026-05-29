# BDDK Package Metadata
# Data objects are created by data-raw/create_internal_data.R 
# and stored in R/sysdata.rda as internal data.
# This approach preserves Turkish characters while maintaining ASCII-only R files.

# Internal data: bddk_groups, finturk_groups, bddk_tables, 
# finturk_tables, finturk_cities

# These objects are not exported but available internally via:
# get("bddk_groups", envir = asNamespace("rbrsa"))