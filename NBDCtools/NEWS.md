# NBDCtools 1.0.1

## Bug fixes

- Fixed an error in the `transf_value_to_label()` function that led to the order
  of categorical levels being incorrect after transformation to labels.
- Fixed an error in the `join_tabulated()` function that occurred if the first
  table/variable in `tables_add`/`vars_add` was static but further
  tables/variables were dynamic.

## Changes

- all `join_by`s in `join_tabulated()` are now dynamically set by intersecting
  with `id_cols`. Hardcoded `join_by` has been removed.

# NBDCtools 1.0.0

## New Features

The initial release of the NBDCtools package.
