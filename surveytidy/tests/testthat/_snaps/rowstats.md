# row_means() errors on bad na.rm argument

    Code
      mutate(d, score = row_means(c(y1, y2), na.rm = "yes"))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `score = row_means(c(y1, y2), na.rm = "yes")`.
      Caused by error in `row_means()`:
      x `na.rm` must be a single non-NA logical value.
      i Got <character> of length 1.

# row_means() errors on bad .label argument

    Code
      mutate(d, score = row_means(c(y1, y2), .label = 123))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `score = row_means(c(y1, y2), .label = 123)`.
      Caused by error in `.validate_transform_args()`:
      x `.label` must be a single character string or `NULL`.
      i Got <numeric> of length 1.

# row_means() errors on bad .description argument

    Code
      mutate(d, score = row_means(c(y1, y2), .description = TRUE))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `score = row_means(c(y1, y2), .description = TRUE)`.
      Caused by error in `.validate_transform_args()`:
      x `.description` must be a single character string or `NULL`.
      i Got <logical> of length 1.

# row_sums() errors on bad na.rm argument

    Code
      mutate(d, total = row_sums(c(y1, y2), na.rm = NA))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `total = row_sums(c(y1, y2), na.rm = NA)`.
      Caused by error in `row_sums()`:
      x `na.rm` must be a single non-NA logical value.
      i Got <logical> of length 1.

# row_sums() errors on bad .label argument

    Code
      mutate(d, total = row_sums(c(y1, y2), .label = c("a", "b")))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `total = row_sums(c(y1, y2), .label = c("a", "b"))`.
      Caused by error in `.validate_transform_args()`:
      x `.label` must be a single character string or `NULL`.
      i Got <character> of length 2.

# row_means() errors on non-numeric column selection

    Code
      mutate(d, score = row_means(c(a, b)))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `score = row_means(c(a, b))`.
      Caused by error in `row_means()`:
      x 1 selected column is not numeric: b.
      i `row_means()` requires all columns to be numeric.

# row_means() errors when .cols matches 0 columns

    Code
      mutate(d, score = row_means(starts_with("z")))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `score = row_means(starts_with("z"))`.
      Caused by error in `row_means()`:
      x `.cols` matched 0 columns.
      i `row_means()` requires at least one numeric column.

# row_sums() errors on non-numeric column selection

    Code
      mutate(d, total = row_sums(c(a, b)))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `total = row_sums(c(a, b))`.
      Caused by error in `row_sums()`:
      x 1 selected column is not numeric: b.
      i `row_sums()` requires all columns to be numeric.

# row_sums() errors when .cols matches 0 columns

    Code
      mutate(d, total = row_sums(starts_with("z")))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `total = row_sums(starts_with("z"))`.
      Caused by error in `row_sums()`:
      x `.cols` matched 0 columns.
      i `row_sums()` requires at least one numeric column.

