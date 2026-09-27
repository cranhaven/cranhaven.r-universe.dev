# make_factor() errors on unsupported type (list)

    Code
      make_factor(list(1, 2, 3))
    Condition
      Error in `make_factor()`:
      x `x` must be a labelled numeric, factor, or character vector.
      i Got class <list>.

# make_factor() errors on bad arg type for ordered

    Code
      make_factor(x, ordered = "yes")
    Condition
      Error in `make_factor()`:
      x `ordered` must be a single <logical> value.
      i Got <character> of length 1.

# make_factor() errors when numeric has no labels and force = FALSE

    Code
      make_factor(x)
    Condition
      Error in `make_factor()`:
      x `x` has no value labels.
      i Numeric input requires a `labels` attribute to determine factor levels.
      v Set `force = TRUE` to coerce via `as.factor()`, or attach labels first.

# make_factor() errors when a non-NA value lacks a label entry

    Code
      make_factor(x)
    Condition
      Error in `make_factor()`:
      x `x` contains 1 value with no label: 4.
      i Every observed value must have a label entry.
      v Add the missing labels or use `na_if()` to convert those values to `NA` first.

# make_dicho() errors when fewer than 2 levels remain after .exclude

    Code
      make_dicho(x, .exclude = "Neutral")
    Condition
      Error in `make_dicho()`:
      x Fewer than 2 levels remain after applying `.exclude`.
      i 1 level excluded; 1 level remain.
      v Remove entries from `.exclude` or check that `x` has sufficient levels.

# make_dicho() errors when collapse is ambiguous (4 distinct stems)

    Code
      make_dicho(x)
    Condition
      Error in `make_dicho()`:
      x First-word stripping produced 4 stems, not 2: "Apple", "Banana", "Cherry", and "Date".
      i Automatic collapse requires exactly 2 unique stems after removing first-word prefixes.
      v Use `.exclude` to remove middle categories, or manually recode to 2 groups before calling `make_dicho()`.

# make_dicho() errors on bad .label type

    Code
      make_dicho(x, .label = 123)
    Condition
      Error in `.validate_transform_args()`:
      x `.label` must be a single character string or `NULL`.
      i Got <numeric> of length 1.

# make_binary() errors on bad .label type

    Code
      make_binary(x, .label = 123)
    Condition
      Error in `.validate_transform_args()`:
      x `.label` must be a single character string or `NULL`.
      i Got <numeric> of length 1.

# make_rev() errors on factor input

    Code
      make_rev(x)
    Condition
      Error in `make_rev()`:
      x `x` must be a numeric vector (double or integer).
      i Got type "integer" with class <factor>.
      v Use `make_factor()` for factor or character inputs.

# make_rev() errors on bad .label type

    Code
      make_rev(x, .label = 123)
    Condition
      Error in `.validate_transform_args()`:
      x `.label` must be a single character string or `NULL`.
      i Got <numeric> of length 1.

# make_flip() errors on non-numeric input

    Code
      make_flip(x, "label")
    Condition
      Error in `make_flip()`:
      x `x` must be a numeric vector (double or integer).
      i Got type "integer" with class <factor>.
      v Use `make_factor()` for factor or character inputs.

# make_flip() errors when label is missing

    Code
      make_flip(x)
    Condition
      Error in `make_flip()`:
      x `label` is required.
      i `make_flip()` reverses the semantic meaning of a variable — a new variable label is needed to document the change.
      v Supply a string describing the flipped meaning, e.g. "\"I dislike the color blue\"".

# make_flip() errors on bad .description type

    Code
      make_flip(x, "label", .description = 123)
    Condition
      Error in `.validate_transform_args()`:
      x `.description` must be a single character string or `NULL`.
      i Got <numeric> of length 1.

