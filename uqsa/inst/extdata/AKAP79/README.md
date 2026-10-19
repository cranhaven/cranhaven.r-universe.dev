# This is the AKAP79 model in a simplified tabular form

This format is virtually identical to SBtab, except that all `!!SBtab`
headers (meta-data) are removed, all exclamation points are removed
with no replacement, and all columns names are lower case, spaced
words.

The files become very normal looking tsv files that can be easily
displayed by formatters that expect normal tsv content (e.g. GitHub's
file browsing interface).

The file names correspond to the TableName of SBtab Documents.

## Rules for Conversion between SBtab and this Format

- Column headers (column names) shouldn't contain exclamation points.
- Columns such as `!DefaultValue` or `!InitialValue` are just labelled `value`
- Columns that contain a math expression are labelled `formula`
- Columns that give measured values for an output, are just labelled
  with the name of that output
  + a measurement of `ABC` is labelled `ABC` in the measurement table
  + no prefix: `>`
- measured values are given in concise error notation,
  + no need for a second column defining the standard error of that
    measurement
- CamelCase is converted to lower case words with spaces:
  + `!IsReversible` maps to `is reversibel`
  + `!Kinetic Law` maps to `kinetic law`
- the reaction formula is split into three fields:
  + reactants
  + arrow
  + products
- the arrow part of the reaction formula isn't interpreted (aesthetics
  only) and can be any symbol, all of the following arrow symbols are
  OK:
  + `⇌`
  + `->`, `<-`
  + `→`
  + `<=>`, `<->`
  + `☺`


# Special Symbols in Column names

Should there be any meaning to any special symbols in the columns?

## Ideas

None of these are currently interpreted like that.

- `!` could mean that this column should have some meaning to the parser, and if misspelled, throws an error.
- `>` could still mean that this is _setting_ a value, or _measuring_ a value
- `~` could still mean _uncertainty_

