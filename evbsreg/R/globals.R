## Declare data-frame column names used inside ggplot2 aes() mappings,
## so R CMD check does not flag them as undefined global variables.
utils::globalVariables(c("y", "label", "y_vals", "fdp", "t"))
