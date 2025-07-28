library("tinytest")
library("checkmate")
using("checkmate")

# test creating an entity relationship diagram
dm <- make_dm()
expect_class(dm, classes = "dm")
