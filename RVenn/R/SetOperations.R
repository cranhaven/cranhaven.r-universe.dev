# Method for intersection =======================
setGeneric("overlap", function(venn, slice = "all") {
  standardGeneric("overlap")
}
)

#' @export
#' @rdname overlap
setMethod("overlap", c(venn = "Venn", slice = "ANY"),
          function(venn, slice = "all") {

            if (slice[1] != "all") {
              venn2 = venn@sets[slice]
              inter = purrr::reduce(venn2, function(x, y) intersect(x, y))
            } else {
                inter = purrr::reduce(venn@sets, function(x, y) intersect(x, y))
              }

            inter
          }
)


# Method for union ==============================
setGeneric("unite", function(venn, slice = "all") {
  standardGeneric("unite")
}
)

#' @export
#' @rdname unite
setMethod("unite", c(venn = "Venn", slice = "ANY"),
          function(venn, slice = "all") {

            if (slice[1] != "all") {
              venn2 = venn@sets[slice]
              uni = purrr::reduce(venn2, function(x, y) union(x, y))
            } else {
              uni = purrr::reduce(venn@sets, function(x, y) union(x, y))
            }

            uni
          }
)


# Method for difference =========================
setGeneric("discern", function(venn,
                               slice1,
                               slice2 = "all") {
  standardGeneric("discern")
}
)

#' @export
#' @importFrom magrittr %>%
#' @rdname discern
setMethod("discern", c(venn = "Venn", slice1 = "ANY", slice2 = "ANY"),
          function(venn,
                   slice1,
                   slice2 = "all") {

            if (is.numeric(slice1)) {
              slice1 = names(venn@sets)[slice1]
            }

            if (is.numeric(slice2)) {
              slice2 = names(venn@sets)[slice2]
            }

            if (slice2[1] == "all") {
              slice2 = setdiff(names(venn@sets), slice1)
              set1 = venn@sets[slice1] %>% purrr::reduce(function(x, y) union(x, y))
              set2 = venn@sets[slice2] %>% purrr::reduce(function(x, y) union(x, y))
              differ = setdiff(set1, set2)
            } else {
              set1 = venn@sets[slice1] %>% purrr::reduce(function(x, y) union(x, y))
              set2 = venn@sets[slice2] %>% purrr::reduce(function(x, y) union(x, y))
              differ = setdiff(set1, set2)
            }

            differ
          }
)

