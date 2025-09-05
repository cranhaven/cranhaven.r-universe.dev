# Method for intersection =======================
setGeneric("overlap_pairs", function(venn, slice = "all") {
  standardGeneric("overlap_pairs")
}
)

#' @export
#' @importFrom utils combn
#' @rdname overlap_pairs
setMethod("overlap_pairs", c(venn = "Venn", slice = "ANY"),
          function(venn, slice = "all") {

            if (slice[1] != "all") {
              venn2 = venn@sets[slice]
              names2 = venn@names[slice]
              pairs = combn(length(venn2), 2)
              inter = vector("list", ncol(pairs))
              for(i in 1:ncol(pairs)) {
                inter[[i]] = intersect(venn2[[pairs[1, i]]],
                                       venn2[[pairs[2, i]]])
                names(inter)[i] = paste(names2[[pairs[1, i]]],
                                        names2[[pairs[2, i]]],
                                        sep = "...")
              }
            } else {
              pairs = combn(length(venn@sets), 2)
              inter = vector("list", ncol(pairs))
              for(i in 1:ncol(pairs)) {
                inter[[i]] = intersect(venn@sets[[pairs[1, i]]],
                                       venn@sets[[pairs[2, i]]])
                names(inter)[i] = paste(venn@names[[pairs[1, i]]],
                                        venn@names[[pairs[2, i]]],
                                        sep = "...")
              }
            }

            inter
          }
)


# Method for union ==============================
setGeneric("unite_pairs", function(venn, slice = "all") {
  standardGeneric("unite_pairs")
}
)

#' @export
#' @importFrom utils combn
#' @rdname unite_pairs
setMethod("unite_pairs", c(venn = "Venn", slice = "ANY"),
          function(venn, slice = "all") {

            if (slice[1] != "all") {
              venn2 = venn@sets[slice]
              names2 = venn@names[slice]
              pairs = combn(length(venn2), 2)
              uni = vector("list", ncol(pairs))
              for(i in 1:ncol(pairs)) {
                uni[[i]] = union(venn2[[pairs[1, i]]],
                                 venn2[[pairs[2, i]]])
                names(uni)[i] = paste(names2[[pairs[1, i]]],
                                      names2[[pairs[2, i]]],
                                      sep = "...")
              }
            } else {
              pairs = combn(length(venn@sets), 2)
              uni = vector("list", ncol(pairs))
              for(i in 1:ncol(pairs)) {
                uni[[i]] = union(venn@sets[[pairs[1, i]]],
                                 venn@sets[[pairs[2, i]]])
                names(uni)[i] = paste(venn@names[[pairs[1, i]]],
                                      venn@names[[pairs[2, i]]],
                                      sep = "...")
              }
            }

            uni
          }
)


# Method for difference =========================
setGeneric("discern_pairs", function(venn, slice = "all") {
  standardGeneric("discern_pairs")
}
)

#' @export
#' @importFrom utils combn
#' @rdname discern_pairs
setMethod("discern_pairs", c(venn = "Venn", slice = "ANY"),
          function(venn, slice = "all") {

            if (slice[1] != "all") {
              venn2 = venn@sets[slice]
              names2 = venn@names[slice]
              pairs = combn(length(venn2), 2)
              differ = vector("list", 2 * ncol(pairs))
              for(i in 1:ncol(pairs)) {
                differ[[i]] = setdiff(venn2[[pairs[1, i]]],
                                      venn2[[pairs[2, i]]])
                differ[[i + ncol(pairs)]] = setdiff(venn2[[pairs[2, i]]],
                                                    venn2[[pairs[1, i]]])
                names(differ)[i] = paste(names2[[pairs[1, i]]],
                                         names2[[pairs[2, i]]],
                                         sep = "...")
                names(differ)[i + ncol(pairs)] = paste(names2[[pairs[2, i]]],
                                                       names2[[pairs[1, i]]],
                                                       sep = "...")
              }
            } else {
              pairs = combn(length(venn@sets), 2)
              differ = vector("list", 2 * ncol(pairs))
              for(i in 1:ncol(pairs)) {
                differ[[i]] = setdiff(venn@sets[[pairs[1, i]]],
                                      venn@sets[[pairs[2, i]]])
                differ[[i + ncol(pairs)]] = setdiff(venn@sets[[pairs[2, i]]],
                                                    venn@sets[[pairs[1, i]]])
                names(differ)[i] = paste(venn@names[[pairs[1, i]]],
                                         venn@names[[pairs[2, i]]],
                                         sep = "...")
                names(differ)[i + ncol(pairs)] = paste(venn@names[[pairs[2, i]]],
                                                       venn@names[[pairs[1, i]]],
                                                       sep = "...")
              }
            }

            differ
          }
)
