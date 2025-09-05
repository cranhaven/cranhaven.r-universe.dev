# Hypergeometric test ===========================
setGeneric("enrichment_test", function(venn, set1, set2, univ = "all", n = 10000, seed = 42) {
  standardGeneric("enrichment_test")
}
)

#' @export
#' @rdname enrichment_test
setMethod("enrichment_test", c(venn = "Venn", set1 = "ANY", set2 = "ANY",
                               univ = "ANY", n = "ANY", seed = "ANY"),
          function(venn, set1, set2, univ = "all", n = 10000, seed = 42) {

            if (univ[1] == "all") {
              univ = unite(venn)
            }

            if (n < 1000) {
              stop("At least 1000 sets are needed for the generation of null distribution.")
            }

            set1 = venn@sets[set1] %>% unlist
            set2 = venn@sets[set2] %>% unlist

            inter = intersect(set1, set2)

            randomSets = vector(mode = "list", length = n)

            set.seed(seed)
            for(i in 1:n) {
              randomSets[[i]] = sample(univ, size = length(set1), replace = FALSE)
            }

            randomInter = sapply(randomSets, function(x) length(intersect(x, set2)))

            p = sum(randomInter >= length(inter))
            p = p / n

            return(list("Significance" = p, "Overlap_Counts" = randomInter))
          }
)
