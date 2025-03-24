#' update.components
#' 
#' @method update components
#' @keywords internal
#' @export
update.components <- function(object, data, colname.objects, colname.scores, ...) {
  components <- object
  facets <- unique(unlist(sapply(components$source, strsplit, split = ":")))
  facets <- facets[facets %in% c(colname.objects, "Residual") == F]
  components.objects <- list()
  for(name.object in unique(data[, colname.objects])) {
    components.objects[[name.object]] <- components
    data.keep <- data[, colname.objects] == name.object & is.na(data[, colname.scores]) == F
    sources.keep <- grepl(pattern = "Residual", x = components.objects[[name.object]]$source)
    n.source <- length(data[data.keep, colname.objects])
    components.objects[[name.object]][sources.keep, "n"] <- n.source
    for(facet in facets) {
      n.source <- length(unique(data[data.keep, facet]))
      sources.keep <- grepl(pattern = facet, x = components.objects[[name.object]]$source)
      if(facet %in% components.objects[[name.object]]$source) {
        components.objects[[name.object]]$n <- ifelse(
          sources.keep, 
          n.source * components.objects[[name.object]]$n, 
          components.objects[[name.object]]$n
        )
      } else {
        components.objects[[name.object]][sources.keep, "n"] <- apply(
          components.objects[[name.object]][sources.keep, "n", drop = F], 
          1, 
          function(x) max(n.source, x)
        )
      }
    }
    components.objects[[name.object]]$var <- components.objects[[name.object]]$var / 
      components.objects[[name.object]]$n
    components.objects[[name.object]]$percent <- round(components.objects[[name.object]]$var / 
      sum(components.objects[[name.object]]$var) * 100, 1)
    components.objects[[name.object]]
  }
  if(length(unique(components.objects)) == 1) {
    components.obs <- unique(components.objects)[[1]]
  } else {
    components.obs <- data.frame(
      "n" = apply(
        X = sapply(
          X = components.objects, 
          FUN = function(x) {
           sapply(X = components$source, FUN = function(y) x[x$source == y, "n"])
         }
        ), 
        MARGIN = 1, 
        FUN = median
      )
    )
    vars.keep <- names(components) != "n"
    components.obs <- merge(components[, vars.keep], components.obs, by.x = "source", by.y = 0, sort = F)
    components.obs$var <- components.obs$var / components.obs$n
    components.obs$percent <- round(components.obs$var / 
      sum(components.obs$var) * 100, 1)
    class(components.obs) <- c("components", class(components.obs))
    attributes(components.obs)$unbalanced <- sapply(
      X = unique(components.objects), 
      FUN = function(x) {
        objects.components <- sapply(
          X = components.objects, 
          FUN = function(y) identical(x, y)
        )
        list(
          "components" = x, 
          "objects" = names(objects.components)[objects.components]
        )
      }, 
      simplify = F
    )
  }
  components.obs
}