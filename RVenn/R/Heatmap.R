# Method for heatmap ============================
setGeneric("setmap", function(venn, slice = "all",
                              element_clustering = TRUE,
                              set_clustering = TRUE,
                              method = "average",
                              legend = TRUE,
                              title = NA,
                              element_fontsize = 10,
                              set_fontsize = 10) {
  standardGeneric("setmap")
}
)

#' @export
#' @rdname setmap
setMethod("setmap", c(venn = "Venn", slice = "ANY",
                      element_clustering = "ANY",
                      set_clustering = "ANY",
                      method = "ANY",
                      legend = "ANY",
                      title = "ANY",
                      element_fontsize = "ANY",
                      set_fontsize = "ANY"),
          function(venn, slice = "all",
                   element_clustering = TRUE,
                   set_clustering = TRUE,
                   method = "average",
                   legend = TRUE,
                   title = NA,
                   element_fontsize = 10,
                   set_fontsize = 10) {
            if (slice[1] != "all") {
              venn2 = venn@sets[slice]
            } else {
              venn2 = venn@sets
            }

            elem = unite(Venn(venn2))
            df = matrix(nrow = length(elem), ncol = length(venn2))
            df = as.data.frame(df)
            row.names(df) = elem
            colnames(df) = Venn(venn2)@names
            for(i in 1:length(venn2)) {
              df[, i] = ifelse(elem %in% venn2[[i]], 1, 0)
            }

            je = vegan::vegdist(df, method = 'jaccard')
            js = vegan::vegdist(t(df), method = 'jaccard')

            pal = rep(c('#EE2C2C', '#00CD66'), each = 2)
            breaks = c(0, 0.49, 0.51, 1)
            pheatmap::pheatmap(df,
                               scale = 'none',
                               cluster_rows = element_clustering,
                               cluster_cols = set_clustering,
                               clustering_distance_rows = je,
                               clustering_distance_cols = js,
                               clustering_method = method,
                               border_color = 'white',
                               breaks = breaks,
                               color = pal,
                               legend = legend,
                               main = title,
                               fontsize_row = element_fontsize,
                               fontsize_col = set_fontsize,
                               legend_breaks = c(0.25, 0.75),
                               legend_labels = c('Absent', 'Present'))
          }
)
