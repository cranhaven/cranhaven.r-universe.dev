aesthetics_to_graph_theme <- function(aestheticConfig) {

  warnForMultipleAesthetics <- rock::opts$get('warnForMultipleAesthetics');

  aestheticConfig <- unname(aestheticConfig);

  themeVectors <-
    unlist(
      lapply(
        aestheticConfig,
        function(currentAesthetics) {

          ### Process graph attributes, if any
          if ("graph" %in% names(currentAesthetics)) {
            graphAttributeNames <-
              names(currentAesthetics$graph);
            graphAttributes <-
              mapply(
                c,
                graphAttributeNames,
                unname(unlist(currentAesthetics$graph)),
                rep("graph", length(graphAttributeNames)),
                SIMPLIFY = FALSE
              );
          } else {
            graphAttributes <- c();
          }

          ### Process node attributes, if any
          if ("nodes" %in% names(currentAesthetics)) {

            areDefaultNodeAttributes <-
              unlist(lapply(currentAesthetics$nodes,
                            `[[`,'type')) == "default";

            if (any(areDefaultNodeAttributes)) {

              defaultNodeAttributes <-
                currentAesthetics$nodes[[which(areDefaultNodeAttributes)]];

              nodeAttributeNames <-
                setdiff(
                  names(defaultNodeAttributes),
                  "type"
                );

              nodeAttributes <-
                mapply(
                  c,
                  nodeAttributeNames,
                  unname(unlist(defaultNodeAttributes[nodeAttributeNames])),
                  rep("node", length(nodeAttributeNames)),
                  SIMPLIFY = FALSE
                );

            } else {
              nodeAttributes <- c();
            }
          } else {
            nodeAttributes <- c();
          }

          ### Process edge attributes, if any
          if ("edges" %in% names(currentAesthetics)) {

            areDefaultEdgeAttributes <-
              unlist(lapply(currentAesthetics$edges,
                            `[[`,'type')) == "default";

            if (any(areDefaultEdgeAttributes)) {

              defaultEdgeAttributes <-
                currentAesthetics$edges[[which(areDefaultEdgeAttributes)]];

              edgeAttributeNames <-
                setdiff(
                  names(defaultEdgeAttributes),
                  "type"
                );

              edgeAttributes <-
                mapply(
                  c,
                  edgeAttributeNames,
                  unname(unlist(defaultEdgeAttributes[edgeAttributeNames])),
                  rep("edge", length(edgeAttributeNames)),
                  SIMPLIFY = FALSE
                );

            } else {
              edgeAttributes <- c();
            }
          } else {
            edgeAttributes <- c();
          }

          return(
            list(
              graphAttributes = graphAttributes,
              nodeAttributes = nodeAttributes,
              edgeAttributes = edgeAttributes
            )
          );
        }
      ),
      recursive = FALSE
    );

  res <- list();

  for (attType in c('graph',
                    'node',
                    'edge')) {

    currentType <- paste0(attType, "Attributes");

    attributeNames <-
      unname(
        unlist(
          lapply(
            unlist(
              themeVectors[names(themeVectors) == currentType],
              recursive = FALSE
            ),
            `[`,
            1
          )
        )
      );

    attributeValues <-
      unlist(
        lapply(
          unlist(
            themeVectors[names(themeVectors) == currentType],
            recursive = FALSE
          ),
          `[`,
          2
        )
      );

    names(attributeValues) <- attributeNames;

    uniqueAttributeNames <- unique(attributeNames);

    for (i in uniqueAttributeNames) {

      attValues <- attributeValues[attributeNames == i];

      res[[currentType]][[i]] <-
        unname(
          c(i, attValues[1], attType)
        );

      if (length(unique(attValues)) > 1) {

        if (warnForMultipleAesthetics) {

          warning(
            "For ", attType, "s, attribute ", i,
            " had multiple values specified: ",
            vecTxtQ(attValues), "; taking the first one."
          );

        }

      }
    }

  }

  return(
    unname(
      unlist(
        res,
        recursive = FALSE
      )
    )
  );

}
