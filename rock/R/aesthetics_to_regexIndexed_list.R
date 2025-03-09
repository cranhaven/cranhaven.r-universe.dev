aesthetics_to_regexIndexed_list <- function(aestheticConfig) {

  warnForMultipleAesthetics <- rock::opts$get('warnForMultipleAesthetics');

  aestheticConfig <- unname(aestheticConfig);

  customAesthetic <-
    unlist(
      lapply(
        aestheticConfig,
        function(currentAesthetics) {

          ### Process node attributes, if any
          if ("nodes" %in% names(currentAesthetics)) {

            areCustomNodeAttributes <-
              unlist(lapply(currentAesthetics$nodes,
                            `[[`,'type')) != "default";

            if (any(areCustomNodeAttributes)) {

              customNodeAttributes <-
                currentAesthetics$nodes[which(areCustomNodeAttributes)];

              regexes <-
                unlist(lapply(currentAesthetics$nodes,
                              `[[`,'type'));

              nodeAttributesByRegex <-
                lapply(
                  customNodeAttributes,
                  function(x) {
                    return(
                      x[names(x) != "type"]
                    )
                  }
                );
              names(nodeAttributesByRegex) <-
                regexes[areCustomNodeAttributes];

            } else {
              nodeAttributesByRegex <- c();
            }
          } else {
            nodeAttributesByRegex <- c();
          }

          ### Process edge attributes, if any
          if ("edges" %in% names(currentAesthetics)) {

            areCustomEdgeAttributes <-
              unlist(lapply(currentAesthetics$edges,
                            `[[`,'type')) != "default";

            if (any(areCustomEdgeAttributes)) {

              customEdgeAttributes <-
                currentAesthetics$edges[which(areCustomEdgeAttributes)];

              regexes <-
                unlist(lapply(currentAesthetics$edges,
                              `[[`,'type'));

              edgeAttributesByRegex <-
                lapply(
                  customEdgeAttributes,
                  function(x) {
                    return(
                      x[names(x) != "type"]
                    )
                  }
                );
              names(edgeAttributesByRegex) <-
                regexes[areCustomEdgeAttributes];

            } else {
              edgeAttributesByRegex <- c();
            }
          } else {
            edgeAttributesByRegex <- c();
          }

          return(
            list(
              nodeAttributes = nodeAttributesByRegex,
              edgeAttributes = edgeAttributesByRegex
            )
          );
        }
      ),
      recursive = FALSE
    );

  res <-
    lapply(
      unique(names(customAesthetic)),
      function(name) {
        res <-
          do.call(
            c,
            customAesthetic[names(customAesthetic) == name]
          );
        if (!is.null(res)) {
          names(res) <- gsub(paste0(name, "."), "", names(res));
        }
        regexes <- unique(names(res));
        if (any(duplicated(names(res)))) {
          for (currentRegex in regexes) {

            attributeDf <-
              do.call(
                rbind,
                lapply(
                  res[names(res) == currentRegex],
                  function(x) {
                    return(
                      data.frame(
                        attName = names(x),
                        attValue = x
                      )
                    )
                  }
                )
              );



            if (any(duplicated(attributeDf$attName))) {

              if (warnForMultipleAesthetics) {

                warning("For ", gsub("Attributes", "", name), "s of types ",
                        "matching regular expression '", currentRegex, "', ",
                        "duplicate attributes were found");

              }

              attributeDf <-
                attributeDf[duplicated(attributeDf$attName), ];

            } else {

            }

            regexIndices <- which(names(res) == currentRegex);




          }

          ### Check the attributes that are set; if the same attribute
          ### occurs multiple times, check the values;
          ### if the values aren't the same, select one of them and
          ### throw a warning
        }
        return(res);
      }
    );
  names(res) <- unique(names(customAesthetic));

  return(
    customAesthetic
  );

}
