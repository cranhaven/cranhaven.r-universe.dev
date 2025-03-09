sectionBreaksByCoder_to_sectionBreaksByUID <- function(sectionBreaksByCoder) {

    simplifiedSectionBreaks <-
      unlist(sectionBreaksByCoder, recursive=FALSE);

    sectionBreaks_pre_uids <-
      lapply(simplifiedSectionBreaks,
             function(sourceCoderCombi) {
               res <- lapply(sourceCoderCombi,
                             function(sectionBreakRegex) {
                               return(sectionBreakRegex$uid_pre);
                             })
               return(stats::setNames(res,
                                      names(sourceCoderCombi)));
             });
    sectionBreaks_at_uids <-
      lapply(simplifiedSectionBreaks,
             function(sourceCoderCombi) {
               res <- lapply(sourceCoderCombi,
                             function(sectionBreakRegex) {
                               return(sectionBreakRegex$uid_at);
                             })
               return(stats::setNames(res,
                                      names(sourceCoderCombi)));
             });
    sectionBreaks_post_uids <-
      lapply(simplifiedSectionBreaks,
             function(sourceCoderCombi) {
               res <- lapply(sourceCoderCombi,
                             function(sectionBreakRegex) {
                               return(sectionBreakRegex$uid_post);
                             })
               return(stats::setNames(res,
                                      names(sourceCoderCombi)));
             });

    sectionBreaks_content <-
      lapply(simplifiedSectionBreaks,
             function(sourceCoderCombi) {
               res <- lapply(sourceCoderCombi,
                             function(sectionBreakRegex) {
                               return(sectionBreakRegex$content);
                             })
               return(stats::setNames(res,
                                      names(sourceCoderCombi)));
             });

    sectionBreaks_pre_uids  <- unname(sectionBreaks_pre_uids);
    sectionBreaks_at_uids   <- unname(sectionBreaks_at_uids);
    sectionBreaks_post_uids <- unname(sectionBreaks_post_uids);
    sectionBreaks_content <- unname(sectionBreaks_content);

    sectionBreakNames <-
      unique(c(unlist(lapply(sectionBreaks_pre_uids, names)),
               unlist(lapply(sectionBreaks_at_uids, names)),
               unlist(lapply(sectionBreaks_post_uids, names)),
               unlist(lapply(sectionBreaks_content, names))));
    sectionRegexes <-
      rock::opts$get(sectionRegexes);
    sectionBreakCodes <-
      stats::setNames(sectionRegexes[gsub("_match",
                                          "",
                                          sectionBreakNames)],
                      sectionBreakNames);

    ### Transpose lists (turn them inside-out, so that the
    ### section break names are the the top level, and within
    ### each section break sub-list, there is just a list of
    ### the relevant UIDs)
    sectionBreaks_pre_uids  <- purrr::transpose(sectionBreaks_pre_uids);
    sectionBreaks_at_uids   <- purrr::transpose(sectionBreaks_at_uids);
    sectionBreaks_post_uids <- purrr::transpose(sectionBreaks_post_uids);
    sectionBreaks_content   <- purrr::transpose(sectionBreaks_content);

    ### Concatenate the UIDs from all lists and select the unique ones
    sectionBreaks_pre_uids <-
      lapply(sectionBreaks_pre_uids,
             function(x) return(unique(unlist(x))));
    sectionBreaks_at_uids <-
      lapply(sectionBreaks_at_uids,
             function(x) return(unique(unlist(x))));
    sectionBreaks_post_uids <-
      lapply(sectionBreaks_post_uids,
             function(x) return(unique(unlist(x))));
    sectionBreaks_content <-
      lapply(sectionBreaks_content,
             function(x) return(unique(unlist(x))));

    ### Simplify into a list where the UID is an index for the
    ### correct section break code
    simplifySectionBreakList <- function(sectionBreakList) {
      return(unlist(lapply(names(sectionBreakList),
                           function(x) {
                             return(stats::setNames(rep(sectionBreakCodes[x],
                                                        length(sectionBreakList[[x]])),
                                                    nm = sectionBreakList[[x]]));
                           })));
    }

    sectionBreaks_pre_uids <-
      simplifySectionBreakList(sectionBreaks_pre_uids);
    sectionBreaks_at_uids <-
      simplifySectionBreakList(sectionBreaks_at_uids);
    sectionBreaks_post_uids <-
      simplifySectionBreakList(sectionBreaks_post_uids);
    # sectionBreaks_content <-
    #   simplifySectionBreakList(sectionBreaks_content);

    return(list(matches_pre = sectionBreaks_pre_uids,
                matches_at = sectionBreaks_at_uids,
                matches_post = sectionBreaks_post_uids,
                content = sectionBreaks_content));
  }
