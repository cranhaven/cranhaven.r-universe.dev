inductiveCodes_to_tree <- function(inductiveCodes,
                                   silent=TRUE) {

  ###---------------------------------------------------------------------------
  ### Build the inductive coding tree
  ###---------------------------------------------------------------------------

  if (!silent) {
    cat0("\nStarting to process the inductive codes and building a code tree.");
  }

  ### Build tree for this code regex. First some preparation.
  localRootsFull <-
    unlist(lapply(inductiveCodes,
                  utils::head, 1));
  localBranchesFull <-
    unlist(lapply(inductiveCodes,
                  utils::tail, -1));

  ### Remove duplicates (no longer maps on 'splitCodings')
  localRoots <-
    unique(localRootsFull);
  localBranches <-
    unique(localBranchesFull);

  ### Check whether any local roots are actually branches; store both
  ### a logical vector that maps on localRootsFull (and therefore, on
  ### splitcodings) and a vector with just the names
  localRootsThatAreBranchesFullLogical <-
    unlist(lapply(localRootsFull, `%in%`, localBranches));

  localRootsThatAreBranchesFull <-
    unique(localRootsFull[localRootsThatAreBranchesFullLogical]);

  ### Also store the local 'true roots' for convenience, where the logical
  ### vectors again maps on splitCodings
  localRootsThatAreTrueRootsFullLogical <-
    !localRootsThatAreBranchesFullLogical
  localRootsThatAreTrueRootsFull <-
    unique(localRootsFull[localRootsThatAreTrueRootsFullLogical]);

  ### Convert split codings into node-ready lists
  subTrees <-
    lapply(inductiveCodes,
           function(subTree) {
             return(lapply(subTree,
                           function(x) {
                             stats::setNames(list(x,x,x),
                                             c('idName',
                                               'labelName',
                                               'codeName'));
                           }));
           });

  ### Local roots that are not branches should be attached to the root of
  ### the inductive code tree for this code set, along with their children.
  inductiveCodeTrees <-
    data.tree::Node$new('codes');

  if (!silent) {
    cat0("\nBuilding tree containing all 'true local roots'.");
  }

  ### First add only the local roots that have no parents
  for (currentLocalRoot in
       localRootsThatAreTrueRootsFull) {

    if (!silent) {
      cat0("\n- Processing 'true local root' '",
                currentLocalRoot,
                "'.");
    }

    ### Add first node to the root
    inductiveCodeTrees$AddChild(currentLocalRoot);
    inductiveCodeTrees[[currentLocalRoot]]$label <-
      currentLocalRoot;
    inductiveCodeTrees[[currentLocalRoot]]$code <-
      currentLocalRoot;
  }

  if (!silent) {
    cat0("\n\nProcessing subtrees of those 'true local roots'.");
  }

  ### Then process their branches/children
  for (currentSubtree in
       inductiveCodes[localRootsThatAreTrueRootsFullLogical]) {
    if (!silent) {
      cat0("\n\n- Processing subtree consisting of the node sequence ",
                vecTxtQ(currentSubtree),
                ".");
    }
    if (length(currentSubtree) > 1) {
      ### Add children; first save reference to this node
      currentNode <-
        inductiveCodeTrees[[currentSubtree[1]]];

      if (!silent) {
        cat0("\n  - This subtree doesn't only consist of the parent/root code, but contains ",
                  length(currentSubtree)-1,
                  " child(ren). Processing child(ren).");
      }
      ### Then loop through children and progressively add them
      for (currentBranch in currentSubtree[2:length(currentSubtree)]) {
        if (!silent) {
          cat0("\n    - Processing node '",
                    currentBranch,
                    "'.");
        }

        if (is.null(currentNode[[currentBranch]])) {
          if (!silent) {
            cat0("\n      - This parent node ('",
                 currentNode$label, "') does not yet have a child with the name '",
                 currentBranch,
                 "', so adding it to that parent node (and moving into the new parent node: '",
                 currentNode$label, "').");
          }
          currentNode <-
            currentNode$AddChild(currentBranch);
          currentNode$label <-
            currentBranch;
          currentNode$code <-
            currentBranch;
        } else {
          currentNode <-
            currentNode[[currentBranch]];
          if (!silent) {
            cat0("\n      - This parent node ('",
                 currentNode$label, "') already has a child with the name '",
                 currentBranch,
                 "', so not adding anything at this point (moving into new parent node: '",
                 currentNode$label, "').");
          }
        }
      }
    } else {
      if (!silent) {
        cat0("\n  - This 'subtree' only consists of the parent/root code, ",
                  "so no further processing required.");
      }
    }
  }

  ### Then start working on the subtrees that should be attached to
  ### a parent

  if (length(inductiveCodes[localRootsThatAreBranchesFullLogical]) > 0) {
    if (!silent) {
      cat0("\n\nProcessing subtrees of 'local roots that are branches', i.e. single codes ",
                "that are descendants of other codes (without the full path to the root being ",
                "specified in the code), or subtrees where the local root is in fact a descendant.");
    }
    for (i in seq_along(inductiveCodes[localRootsThatAreBranchesFullLogical])) {
      currentSubtree <-
        inductiveCodes[localRootsThatAreBranchesFullLogical][[i]];

      if (!silent) {
        cat0("\n\n- Processing subtree consisting of the node sequence ",
                  vecTxtQ(currentSubtree),
                  ".");
      }

      if (length(currentSubtree) == 1) {

        if (!silent) {
          cat0("\n  - This 'subtree' only consists of the parent/root code, ",
                    "so no further processing required.");
        }

      } else {

        currentNode <-
          data.tree::FindNode(inductiveCodeTrees,
                              currentSubtree[1]);

        if (is.null(currentNode)) {
          ### Code not found - should normally not be possible

          if (!silent) {
            cat0("\n  - I cannot find the local root of this subtree ('",
                      currentSubtree[1], "') in the inductive code tree - this ",
                      "should normally not occur (actually, it should not be ",
                      "possible). Not processing this subtree further.");
          }

          warning(paste0("Code '", currentSubtree[1], "' does not ",
                         "have a parent I can find!"));

        } else {

          if (!silent) {
            cat0("\n  - This subtree doesn't only consist of the parent/root code, but contains ",
                      length(currentSubtree)-1,
                      " child(ren). Processing child(ren).");
          }

          ### If it's found, loop through the children and progressively add them
          for (currentBranch in currentSubtree[2:length(currentSubtree)]) {
            if (currentBranch %in% data.tree::Get(currentNode$children, 'name')) {

              currentNode <-
                currentNode[[currentBranch]];

              if (!silent) {
                cat0("\n      - This parent node already has a child with the name '",
                          currentBranch,
                          "', so not adding anything at this point (moving into new parent node: '",
                     currentNode$label, "').");
              }

            } else {

              if (!silent) {
                cat0("\n      - This parent node does not yet have a child with the name '",
                          currentBranch,
                          "', so adding it to that parent node (and moving into the new parent node: '",
                     currentNode$label, "').");
              }

              currentNode <-
                currentNode$AddChild(currentBranch);
              currentNode$label <-
                currentBranch;
              currentNode$code <-
                currentBranch;
            }
          }
        }
      }
    }
  } else {
    if (!silent) {
      cat0("\n\nNo subtrees of 'local roots that are branches', i.e. single codes ",
                "that are descendants of other codes (without the full path to the root being ",
                "specified in the code) found, so no further processing required.");
    }
  }
  if (!silent) {
    cat0("\n\nDone processing the inductive code tree.");
  }
  return(inductiveCodeTrees);

}
