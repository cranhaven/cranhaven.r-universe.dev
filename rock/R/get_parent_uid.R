get_parent_uid <- function(uids, nestingLevels) {

  indices <- seq_along(uids);
  parents <- rep("", length(uids));

  threading <- FALSE;
  maxThreadDepth <- 0;
  threadingDepth <- 0;
  threadParentIndices <- c();

  for (i in indices) {
    if (nestingLevels[i] == 0) {
      ### No thread; potentially left a thread; reset everything
      threading <- FALSE;
      maxThreadDepth <- 0;
      threadingDepth <- 0;
      threadParentIndices <- c();
    } else if ((threading == FALSE) && (nestingLevels[i] == 1)) {
      ### Starting a thread; so the preceding utterance becomes the level 1
      ### parent
      threading <- TRUE;
      maxThreadDepth <- 1;
      threadingDepth <- 1;
      threadParentIndices[threadingDepth] <- i - 1;
      parents[i] <- uids[threadParentIndices[threadingDepth]];
    } else if (threading == FALSE) {
      stop("Starting a thread at a deeper level than 1!");
    } else if (nestingLevels[i] > (maxThreadDepth + 1)) {
      stop("Threading too deep without having progressed through previous levels!");
    } else if (nestingLevels[i] == (maxThreadDepth + 1)) {
      ### Moving one level deeper, and reaching a new threading maximum;
      ### so the preceding utterance becomes the reference at this level
      maxThreadDepth <- nestingLevels[i];
      threadingDepth <- maxThreadDepth;
      threadParentIndices[threadingDepth] <- i - 1;
      parents[i] <- uids[threadParentIndices[threadingDepth]];
    } else if (nestingLevels[i] == (threadingDepth)) {
      ### Staying on the same level, so the parent of this utterance is that
      ### set for this threading level
      parents[i] <- uids[threadParentIndices[threadingDepth]];
    } else if (nestingLevels[i] > (threadingDepth)) {
      ### Moving one level deeper; so the preceding utterance becomes the
      ### parent at this level
      threadingDepth <- nestingLevels[i];
      threadParentIndices[threadingDepth] <- i - 1;
      parents[i] <- uids[threadParentIndices[threadingDepth]];
    } else if (nestingLevels[i] < (threadingDepth)) {
      ### Moving to a lower level; so reset the threadingDepth only
      threadingDepth <- nestingLevels[i];
      parents[i] <- uids[threadParentIndices[threadingDepth]];
    }
  }

  return(parents);

}
