#' Synchronize multiple streams
#'
#' This function maps the codes from multiple streams onto a primary stream.
#'
#' @param x The object with the parsed sources.
#' @param primaryStream The identifier of the primary stream.
#' @param columns The names of the column(s) to synchronize.
#' @param anchorsCol The column containing the anchors.
#' @param sourceId The column containing the source identifiers.
#' @param streamId The column containing the stream identifiers.
#' @param neverFill Columns to never fill regardless of whether fill
#' is `TRUE`. Set to `NULL` to always respect the setting of `fill`. By default,
#' the raw versions of the class instance identification columns are never
#' duplicated (found with regular expression `"_raw$"`), since those are used
#' for state transition computations.
#' @param prependStreamIdToColName,appendStreamIdToColName Whether to append
#' or prepend the stream identifier before merging the dataframes together.
#' @param carryOverAnchors Whether to carry over anchors for each source
#' @param colNameGlue When appending or prepending stream identifiers, the
#' character(s) to use as "glue" or separator.
#' @param silent Whether to be silent (`TRUE`) or chatty (`FALSE`).
#'
#' @return The object with parsd sources, `x`, with the synchronization results
#' added in the `$syncResults` subobject.
#' @inheritParams syncing_vector_compress
#' @inheritParams syncing_vector_expand
#' @export
#'
#' @examples ### Get a directory with example sources
#' examplePath <-
#'   file.path(
#'     system.file(package="rock"),
#'     'extdata',
#'     'streams'
#'   );
#'
#' ### Parse the sources
#' parsedSources <- rock::parse_sources(
#'   examplePath
#' );
#'
#' ### Add a dataframe, syncing all streams to primary stream !
#' parsedSources <- rock::sync_streams(
#'   parsedSources,
#'   primaryStream = "streamA",
#'   columns = c("Code1", "Code2", "Code3"),
#'   prependStreamIdToColName = TRUE
#' );
#'
#' ### Look at two examples
#' parsedSources$syncResults$qdt[
#'   ,
#'   c("streamB_Code3_streamB", "streamC_Code1_streamC")
#' ];
sync_streams <- function(x,
                         primaryStream,
                         columns = NULL,
                         anchorsCol = rock::opts$get('anchorsCol'),
                         sourceId = rock::opts$get('sourceId'),
                         streamId = rock::opts$get('streamId'),
                         prependStreamIdToColName = FALSE,
                         appendStreamIdToColName = TRUE,
                         sep = " ",
                         fill = TRUE,
                         paddingValue = NA,
                         neverFill = grep("_raw$", names(x$qdt), value=TRUE),
                         compressFun = NULL,
                         compressFunPart = NULL,
                         expandFun = NULL,
                         carryOverAnchors = FALSE,
                         colNameGlue = rock::opts$get('colNameGlue'),
                         silent = rock::opts$get('silent')) {

  df <- x$qdt;

  ### Added 2023-08-20: removing lines with "no_id" for source and stream
  df <-
    df[!(df[, sourceId]=="no_id"), ];
  df <-
    df[!(df[, streamId]=="no_id"), ];

  if (is.null(columns)) {
    columns <- names(df);
  }

  sourceIds <-
    unique(df[, sourceId]);

  sourceIds <-
    sourceIds[!is.na(sourceIds)];

  streamIds <-
    unique(df[, streamId]);

  streamIds <-
    streamIds[!is.na(streamIds)];

  dfBySource <-
    lapply(
      sourceIds,
      function(currentSourceId) {
        return(
          df[df[, sourceId] == currentSourceId, ]
        );
      }
    );

  names(dfBySource) <- sourceIds;

  dfBySourceAndStream <-
    lapply(
      dfBySource,
      function(currentDfBySource) {
        res <-
          lapply(
            streamIds,
            function(currentStreamId) {
              ### Extract relevant data frame rows
              return(
                currentDfBySource[
                  currentDfBySource[, streamId] == currentStreamId, ]
              );
            }
          );
        names(res) <-
          streamIds;
        return(res);
      }
    );

  ### Now we have the dataframes neatly split by source and stream,
  ### get the vectors with anchors
  fullAnchorVectors <-
    lapply(
      dfBySourceAndStream,
      function(currentDfBySource) {
        return(
          lapply(
            currentDfBySource,
            function(currentDfBySourceAndStream) {

              currentSourceId <-
                unique(currentDfBySourceAndStream[, sourceId]);
              currentStreamId <-
                unique(currentDfBySourceAndStream[, streamId]);

              res <- currentDfBySourceAndStream[, anchorsCol];

              if (is.na(res[1]) || nchar(res[1]) == 0) {
                msg(
                  "Auto-inserting a start anchor at position 1 in ",
                  "anchor column '", anchorsCol, "' in source with identifier '",
                  currentSourceId, "' and stream with identifier '",
                  currentStreamId, "'.\n",
                  silent = silent
                );
                res[1] <- "ROCK_AUTO_INSERTED_START_ANCHOR";
              }
              if (is.na(res[length(res)]) || nchar(res[length(res)]) == 0) {
                msg(
                  "Auto-inserting an end anchor at position ",
                  length(res), " in ",
                  "anchor column '", anchorsCol, "' in source with identifier '",
                  currentSourceId, "' and stream with identifier '",
                  currentStreamId, "'.\n",
                  silent = silent
                );
                res[length(res)] <- "ROCK_AUTO_INSERTED_END_ANCHOR";
              }
              return(res);
            }
          )
        );
      }
    );

  anchorOnlyVectors <-
    lapply(
      fullAnchorVectors,
      function(currentSourceAnchors) {
        return(
          lapply(
            currentSourceAnchors,
            function(allAnchors) {
              ### Get and check anchors
              anchorsOnly <-
                #sort(
                  allAnchors[!is.na(allAnchors) & nchar(allAnchors) > 0];
                #);
              if (any(duplicated(anchorsOnly))) {
                stop("Found a duplicate anchor! Specifically: ",
                     vecTxtQ(anchorsOnly[which(duplicated(anchorsOnly))]), ".");
              }
              return(anchorsOnly);
            }
          )
        );
      }
    );

  ###---------------------------------------------------------------------------
  ### Check for uniqueness
  ###
  ### Note: if we ever want to make it possible to have fewer anchors in the
  ### non-primary streams, this bit below has to be updated.
  ###---------------------------------------------------------------------------

  lapply(
    anchorOnlyVectors,
    function(anchorVectorForCurrentSource) {

      vectorLengths <-
        unlist(lapply(anchorVectorForCurrentSource, length));
      if (length(unique(vectorLengths)) > 1) {
        stop("The number of anchors in each stream is not ",
             "identical! I encountered the following numbers of ",
             "anchors: ",
             vecTxtQ(paste0(vectorLengths, " (for stream with stream identifier ", names(vectorLengths), ")")),
             "It is possible that for one of the streams, ",
             "an anchor was omitted; or, alternatively, that for ",
             "some of the streams, anchors were automatically added ",
             "to the beginning or the end of the stream, but not for ",
             "others (because they already started or ended with an anchor). ",
             "The last anchor in each stream was: ",
             vecTxtQ(
               unlist(
                 lapply(
                   anchorVectorForCurrentSource,
                   function(x) return(x[length(x)])
                 )
               )
             ),
            "."
          );
      }

      anchorDf <-
        as.data.frame(
          anchorVectorForCurrentSource
        );
      res <-
        lapply(
          as.data.frame(
            t(
              anchorDf
            )
          ),
          unique
        );

      if (!all(unlist(lapply(res, length)) == 1)) {

        rowsWithMismatches <-
          which(unlist(
            lapply(apply(anchorDf, 1, unique), function(x) return(length(x) > 1))
          ));

        streamIds <- names(anchorDf);

        mismatchedAnchors <-
          apply(anchorDf[rowsWithMismatches, ], 1, as.vector,
                simplify = FALSE);

        errorMessageBits <-
          lapply(
            seq_along(rowsWithMismatches),
            function(i) {
              return(
                paste0("anchor in sequence position ",
                       rowsWithMismatches[i],
                       ", which has anchor identifiers ",
                       vecTxt(
                         paste0(
                           mismatchedAnchors[[i]],
                           " (for stream with identifier ",
                           streamIds,
                           ")"
                         )
                       )
                )
              );
            }
          );

        stop(
          "Not all anchors align! Specifically, mismatches occur for ",
          paste0(errorMessageBits, collapse="; ")
        );
      }
      return(anchorDf);
    }
  );

  ###---------------------------------------------------------------------------
  ### Get anchor locations and pairs from every stream
  ###---------------------------------------------------------------------------

  anchorIndices <- list();

  for (currentSourceName in names(anchorOnlyVectors)) {

    currentSource <- anchorOnlyVectors[[currentSourceName]];
    anchorIndices[[currentSourceName]] <- list();

    for (currentStream in names(currentSource)) {

      anchorIndices[[currentSourceName]][[currentStream]] <- list();

      for (currentStartAnchor in 1:(length(currentSource[[primaryStream]])-1)) {

        anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]] <-
          list(
            startAnchor = currentSource[[primaryStream]][currentStartAnchor],
            endAnchor = currentSource[[primaryStream]][currentStartAnchor + 1]
          );
        anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]]$startIndex <-
          which(
            fullAnchorVectors[[currentSourceName]][[currentStream]] ==
              anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]]$startAnchor
          );
        anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]]$endIndex <-
          which(
            fullAnchorVectors[[currentSourceName]][[currentStream]] ==
              anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]]$endAnchor
          );

        if (anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]]$endIndex - 1 ==
            anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]]$startIndex) {

          ### Update 2023-08-20: adjusting to retention of lines in the source that
          ### only have identifiers, to allow encoding of state transitions to same state

          # anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]]$indicesToExtract <-
          #   c();

          anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]]$indicesToExtract <-
            anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]]$startIndex;

        } else {
          anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]]$indicesToExtract <-
            (anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]]$startIndex):
            (anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]]$endIndex-1);
        }

        anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]]$length <-
          length(
            anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]]$indicesToExtract
          );

      }

    }
  }

  ###---------------------------------------------------------------------------
  ### Create vectors with new indices for each non-primary stream
  ###---------------------------------------------------------------------------

  msg("\nStarting stream synchronization. Primary stream: ", primaryStream, ".\n",
      silent = silent);

  streamMapping <-
    lapply(
      names(anchorIndices),
      function(currentSourceName) {
        currentSource <- anchorIndices[[currentSourceName]];
        otherStreams <- setdiff(names(currentSource), primaryStream);

        msg("\n - Processing source with identifier: ", currentSourceName, ".\n",
            silent = silent);

        res <-
          lapply(
            names(currentSource[otherStreams]),
            function(currentStreamName) {

              currentStream <- currentSource[[currentStreamName]];
              res <- list();

              msg("  - Processing stream with identifier: ", currentStreamName, ".\n",
                  silent = silent);

              for (anchorPair in seq_along(currentStream)) {

                msg("   - Processing anchor pair ",
                    currentStream[[anchorPair]]$startAnchor,
                    ":", currentStream[[anchorPair]]$endAnchor,
                    " at indices ",
                    currentStream[[anchorPair]]$startIndex, ":",
                    currentStream[[anchorPair]]$endIndex, ", so ",
                    currentStream[[anchorPair]]$length,
                    " indices to sync to length ",
                    currentSource[[primaryStream]][[anchorPair]]$length,
                    " (indices ",
                    currentSource[[primaryStream]][[anchorPair]]$startIndex, ":",
                    currentSource[[primaryStream]][[anchorPair]]$endIndex,
                    ").\n",
                    silent = silent);

                if (currentStream[[anchorPair]]$length == 0) {

                  msg("     No utterances in this anchor pair (their indices ",
                      "are ", currentStream[[anchorPair]]$startIndex,
                      " and ", currentStream[[anchorPair]]$endIndex,
                      "). Returning empty data frame of ",
                      currentSource[[primaryStream]][[anchorPair]]$length,
                      " rows.\n",
                      silent = silent);

                  res[[anchorPair]] <-
                    create_glue_df(
                      dfBySourceAndStream[[currentSourceName]][[currentStreamName]][
                        ,
                        columns,
                        drop=FALSE
                      ],
                      rows = currentSource[[primaryStream]][[anchorPair]]$length
                    );

                } else if (currentSource[[primaryStream]][[anchorPair]]$length ==
                           currentStream[[anchorPair]]$length) {

                  msg("     This anchor pair (with indices ",
                      currentStream[[anchorPair]]$startIndex,
                      " and ", currentStream[[anchorPair]]$endIndex,
                      ") is already the same size as the primary stream (",
                      currentSource[[primaryStream]][[anchorPair]]$length,
                      "), returning as is.\n",
                      silent = silent);

                  res[[anchorPair]] <-
                    dfBySourceAndStream[[currentSourceName]][[currentStreamName]][
                      currentStream[[anchorPair]]$indicesToExtract,
                      columns,
                      drop = FALSE
                    ];

                } else if (currentSource[[primaryStream]][[anchorPair]]$length <
                           currentStream[[anchorPair]]$length) {

                  msg("     This anchor pair (with indices ",
                      currentStream[[anchorPair]]$startIndex,
                      " and ", currentStream[[anchorPair]]$endIndex,
                      ") is longer than the primary stream (",
                      currentSource[[primaryStream]][[anchorPair]]$length,
                      "), compressing to that size.\n",
                      silent = silent);

                  res[[anchorPair]] <-
                    syncing_df_compress(
                      dfBySourceAndStream[[currentSourceName]][[currentStreamName]][
                        currentStream[[anchorPair]]$indicesToExtract,
                        columns,
                        drop=FALSE
                      ],
                      newLength = currentSource[[primaryStream]][[anchorPair]]$length,
                      sep = sep,
                      compressFun = compressFun,
                      compressFunPart = compressFunPart,
                      silent = silent
                    );

                  msg("     Done compressing: the resulting data frame has length ",
                      nrow(res[[anchorPair]]),
                      ".\n",
                      silent = silent);

                } else if (currentSource[[primaryStream]][[anchorPair]]$length >
                           currentStream[[anchorPair]]$length) {

                  msg("     This anchor pair (with indices ",
                      currentStream[[anchorPair]]$startIndex,
                      " and ", currentStream[[anchorPair]]$endIndex,
                      ") is shorter than the primary stream (",
                      currentSource[[primaryStream]][[anchorPair]]$length,
                      "), expanding to that size.\n",
                      silent = silent);

                  res[[anchorPair]] <-
                    syncing_df_expand(
                      dfBySourceAndStream[[currentSourceName]][[currentStreamName]][
                        currentStream[[anchorPair]]$indicesToExtract,
                        columns,
                        drop=FALSE
                      ],
                      newLength = currentSource[[primaryStream]][[anchorPair]]$length,
                      fill = fill,
                      neverFill = neverFill,
                      expandFun = expandFun,
                      silent = silent
                    );

                  msg("     Done expanding: the resulting data frame has length ",
                      nrow(res[[anchorPair]]),
                      ".\n",
                      silent = silent);

                }
              }

              return(res);

            }
          );
        names(res) <- names(currentSource[otherStreams]);
        return(res);
      }
    );

  names(streamMapping) <- names(anchorIndices);

  ###---------------------------------------------------------------------------
  ### Merge these dataframes into one dataframe per stream per source
  ###---------------------------------------------------------------------------

  msg("\n\n- Proceeding to merge the data frames for each anchor pair into a data frame for each stream.\n",
      silent = silent);

  mergedStreamDfs <-
    lapply(
      names(streamMapping),
      function(currentSourceName) {

        msg(" - Processing source with identifier: ", currentSourceName, ".\n",
            silent = silent);

        currentSource <- streamMapping[[currentSourceName]];

        res <-
          lapply(
            names(currentSource),
            function(currentStreamName) {

              msg("  - Processing stream with identifier: ", currentStreamName, ".\n",
                  silent = silent);

              msg("    Merging ", length(currentSource[[currentStreamName]]),
                  " 'anchor pair data frames' with ",
                  vecTxt(unlist(lapply(currentSource[[currentStreamName]], nrow))),
                  " rows.\n",
                  silent = silent);

              ### Note: changed on 2023-08-26 in response to the QUEST changes
              ### (i.e. no longer removing lines with only a code identifier)

              res <-
                rbind_df_list(
                #glue_df_list(
                  currentSource[[currentStreamName]]
                );

              if (prependStreamIdToColName) {
                names(res) <- paste0(currentStreamName,
                                     colNameGlue,
                                     names(res));
              }

              if (appendStreamIdToColName) {
                names(res) <- paste0(names(res),
                                     colNameGlue,
                                     currentStreamName);
              }

              msg("    Resulting data frame has ", nrow(res), " rows.\n",
                  silent = silent);

              codeIdentifiers <-
                x$convenience$codingLeaves;

              res <- rbind(
                res,
                ifelse(
                  names(res) %in% codeIdentifiers,
                  0,
                  NA
                )
              );

              msg("    Added one more row with just 0s ",
                  "(for code identifier columns) and NAs",
                  " to represent the row with the last anchor.\n",
                  silent = silent);

              return(res);
            }
          );
        names(res) <- names(currentSource);
        return(res);
      }
    );
  names(mergedStreamDfs) <- names(streamMapping);

  ###---------------------------------------------------------------------------
  ### Merge dataframes for each source
  ###---------------------------------------------------------------------------

  msg("\n- Proceeding to bind together the stream data frames.\n",
      silent = silent);

  syncedStreamDfs <-
    lapply(
      names(dfBySourceAndStream),
      function(sourceDfName) {

        msg(" - Processing the source with identifier '",
            sourceDfName, "'. Proceeding to bind together the data frames ",
            "from each stream. The primary stream ('", primaryStream,
            "') has 'anchor pair data frames' with ",
            vecTxt(unlist(lapply(anchorIndices[[sourceDfName]][[primaryStream]], `[[`, 'length'))),
            " rows, and in total, its data frame has ",
            nrow(dfBySourceAndStream[[sourceDfName]][[primaryStream]]),
            " rows; the other streams' data frames have ",
            vecTxt(unlist(lapply(mergedStreamDfs[[sourceDfName]], nrow))),
            " rows, respectively (this should be the same number).\n",
            silent = silent);

        return(
          do.call(
            cbind,
            c(
              list(
                dfBySourceAndStream[[
                  sourceDfName
                ]][[
                  primaryStream
                ]]
              ),
              unname(
                mergedStreamDfs[[
                  sourceDfName
                ]]
              )
            )
          )
        );
      }
    );

  names(syncedStreamDfs) <- names(dfBySourceAndStream);

  ###---------------------------------------------------------------------------
  ### Potentially carry-over anchors
  ###---------------------------------------------------------------------------

  if (carryOverAnchors) {
    syncedStreamDfs <-
      lapply(
        syncedStreamDfs,
        function(syncedStreamDf) {
          syncedStreamDf[[paste0(anchorsCol, "_persistent")]] <-
            rock::carry_over_values(
              syncedStreamDf[[anchorsCol]]
            );
          return(syncedStreamDf);
        }
      )

  }

  ###---------------------------------------------------------------------------
  ### Merge the synced stream data frames into one new 'mergedSourceDf'
  ###---------------------------------------------------------------------------

  mergedSourceDf <-
    rbind_df_list(
      syncedStreamDfs
    );

  ###---------------------------------------------------------------------------
  ### Compose sub-object with results and return it
  ###---------------------------------------------------------------------------

  x$syncResults <-
    list(
      sourceIds = sourceIds,
      streamIds = streamIds,
      dfBySource = dfBySource,
      dfBySourceAndStream = dfBySourceAndStream,
      fullAnchorVectors = fullAnchorVectors,
      anchorOnlyVectors = anchorOnlyVectors,
      anchorIndices = anchorIndices,
      streamMapping = streamMapping,
      mergedStreamDfs = mergedStreamDfs,
      syncedStreamDfs = syncedStreamDfs,
      mergedSourceDf = mergedSourceDf,
      qdt = mergedSourceDf
    );

  return(
    invisible(
      x
    )
  );

}
