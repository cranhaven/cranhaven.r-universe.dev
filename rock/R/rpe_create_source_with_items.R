#' Create a source with items to code for Response Process Evaluation
#'
#' This function creates a plain text file, a `.rock` source, that can be
#' coded when conducting Response Process Evaluation.
#'
#' @param data A (wide) data frame containing at least the participants'
#' answers to the items and to the meta questions (but optionally, the
#' iteration, batch, and population).
#' @param iterationId,batchId,populationId If the iteration, batch, and
#' population identifiers are contained in the data frame passed as `data`,
#' the variable names holding that information for each participant;
#' otherwise, either a single value or a vector of length `nrow(data)` that
#' contains that information for each participant.
#' @param metaquestionIdentifiers A named list of unnamed character vectors,
#' with each character vector element specifying the identifier of a meta
#' question, and each list element (i.e. the name of each character vector)
#' specifying the item identifier that the meta questions in the corresponding
#' character vector belong to.
#' @param itemVarNames The variable names with the participants' responses
#' to the items, in a named character vector, with each element's name being
#' the item's identifier, and each element the variable name in `data` holding
#' the participants' responses to the item.
#' @param metaquestionVarNames The variable names with the participants'
#' responses to the meta questions, in a named character vector, with each
#' element's name being the meta question's identifier, and each element
#' the variable name in `data` holding the participants' responses to the
#' meta question.
#' @param itemContents A named character vector with each item's content, with
#' the values being the content and the names the item identifiers.
#' @param metaquestionContents A named character vector with each meta
#' question's content, with the values being the content and the names the
#' meta question identifiers.
#' @param coderId The identifier of the coder that will code this source.
#' @param caseIds The variable name with the participants' case identifiers (i.e.
#' a unique identifier for each participant).
#' @param preventOverwriting Whether to overwrite existing files (`FALSE`) or
#' prevent that from happening (`TRUE`).
#' @param encoding The encoding to use when writing the source(s).
#' @param silent Whether to the silent (`TRUE`) or chatty (`FALSE`).
#' @param outputFile Optionally, a file to write the source to.
#'
#' @return The created source, as a character vector (invisibly);
#' @export
rpe_create_source_with_items <- function(data,
                                         iterationId,
                                         batchId,
                                         populationId,
                                         itemVarNames,
                                         metaquestionIdentifiers,
                                         metaquestionVarNames,
                                         itemContents,
                                         metaquestionContents,
                                         coderId,
                                         caseIds = NULL,
                                         outputFile = NULL,
                                         preventOverwriting = rock::opts$get("preventOverwriting"),
                                         encoding = rock::opts$get("encoding"),
                                         silent = rock::opts$get("silent")) {

  bar <- paste0("###", repStr("-", 75));

  res <- c(bar,
           "### ROCK RPE file",
           bar,
           "");

  if (iterationId %in% names(data)) {
    iterationId <- data[, iterationId];
  }
  if (batchId %in% names(data)) {
    batchId <- data[, batchId];
  }
  if (populationId %in% names(data)) {
    populationId <- data[, populationId];
  }

  for (participantRowNr in 1:nrow(data)) {
    for (itemId in names(itemVarNames)) {

      res <- c(
        res,
        rpe_create_itemFragment(
          item_text = itemContents[itemId],
          uiid = itemId,
          itemResponse =
            data[participantRowNr, itemVarNames[itemId]],
          metaquestions =
            metaquestionContents[metaquestionIdentifiers[[itemId]]],
          metaquestion_responses =
            data[
              participantRowNr,
              metaquestionVarNames[metaquestionIdentifiers[[itemId]]]
            ],
          coderId = coderId,
          rpe_iterId = iterationId[participantRowNr],
          rpe_batchId = batchId[participantRowNr],
          rpe_popId = populationId[participantRowNr],
          caseId = ifelse(is.null(caseIds),
                          list(NULL),
                          caseIds[participantRowNr])
        )
      );
    }
  }

  if (!is.null(outputFile)) {

    writeTxtFile(x = res,
                 output = outputFile,
                 encoding = encoding,
                 preventOverwriting = preventOverwriting,
                 silent = silent);

  }

  return(res);

}
