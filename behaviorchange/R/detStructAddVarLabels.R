#' @rdname detStructPreprocessing
#' @export
detStructAddVarLabels <- function(determinantStructure,
                                  varLabelDf,
                                  varNameCol = 'varNames.cln',
                                  leftAnchorCol = 'leftAnchors',
                                  rightAnchorCol = 'rightAnchors',
                                  subQuestionCol = 'subQuestions',
                                  questionTextCol = 'questionText') {

  data.tree::Do(nodes=data.tree::Traverse(determinantStructure,
                                          traversal = 'level',
                                          filterFun = function(x) return(!is.null(x$varNames))),
                fun=function(currentNode) {
    if (currentNode$type == 'subdeterminantProducts') {
      ### Look at $varNames[[1]] - check which ones occur in
      ### the list of productVarNames; then select the corresponding
      ### anchors, subquestions, and questiontexts.
      currentNode$leftAnchors <- rep('lo', length(currentNode$productVarNames));
      currentNode$rightAnchors <- rep('hi', length(currentNode$productVarNames));
      currentNode$subQuestions <- currentNode$productVarNames;
    } else {

      if (getOption('ufs.debug', FALSE)) {
        message("Debugging message:\n  Found ",
                ufs::vecTxtQ(unlist(currentNode$varNames)), ".\n");
      }

      currentNode$leftAnchors <- varLabelDf[varLabelDf[, varNameCol] %in%
                                              unlist(currentNode$varNames),
                                            leftAnchorCol];
      currentNode$rightAnchors <- varLabelDf[varLabelDf[, varNameCol] %in%
                                               unlist(currentNode$varNames),
                                             rightAnchorCol];
      currentNode$subQuestions <- varLabelDf[varLabelDf[, varNameCol] %in%
                                               unlist(currentNode$varNames),
                                             subQuestionCol];
      currentNode$questionTexts <- varLabelDf[varLabelDf[, varNameCol] %in%
                                                unlist(currentNode$varNames),
                                              questionTextCol];
    }
  });

}
