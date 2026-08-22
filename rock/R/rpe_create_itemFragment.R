rpe_create_itemFragment <- function(item_text,
                                    uiid,
                                    itemResponse,
                                    metaquestions,
                                    metaquestion_responses,
                                    coderId,
                                    rpe_iterId,
                                    rpe_batchId,
                                    rpe_popId,
                                    caseId = NULL,
                                    rpe_itemEval_template = rock::opts$get("rpe_itemEval_template")) {

  rpe_uiid_idName = rock::opts$get("uiid_idName");
  rpe_iterId_idName = rock::opts$get("rpe_iterId");
  rpe_batchId_idName = rock::opts$get("rpe_batchId");
  rpe_popId_idName = rock::opts$get("rpe_popId");
  rpe_mq_idName = rock::opts$get("rpe_mq_idName");
  coderId_name = rock::opts$get("coderId_name");
  caseId_name = rock::opts$get("caseId_name");

  if (is.null(caseId) || (toupper(as.character(caseId)) == "NULL")) {
    caseIdBit <- "";
  } else {
    caseIdBit <- paste0("[[", caseId_name, "=", caseId, "]] ");
  }

  bar <- paste0("###", repStr("-", 75));

  res <- paste0("\n",
                bar,
                "\n---<<item_break>>---

[[", rpe_iterId_idName, "=", rpe_iterId, "]] [[",
rpe_batchId_idName, "=", rpe_batchId, "]] [[",
rpe_popId_idName, "=", rpe_popId, "]] ", caseIdBit, "[[",
coderId_name, "=", coderId, "]]

### Item text:

", item_text, "   [[", rpe_uiid_idName, ":", uiid, "]]

### Participants' response:

", itemResponse, "

### Meta questions:

",
paste0(metaquestions, "   [[", rpe_mq_idName, "=", names(metaquestions), "]]

", metaquestion_responses[names(metaquestions)], "

", collapse=""), "

", rpe_itemEval_template);

  return(res);

}
