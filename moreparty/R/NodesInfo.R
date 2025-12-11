#' @importFrom partykit nodeapply as.simpleparty nodeids info_node

#' @export

NodesInfo <- function(ct) {
  
  response <- ct$fitted[, "(response)"]
  
  rule <- list.rules.party(ct)
  id <- names(rule)
  rule <- as.character(rule)
  rule <- gsub("%in%", "in", rule, fixed = TRUE)
  rule <- gsub("c(", "(", rule, fixed = TRUE)
  rule <- gsub("\"", "", rule, fixed = TRUE)
  rule <- gsub("NA, ", "", rule)
  rule <- gsub(", NA", "", rule)
  rule <- sapply(rule, simplify_rule)
  rule <- data.frame(id = factor(id), rule)
  rownames(rule) <- NULL

  if (is.numeric(response)) {
    info <- partykit::nodeapply(partykit::as.simpleparty(ct), ids = partykit::nodeids(ct, terminal = TRUE), partykit::info_node)
    rule$freq <- sapply(info, function(x) x$n)
    rule$prediction <- sapply(info, function(x) x$prediction)
    names(rule)[names(rule) == "prediction"] <- setdiff(
      as.character(attr(ct$terms, "variables"))[-1],
      attr(ct$terms, "term.labels"))
    
  } else if (is.factor(response)) {
    info <- partykit::nodeapply(partykit::as.simpleparty(ct), ids = partykit::nodeids(ct, terminal = TRUE), partykit::info_node)
    rule$freq <- sapply(info, function(x) x$n)
    prob <- t(sapply(info, function(x) x$distribution))
    prob <- round(apply(prob, 2, function(x) x/rule$freq) * 100, 2)
    prob <- as.data.frame(prob)
    yname <- setdiff(
      as.character(attr(ct$terms, "variables"))[-1],
      attr(ct$terms, "term.labels"))
    names(prob) <- paste(yname, levels(response), sep = ".")
    rule <- data.frame(rule, prob)
    rownames(rule) <- NULL

  } else if (is.data.frame(response)) {
    pred_node <- predict(ct, type = "node")
    weights <- ct$fitted[, "(weights)"]
    rule$freq <- as.numeric(stats::xtabs(weights ~ pred_node))
    res <- list()
    for(i in 1:ncol(response)) {
      if(is.numeric(response[,i])) {
        res[[i]] <- data.frame(GDAtools:::agg.wtd.mean(response[,i], pred_node, weights))
        names(res[[i]]) <- names(response)[i]
      } else if(is.factor(response[,i])) {
        res[[i]] <- descriptio::weighted.table(pred_node, response[,i], weights = weights, stat = "rprop", digits = 2)
        res[[i]] <- as.data.frame(res[[i]])
        names(res[[i]]) <- paste(names(response)[i], levels(response[,i]), sep = ".")
      }
    }
    res <- do.call("cbind.data.frame", res)
    rule <- data.frame(rule, res)
    rownames(rule) <- NULL
  }

  return(rule)
}
