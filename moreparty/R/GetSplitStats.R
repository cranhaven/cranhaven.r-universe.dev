#' @export

GetSplitStats <- function(ct) {

  if(class(ct)[1]!="constparty") stop("ctree is not of class constparty from partykit package")
  
  allnodes <- partykit::nodeids(ct, terminal = FALSE)
  ternodes <- partykit::nodeids(ct, terminal = TRUE)
  splnodes <- setdiff(allnodes,ternodes)
  stats <- partykit::nodeapply(ct, ids = splnodes, FUN = function(x) partykit::info_node(x)$criterion)
  for(i in 1:length(stats)) {
    z <- as.data.frame(t(stats[[i]]))
    z <- z[order(-z$criterion),]
    z$ratio <- z$criterion/max(z$criterion)
    stats[[i]] <- z
  }

  su <- do.call("rbind.data.frame", lapply(stats, function(x) c(rownames(x)[1:2],x$ratio[2])))
  su <- data.frame(names(stats), su)
  names(su) <- c("node", "split_var", "best_var", "temp")
  su$temp <- as.numeric(su$temp)
  su$ratio <- character(length = nrow(su))
  su$ratio[su$temp <= 1000] <- as.character(round(su$temp[su$temp <= 1000], 2))
  su$ratio[su$temp > 1000] <- formatC(su$temp[su$temp > 1000], format="e", digits=1)
  su$temp <- NULL
  
  return(list(details = stats,
              summary = su))
}
