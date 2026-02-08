#' Resolve canonical names against GNA
#'
#' Resolve names against Global Names Architecture (GNA) to make sure the name
#' exists
#'
#' @param taxolist (data frame) taxonomic list
#' @param sciname () column name for scientific names
#' @param score_threshold (numeric) to make sure names match as desired. 
#' Default (0.98)
#' Higher value indicates best match, lower values would return matches at 
#' genus level 
#' @param best_match_only (logical) If TRUE, best match only returned else 
#' return all records returned by GNA. Default: TRUE
#' @param add_fields (character) One of NA (default) , minimal or all. NA adds a 
#' logical column 'resolved', Minimal gives back just four fields, whereas all 
#' gives all fields back.
#' @param verbose (logical) verbose output, Default: FALSE
#' @return (data frame) names list resolves
#'
#' @family Name functions
#' @importFrom taxize gna_verifier
#' @examples
#' \donttest{
#' mylist <- data.frame("canonical" = c("Abrothrix longipilis",
#'                                      "Acodon hirtus",
#'                                      "Akodon longipilis apta",
#'                                      "AKODON LONGIPILIS CASTANEUS",
#'                                      "Chroeomys jelskii",
#'                                      "Acodon jelskii pyrrhotis"),
#'                      stringsAsFactors = FALSE)
#' test <- resolve_names(mylist)
#' test1 <- resolve_names(mylist,add_fields = "minimal")
#' test2 <- resolve_names(mylist,best_match_only = FALSE,add_fields = "minimal")
#' test3 <- resolve_names(mylist,best_match_only = FALSE,add_fields = "all")
#' }
#' @export
resolve_names <- function(taxolist,
                           sciname="canonical",
                           score_threshold=0.98,
                           best_match_only=TRUE,
                           add_fields= NA,
                           verbose=TRUE){
  taxolist <- rename_column(taxolist,sciname,"canonical__")
  taxolist$resolved <- FALSE
  taxores <- NULL
  get_fields <- ifelse(!is.na(add_fields) & add_fields=="all","all","minimal")
  if(verbose){pb = txtProgressBar(min = 0, max = nrow(taxolist), initial = 0)}
  for(i in 1:nrow(taxolist)){
    recres <- NULL
    recres <- tryCatch(
      expr = {
        message(gna_verifier(sci  = c(taxolist$canonical__[i])))
      },
      error = function(e){
        cat(paste("\n",e))
        resrec <- NULL
      },
      warning = function(w){
        cat(paste("\n",w))
      },
      finally = {
      }
    )   
    if(is.null(recres)){return(NULL)}
    recres <- recres[which(recres$score>score_threshold),]
    if(nrow(recres)>0){
      # Only add a tag
      if(is.na(add_fields)){
        taxolist$resolved[i] <- TRUE
        if(verbose){setTxtProgressBar(pb,i)}
        next
      } 
      #use only best matching (first) result
      if(best_match_only){
        recres <- recres[1,]
      } 
      recres <- cbind(taxolist[i,], recres, row.names = NULL)
      taxores <- plyr::rbind.fill(taxores,recres)
    }
    
    if(verbose){setTxtProgressBar(pb,i)}
  }
  if(verbose){cat("\n")}
  if(is.na(add_fields)){
    taxolist <- rename_column(taxolist,"canonical__",sciname)
    return(taxolist) 
  }else {
    taxores <- rename_column(taxores,"canonical__",sciname)
    return(taxores)
  }
}
