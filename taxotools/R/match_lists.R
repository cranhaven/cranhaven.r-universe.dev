#' match two taxonomic lists
#'
#' match two taxonomic lists using canonical names
#'
#' @param master master taxonomic list
#' @param checklist match  taxonomic list
#' @param masterfld field name for canonical name in master list
#' @param checklistfld field name for canonical name in match list
#' @family List functions
#' @return a list with data frames containing matched records,
#' records only in master and checklist and statistics about the
#' records including Jaccard index
#' @examples
#' \donttest{
#'master <- data.frame("canonical" = c("Abrothrix longipilis",
#'                                     "Acodon hirtus",
#'                                     "Akodon longipilis apta",
#'                                     "Akodon longipilis castaneus",
#'                                     "Chroeomys jelskii",
#'                                     "Acodon jelskii pyrrhotis"),
#'                     stringsAsFactors = FALSE)
#'checklist <- data.frame("canonical" = c("Abrothrix longipilis",
#'                                     "Akodon longipilis apta",
#'                                     "Akodon longipilis castaneus",
#'                                     "Abrothrix jelskii",
#'                                     "Acodon jelskii pyrrhotis"),
#'                     stringsAsFactors = FALSE)
#' match_lists(master,checklist,"canonical","canonical")
#' }
#' @export
match_lists <- function(master,checklist,masterfld,checklistfld){
  retval <- NULL
  if(masterfld==""){
    return(NULL)
  } else {
    master <- rename_column(master,masterfld,"masterfld")
    master$masterfld <- as.character(master$masterfld)
  }
  if(checklistfld==""){
    return(NULL)
  } else {
    checklist <- rename_column(checklist,checklistfld,"checklistfld")
    checklist$checklistfld <- as.character(checklist$checklistfld)
  }
  retval$matchlist <- master[which(master$masterfld %in%
                                     checklist$checklistfld),]
  retval$onlymaster <- master[which(master$masterfld %!in%
                                      checklist$checklistfld),]
  retval$onlychecklist <-  checklist[which(checklist$checklistfld %!in%
                                             master$masterfld),]
  retval$matchlist <- rename_column(retval$matchlist,"masterfld",
                                    masterfld,silent=TRUE)
  retval$onlymaster <- rename_column(retval$onlymaster,"masterfld",
                                     masterfld,silent=TRUE)
  retval$onlychecklist <- rename_column(retval$onlychecklist,"checklistfld",
                                        checklistfld,silent=TRUE)
  retval$stat$masterrec <- nrow(master)
  retval$stat$checkrec <- nrow(checklist)
  retval$stat$matchrec <- nrow(retval$matchlist)
  retval$stat$onlymasterrec <- nrow(retval$onlymaster)
  retval$stat$onlychecklistrec <- nrow(retval$onlychecklist)
  if("accid" %in% names(master) & "accid" %in% names(checklist)){
    retval$stat$mastertaxa <- nrow(master[which(master$accid==0),])
    retval$stat$checktaxa <- nrow(checklist[which(checklist$accid==0),])
    retval$stat$matchtaxa <- nrow(retval$matchlist[which(retval$matchlist$accid==0),])
    retval$stat$onlymastertaxa <- nrow(retval$onlymaster[which(retval$onlymaster$accid==0),])
    retval$stat$onlychecklisttaxa <- nrow(retval$onlychecklist[which(retval$onlychecklist$accid==0),])
  }
  retval$stat$jaccard <- nrow(retval$matchlist) /
    ( nrow(retval$matchlist) +
        nrow(retval$onlymaster) +
        nrow(retval$onlychecklist) )
  return(retval)
}
