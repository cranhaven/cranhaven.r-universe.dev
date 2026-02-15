#' @import org.Hs.eg.db
entrez_to_name<-function(list_of_ent){
  ln<-length(list_of_ent[[1]])
  list_of_ent<-list_of_ent[[1]]
  out<-unlist(lapply(1:ln, function(i){
    return(annotate::lookUp(list_of_ent[i],'org.Hs.eg', 'SYMBOL'))
  }))
  return(out)
}
