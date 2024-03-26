GetOrder <- function(df){
  ord <- as.list( order( apply( df[!duplicated(df),], 1, paste, collapse="" ) ) )
  names(ord)<-apply(df[!duplicated(df),],1,paste,collapse="")
  uniq <- apply( df, 1, function(x) ord[[ paste0(x,collapse="") ]] )
  cbind(df, uniq )
}
