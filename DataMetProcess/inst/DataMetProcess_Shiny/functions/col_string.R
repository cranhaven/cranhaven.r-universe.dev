col_string <- function(
    data = NULL,
    ncol = 1,
    str = NULL,
    usestr = FALSE
){
  if(usestr){
    unlist(data[str],use.names = F)
  }else{
    unlist(data[colnames(data)[ncol]],use.names = F)
  }
}
