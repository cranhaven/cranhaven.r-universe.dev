### Helper functions



`stcode_append<-` = function(x, value) {
  paste(x, value, sep='\n');
}


`stdata_append<-` = function(x, value) {
  if(!is.null(names(value))){
    x = c(x, value)
  }else{
    vv = deparse(substitute(value))
    x[[vv]] = value
  }
  x
}

set_var = function(var, type, dims=NULL, value=NULL, upper=NULL, lower=NULL, add_dims=NULL, add_exp="", ...){
  stopifnot(!is.null(var), !is.null(type))
  if(!is.null(dims)){
    stopifnot(all(is.integer(dims)))
    st_dims = glue("[{paste(dims, collapse=',')}]")
  } else {
    st_dims = ""
  }
  if(!is.null(add_dims)){
    stopifnot(all(is.integer(add_dims)))
    st_add_dims = glue("[{paste(add_dims, collapse=',')}]")
  } else {
    st_add_dims = ""
  }
  if(!is.null(value)){
    stopifnot(is.numeric(value))
    if(length(dims>1)) stop("To assign more complex intial values use the add_exp argument.")
    st_value = glue("= {value}")
  } else {
    st_value=""
  }
  if(!is.null(upper)){
    stopifnot(is.numeric(upper), length(upper)==1)
    st_upper = glue("upper={upper}>")
    if(is.null(lower)) st_upper = paste0("<",st_upper)
  } else {
    st_upper = ""
  }
  if(!is.null(lower)){
    stopifnot(is.numeric(lower), length(lower)==1)
    st_lower = as.character(glue("<lower={lower}"))
    st_lower = paste0(st_lower, ifelse(is.null(upper),  ">",  ","))
    
  } else {
    st_lower = ""
  }
  glue("{type}{st_lower}{st_upper}{st_dims} {var}{st_add_dims}{st_value}{add_exp};", ...)
}

set_prior = function(var, dist, params, cond=F, target=NULL){
  if(!cond){
    glue("{var} ~ {dist}({paste0(params, collapse=',')}); ")  
  }else{
    link = ifelse(target=="target", "+=", "=")
    glue("{target} {link} {dist}({var}|{paste0(params, collapse=',')}); ")
  }
}

