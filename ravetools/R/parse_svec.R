deparse_svec <- function(nums, connect = '-', concatenate = TRUE, collapse = ',', max_lag = 1){
  nums <- nums[is.finite(nums)]
  if(length(nums) == 0){
    return('')
  }
  alag <- seq_len(max(1, max_lag))
  nums <- sort(unique(nums))
  lg <- c(NA, nums)[seq_len(length(nums))]
  ind <- nums - lg
  ind[1] <- 0
  ind2 <- c(ind[-1], -1)
  re <- apply(cbind(nums[!ind %in% alag], nums[!ind2 %in% alag]), 1, function(x){
    if(x[1] == x[2]){
      as.character(x[1])
    }else{
      paste(x, collapse = connect)
    }
  })
  if(concatenate){
    re <- paste(re, collapse = collapse)
  }
  re
}


parse_svec <- function(text, sep = ',', connect = '-:|', sort = FALSE, unique = TRUE){
  connect <- unique(unlist(strsplit(connect, '')))
  connect[connect %in% c('|', ':', '~')] <- paste0('\\', connect[connect %in% c('|', ':', '~')])
  if('-' %in% connect) {
    connect <- c(connect[connect != "-"], "-")
  }
  connect <- paste(connect, collapse = '')

  if(length(text) != 1) {
    text <- paste(text, collapse = sep)
  }


  if(length(text) == 0 || !nzchar(trimws(text))){
    return(NULL)
  }

  if(is.numeric(text)){
    if(unique) {
      text <- unique(text)
    }
    if(sort) {
      text <- sort(text)
    }
    return(text)
  }
  s <- unlist(strsplit(text, sep, perl = TRUE))
  s <- trimws(s)
  s <- s[s!='']

  s <- s[grepl(sprintf('^[0-9\\ %s]+$', connect), s)]

  re <- NULL
  for(ss in s){
    if(grepl(sprintf('[%s]', connect), ss)){
      ss <- unlist(strsplit(ss,  sprintf('[%s]', connect), perl = TRUE))
      ss <- trimws(ss)
      ss <- ss[grepl('^[0-9]+$', ss)]
      ss <- as.numeric(ss)
      ss <- ss[!is.na(ss)]
      if(length(ss) >= 2){
        re <- c(re, (ss[1]:ss[2]))
      }
    }else{
      re <- c(re, as.numeric(ss))
    }
  }

  if(unique){
    re <- unique(re)
  }

  if(sort){
    re <- sort(re)
  }

  return(re)
}
