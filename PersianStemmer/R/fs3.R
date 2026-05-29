fs3 <-
function(word){
  suf1 <- c('\u06CC', # 'y': indefinite maker
            '\u0645', # 'm': possessive suffix (first person singular)
            '\u062A', # 't': possessive suffix (second person singular)
            '\u0634') # 'sh': possessive suffix (third person singular)
  if(length(word) >3){
    if((paste(word[length(word)],collapse="") %in% suf1) & !(word[length(word)-1] %in% c("\u0627", "\u0648"))){
      word <- word[1: length(word)-1]}}; return(word)}
