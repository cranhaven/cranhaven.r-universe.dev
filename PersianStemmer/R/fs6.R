fs6 <-
function(word){
  suf2 <- c('\u0647\u0627', # 'ha': plural maker
            '\u0627\u0646', # 'an': plural maker
            '\u06CC\u0646', # 'in': plural maker
            '\u062A\u0631') # 'tar': comparative adjective maker
  if(length(word) >4){
    if(paste(word[(length(word)-1):length(word)],collapse="") %in% suf2){
      word <- word[1:(length(word)-2)]}}; return(word)}
