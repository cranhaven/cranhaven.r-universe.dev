fs4 <-
function(word){
  if(length(word) >4){
    if(paste(word[(length(word)-1):length(word)],collapse="") == '\u06CC\u06CC'){
      word <- word[1:(length(word)-2)]}}; return(word)}
