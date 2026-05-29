fs1 <-
function(word){
  if(length(word) >4){
    if(paste(word[(length(word)-1):length(word)],collapse="") == '\u0633\u062A'){
      word <- word[1:(length(word)-2)]}}; return(word)}
