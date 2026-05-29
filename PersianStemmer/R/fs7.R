fs7 <-
function(word){
  if(length(word) >6){
    if(paste(word[(length(word)-3):length(word)],collapse="") == '\u062A\u0631\u06CC\u0646'){
      word <- word[1:(length(word)-4)]}}; return(word)}
