RFPunctuation <-
function(texts,NoPunctuation){
  punctuation <-  c("\u060c","\u061b","\u061f","\u066c",
                     "\u066a","\u066b","\u066c","\u066d",
                     "\u06d4","\u2018","\u2022","\u2013",
                     "\u2026","\u201c","\u201d","\u2019",
                     "\u00AB","\u00BB","\u00D7","\u00F7",
                     "\u0021","\u0022","\u0023","\\\u0024",
                     "\u0025","\u0026","\u0027","\\\u0028",
                     "\u0029","\\\u002A","\\\u002B","\u002C",
                     "\u002D","\\\u002E","\u002F","\u003A",
                     "\u003B","\u003C","\u003D","\u003E",
                     "\\\u003F","\u0040","\\\u005B","\\\u005C",
                     "\u005D","\\\u005E","\u005F","\u0060",
                     "\\\u007B","\\\u007C","\u007D","\u007E")
  # if NoPunctuation = TRUE, removes punctuation
  if(NoPunctuation){
    puns <- paste(punctuation,collapse = "|")
    texts <-gsub(puns,"",texts)
  }
  # if NoPunctuation = FALSE, inserts spaces before and after punctuation.
  # This way punctuation characters can be used in text analysis as separate units. 
  if(!NoPunctuation){
    for (i in 1:length(punctuation)){
      x <- punctuation[i]
      y <- paste0(" ",punctuation[i]," ")
      texts <-gsub(x,y,texts)
    }
  }
  # removes extra spaces
  return(trim(gsub(" {2,}"," ", texts)))
}
