RemovePreSuffix <-
function(texts,Context){
  ## No-Stem List: a list of words to not stem
  notstemlist <- c('\u0627\u06CC\u0631\u0627\u0646', #Iran
                   '\u0645\u0634\u0631\u0648\u0637\u06CC\u062A', #Mashrutiat
                   '\u0639\u0645\u0631\u0627\u0646', #omrAn
                   '\u0628\u0631\u062C\u0627\u0645') #Barjam
  arabicplurals <- list(c('\u062F\u0648\u0644','\u062F\u0648\u0644\u062A'), #doval,dolat
                        c('\u0645\u0646\u0627\u0628\u0639','\u0645\u0646\u0628\u0639'), #manabe,manba
                        c('\u0627\u0645\u0648\u0631','\u0627\u0645\u0631'), #omur,amr
                        c('\u0627\u0641\u0631\u0627\u062F','\u0641\u0631\u062F'), #afrad,fard
                        c('\u0631\u0648\u0627\u0628\u0637','\u0631\u0627\u0628\u0637\u0647'), #ravabet,rabeteh
                        c('\u0636\u0648\u0627\u0628\u0637','\u0636\u0627\u0628\u0637\u0647'), #zavabet,zabeteh
                        c('\u0634\u0631\u0627\u06CC\u0637','\u0634\u0631\u0637'), #sharayet,shart
                        c('\u0642\u0648\u0627\u0646\u06CC\u0646','\u0642\u0627\u0646\u0648\u0646'), #ghavanin,ghanun
                        c('\u062D\u0642\u0648\u0642','\u062D\u0642'), #hoghuh,hagh
                        c('\u062D\u062F\u0648\u062F','\u062D\u062F'), #hodud,had
                        c('\u0645\u0631\u0627\u06A9\u0632','\u0645\u0631\u06A9\u0632'), #marakez,markaz
                        c('\u0627\u062D\u0632\u0627\u0628','\u062D\u0632\u0628'), #ahzab,hezb
                        c('\u0627\u0647\u062F\u0627\u0641','\u0647\u062F\u0641'), #ahdaf,hadaf 
                        c('\u0645\u062F\u0627\u0631\u06A9','\u0645\u062F\u0631\u06A9'), #madarek,madrak
                        c('\u0639\u0644\u0648\u0645','\u0639\u0644\u0645'), #olum,elm 
                        c('\u0645\u0642\u0627\u0644\u0627\u062A','\u0645\u0642\u0627\u0644\u0647'), #maghalat,maghaleh
                        c('\u0648\u0638\u0627\u06CC\u0641','\u0648\u0638\u06CC\u0641\u0647'), #vazayef,vazifeh
                        c('\u0648\u0632\u0631\u0627','\u0648\u0632\u06CC\u0631'), #vozara,vazir
                        c('\u0645\u0635\u0627\u062F\u06CC\u0642','\u0645\u0635\u062F\u0627\u0642'), #masadigh,mesdagh
                        c('\u0645\u0631\u0627\u062C\u0639','\u0645\u0631\u062C\u0639'), #maraje,marja
                        c('\u0627\u0634\u062E\u0627\u0635','\u0634\u062E\u0635'), #ashkhas,shakhs
                        c('\u062E\u0635\u0627\u06CC\u0635','\u062E\u0635\u06CC\u0635\u0647'), #khasayes,khasiseh
                        c('\u0634\u0648\u0627\u0647\u062F','\u0634\u0627\u0647\u062F')) #shavahed,shahed 
  textsSplit <- strsplit(texts," ")[[1]]
  if ("TRUE" %in% grepl("[0-9]$", textsSplit)){a <- textsSplit[-grep("[0-9]$", textsSplit)]}else{a <- textsSplit}
  if(length(textsSplit) > 0){
    for(i in 1:length(textsSplit)){
      word <- textsSplit[i]
      if(!(word %in% notstemlist) & !(word %in% unlist(arabicplurals)) & nchar(word)>2){
        word0 <- strsplit(word,"")[[1]]
        word1 <- fp1(word0);if((paste(word1,collapse="")) %in% c(notstemlist, unlist(arabicplurals))){
          textsSplit[i] <- paste(word1,collapse=""); next}
        word2 <- fs1(word1);if((paste(word2,collapse="")) %in% c(notstemlist, unlist(arabicplurals))){
          textsSplit[i] <- paste(word2,collapse=""); next}
        word3 <- fs2(word2);if((paste(word3,collapse="")) %in% c(notstemlist, unlist(arabicplurals))){
          textsSplit[i] <- paste(word3,collapse=""); next}
        word4 <- fs3(word3);if((paste(word4,collapse="")) %in% c(notstemlist, unlist(arabicplurals))){
          textsSplit[i] <- paste(word4,collapse=""); next}
        word5 <- fs4(word4);if((paste(word5,collapse="")) %in% c(notstemlist, unlist(arabicplurals))){
          textsSplit[i] <- paste(word5,collapse=""); next} 
        word6 <- fs5(word5);if((paste(word6,collapse="")) %in% c(notstemlist, unlist(arabicplurals))){
          textsSplit[i] <- paste(word6,collapse=""); next} 
        word7 <- fs6(word6);if((paste(word7,collapse="")) %in% c(notstemlist, unlist(arabicplurals))){
          textsSplit[i] <- paste(word7,collapse=""); next}
        word8 <- fs4(word7);if((paste(word8,collapse="")) %in% c(notstemlist, unlist(arabicplurals))){
          textsSplit[i] <- paste(word8,collapse=""); next} 
        word9 <- fs5(word8);if((paste(word9,collapse="")) %in% c(notstemlist, unlist(arabicplurals))){
          textsSplit[i] <- paste(word9,collapse=""); next} 
        word10 <- fs7(word9);if((paste(word10,collapse="")) %in% c(notstemlist, unlist(arabicplurals))){
          textsSplit[i] <- paste(word10,collapse=""); next} 
        if(!Context){textsSplit[i] <- paste(word10,collapse="")}
        if(Context){
          stems <- unique(c(word,
                            paste(word1,collapse=""),
                            paste(word2,collapse=""),
                            paste(word3,collapse=""),
                            paste(word4,collapse=""),
                            paste(word5,collapse=""),
                            paste(word6,collapse=""),
                            paste(word7,collapse=""),
                            paste(word8,collapse=""),
                            paste(word9,collapse=""),
                            paste(word10,collapse="")
          ))
          for (j in length(stems):1){
            b <- strsplit(stems[j],"")[[1]]
            if(b[length(b)] =="\u06AF"){
              b[length(b)] <- "\u0647"
              if(paste0(b,collapse="")%in%a){stems[j] <- paste0(b,collapse="")}
            }
            textsSplit[i] <- stems[j]
            if(stems[j]%in%a){break}
          }
        }
      }
    }
  } 
  texts <- paste(textsSplit,collapse=" ")
  texts <- gsub("\u0030|\u0031|\u0032|\u0033|\u0034|\u0035","",texts)
  texts <- trim(gsub(" {2,}"," ", texts))
  return(texts)
}
