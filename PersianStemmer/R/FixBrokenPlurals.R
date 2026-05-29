FixBrokenPlurals <-
function(texts){
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
  if(length(textsSplit) > 0){
    for(i in 1:length(arabicplurals)){
      textsSplit <- gsub(paste0("^",arabicplurals[[i]][1],"$"),arabicplurals[[i]][2],textsSplit)
    } 
  }
  texts <- paste(textsSplit, collapse=" ")
  return(texts)
}
