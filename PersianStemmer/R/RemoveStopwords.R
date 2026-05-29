RemoveStopwords <-
function(texts){
  textsSplit <- strsplit(texts," ")[[1]]
  # Conjunction (harf-e rabt)
  conjs <- c('\u0648',  #va
             '\u06CC',  #ye
             '\u067E\u0633',  #pas
             '\u0631\u0627',  #ra
             '\u062A\u0627',  #ta
             '\u0644\u06CC\u06A9\u0646',  #likan
             '\u0647\u0645',  #ham
             '\u06CC\u0627',  #ya
             '\u0627\u06AF\u0631',  #agar
             '\u0686\u0648\u0646',  #chun
             '\u0627\u0645\u0627',  #amma
             '\u0632\u06CC\u0631\u0627',  #zira
             '\u0646\u06CC\u0632',  #niz
             '\u0686\u0647',  #cheh
             '\u06A9\u0647', #keh
             '\u0648\u0644\u06CC', #vali
             '\u0628\u0644\u06A9\u0647',  #balkeh
             '\u0686\u0646\u0627\u0646\u0686\u0647', #chenancheh
             '\u0686\u0646\u0627\u0646\u06A9\u0647', #chenankeh
             '\u062E\u0648\u0627\u0647') #khah
  # Preposition 
  preps  <- c('\u062A\u0627',  #ta
              '\u0627\u0632',  #az
              '\u0628\u0647',  #beh
              '\u062F\u0631',  #dar
              '\u0628\u0627',  #ba
              '\u0628\u06CC\u0646', #bein
              '\u062C\u0644\u0648', #jelo
              '\u0645\u06CC\u0627\u0646') #mian
  # Subject pronouns
  pronouns <- c('\u0645\u0646',  #man
                '\u062A\u0648',  #to
                '\u0627\u0648',  #uo
                '\u0648\u06CC',  #vey
                '\u0645\u0627',  #ma
                '\u0634\u0645\u0627',  #shoma
                '\u0622\u0646\u0647\u0627',  #anha
                '\u0622\u0646\u0627\u0646',  #anan
                '\u0627\u06CC\u0634\u0627\u0646',  #ishan
                '\u0622\u0646',  #an
                '\u0627\u06CC\u0646') #in
  # Particles
  particles <- c('\u0622\u06CC\u0627',  #aya
                 '\u0631\u0627',  #ra
                 '\u0648')  #v
  # Other
  other <- c('\u0647\u0627',  #ha
             '\u0627\u06CC',  #i
             '\u0647\u0627\u06CC',#haye
             '\u0647\u0627\u06CC\u06CC',#hayi
             '\u0628\u0631',  #bar
             '\u067E\u06CC\u0634',  #pish
             '\u0628\u0631\u0627\u06CC',  #baray
             '\u0631\u0648\u06CC', #ruy
             '\u0628\u0643\u0644',  #bkl
             '\u062d\u062a\u0649',  #hatta
             '\u062a\u062d\u062a',  #taht
             '\u0641\u0648\u0642',  #fogh
             '\u062c\u0627\u0646\u0628',  #janb
             '\u062d\u0648\u0644', #hol
             '\u062e\u0644\u0627\u0644',  #Kklal
             '\u0642\u0628\u0644',  #ghabl
             '\u0628\u0639\u062f',  #ba'ad
             '\u0628\u062f\u0648\u0646',  #bdun
             '\u0641\u0642\u0637',  #faghat
             '\u0631\u063a\u0645',  #raghm
             '\u062d\u064a\u062b', #heith
             '\u0633\u062A', #st: short form of ast
             '\u0628\u0627\u0631', #bAr
             '\u0686\u0646\u062F', #chand
             '\u0628\u06CC', #bi
             '\u0627\u0633\u062A',#ast
             '\u062A\u0631\u06CC', #tari
             '\u062A\u0631\u06CC\u0646', #tarin
             '\u0622\u0646\u062C\u0627',#anja
             '\u0627\u06CC\u0646\u062C\u0627',#inja 
             '\u0622\u0646\u0686\u0647', #ancheh
             '\u0648\u0644\u0648', #valo
             '\u0627\u06CC\u0646\u06A9\u0647', #inkeh
             '\u0628\u0646\u0627\u0628\u0631\u06CC\u0646', #banabarin
             '\u062A\u0631', #tar
             '\u0647\u0631') #har 
  allstopwords <- c(conjs,preps,pronouns,particles,other)
  if(length(textsSplit) > 0){
    for(i in 1:length(allstopwords)){
      textsSplit <- gsub(paste0("^",allstopwords[i],"$"),"",textsSplit)
    } 
  }
  texts <- paste(textsSplit, collapse=" ")
  texts <- trim(gsub(" {2,}"," ", texts))
  return(texts)
}
