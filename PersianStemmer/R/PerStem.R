PerStem <-
function(dat,NoEnglish=TRUE, NoNumbers=TRUE, 
                    NoStopwords=TRUE, NoPunctuation=TRUE,
                    StemVerbs = TRUE, NoPreSuffix= TRUE, Context = TRUE,
                    StemBrokenPlurals=TRUE,Transliteration=TRUE){
  # 1: Removes new line characters and fixes half-spaces
  dat <- RemNewlineHalfspace(dat)
  # 2: removes all unicode characters except Latin, Persian or General Punctuation characters 
  # and standardizes Persian characters.
  dat <- RefineChars(dat)
  # 3: removes English Characters
  if(NoEnglish){dat <- RemoveEnglish(dat)}
  # 4: removes numbers
  if(NoNumbers){dat <- RemoveNumbers(dat)}
  # 5: removes or fixes punctuation
  dat <- RFPunctuation(dat,NoPunctuation)
  # 6: fixes verbs
  if(StemVerbs){dat <- FixVerbs(dat,Context)}
  # 7: removes stopwords
  if(NoStopwords){dat <- RemoveStopwords(dat)}
  # 8: removes prefixes and suffixes
  if(NoPreSuffix){dat <- RemovePreSuffix(dat,Context)}
  # removes the numeric signifiers used in step six
  if(!NoPreSuffix){dat <- gsub("\u0030|\u0031|\u0032|\u0033|\u0034|\u0035","",dat)}
  # 9: fixes arabic broken plurals
  if(StemBrokenPlurals){dat <- FixBrokenPlurals(dat)}
  # 10: performs transliteration
  if(Transliteration){dat <- Transliterate(dat)}
  # removes extra spaces
  return(trim(gsub(" {2,}"," ", dat)))
}
