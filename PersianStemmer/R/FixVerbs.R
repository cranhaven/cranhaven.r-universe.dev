FixVerbs <-
function(texts,Context){
  # Present Perfect Suffixes
  preper <- c("\u0627\u0645",
              "\u0627\u06CC",
              "\u0627\u0633\u062A",
              "\u0627\u06CC\u0645",
              "\u0627\u06CC\u062F",
              "\u0627\u0646\u062F")
  # Past Perfect Suffixes
  pasper <- c("\u0628\u0648\u062F\u0645",
              "\u0628\u0648\u062F\u06CC",
              "\u0628\u0648\u062F",
              "\u0628\u0648\u062F\u06CC\u0645",
              "\u0628\u0648\u062F\u06CC\u062F",
              "\u0628\u0648\u062F\u0646\u062F")
  # Past Subjunctive Suffixes
  passub <- c("\u0628\u0627\u0634\u0645",
              "\u0628\u0627\u0634\u06CC",
              "\u0628\u0627\u0634\u062F",
              "\u0628\u0627\u0634\u06CC\u0645",
              "\u0628\u0627\u0634\u06CC\u062F",
              "\u0628\u0627\u0634\u0646\u062F")
  # Supplement Suffixes
  sup <- c("\u0634\u062F\u0647",
           "\u0628\u0648\u062F\u0647")
  # Present Perfect and Past Subjunctive Suffixes (Negative and Positive), 
  # Past Perfect and Supplement Suffixes (Negative, Positive, Simple and Progressive)
  paslist <- c(preper,
               pasper,
               paste('\u0646\u0645\u06CC',pasper,sep=""),
               paste('\u0645\u06CC',pasper,sep=""),
               paste('\u0646',pasper,sep=""),
               passub,
               paste('\u0646\u0645\u06CC',passub,sep=""),
               paste('\u0645\u06CC',passub,sep=""),
               paste('\u0646',passub,sep=""),
               sup,
               paste('\u0646\u0645\u06CC',sup,sep=""),
               paste('\u0645\u06CC',sup,sep=""),
               paste('\u0646',sup,sep=""))
  # Present Perfect and Past Subjunctive Suffixes (Negative), 
  # Past Perfect and Supplement Suffixes (Negative, Simple and Progressive)
  paslistneg <- c(paste('\u0646\u0645\u06CC',pasper,sep=""),
                  paste('\u0646',pasper,sep=""),
                  paste('\u0646\u0645\u06CC',passub,sep=""),
                  paste('\u0646',passub,sep=""),
                  paste('\u0646\u0645\u06CC',sup,sep=""),
                  paste('\u0646',sup,sep=""))
  # Past Perfect and Supplement Suffixes (Negative Progressive)
  paspersupnegpro <- c(paste('\u0646\u0645\u06CC',pasper,sep=""),
                       paste('\u0646\u0645\u06CC',sup,sep=""))
  # Future Prefixes (Positive)
  futurepossin <- c("\u062E\u0648\u0627\u0647\u0645",
                    "\u062E\u0648\u0627\u0647\u06CC",
                    "\u062E\u0648\u0627\u0647\u062F")
  futureposplu <- c("\u062E\u0648\u0627\u0647\u06CC\u0645",
                    "\u062E\u0648\u0627\u0647\u06CC\u062F",
                    "\u062E\u0648\u0627\u0647\u0646\u062F")
  futurepos <- c(futurepossin,futureposplu)
  # Future Prefixes (Negative)
  futureneg <- paste('\u0646',futurepos,sep="")
  # Future Prefixes (Positive and Negative)
  future <- c(futurepos,futureneg)
  # Progressive Future Prefixes (Positive)
  futureposprogsin <- paste('\u0645\u06CC',futurepossin,sep="")
  futureposprogplu <- paste('\u0645\u06CC',futureposplu,sep="")
  futureposprog <- paste('\u0645\u06CC',futurepos,sep="")
  # Progressive Future Prefixes (Negative)
  futurenegprogsin <- paste('\u0646',futureposprogsin,sep="")
  futurenegprogplu <- paste('\u0646',futureposprogplu,sep="")
  futurenegprog <- paste('\u0646',futureposprog,sep="")
  # Progressive Future Prefixes (Positive and Negative)
  futureprog <- c(futureposprog,futurenegprog)
  # Simple Present Suffixes (Singular)
  pressinsuf <- c("\u0645","\u06CC","\u062F")
  # Simple Present Suffixes (Plural)
  presplusuf <- c("\u06CC\u0645","\u06CC\u062F","\u0646\u062F")
  # Passive Simple Present Suffixes (Positive)
  simprepasspos <- c("\u0634\u0648\u0645",
                     "\u0634\u0648\u06CC",
                     "\u0634\u0648\u062F",
                     "\u0634\u0648\u06CC\u0645",
                     "\u0634\u0648\u06CC\u062F",
                     "\u0634\u0648\u0646\u062F")
  # Passive Simple Present Suffixes (Negative)
  simprepassneg <- paste('\u0646',simprepasspos,sep="")
  # Passive Simple Present Suffixes (Positive and Negative)
  simprepass <- c(simprepasspos,simprepassneg)
  # Passive Progressive Present Suffixes (Positive)
  proprepasspos <- paste('\u0645\u06CC',simprepasspos,sep="")
  # Passive Progressive Present Suffixes (Negative)
  proprepassneg <- paste('\u0646',proprepasspos,sep="")
  # Passive Progressive Present Suffixes (Positive and Negative)
  proprepass <- c(proprepasspos,proprepassneg)
  # Passive Simple Past Suffixes (Positive)
  simpaspasspos <- c("\u0634\u062F\u0645",
                     "\u0634\u062F\u06CC",
                     "\u0634\u062F",
                     "\u0634\u062F\u06CC\u0645",
                     "\u0634\u062F\u06CC\u062F",
                     "\u0634\u062F\u0646\u062F")
  # Passive Simple Past Suffixes (Negative)
  simpaspassneg <- paste('\u0646',simpaspasspos,sep="")
  # Passive Simple Past Suffixes (Positive and Negative)
  simpaspass <- c(simpaspasspos,simpaspassneg)
  # Passive Progressive Past Suffixes (Positive)
  propaspasspos <- paste('\u0645\u06CC',simpaspasspos,sep="")
  # Passive Progressive Past Suffixes (Negative)
  propaspassneg <- paste('\u0646',propaspasspos,sep="")
  # Passive Progressive Past Suffixes (Positive and Negative)
  propaspass <- c(propaspasspos,propaspassneg)
  # All Passive Past and Present Suffixes (Simple and Progressive)
  pass <- c(simprepass,proprepass,simpaspass,propaspass)
  # Suppliment Prefixes before Progressive Past and Presenet: 
  # (ex: "dashtam" miraftam, "daram" miravam)
  suppresinpro <- c("\u062F\u0627\u0631\u0645",
                    "\u062F\u0627\u0631\u06CC",
                    "\u062F\u0627\u0631\u062F")
  suppreplupro <- c("\u062F\u0627\u0631\u06CC\u0645", 
                    "\u062F\u0627\u0631\u06CC\u062F",
                    "\u062F\u0627\u0631\u0646\u062F")
  supprepro <- c(suppresinpro, suppreplupro)
  suppassinpro <- c("\u062F\u0627\u0634\u062A\u0645", 
                    "\u062F\u0627\u0634\u062A\u06CC", 
                    "\u062F\u0627\u0634\u062A")
  suppasplupro <- c("\u062F\u0627\u0634\u062A\u06CC\u0645", 
                    "\u062F\u0627\u0634\u062A\u06CC\u062F", 
                    "\u062F\u0627\u0634\u062A\u0646\u062F")
  suppaspro <- c(suppassinpro, suppasplupro)
  # This is a list of words that can be confused with verbs.
  NoStemVerb <- c('\u0647\u0645\u0647', #hameh
                  '\u0639\u062F\u0647', #edeh
                  '\u0646\u0627\u0645\u0647', #nameh
                  '\u0645\u06CC\u0644\u0627\u062F\u06CC') #miladi
  # This loop removes the space between the "mi" and "nmi" prefixes and verbs
  textsSplit <- strsplit(texts," ")[[1]]
  for(i in 1:length(textsSplit)){
    if(textsSplit[i] == '\u0646\u0645\u06CC'){
      textsSplit[i] <- ""
      textsSplit[i+1] <- paste('\u0646\u0645\u06CC', textsSplit[i+1], sep="")}
    if(textsSplit[i] == '\u0645\u06CC'){
      textsSplit[i] <- ""
      textsSplit[i+1] <- paste('\u0645\u06CC', textsSplit[i+1], sep="")}
  }
  # This loop detects and stems the verbs
  texts <- paste(textsSplit, collapse=" ")
  texts <- trim(gsub(" {2,}"," ", texts))
  textsSplit <- strsplit(texts," ")[[1]]
  for(i in 1:length(textsSplit)){
    word1 <- textsSplit[i]
    word2 <- textsSplit[i+1]
    word3 <- textsSplit[i+2]
    word4 <- textsSplit[i+3]
    word1str <- strsplit(word1,"")[[1]]
    word2str <- strsplit(word2,"")[[1]]
    word3str <- strsplit(word3,"")[[1]]
    # 1- Absolute Past Perfect - Passive 
    # (ex.neveshteh shodeh budeh ast)
    if (!(word1 %in% NoStemVerb) & (word4 %in% paslist) & (word3 %in% paslist) & (word2 %in% paslist)){
      if(length(word1str) >= 3){
        if(word1str[length(word1str)] == '\u0647'){
          word1str[length(word1str)] <- ""
          if(paste(word1str[1:3],collapse="") == '\u0646\u0645\u06CC'){word1str[2:3] <- ""}
          if(paste(word1str[1:2],collapse="") == '\u0645\u06CC'){word1str[1:2] <- ""}
          Test <- paste0("^(|\u0646\u0645\u06CC|\u0645\u06CC|\u0646)",paste0(word1str,collapse=""),"(|\u0646|\u0645|\u06CC|\u06CC\u0645|\u06CC\u062F|\u0646\u062F)$")
          PWord <- paste0(word1str,collapse="")
          PWord <- paste(PWord,"\u0030",sep="")
          if((word2 %in% paslistneg) | (word3 %in% paslistneg)){PWord <- paste("\u0646",PWord,sep="")}
          if(Context){if(TRUE %in% grepl(Test, textsSplit)){textsSplit[i] <- PWord}}
          if(!Context){textsSplit[i] <- PWord}
          textsSplit[i+1] <- ""
          textsSplit[i+2] <- ""
          textsSplit[i+3] <- ""
          next
        }
      }
    }
    # 2- Past Perfect, Present Perfect and Subjunctive Past - Passive 
    # (ex.neveshteh shodeh ast, neveshteh shodeh bud, neveshteh shodeh bashad)
    if (!(word1 %in% NoStemVerb) & (word3 %in% paslist) & (word2 %in% paslist)){
      if(length(word1str) >= 3){
        if(word1str[length(word1str)] == '\u0647'){
          word1str[length(word1str)] <- ""
          if(paste(word1str[1:3],collapse="") == '\u0646\u0645\u06CC'){word1str[2:3] <- ""}
          if(paste(word1str[1:2],collapse="") == '\u0645\u06CC'){word1str[1:2] <- ""}
          Test <- paste0("^(|\u0646\u0645\u06CC|\u0645\u06CC|\u0646)",paste0(word1str,collapse=""),"(|\u0646|\u0645|\u06CC|\u06CC\u0645|\u06CC\u062F|\u0646\u062F)$")
          PWord <- paste0(word1str,collapse="")
          PWord <- paste(PWord,"\u0030",sep="")
          if((word2 %in% paslistneg) | (word3 %in% paslistneg)){PWord <- paste("\u0646",PWord,sep="")}
          if(Context){if(TRUE %in% grepl(Test, textsSplit)){textsSplit[i] <- PWord}}
          if(!Context){textsSplit[i] <- PWord}
          textsSplit[i+1] <- ""
          textsSplit[i+2] <- ""
          next
        }
      }
    }
    # 3- Future - Passive 
    # (eg. neveshteh khahad shod)
    if (!(word1 %in% NoStemVerb) & (word3 %in% c("\u0634\u062F", "\u0646\u0634\u062F")) & (word2 %in% future)){
      if(length(word1str) >= 3){
        if(word1str[length(word1str)] == '\u0647'){
          word1str[length(word1str)] <- ""
          Test <- paste0("^(|\u0646\u0645\u06CC|\u0645\u06CC|\u0646)",paste0(word1str,collapse=""),"(|\u0646|\u0645|\u06CC|\u06CC\u0645|\u06CC\u062F|\u0646\u062F)$")
          PWord <- paste0(word1str,collapse="")
          PWord <- paste(PWord,"\u0030",sep="")
          if((word2 %in% futureneg) | (word3 == "\u0646\u0634\u062F")){PWord <- paste("\u0646",PWord,sep="")}
          if(Context){if(TRUE %in% grepl(Test, textsSplit)){textsSplit[i] <- PWord}}
          if(!Context){textsSplit[i] <- PWord}
          textsSplit[i+1] <- ""
          textsSplit[i+2] <- ""
          next
        }
      }
    }
    # 4- Progressive Past and Present with Supplement Prefixes - Passive 
    # (eg. dasht neveshteh mishod, darad neveshteh mishavad)
    if (!(word2 %in% NoStemVerb) & (word1 %in% c(supprepro,suppaspro)) & (word3 %in% c(proprepass, propaspass))){
      if(length(word2str) >= 3){
        if(word2str[length(word2str)] == '\u0647'){
          word2str[length(word2str)] <- ""
          Test <- paste0("^(|\u0646\u0645\u06CC|\u0645\u06CC|\u0646)",paste0(word2str,collapse=""),"(|\u0646|\u0645|\u06CC|\u06CC\u0645|\u06CC\u062F|\u0646\u062F)$")
          PWord <- paste0(word2str,collapse="")
          PWord <- paste(PWord,"\u0030",sep="")
          if(word3 %in% c(proprepassneg,propaspassneg)){PWord <- paste("\u0646",PWord,sep="")}
          if(Context){if(TRUE %in% grepl(Test, textsSplit)){textsSplit[i+1] <- PWord}}
          if(!Context){textsSplit[i+1] <- PWord}
          textsSplit[i] <- ""
          textsSplit[i+2] <- ""
          next
        }
      }
    }
    # 5- Progressive Present with Supplement Prefixes - Active 
    # (eg.darad minevisad)
    if (word1 %in% supprepro){
      if((paste(word2str[1:2],collapse="") == '\u0645\u06CC')|(paste(word2str[1:3],collapse="") == '\u0646\u0645\u06CC')){
        pre <- word2str[1]
        if(paste(word2str[1:3],collapse="") == '\u0646\u0645\u06CC'){word2str[1:3] <- ""}
        if(paste(word2str[1:2],collapse="") == '\u0645\u06CC'){word2str[1:2] <- ""}
        if (word1 %in% suppresinpro){word2str[length(word2str)]<-""}
        if (word1 %in% suppreplupro){word2str[(length(word2str)-1):length(word2str)] <-""}
        textsSplit[i] <- ""
        textsSplit[i+1] <- paste(word2str,collapse="")
        if(pre=="\u0646"){textsSplit[i+1] <- paste("\u0646",textsSplit[i+1],sep="")}
        textsSplit[i+1] <- paste(textsSplit[i+1],"\u0031",sep="")
        next
      }
    }
    # 6- Progressive Past with Supplement Prefixes - Active 
    # (eg.dasht minevesht)
    if (word1 %in% suppaspro){
      if((paste(word2str[1:2],collapse="") == '\u0645\u06CC')|(paste(word2str[1:3],collapse="") == '\u0646\u0645\u06CC')){
        pre <- word2str[1]
        if(paste(word2str[1:3],collapse="") == '\u0646\u0645\u06CC'){word2str[1:3] <- ""}
        if(paste(word2str[1:2],collapse="") == '\u0645\u06CC'){word2str[1:2] <- ""}
        if ((word1 %in% suppassinpro) & (word1 != "\u062F\u0627\u0634\u062A")){word2str[length(word2str)]<-""}
        if (word1 %in% suppasplupro){word2str[(length(word2str)-1):length(word2str)] <-""}
        textsSplit[i] <- ""
        textsSplit[i+1] <- paste(word2str,collapse="")
        if(pre=="\u0646"){textsSplit[i+1] <- paste("\u0646",textsSplit[i+1],sep="")}
        textsSplit[i+1] <- paste(textsSplit[i+1],"\u0030",sep="")
        next
      }
    }
    # 7- Past Perfect, Present Perfect and Subjunctive Past - Active 
    # (eg. neveshteh bud, neveshteh ast, neveshteh bashad)
    if (!(word1 %in% NoStemVerb) & (word2 %in% paslist)){
      if(length(word1str) >= 3){
        if(word1str[length(word1str)] == '\u0647'){
          word1str[length(word1str)] <- ""
          if(paste(word1str[1:3],collapse="") == '\u0646\u0645\u06CC'){word1str[2:3] <- ""}
          if(paste(word1str[1:2],collapse="") == '\u0645\u06CC'){word1str[1:2] <- ""}
          Test <- paste0("^(|\u0646\u0645\u06CC|\u0645\u06CC|\u0646)",paste0(word1str,collapse=""),"(|\u0646|\u0645|\u06CC|\u06CC\u0645|\u06CC\u062F|\u0646\u062F)$")
          PWord <- paste0(word1str,collapse="")
          PWord <- paste(PWord,"\u0030",sep="")
          if(word2 %in% paslistneg){PWord <- paste("\u0646",PWord,sep="")}
          if(Context){if(TRUE %in% grepl(Test, textsSplit)){textsSplit[i] <- PWord}}
          if(!Context){textsSplit[i] <- PWord}
          textsSplit[i+1] <- ""
          next
        }
      }
    }
    # 8- Simple and Progressive Past and Present - Passive 
    # (eg. neveshteh mishavad, neveshteh mishod)
    if (!(word1 %in% NoStemVerb) & (word2 %in% pass)){
      if(length(word1str) >= 3){
        if(word1str[length(word1str)] == '\u0647'){
          word1str[length(word1str)] <- ""
          if(paste(word1str[1:3],collapse="") == '\u0646\u0645\u06CC'){word1str[2:3] <- ""}
          if(paste(word1str[1:2],collapse="") == '\u0645\u06CC'){word1str[1:2] <- ""}
          Test <- paste0("^(|\u0646\u0645\u06CC|\u0645\u06CC|\u0646)",paste0(word1str,collapse=""),"(|\u0646|\u0645|\u06CC|\u06CC\u0645|\u06CC\u062F|\u0646\u062F)$")
          PWord <- paste0(word1str,collapse="")
          PWord <- paste(PWord,"\u0030",sep="")
          if ((word2 %in% c(proprepassneg,propaspassneg,simprepassneg,simpaspassneg))){PWord <- paste("\u0646",PWord,sep="")}
          if(Context){if(TRUE %in% grepl(Test, textsSplit)){textsSplit[i] <- PWord}}
          if(!Context){textsSplit[i] <- PWord}
          textsSplit[i+1] <- ""
          next
        }
      }
    }
    # 9- Simple Future - Active 
    # (eg. khahad nevesht)
    if (word1 %in% future){
      if(length(word2str) >= 2){
        textsSplit[i] <- paste(word2str,collapse="")
        if(word1 %in% futureneg){textsSplit[i] <- paste("\u0646",textsSplit[i],sep="")}
        textsSplit[i] <- paste(textsSplit[i],"\u0030",sep="")
        textsSplit[i+1] <- ""
        next
      }
    }
    # 10- Progressive Future - Active 
    # (eg. mikhahad benevisad)
    if (word1 %in% futureprog){
      if(length(word2str) >= 3){
        if(word2str[1] %in% c("\u0628","\u0646")){
          if((word2str[length(word2str)] %in% pressinsuf) | (paste(word2str[(length(word2str)-1):length(word2str)],collapse="") %in% presplusuf)){
            if(word1 %in% c(futureposprogplu,futurenegprogplu)){word2str[(length(word2str)-1):length(word2str)] <-""}
            if(word1 %in% c(futureposprogsin,futurenegprogsin)){word2str[length(word2str)]<-""}
            pre <- word2str[1]
            word2str[1] <- ""
            textsSplit[i+1] <- paste(word2str,collapse="")
            if((word1 %in% futurenegprog) | (pre=="\u0646")){textsSplit[i+1] <- paste("\u0646",textsSplit[i+1],sep="")}
            textsSplit[i] <- ""
            textsSplit[i+1] <- paste(textsSplit[i+1],"\u0031",sep="")
            next
          }
        }
      }
    }
    # 11- Progressive Presnet and Past - Active 
    # (eg. minevesht, minevisad)
    if(!(word1 %in% NoStemVerb) & length(word1str) >= 4){
      if((paste(word1str[1:2],collapse="") == '\u0645\u06CC')|(paste(word1str[1:3],collapse="") == '\u0646\u0645\u06CC')){
        if((word1str[length(word1str)] %in% pressinsuf) | (paste(word1str[(length(word1str)-1):length(word1str)],collapse="") %in% presplusuf)){
          pre <- word1str[1]
          if(paste(word1str[1:2],collapse="") == '\u0645\u06CC'){word1str <- word1str[3:length(word1str)]}
          if(paste(word1str[1:3],collapse="") == '\u0646\u0645\u06CC'){word1str <- word1str[4:length(word1str)]}
          if((length(word1str)>2) & paste(word1str[(length(word1str)-1):length(word1str)],collapse="") %in% presplusuf){
            if (!(word1str[(length(word1str)-2)] %in% c("\u0627", "\u0648")) & (length(word1str) > 3)){word1str[(length(word1str)-1):length(word1str)] <-""}}
          if((word1str[length(word1str)] %in% pressinsuf) & (word1str[length(word1str)] !="\u062F")){word1str[length(word1str)]<-""}
          if(word1str[length(word1str)]=="\u062F"){word1str[length(word1str)]<-"\u062F\u0034"}
          textsSplit[i] <- paste(word1str,collapse="")
          if(pre=="\u0646"){textsSplit[i] <- paste("\u0646",textsSplit[i],sep="")}
          textsSplit[i] <- paste(textsSplit[i],"\u0032",sep="")
          next
        }
      }
    }
  } 
  # This section uses the past and present roots we get in the above 11 stpes 
  # to detect and stem remaining verbs in text.
  # present roots
  preroot <- grep("\u0031$|\u0032$",textsSplit, value = TRUE)
  preroot <- c(unique(gsub("\u0031$|\u0032$|\u0034\u0032$","",preroot)))
  preroot <- unique(c(preroot,
                      gsub("^\u06CC\u0627","\u0622",preroot),
                      gsub("^\u0622","\u06CC\u0627",preroot),
                      gsub("^\u0622","\u0627",preroot),
                      gsub("^\u0627","\u0622",preroot),
                      gsub("^\u06CC\u0627!","\u0627",preroot),
                      gsub("^\u0646\u0622","\u0646\u06CC\u0627",preroot),
                      gsub("^\u0646\u0627","\u0646\u06CC\u0627",preroot)))
  prerootpos <- unique(c(preroot[!grepl("^\u0646",preroot)],gsub("^\u0646","",grep("^\u0646\u0646",preroot, value =TRUE))))
  prerootneg <- preroot[grepl("^\u0646",preroot)]
  # past roots
  pasroot <- grep(("\u0030$|\u0032$"),textsSplit, value = TRUE)
  pasroot <- c(unique(gsub("|\u0030$|\u0032$|\u0034\u0032$","",pasroot)))
  pasroot <- unique(c(pasroot,
                      gsub("^\u06CC\u0627","\u0622",pasroot),
                      gsub("^\u0622","\u06CC\u0627",pasroot),
                      gsub("^\u0622","\u0627",pasroot),
                      gsub("^\u0627","\u0622",pasroot),
                      gsub("^\u06CC\u0627!","\u0627",pasroot),
                      gsub("^\u0646\u0622","\u0646\u06CC\u0627",pasroot),
                      gsub("^\u0646\u0627","\u0646\u06CC\u0627",pasroot)))
  pasrootpos <- unique(c(pasroot[!grepl("^\u0646",pasroot)],gsub("^\u0646","",grep("^\u0646\u0646",pasroot, value =TRUE))))
  pasrootneg <- pasroot[grepl("^\u0646",pasroot)]
  # The four loops below uses verb roots to detect and stem remaining verbs in text
  if(length(prerootpos)>0){
    for (i in 1:length(prerootpos)){
      if(grepl("^\u0627",prerootpos[i])==TRUE){
        preverbpos1 <- paste0("^(|\u0628\u06CC)",prerootpos[i],"(|\u0645|\u06CC|\u062F|\u06CC\u0645|\u06CC\u062F|\u0646\u062F)$")
        preverbneg1 <- paste0("^\u0646\u06CC",prerootpos[i],"(|\u0645|\u06CC|\u062F|\u06CC\u0645|\u06CC\u062F|\u0646\u062F)$")
        textsSplit <- gsub(preverbpos1,paste0(prerootpos[i],"\u0033"),textsSplit)
        textsSplit <- gsub(preverbneg1,paste0("\u0646\u06CC",prerootpos[i],"\u0033"),textsSplit)
      }else{
        preverbpos1 <- paste0("^(|\u0628)",prerootpos[i],"(|\u0645|\u06CC|\u062F|\u06CC\u0645|\u06CC\u062F|\u0646\u062F)$")
        preverbneg1 <- paste0("^\u0646",prerootpos[i],"(|\u0645|\u06CC|\u062F|\u06CC\u0645|\u06CC\u062F|\u0646\u062F)$")
        textsSplit <- gsub(preverbpos1,paste0(prerootpos[i],"\u0033"),textsSplit)
        textsSplit <- gsub(preverbneg1,paste0("\u0646",prerootpos[i],"\u0033"),textsSplit)
      }
    }
  }
  if(length(prerootneg)>0){
    for (i in 1:length(prerootneg)){
      if(grepl("^\u0627",prerootneg[i])==TRUE){
        preverbpos2 <- paste0("^(|\u0628\u06CC)",prerootneg[i],"(|\u0645|\u06CC|\u062F|\u06CC\u0645|\u06CC\u062F|\u0646\u062F)$")
        preverbneg2 <- paste0("^\u0646\u06CC",prerootneg[i],"(|\u0645|\u06CC|\u062F|\u06CC\u0645|\u06CC\u062F|\u0646\u062F)$")
        textsSplit <- gsub(preverbpos2,paste0(prerootneg[i],"\u0033"),textsSplit)
        textsSplit <- gsub(preverbneg2,paste0("\u0646\u06CC",prerootneg[i],"\u0033"),textsSplit)
      }else{
        preverbpos2 <- paste0("^(|\u0628)",prerootneg[i],"(|\u0645|\u06CC|\u062F|\u06CC\u0645|\u06CC\u062F|\u0646\u062F)$")
        preverbneg2 <- paste0("^\u0646",prerootneg[i],"(|\u0645|\u06CC|\u062F|\u06CC\u0645|\u06CC\u062F|\u0646\u062F)$")
        textsSplit <- gsub(preverbpos2,paste0(prerootneg[i],"\u0033"),textsSplit)
        textsSplit <- gsub(preverbneg2,paste0("\u0646",prerootneg[i],"\u0033"),textsSplit)
      }
    }
  }
  if(length(pasrootpos)>0){
    for (i in 1:length(pasrootpos)){
      if(grepl("^\u0627",pasrootpos[i])==TRUE){
        pasverbpos1 <- paste0("^(|\u0645\u06CC)",pasrootpos[i],"(|\u0646|\u0645|\u06CC|\u062F|\u06CC\u0645|\u06CC\u062F|\u0646\u062F)$")
        pasverbneg1 <- paste0("^(\u0646\u06CC|\u0646\u0645\u06CC)",pasrootpos[i],"(|\u0646|\u0645|\u06CC|\u062F|\u06CC\u0645|\u06CC\u062F|\u0646\u062F)$")
        textsSplit <- gsub(pasverbpos1,paste0(pasrootpos[i],"\u0033"),textsSplit)
        textsSplit <- gsub(pasverbneg1,paste0("\u0646\u06CC",pasrootpos[i],"\u0033"),textsSplit)
      }else{
        pasverbpos1 <- paste0("^(|\u0645\u06CC)",pasrootpos[i],"(|\u0646|\u0645|\u06CC|\u062F|\u06CC\u0645|\u06CC\u062F|\u0646\u062F)$")
        pasverbneg1 <- paste0("^(\u0646|\u0646\u0645\u06CC)",pasrootpos[i],"(|\u0646|\u0645|\u06CC|\u062F|\u06CC\u0645|\u06CC\u062F|\u0646\u062F)$")
        textsSplit <- gsub(pasverbpos1,paste0(pasrootpos[i],"\u0033"),textsSplit)
        textsSplit <- gsub(pasverbneg1,paste0("\u0646",pasrootpos[i],"\u0033"),textsSplit)
      }
    }
  }
  if(length(pasrootneg)>0){
    for (i in 1:length(pasrootneg)){
      if(grepl("^\u0627",pasrootpos[i])==TRUE){
        pasverbpos2 <- paste0("^(|\u0645\u06CC)",pasrootneg[i],"(|\u0646|\u0645|\u06CC|\u062F|\u06CC\u0645|\u06CC\u062F|\u0646\u062F)$")
        pasverbneg2 <- paste0("^(\u0646\u06CC|\u0646\u0645\u06CC)",pasrootneg[i],"(|\u0646|\u0645|\u06CC|\u062F|\u06CC\u0645|\u06CC\u062F|\u0646\u062F)$")
        textsSplit <- gsub(pasverbpos2,paste0(pasrootneg[i],"\u0033"),textsSplit)
        textsSplit <- gsub(pasverbneg2,paste0("\u0646\u06CC",pasrootneg[i],"\u0033"),textsSplit)
      }else{
        pasverbpos2 <- paste0("^(|\u0645\u06CC)",pasrootneg[i],"(|\u0646|\u0645|\u06CC|\u062F|\u06CC\u0645|\u06CC\u062F|\u0646\u062F)$")
        pasverbneg2 <- paste0("^(\u0646|\u0646\u0645\u06CC)",pasrootneg[i],"(|\u0646|\u0645|\u06CC|\u062F|\u06CC\u0645|\u06CC\u062F|\u0646\u062F)$")
        textsSplit <- gsub(pasverbpos2,paste0(pasrootneg[i],"\u0033"),textsSplit)
        textsSplit <- gsub(pasverbneg2,paste0("\u0646",pasrootneg[i],"\u0033"),textsSplit)
      }
    }
  }
  # This section fixes verbs starting with "alef", "ye", and "ye and alef"
  yaA <- grep("\u0030$|\u0031$|\u0032$|\u0033$",textsSplit, value = TRUE)
  yaA <- gsub("\u0030$|\u0031$|\u0032$|\u0034\u0032$|\u0033$","",yaA)
  y <- unique(grep("^\u06CC\u0627|^\u06CC",yaA, value = TRUE))
  a <- unique(grep("^\u0627",yaA, value = TRUE))
  A <- unique(grep("^\u0622",yaA, value = TRUE))
  for (i in 1:length(y)){
    if (length(gsub("^\u06CC\u0627","\u0622", y[i]))>0){
      if (gsub("^\u06CC\u0627","\u0622", y[i]) %in% A){
        v1 <- gsub("^\u06CC\u0627","\u0622", y[i])
        v2 <- paste0(v1,"(|\u0645|\u06CC|\u062F|\u06CC\u0645|\u06CC\u062F|\u0646\u062F)$")
        textsSplit <- gsub(paste0("^",y[i],"(\u0030|\u0031|\u0032|\u0034\u0032|\u0033)$"),paste0(v1,"\u0035"),textsSplit)
        textsSplit <- gsub(v2,paste0(v1,"\u0035"),textsSplit);
        next}};
    if (length(gsub("^\u06CC","\u0627", y[i]))>0){
      if (gsub("^\u06CC","\u0627", y[i]) %in% a){
        v1 <- gsub("^\u06CC","\u0627", y[i])
        v2 <- paste0(v1,"(|\u0645|\u06CC|\u062F|\u06CC\u0645|\u06CC\u062F|\u0646\u062F)$")
        textsSplit <- gsub(paste0("^",y[i],"(\u0030|\u0031|\u0032|\u0034\u0032|\u0033)$"),paste0(v1,"\u0035"),textsSplit)
        textsSplit <- gsub(v2,paste0(v1,"\u0035"),textsSplit);
        next}}
  }
  for (i in 1:length(a)){
    if (length(gsub("^\u0627","\u0622", a[i]))>0){
      if (gsub("^\u0627","\u0622", a[i]) %in% A){
        v1 <- gsub("^\u0627","\u0622", a[i])
        v2 <- paste0(v1,"(|\u0645|\u06CC|\u062F|\u06CC\u0645|\u06CC\u062F|\u0646\u062F)$")
        textsSplit <- gsub(paste0("^",a[i],"(\u0030|\u0031|\u0032|\u0034\u0032|\u0033)$"),paste0(v1,"\u0035"),textsSplit)
        textsSplit <- gsub(v2,paste0(v1,"\u0035"),textsSplit);
        next}};
    if (length(gsub("^\u0627","\u06CC\u0627", a[i]))>0){
      if (gsub("^\u0627","\u06CC\u0627", a[i]) %in% y){
        v1 <- gsub("^\u0627","\u06CC\u0627", a[i])
        v2 <- paste0(v1,"(|\u0645|\u06CC|\u062F|\u06CC\u0645|\u06CC\u062F|\u0646\u062F)$")
        textsSplit <- gsub(paste0("^",a[i],"(\u0030|\u0031|\u0032|\u0034\u0032|\u0033)$"),paste0(v1,"\u0035"),textsSplit)
        textsSplit <- gsub(v2,paste0(v1,"\u0035"),textsSplit);
        next}}
  }
  textsSplit <- gsub("^\u0646\u0622","\u0646\u06CC\u0627",textsSplit)
  texts <- paste(textsSplit, collapse=" ")
  texts <- trim(gsub(" {2,}"," ", texts))
  return(texts)
}
