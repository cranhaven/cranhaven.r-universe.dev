######################################################################################################################################
######################################################################################################################################
#
# Helper Functions
#
######################################################################################################################################


######################################################################################################################################
## Parse Distribution Data
######################################################################################################################################
get_distribution <- function(local.result) {
  tryCatch(
    expr = {result <- gsub("U\\.S\\.A\\.", "UnitedStatesOfAmerica", local.result) #Replace U.S.A. with placeholder, temporarily
    result <- gsub("U\\.K\\.", "UnitedKingdom", result) #Replace U.S.A. with placeholder, temporarily
    result <- stringr::str_extract(result, "Distribution: .*? Habitat:")#find distribution
    result <- gsub("Distribution: ", "", result)#split distribution from data
    result <- gsub("Habitat:", "", result) #remove habitat
    result <- str_remove(result, "[Ii][Uu][Cc][Nn].*?:.*")
    result <- gsub("\\.$", "", result)#remove period
    result <- gsub("Habitat:.*|<\\/p>", "", result) #Clean up end of results
    result <- gsub("UnitedStatesOfAmerica", "U\\.S\\.A\\.", result) #Undo USA placeholder
    result <- gsub("UnitedKingdom", "U\\.K\\.", result) #Undo UK placeholder
    if (grepl("<", result, fixed = T)) {result <- StripFun(result)}
    result <- trimws(result)
    return(result)},
    
    error = function(cond) {return("Error parsing distribution")})
}

######################################################################################################################################
## Parse Habitat Data
######################################################################################################################################
get_habitat <- function(local.result) {
  tryCatch(expr = {
    result <- stringr::str_extract(local.result, "Habitat: .*?\\.")#Find habitat flag
    result <- gsub("Habitat: ", "", result)#Remove habitat flag
    result <- gsub("\\.", "", result)#remove period
    HabDat <- as.numeric(sapply(c("freshwater","brackish","marine"), grepl, result))#Get habitats
    HabDat <- ifelse(HabDat==c(0,0,0), yes = rep(NA,3), no =  HabDat)
    return(HabDat)},#return data
    
    error = function(cond) {return("Error parsing habitat")})
}

######################################################################################################################################
## Parse Family/Subfamily Data
######################################################################################################################################
get_family <- function(local.result) {
  tryCatch(expr = {
    result <- gsub("Distribution: .*","",local.result)#remove record from distribution flag on
    result <- gsub("Habitat: .*","",result)#remove record from habitat flag on
    result <- trimws(gsub("<\\/p>|<\\/b>", "",result,fixed = FALSE))#remove last flag if no distribution or habitat data is present
    family.dat <- rev(strsplit(result, "\\.")[[1]])[1]#split on period and take the family info
    result_strings <- sapply(family.dat, strsplit, ": ")#split family/subfamily
    result_strings <- sapply(result_strings, trimws) #trim whitespace
    result_strings[1] <- ifelse(grepl("idae$", result_strings[1]), result_strings[1], NA) #confirm family has suffix -idae
    result_strings[2] <- ifelse(grepl("inae$", result_strings[2]), result_strings[2], NA) #confirm subfamily has suffix -inae
    result_strings[1] <- ifelse(grepl(" ", result_strings[1]),  utils::tail(unlist(str_split(result_strings[1], " ")), 1), result_strings[1]) #extract family if extra data has been included
    result_strings[2] <- ifelse(grepl(" ", result_strings[2]),  utils::tail(unlist(str_split(result_strings[2], " ")), 1), result_strings[2]) #extract family if extra data has been included
    return(unname(unlist(result_strings)))},#return unlisted unnamed family info
    
    error = function(cond) {return(c("Error parsing family", "Error parsing subfamily"))})
}

######################################################################################################################################
## Parse Status Data
######################################################################################################################################
get_current_status <- function(local.result) {
  tryCatch(expr = {
    result <- stringr::str_extract(local.result, "Current status.*") #Find status flag
    result <-  stringr::str_split(string = result,pattern = "\\.")[[1]] #split at periods
    if (grepl("^ \\&|^ \\d{4}|^, ", result[2])) {result <- paste0(result[1], fixed("."), result[2])} #Fix double authors
    else if (grepl("et al$", result[1])) {result <- paste0(result[1], fixed("."), result[2])} #Fix et al.
    else {result <- result[1]}
    if (grepl("</b> Uncertain$", result)) {
      status <- "Uncertain"
      species <- NA
    } else if (grepl("</b> Unknown$", result)) {
      status <- "Unknown"
      species <- NA
    } else if (grepl("</b> Synonym$", result)) {
      status <- "Synonym"
      species <- NA 
    } else {
      status<-trimws(gsub("</b>|<i>","", stringr::str_extract(result, "</b>.+<i>")))#get the status
      species<-gsub("<i>|</i>","", stringr::str_extract(result, "<i>.+</i>"))#get the recognied species name
    }
    ifelse(is.na(species) | is.na(stringr::str_extract(result, "</i>.+")),author <- NA, author <- trimws(StripFun(stringr::str_extract(result, "</i>.+"))))
    return(c(status,species,author))},
    
    error = function(cond) {return(c("Error parsing status", "Error parsing species", "Error parsing author"))})
}

######################################################################################################################################
## Get Focal Taxon Name and Author of Description
######################################################################################################################################
get_focal_name <- function(local.result){
  tryCatch(expr = {
    focal.binom <- StripFun(stringr::str_extract(local.result, "<b><i>.+</i></b>"))#Get nominal name
    focal.binom <- strsplit(focal.binom,",")#split nominal name elements
    binom.maker<-function(x){#funtion to make binomial
      paste(rev(x), collapse = " ")
    }
    focal.binom.final <- trimws(lapply(focal.binom, binom.maker))#make binomial
    if (grepl("[12]\\d\\d\\d\\-\\d\\d:", local.result)) {
      Author = qdapRegex::ex_between(local.result, left = "</i></b> ",right = "[12]\\d\\d\\d\\-\\d\\d",fixed = FALSE,include.markers = TRUE)
      DescriptionYear = str_extract(string = Author, pattern = "[12]\\d\\d\\d\\-\\d\\d")
    } else {
      Author <- qdapRegex::ex_between(local.result, left = "</i></b> ",right = "[12]\\d\\d\\d",fixed = FALSE,include.markers = TRUE)#extract author year
      DescriptionYear <-  stringr::str_extract(string = Author, pattern = "[12]\\d\\d\\d")
    }
    Author <- trimws(gsub("amp;|</i>|</b>","",Author))#remove xml flags from author name
    Description <- qdapRegex::ex_between(local.result, left = " ref.",right = "]")[[1]][1]#get description source
    DescriptionRef <- qdapRegex::ex_between(Description,left = "\">",right = "</a>")#get reference number
    return(unlist(c(focal.binom.final,Author, DescriptionRef, DescriptionYear)))},
    
    error = function(cond) {return(c("Error parsing focal name", "Error parsing focal author", "Error parsing description reference", "Error parsing description year"))})
}

######################################################################################################################################
## Parse Type Information
######################################################################################################################################
get_types <- function(local.result){
  tryCatch(expr = {
    type.vec<-rep(NA,7)#make vector to store type information
    names(type.vec)<-c("Holotype","Paratype","Lectotype","Paralectotype","Neotype","Syntype", "NoTypes")#name types
    
    replacements <- c(" Ea\\. ", " Uncat\\. ", " ea\\. ", " uncat\\. ", " Fish\\. ",
                      " No\\. ", " Orig\\. ", " no\\. ", " orig\\. ", " Cat\\. ",
                      " P\\. ", " p\\. ", " coll\\. ", " cat\\. ", " Scot\\. ",
                      "\\(Orig\\. ", " Coll\\. ", "\\(orig\\. ", " Edinb\\. ",
                      " Univ\\. ", " univ\\. ", " mus\\. ", " Mus\\. ", "-zool\\. ",
                      " Pl\\. ", "\\(Ca\\. ", " pl\\. ", "\\(ca\\. ", "\\[orig\\. ",
                      "\\[Ex\\. ", " Inst\\. ", "\\[ex\\. ", " inst\\. ",
                      " Hydrobiol\\. ", " Zool\\. ", " hydrobiol\\. ", " zool\\. ",
                      " Dept\\. ", " Agric\\. ", " dept\\. ", " agric\\. ",
                      " Acad\\. ", " Sci\\. ", " Ist\\. ", " acad\\. ",
                      " sci\\. ", " ist\\. ", "\\>m\\. ", "\\>M\\. ", " Ex\\. ", " ex\\. ",
                      "\\[Mus\\. ", " Biol\\. ", " Lab\\. ", " Soc\\. ", " Sci\\. ", " Imp\\. ",
                      "65\\. \\(1, sk", "3-64\\. \\(2\\)", " ca\\. ", "898\\.9\\.9\\. 40", 
                      "90 \\(1\\), 2318 \\(1\\)\\. 53", "ens\\)\\. CF", " spec\\. ", " E\\.A\\.F\\.R\\.O\\. ",
                      "\\(ex\\. ", "MNHN 2011-0232\\. M", "7\\. now 6", "57 \\(2\\)\\. P023",
                      "so paratype of P\\. ri", "pes of L\\. wern", " Depto\\. ", " depto\\. ", "RAC\\. ZSM\\.",
                      " Geol\\. ", " Nat\\. ", " Hist\\. ", " Surv\\. ", "Belfast, U\\.K\\. \\(1",
                      "348 \\(1\\)\\. 1029", " U\\.S\\. ", " Bur\\. ", " Fish\\. ", " Pref\\. ",
                      " Exper\\. ", " Mem\\. ", " CAS\\]\\. CAS", " Prov\\. ", "IA1 H\\. N", "82\\-04 \\(1\\)\\. H",
                      " Res\\. ", "\\[Stadt\\. ", " MCZ\\)\\. ")
    for (i in seq_along(replacements)) {
      newString <- str_replace(replacements[i], "\\. ", "xxyyzz ")
      local.result <- str_replace_all(local.result, replacements[i], newString)
    }
    
    HolotypeCheck <- grep("Holotype",local.result)#check for holotypes and extract if present
    if(length(HolotypeCheck)>0){
      type.vec[1] <- qdapRegex::ex_between(local.result[HolotypeCheck], left = c("Holotype \\(unique\\):","Holotype:","Holotype \\(\\?\\):", "Holotype \\(unique\\) \\(\\?\\):"), right = c("\\. ","\\. ", "\\. ", "\\. "), fixed = FALSE)
    }
    ParatypeCheck <- grep("Paratype", local.result)#check for paratypes and extract if present
    if(length(ParatypeCheck)>0){
      type.vec[2] <- qdapRegex::ex_between(local.result[ParatypeCheck], left = c("Paratype:","Paratypes:", "Paratype \\(\\?\\):", "Paratypes \\(\\?\\):", "Paratypes \\(listed as additional material\\):", "Paratypes \\(11\\):"), right = c("\\. ","\\. ","\\. ","\\. ", "\\. ", "\\. "), fixed = FALSE)
      type.vec[2] <- str_remove(type.vec[2], "First appeared in Nguyen et al")
    }
    LectotypeCheck <- grep("Lectotype",local.result)#check for lectotypes and extract if present
    if(length(LectotypeCheck)>0){
      type.vec[3] <- qdapRegex::ex_between(local.result[LectotypeCheck], left = c("Lectotype:","Lectotypes:"), right = c("\\. ","\\. "), fixed = FALSE)
    }
    ParalectotypeCheck <- grep("Paralectotype",local.result)#check for paralectotypes and extract if present
    if(length(ParalectotypeCheck)>0){
      type.vec[4] <- qdapRegex::ex_between(local.result[ParalectotypeCheck], left = c("Paralectotype:","Paralectotypes:"), right = c("\\. ","\\. "), fixed = FALSE)
    }
    NeotypeCheck <- grep("Neotype",local.result)#check for neotypes and extract if present
    if(length(NeotypeCheck)>0){
      type.vec[5] <- qdapRegex::ex_between(local.result[NeotypeCheck], left = c("Neotype:","Neotypes:"), right = c("\\. ","\\. "), fixed = FALSE)
    }
    SyntypeCheck <- grep("Syntype",local.result, ignore.case = TRUE)#check for syntypes and extract if present
    if(length(SyntypeCheck)>0){
      type.vec[6] <- qdapRegex::ex_between(local.result[SyntypeCheck], left = c("Syntype:","Syntypes:", "Syntypes \\(\\?\\):", "Syntypes \\(\\?\\) \\[or one is the holotype\\]:", "syntypes:"), right = c("\\. ","\\. ","\\. ","\\. ","\\. "), fixed = FALSE)
    }
    NoTypesCheck <- grep("No types known|No type known",local.result) #Check for no types known and extract if present
    if(length(NoTypesCheck)>0){
      if(grepl("No types known",local.result)) {
        type.vec[7] <- "No types known"
      }
      if(grepl("No type known",local.result)) {
        type.vec[7] <- "No type known"
      }
    }
    type.vec <- str_replace_all(type.vec, "xxyyzz", "\\.")
    local.result <- str_replace_all(local.result, "xxyyzz", "\\.")
    type.vec <- stringr::str_replace_all(type.vec, fixed("&amp;"), fixed("&"))
    type.vec <- stringr::str_remove_all(type.vec, "\\u2022.*")
    type.vec <- stringr::str_remove(type.vec, "Type catalog:.*")
    type.vec <- stringr::str_remove(type.vec, "Type cataqlog:.*")
    type.vec <- stringr::str_remove_all(type.vec, "\\<i\\>|\\<\\/i\\>")
    return(type.vec)},
    
    error = function(cond) {return(rep("Error parsing types", times = 7))})
}

######################################################################################################################################
## Get Type Locality
######################################################################################################################################
get_type_locality <- function(local.result){
  tryCatch(expr = {
    locality <- qdapRegex::ex_between(local.result, left = c("</a>]","</a>]"), right = c(" *type"," *types "), fixed = FALSE)[[1]][1]#Get type locality
    locality <- gsub(" [Hh]olo$| [Ss]yn$| [Ll]ecto$| [Pp]ara$| [Pp]aralecto$| [Nn]eo$| No$| Valenciennes$", "",locality)#strip out trailing info on types
    if (grepl("^Holo$|^Syn$|^Lecto$|^Paralecto$|^Para$|^Neo$|^No$", locality)) {locality <- NA}
    locality <- trimws(locality)
    locality <- str_remove(locality, " Plus non\\-") #remove plus 'non-type material' in mistypes
    locality <- str_remove(locality, " Possible$")
    locality <- str_remove(locality, "Species illustrated and described in more detail.*?$")
    locality <- str_remove(locality, "Not translated, perhaps")
    locality <- stringr::str_replace_all(locality, fixed("&amp;"), fixed("&"))
    locality <- str_remove(locality, "Non-$")
    if (is.na(locality)) {
      locality <- qdapRegex::ex_between(local.result, left = "</a>]", right = "\\u2022", fixed = FALSE)[[1]][1]#Get type locality
      locality <- unlist(strsplit(locality, "\\. "))
      locVec <- c()
      for (i in 1:nrow(GeographicTerms)) {
        locFind <- grep(str_remove_all(GeographicTerms[i,1], "\\(|\\)"), locality)
        if (length(locFind) > 0) {
          locVec <- c(locVec, locFind)
        }
      }
      locVec <- locVec[1]
      locality <- locality[locVec]
    }
    locality <- str_remove(locality, "\\.$")
    if (length(locality) == 0) {locality <- ""}
    if (grepl("<", locality, fixed = T)) {locality <- StripFun(locality)}
    if (locality == "") {locality <- NA} 
    return(locality)},
    
    error = function(cond) {return("Error parsing locality")})
}

######################################################################################################################################
## Get Gender
######################################################################################################################################
get_gender <- function(local.result){
  tryCatch(expr = {
    genders2mine <- c("Masc.","Fem.","Neut.")#Specify genders to mine
    gender <- NA#if no gender, set as NA and if gender is found set it
    #loop to find gender
    for(gender.index in 1:3) {
      ifelse(length(grep(genders2mine[gender.index],local.result))==0, gender <- NA, gender <- genders2mine[gender.index])
      if (!is.na(gender)) {
        break
      }
      ifelse(length(grep(genders2mine[gender.index],local.result))==0, gender <- NA, gender <- genders2mine[gender.index])
      if (!is.na(gender)) {
        break
      }
      ifelse(length(grep(genders2mine[gender.index],local.result))==0, gender <- NA, gender <- genders2mine[gender.index])
    }
    return(gender)},
    
    error = function(cond) {return("Error parsing gender")})
}

######################################################################################################################################
## Get Focal Genus
######################################################################################################################################
get_focal_genus <- function(local.result){
  tryCatch(
    expr = {  genus <- qdapRegex::ex_between(local.result, left = "<b><i>","</i></b>")[[1]]#Get focal genus names
    Author <- qdapRegex::ex_between(local.result, left = "</i></b> ",right = ":")#Get author of description
    genus.note <- stringr::str_replace_all(qdapRegex::ex_between(local.result, left = "<\\/b> (",right = ")")[[1]][1],"<i>|</i>|<b>|</b>","")#check for notes (i.e., subgenus)
    Author <- trimws(gsub("\\s*\\([^\\)]+\\)", "",Author,fixed = FALSE))#remove author and trim spaces
    Author <- gsub("amp;","",Author)#remove xml & flags
    DescriptionYear <- stringr::str_extract(string = Author, pattern = "[12]\\d\\d\\d")
    Description<- qdapRegex::ex_between(local.result, left = "; ref.",right = "]")#remove reference for description
    DescriptionRef <- qdapRegex::ex_between(Description,left = "\">",right = "</a>")#obtain reference numbers
    return(unlist(c(genus,Author,DescriptionRef, DescriptionYear, genus.note)))},
    
    error = function(cond) {return("Error parsing focal genus")})
}

######################################################################################################################################
## Get Type Species
######################################################################################################################################
get_type_species <- function(local.result){
  tryCatch(
    expr = {type.species <- qdapRegex::ex_between(local.result, left = ". <i>","</i>")#get the species using italics flgs
    return(type.species)},
    
    error = function(cond) {return("Error parsing type species")})
}

######################################################################################################################################
## Get Type by
######################################################################################################################################
get_type_by <- function(local.result){
  tryCatch(
    expr = {  type.species <- qdapRegex::ex_between(local.result, left = "Type by",".",include.markers = TRUE)#Find type by flag and remove
    return(type.species)},
    
    error = function(cond) {return("Error parsing type by")})
}

######################################################################################################################################
## Get Genus Family
######################################################################################################################################
get_family_genus <- function(local.result) {
  tryCatch(
    expr = {  mine.family <- qdapRegex::ex_between(local.result, left = "<b>Current status:</b> ","</p>",include.markers = FALSE)#Find current status info
    family <- qdapRegex::ex_between(mine.family, left = ". ",".",include.markers = FALSE)#separate out family info
    result_strings <- sapply(family, strsplit, ": ")#separate family/subfamily
    return(unlist(result_strings))},
    
    error = function(cond) {return("Error parsing family genus")})
}

######################################################################################################################################
## Get Genus Notes
######################################################################################################################################
get_genus_notes <- function(local.result) {
  tryCatch(expr = {
    find.note<-qdapRegex::ex_between(local.result, left = "Type ","\\u2022",include.markers = TRUE)#get note section
    notes <- gsub("Type by.*?\\.\\s\\s","",find.note)#remove type by info
    for (note.index in seq_along(notes)) {
      if(is.na(notes[note.index])|nchar(notes[note.index])<=1){
        notes[note.index] <- NA#if no notes, set to NA
      }else{
        if (grepl("<.*?>", notes[note.index])) {
          notes[note.index] <- trimws(gsub(pattern = "\u2022", replacement = "", StripFun(notes[note.index]),fixed = TRUE))#if notes are present in html form, extract and trim
        } else {
          notes[note.index] <- trimws(gsub(pattern = "\u2022", replacement = "", notes[note.index],fixed = TRUE))#if notes are present otherwise, extract and trim
        }
      }
    }
    return(notes)},
    error = function(cond) {return("Error parsing genus notes")})
}

######################################################################################################################################
## Get Nomenclature Notes
######################################################################################################################################
get_nomenclature_notes <- function(local.result) {
  tryCatch(expr = {
    result <- gsub(pattern = "<b><i>.*<\\/i><\\/b>", replacement = "", local.result) # Remove genus and species
    result <- stringr::str_extract(result, "<b>.*<\\/b>") # Get all bolded text
    result <- trimws(gsub("<b>|<\\/b>|Current status|:|\\.", "", result))
    for (i in seq_along(result)) {
      if (identical("",result[i])) {
        result[i] <- NA
      } else {
        next
      }
    }
    return(result)},
    error = function(cond) {return("Error parsing nomenclature notes")})
}

######################################################################################################################################
## Get IUCN Notes
######################################################################################################################################
get_iucn <- function(local.result) {
  tryCatch(
    expr = {
      result <- stringr::str_extract(local.result, "[Ii][Uu][Cc][Nn].*?:.*?\\.")
      if (grepl(".*?:.*?\\(", result)) {
        result <- stringr::str_extract(local.result, "[Ii][Uu][Cc][Nn].*?:.*?\\(.*?\\).*?\\.")
      }
      result <- trimws(gsub(pattern = "Habitat:.*", replacement = "", x = result))
      result <- gsub("[Ii][Uu][Cc][Nn]|\\.|<i>|<\\/i>", "", result)
      year <- stringr::str_extract(result, "\\d{4}")
      result <- str_remove(result, "^.*?:")
      result <- str_remove(result, ",$")
      status <- trimws(gsub("\\d{4}", "", result))
      status <- trimws(str_remove(status, "^\\(\\)[; ]"))
      return(matrix(data = c(year, status), nrow = length(year), ncol = 2))},
    
    error = function(cond) {return(c("Error parsing IUCN year", "Error parsing IUCN status"))})
}

######################################################################################################################################
## Resolve Names
######################################################################################################################################
Resolve_Names <- function(NameInput){
  #Below pings the Global Names Verifier API to perform a query
  VerifyNames <- httr::POST("https://verifier.globalnames.org/api/v1/verifications", 
                            config = httr::add_headers(accept = "application/json", `Content-Type` = "application/json"),
                            body = jsonlite::toJSON(list(nameStrings=c(NameInput))))
  #Convert to machine readable
  VerifyNames.Res <- jsonlite::fromJSON(rawToChar(VerifyNames$content))
  if(!VerifyNames.Res$names$matchType=="NoMatch"){#if result is anything other than no match, proceed
    if(VerifyNames.Res$names$cardinality == VerifyNames.Res$names$bestResult$matchedCardinality){#Check cardinality match
      return(VerifyNames.Res$names$bestResult$matchedCanonicalFull)
    }else{
      message(paste0("Unable to resolve query ", NameInput))
    }
  }else{
    message(paste0("Unable to resolve query ", NameInput))
  }
}

######################################################################################################################################
## Get Infrasubspecific Tags
######################################################################################################################################
get_infrasub <- function(local.author) {
  tryCatch(
    expr = {
      infrasub_tags <- c("^natio ", "^var\\. ", "^forma ", "^form ", "^morpha ", "^infra. ", "^subform ", "^race ", "^type ", "^gruppe ", "^aberr. ", "^facies ") #Vector of potential tags to find
      
      new.author <- local.author
      infrasub <- NA
      
      for (infra_index in seq_along(infrasub_tags)) {
        if (grepl(pattern = infrasub_tags[infra_index], x = local.author)) {
          new.author <- str_remove(local.author, infrasub_tags[infra_index])
          infrasub <- trimws(str_extract(local.author, infrasub_tags[infra_index]))
        } else {
          next
        }
      }
      
      return(unlist(c(new.author, infrasub)))},
    
    error = function(cond) {return(c(local.author, "Error parsing infrasubspecific tags"))})
}

######################################################################################################################################
## Get Miscellaneous Data
######################################################################################################################################
get_miscellanea <- function(raw.input, processed.input) {
  tryCatch(
    expr = {
      taxon.history_data <- unlist(qdapRegex::ex_between(raw.input,c("\\u2022","\\u2022"),c(". <b>"," <b>"))) #Pull data used for taxon history
      if (!is.na(taxon.history_data) && grepl(fixed("<"), taxon.history_data)) {
        taxon.history_data <- rvest::html_text(xml2::read_html(taxon.history_data)) #Convert taxon.history info to text
      } 
      final.output <- rvest::html_text(xml2::read_html(raw.input)) #convert raw data to text
      
      #Remove nominal taxa
      if (!is.na(processed.input$NominalTaxa)) {
        name_components <- unlist(str_split(processed.input$NominalTaxa, " "))
        binomial_min <- utils::tail(name_components, 1)
        binomial_rest <- paste(name_components[1:length(name_components)-1], collapse = " ")
        full_binomial <- paste("^", binomial_min, ", ", binomial_rest, sep = "")
        full_binomial <- str_replace(full_binomial, fixed("("), "\\(")
        full_binomial <- str_replace(full_binomial, fixed(")"), "\\)")
        final.output <- trimws(str_remove(final.output, full_binomial))
      }
      #Remove author and year
      if (!is.na(processed.input$Author)) {
        final.output <- str_remove(final.output, fixed(processed.input$Author))
      }
      #Remove habitat data
      if (is.na(processed.input$Fresh) && is.na(processed.input$Brackish) && is.na(processed.input$Marine)) {
      } else if (processed.input$Fresh == 1 || processed.input$Brackish == 1 || processed.input$Marine == 1) {
        final.output <- str_remove(final.output, "Habitat: .*?\\.")
      }
      #Remove infrasubspecific tags
      if (!is.na(processed.input$Infrasubspecific)) {
        infra_tag <- processed.input$Infrasubspecific
        infra_tag <- paste("^", str_replace(infra_tag, fixed("."), "\\."), sep = "")
        final.output <- str_remove(final.output, infra_tag)
      }
      #Remove distribution data
      if (!is.na(processed.input$Distribution)) {
        final.output <- str_remove(final.output, fixed(paste("Distribution:", processed.input$Distribution)))#remove distribution
      }
      #Remove family/subfamily
      if (!is.na(processed.input$Family)) {
        if (!is.na(processed.input$Subfamily)) {
          final.output <- str_remove(final.output, pattern = paste(processed.input$Family, ": ", processed.input$Subfamily, "\\.", sep = ""))
        } else {
          final.output <- str_remove(final.output, pattern = paste(processed.input$Family, "\\.", sep = ""))
        }
      }
      #Remove current status, current authority, and current nomenclature
      if (!is.na(processed.input$Status)) {
        if (processed.input$Status == "Unknown" || processed.input$Status == "Uncertain") {
          currentStatus <- paste(fixed("Current status:"), processed.input$Status)
        }
        else if (!is.na(processed.input$CurrentAuthority)) {
          currentStatus <- paste(fixed("Current status:"), processed.input$Status, processed.input$CurrentNomenclature, processed.input$CurrentAuthority)
        } else {
          currentStatus <- paste(fixed("Current status:"), processed.input$Status, processed.input$CurrentNomenclature)
        }
        final.output <- str_remove_all(final.output, fixed(currentStatus))
      }
      #Remove nomenclature notes
      if (!is.na(processed.input$NomenclatureNotes)) {
        final.output <- str_remove(final.output, paste("\\. ", processed.input$NomenclatureNotes, "\\.", sep = ""))
      }
      #Remove description refernece 
      if (!is.na(processed.input$DescriptionRef)) {
        description <- paste("\\[.*?", processed.input$DescriptionRef, "\\]", sep = "")
        #description <- str_extract(final.output, description)
        final.output <- str_remove(final.output, description)
      }
      #Remove type locality
      if (!is.na(processed.input$TypeLocality)) {
        final.output <- str_remove(final.output, fixed(processed.input$TypeLocality))
      }
      #Remove types
      if (!is.na(processed.input$Holotype)) {
        final.output <- str_remove(final.output, fixed(paste("Holotype:", processed.input$Holotype)))
        final.output <- str_remove(final.output, fixed(paste("Holotype (unique):", processed.input$Holotype)))
        final.output <- str_remove(final.output, fixed(paste("Holotype (?):", processed.input$Holotype)))
        final.output <- str_remove(final.output, fixed(paste("Holotype (unique) (?):", processed.input$Holotype)))
      }
      if (!is.na(processed.input$Paratype)) {
        final.output <- str_remove(final.output, fixed(paste("Paratype:", processed.input$Paratype)))
        final.output <- str_remove(final.output, fixed(paste("Paratypes:", processed.input$Paratype)))
        final.output <- str_remove(final.output, fixed(paste("Paratypes: ", processed.input$Paratype)))
        final.output <- str_remove(final.output, fixed(paste("Paratypes (listed as additional material):", processed.input$Paratype)))
        final.output <- str_remove(final.output, fixed(paste("Paratypes (11):", processed.input$Paratype)))
        
      }
      if (!is.na(processed.input$Lectotype)) {
        final.output <- str_remove(final.output, fixed(paste("Lectotype:", processed.input$Lectotype)))
        final.output <- str_remove(final.output, fixed(paste("Lectotypes:", processed.input$Lectotype)))
      }
      if (!is.na(processed.input$Paralectotype)) {
        final.output <- str_remove(final.output, fixed(paste("Paralectotype:", processed.input$Paralectotype)))
        final.output <- str_remove(final.output, fixed(paste("Paralectotypes:", processed.input$Paralectotype)))
      }
      if (!is.na(processed.input$Neotype)) {
        final.output <- str_remove(final.output, fixed(paste("Neotype:", processed.input$Neotype)))
        final.output <- str_remove(final.output, fixed(paste("Neotypes:", processed.input$Neotype)))
      }
      if (!is.na(processed.input$Syntype)) {
        final.output <- str_remove(final.output, fixed(paste("Syntype:", processed.input$Syntype)))
        final.output <- str_remove(final.output, fixed(paste("Syntypes:", processed.input$Syntype)))
      }
      if (!is.na(processed.input$NoTypes)) {
        final.output <- str_remove(final.output, processed.input$NoTypes)
      }
      #Remove IUCN Year and Status
      if (!is.na(processed.input$IUCNYear) || !is.na(processed.input$IUCNStatus)) {
        if (grepl("\\(", processed.input$IUCNStatus)) {
          final.output <- str_remove(final.output, "[Ii][Uc][Cc][Nn].*?:.*?\\(.*?\\).*?\\.")
        } else {
          final.output <- str_remove(final.output, "[Ii][Uu][Cc][Nn].*?:.*?\\.")
        }
      }
      #Remove items that should theoretically be pulled by taxon history
      final.output <- str_remove(final.output, fixed(taxon.history_data))
      final.output <- str_remove(final.output, "\\u2022")
      #Remove page numbers
      if (!is.na(processed.input$RefPage)) {
        capturedPage <- str_replace_all(processed.input$RefPage, fixed("("), replacement = "\\(")
        capturedPage <- str_replace_all(capturedPage, fixed(")"), replacement = "\\)")
        capturedPage <- str_replace_all(capturedPage, fixed("."), replacement = "\\.")
        capturedPage <- str_replace_all(capturedPage, fixed("["), replacement = "\\[")
        capturedPage <- str_replace_all(capturedPage, fixed("]"), replacement = "\\]")
        capturedPage <- str_replace_all(capturedPage, fixed("?"), replacement = "\\?")
        capturedPage <- str_replace_all(capturedPage, fixed("+"), replacement = "\\+")
        final.output <- str_remove(final.output, paste(":", capturedPage, sep = ""))
      }
      
      #Clean up results
      final.output <- str_replace_all(final.output, "(\\. )+", "\\. ") # Clean up repeated periods
      final.output <- str_remove(final.output, "[\\. ]*$") # Clean excess spaces/periods at end of string
      final.output <- str_remove(final.output, "^[ \\.]*") # Clean excess spaces/periods at start of string
      
      if (!is.na(final.output)) {
        if (nchar(as.character(final.output)) == 0) {final.output <- NA} # Change blanks to NA
      }
      
      return(trimws(final.output))
    },
    
    error = function (cond) {return(paste("Error parsing miscellaneous data", cond))}
  )
}

######################################################################################################################################
## Get Page Number and Figure Data
######################################################################################################################################
get_page_number <- function(local.result, processed.input) {
  pageNum <- qdapRegex::ex_between(StripFun(local.result), left = processed.input, right = "[", fixed = TRUE)
  pageNum <- str_remove(pageNum, "^:")
  return(pageNum)
}

######################################################################################################################################
## Get Scientific Names From Common Names
######################################################################################################################################
GetSci <- function(query = query, language = language){
  message(paste0("Now matching common names to scientific names"))
  SciNames <- data.frame(matrix(nrow = 0, ncol = 3))
  colnames(SciNames) <- c("Query","Species","ComName")
  for(i in 1:length(query)){
    NameQuery <- rfishbase::common_to_sci(query[i], Language = language)
    Current.Dat <- cbind.data.frame(rep(query[i],dim(NameQuery)[1]),NameQuery$Species,NameQuery$ComName)
    SciNames <- rbind.data.frame(SciNames,Current.Dat)
  }
  return(SciNames)
}

######################################################################################################################################
## Search Catalog to Retrieve local.result 
######################################################################################################################################
catalog_search <- function(query, type, unavailable = FALSE, resolve = FALSE, phrase = FALSE) {
  ##### Add Headers for httr POST Requests #####
  headers = c(
    `Content-Type` = "application/x-www-form-urlencoded"
  )
  
  ##### Obtain Data from Cal Acad #####
  FormData = list(
    `name` = "searchform",
    `contains` = ifelse(phrase == TRUE, shQuote(query, type = "cmd"), query),
    `tbl` = type,
    `Submit` = "Search",
    `unavailable` = ifelse(unavailable == F, "Off", "On")
  )
  result <- httr::POST(url = "https://researcharchive.calacademy.org/research/ichthyology/catalog/fishcatmain.asp", httr::add_headers(.headers=headers), body = FormData, encode = "form", config = httr::config(ssl_verifypeer = FALSE))
  
  ##### Parse Result Into Usable Format #####
  result.rv <- xml2::read_html(result) # Parse results into html
  local.result <- as.character(rvest::html_elements(result.rv, '.result')[-1]) # Strip out header lines
  local.result <- local.result[!grepl('<p class=\"result\">(\r)?\n</p>\n', local.result)] # Strip out returns and form into list 
  
  ##### Resolve Names if Necessary #####
  if((length(local.result) == 0) && (resolve == TRUE)){
    resolved.names <- Resolve_Names(NameInput = query)#Try fuzzy searching and resolving names
    if(length(resolved.names) > 0) {
      message(paste0("No results found for supplied taxon ", query, ".", " Trying the best resolved name ", resolved.names, "."))
      FormData = list(
        `name` = "searchform",
        `contains` = ifelse(phrase == TRUE, shQuote(resolved.names, type = "cmd"), resolved.names),
        `tbl` = type,
        `Submit` = "Search",
        `unavailable` = ifelse(unavailable == F, "Off", "On")
      )
      result <- httr::POST(url = "https://researcharchive.calacademy.org/research/ichthyology/catalog/fishcatmain.asp", httr::add_headers(.headers=headers), body = FormData, encode = "form", config = httr::config(ssl_verifypeer = FALSE))
      result.rv <- xml2::read_html(result)#parse results
      local.result <- as.character(rvest::html_elements(result.rv, '.result')[-1])#Strip out header
      local.result <- local.result[!grepl('<p class=\"result\">\r\n</p>\n', local.result)]    #Strip out returns and form into list 
    }
  }
  
  return(local.result)
}

######################################################################################################################################
## Parse Taxon History Changes
######################################################################################################################################
tax_hist_changes <- function(changes) {
  ref.dat <- qdapRegex::ex_between(changes, left = "-- (",right = ")")
  if (is.na(ref.dat)) {
    ref.dat <- qdapRegex::ex_between(changes, left = "(",right = ")", include.markers = T)#separate reference data, include markers if needed
  }
  if (length(ref.dat[[1]]) > 1) { #When missing double hyphens with subfamily, as in floridana, Cichla
    ref.dat <- ref.dat[[1]][-1]
  }
  split.refs <- trimws(unlist(str_split(ref.dat, ",")))
  oriSplit <- split.refs
  
  startRefs <- str_extract(split.refs, "ref\\..*?\\>.*?\\</a")
  startRefs <- str_remove(startRefs, "\\<.*?\\>")
  startRefs <- str_remove(startRefs, "\\</a")
  startRefs <- str_remove(startRefs, "^ref\\. ")
  
  missingDataIndices <- sort(unique(c(which(!grepl(":", split.refs)), which(!grepl("\\[ref.*?\\]", split.refs))))) # Get positions of lines with issues
  toDeleteIndices <- c() # Storage for the lines that will need to be removed
  
  for (i in missingDataIndices) {
    if (i %in% toDeleteIndices | length(split.refs) == 1) {next} # Skip entries already marked for deletion
    
    if ((i+1) %in% missingDataIndices) { # If sequential entries are missing information, smash them together
      if ((i+2) %in% missingDataIndices) {
        split.refs[i] <- paste(split.refs[i], ", ", split.refs[i+1], ", ", split.refs[i+2], sep = "")
        toDeleteIndices <- c(toDeleteIndices, (i+1), (i+2))
      } else {
        split.refs[i] <- paste(split.refs[i], ", ", split.refs[i+1], sep = "")
        toDeleteIndices <- c(toDeleteIndices, (i+1))
      }
    } 
    
    else if (grepl(":", split.refs[i]) && !grepl("\\[ref.*?\\]", split.refs[i])) { # Handle cases that do not have a reference number
      split.refs[i] <- paste(split.refs[i], fixed("[ref.NA]"), sep = "")
    } 
    
    else {
      split.refs[i-1] <- paste(split.refs[i-1], ", ", split.refs[i], sep = "") # Handle cases when authors contain a comma
      toDeleteIndices <- c(toDeleteIndices, i)
    }
  }
  if(!is.null(toDeleteIndices)) {split.refs <- split.refs[-1*toDeleteIndices]} # Remove modified indices
  
  tbl_tempRef <- data.frame(matrix(nrow = length(split.refs), ncol = 3))
  colnames(tbl_tempRef) <- c("nom.ref", "nom.ref.no", "ref.order")
  for (i in seq_along(split.refs)) {
    if (grepl("^[0-9]+:", split.refs[i])) { # If no author, set nom.ref to NA
      tbl_tempRef[i, 1] <- NA
    } 
    else { # Otherwise find the author and year, stopping before page number
      nom.ref <- str_remove(split.refs[i], "\\[ref\\..*?\\]")
      nom.ref <- str_remove(nom.ref, ":.*")
      tbl_tempRef[i, 1] <- nom.ref
    }
    
    if (!grepl("\\[ref\\..*?\\]", split.refs[i])) { # If no reference, set nom.ref.no to NA
      tbl_tempRef[i, 2] <- NA
    }
    else { # Otherwise extract the reference
      nom.ref.no <- str_extract(split.refs[i], "\\[ref\\..*?\\]")
      nom.ref.no <- trimws(str_remove_all(nom.ref.no, "\\<a.*?\\>|\\<\\/a\\>|\\[|\\]|ref\\."))
      tbl_tempRef[i, 2] <- nom.ref.no
    }
    tbl_tempRef[i, 3] <- which(grepl(tbl_tempRef[i, 2], oriSplit))[1]
  }
  
  missingRefs <- data.frame(matrix(nrow = 0, ncol = 2))
  if(length(tbl_tempRef$nom.ref.no) != length(startRefs)) {
    missing <- c(setdiff(tbl_tempRef$nom.ref.no, startRefs), setdiff(startRefs, tbl_tempRef$nom.ref.no))
    for (i in seq_along(missing)) {
      if (!is.na(missing[i])) {
        refi <- which(grepl(missing[i], oriSplit))[1]
        nom.ref <- str_remove(str_extract(oriSplit[refi], "^.*?ref\\."), "ref\\.")
        nom.ref <- trimws(str_remove(nom.ref, "\\["))
        tbl_tempRef <- rbind(tbl_tempRef, c(nom.ref, missing[i], refi))
      }
    }
  }
  
  tbl_tempRef <- tbl_tempRef[order(tbl_tempRef$ref.order),]
  
  return(list(tbl_tempRef$nom.ref, tbl_tempRef$nom.ref.no))
}
