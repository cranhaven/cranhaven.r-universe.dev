#' @title get_accepted_names
#' @description Match namelist with master and fetch the accepted names
#' using the linkages provided within the data
#' @param namelist data frame of the list of names to be resolved. Must
#' contain either column canonical containing binomial or trinomial name
#' without spp. and var. etc. or may contain columns for genus, species
#' and subspecies (any sub-specific unit) and the names of the columns are
#' passed as subsequent parameters.
#' @param master data frame with required columns id, canonical and accid.
#' Other columns like order, family are optional. Column id is typically
#' running ids for each record and accid will contain 0 if the name is
#' currently accepted name and id number of accepted name in case the name
#' is a synonym. Column canonical contains binomial or trinomial without
#' spp. var. etc.
#' @param gen_syn data frame with columns Original_Genus and Valid_Genus
#'  where Original_genus is synonym and valid_genus is one present in the
#'  master. Default: NA when gen_syn is not used.
#' @param namelookup Lookup data frame for names where some names might
#' need manual lookup. The columns required are binomial and validname
#' where binomial is new name and validname is present in the master.
#'  Default: NA when namelookup is not used.
#' @param mastersource vector of sources to be used for assignment with
#' priority
#' @param match_higher match genus and family names present in canonical
#' field
#' @param fuzzymatch attempt fuzzy matching or not. Default: TRUE
#' @param fuzzydist fuzzy distance while matching. Default : 2
#' @param canonical column containing names to be resolved to accepted names
#' , Default: NA when columns for genus and species are specified.
#' @param genus column containing genus names to be resolved to accepted
#' names and typically accompanied by species and subspecies columns, Default: NA
#' when canonical parameter is supplied.
#' @param species column containing species names to be resolved to accepted
#' names and is accompanied by genus, Default: NA
#' @param subspecies column containing species names to be resolved to accepted
#' names and is accompanied by genus and species, Default: NA
#' @param prefix to be added to all the return fields
#' @param verbose display process messages, Default: TRUE
#' @return data frame containing all the original columns with following
#' additional columns:\describe{
#' \item{accepted_name - }{Accepted name present in the master. NA is not resolved}
#' \item{method - }{method used to resolve the name. See details for explanation
#' of each method}
#' }
#' @details
#' Name resolution methods:\describe{
#' \item{direct - }{was a direct match with name or a synonym}
#' \item{direct2 - }{was a direct match with name or a synonym in non mastersource}
#' \item{fuzzy - }{used fuzzy matching}
#' \item{gensyn - }{genus substitution with known genus level synonyms}
#' \item{lookup - }{Manual lookup in earlier processing}
#' \item{sppdrop - }{subspecies was dropped}
#' \item{sub2sp - }{subspecies elevated to species}
#' \item{genus - }{genus was matched}
#' \item{family - }{family was matched}
#' \item{NA - }{could not be resolved}
#' }
#' Note: Make sure all the data frames have same character encoding to prevent
#'  errors.
#' @importFrom stringr word
#' @family Name functions
#' @examples
#' \donttest{
#'master <- data.frame("id" = c(1,2,3,4,5,6,7),
#'                     "canonical" = c("Hypochlorosis ancharia",
#'                                     "Hypochlorosis tenebrosa",
#'                                     "Pseudonotis humboldti",
#'                                     "Myrina ancharia",
#'                                     "Hypochlorosis ancharia tenebrosa",
#'                                     "Hypochlorosis ancharia obiana",
#'                                     "Hypochlorosis lorquinii"),
#'                      "family" = c("Lycaenidae", "Lycaenidae", "Lycaenidae",
#'                                   "Lycaenidae", "Lycaenidae", "Lycaenidae",
#'                                   "Lycaenidae"),
#'                     "accid" = c(0,1,1,1,0,0,0),
#'                     "source" = c("itis","itis","wiki","wiki","itis",
#'                                  "itis","itis"),
#'                     stringsAsFactors = FALSE)
#'
#'mylist <- data.frame("id"= c(11,12,13,14,15,16,17,18,19),
#'                     "scname" = c("Hypochlorosis ancharia",
#'                                  "Hypochlorosis ancharii",
#'                                  "Hypochlorosis tenebrosa",
#'                                  "Pseudonotis humboldtii",
#'                                  "Abrothrix longipilis",
#'                                  "Myrinana anchariana",
#'                                  "Hypochlorosis ancharia ancharia",
#'                                  "Myrina lorquinii",
#'                                  "Sithon lorquinii"),
#'                     stringsAsFactors = FALSE)
#'
#'res <- get_accepted_names(namelist = mylist,
#'                          master=master,
#'                          canonical = "scname")
#'
#'gen_syn_list <- data.frame("Original_Genus"=c("Pseudonotis",
#'                                              "Myrina"),
#'                           "Valid_Genus"=c("Hypochlorosis",
#'                                           "Hypochlorosis"),
#'                           stringsAsFactors = FALSE)
#'
#'res <- get_accepted_names(namelist = mylist,
#'                          master=master,
#'                          gen_syn = gen_syn_list,
#'                          canonical = "scname")
#'
#'lookup_list <- data.frame("binomial"=c("Sithon lorquinii",
#'                                       "Hypochlorosis humboldti"),
#'                          "validname"=c("Hypochlorosis lorquinii",
#'                                        "Hypochlorosis lorquinii"),
#'                          stringsAsFactors = FALSE)
#'
#'res <- get_accepted_names(namelist = mylist,
#'                          master=master,
#'                          gen_syn = gen_syn_list,
#'                          namelookup = lookup_list,
#'                          canonical = "scname")
#'
#'mylist_s <- melt_canonical(mylist,canonical = "scname",
#'                           genus = "genus",
#'                           species = "species",
#'                           subspecies = "subspecies")
#'
#'res <- get_accepted_names(namelist = mylist_s,
#'                          master=master,
#'                          gen_syn = gen_syn_list,
#'                          namelookup = lookup_list,
#'                          genus = "genus",
#'                          species = "species",
#'                          subspecies = "subspecies")
#'
#'res <- get_accepted_names(namelist = mylist_s,
#'                          master=master,
#'                          gen_syn = gen_syn_list,
#'                          namelookup = lookup_list,
#'                          mastersource = c("itis"),
#'                          genus = "genus",
#'                          species = "species",
#'                          subspecies = "subspecies")
#'
#'mylist <- data.frame("id"= c(11,12,13,14,15,16,17,18),
#'                     "scname" = c("Hypochlorosis ancharia",
#'                                  "Hypochlorosis ancharii",
#'                                  "Hypochlorosis",
#'                                  "Pseudonotis",
#'                                  "Lycaenidae",
#'                                  "Pseudonotis humboldtii",
#'                                  "Abrothrix longipilis",
#'                                  "Myrinana anchariana"),
#'                     stringsAsFactors = FALSE)
#'
#'res <- get_accepted_names(namelist = mylist,
#'                          master=master,
#'                          match_higher = TRUE,
#'                          canonical = "scname")
#' }
#' @rdname get_accepted_names
#' @export
get_accepted_names <- function(namelist, master, gen_syn=NA, namelookup=NA,
                               mastersource=NA, match_higher=FALSE, 
                               fuzzymatch=TRUE, fuzzydist=2, canonical=NA, 
                               genus=NA, species=NA, subspecies=NA, 
                               prefix="", verbose=TRUE){
  # Set the data
  names(master) <- tolower(names(master))
  if(!missing(mastersource)){
    orig_master <- master
    if("source" %in% names(master) ){
      master <- master[which(master$source %in% mastersource),]
      if(nrow(master)==0){
        cat("\nProblem matching mastersource")
        return(NULL)
      }
    } else {
      cat("\nmaster data frame needs to have source column to use mastersource option")
      return(NULL)
    }
  }
  if(is.na(canonical)){
    if(verbose){cat("\nConstructing canonical name field\n")}
    namelist <- cast_canonical(namelist,"canonical",genus,species,subspecies)
    canonical="canonical"
  }
  if(!missing(gen_syn)){
    gen_synr <- data.frame(cbind(gen_syn[,2],gen_syn[,1]))
    names(gen_synr) <- names(gen_syn)
    gen_syn <- rbind(gen_syn,gen_synr)
    gen_syn <- gen_syn[!duplicated(gen_syn),]
  }
  namelist <- rename_column(namelist,canonical,"canonical_")
  new <- namelist
  #--- Add a dummy rec
  tmprec <- new[1,]
  tmprec$canonical_ <- master$canonical[1]
  new <- rbind(new,tmprec)
  #---
  new$orig_canonical <- new$canonical_
  new$method <- NA
  new$source <- NA
  new$id_ <- 0
  new$accid_ <- 0
  new$mysrno_ <- seq(1:nrow(new))
  
  # Get Name lookup table names replaced
  if(!missing(namelookup)){
    if(verbose){cat("\nUsing lookup table")}
    for(i in 1:nrow(new)){
      if(new$orig_canonical[i] %in% namelookup$binomial) {
        if(verbose){cat("+")}
        new$canonical_[i] <- namelookup$validname[which(new$orig_canonical[i]==namelookup$binomial)]
        new$method[i] <- "lookup"
      }
    }
  }
  
  # Remove repeating species and subspecies
  if(verbose){cat("\nRemoving repeating subspecies names")}
  for(i in 1:nrow(new)){
    if(length(strsplit(new$canonical_[i],' ')[[1]])>2){
      if(word(new$canonical_[i],2,2)==word(new$canonical_[i],3,3)){
        new$canonical_[i] <- word(new$canonical_[i],1,2)
        new$method[i] <- "repsub"
      }
    }
  }
  
  # Direct Matches
  if(verbose){cat("\nFetching accepted names")}
  new$id_<- master$id[match(new$canonical_, master$canonical)]
  new$accid_<- master$accid[match(new$canonical_, master$canonical)]
  new$newid_[which(new$accid_==0)] <- new$id_[which(new$accid_==0)]
  new$newid_[which(new$accid_!=0)] <- new$accid_[which(new$accid_!=0)]
  new$accepted_name <- master$canonical[match(new$newid_, master$id)]
  new$source <- master$source[match(new$id_, master$id)]
  new$method[which(is.na(new$method) & !is.na(new$accepted_name))] <- "direct"
  
  # Genus swap
  if(nrow(new[which(is.na(new$accepted_name)),])>0){
    if(!missing(gen_syn)){
      if(verbose){cat("\nTrying Genus level synonyms")}
      for(i in 1:nrow(new)){
        if(is.na(new$accepted_name[i])){
          curgenus <- word(new$canonical_[i],1,1)
          genlist <- as.character(gen_syn[which(gen_syn$Valid_Genus==
                                                  curgenus),
                                          c("Original_Genus")])
          if(length(genlist)>0){
            for(j in 1:length(genlist)){
              combi_name <- paste(genlist[j],
                                  word(new$canonical_[i],
                                       2,length(strsplit(new$canonical_[i],
                                                         ' ')[[1]])))
              name_match <- master[which(master$canonical==combi_name),]
              if(dim(name_match)[1]==1){
                if(name_match$accid[1]==0){
                  match_rec <- name_match
                  if(verbose){cat("+")}
                } else {
                  match_rec <- master[which(master$id==name_match$accid[1]),]
                  if(verbose){cat("*")}
                }
                new$accepted_name[i] <- match_rec$canonical[1]
                new$source[i] <- name_match$source[1]
                new$method[i] <- "gensyn"
              }
            }
          }
        }
      }
    }
  }
  
  # Subspecies to species
  if(nrow(new[which(is.na(new$accepted_name)),])>0){
    if(verbose){cat("\nTrying Subspecies to species ")}
    for(i in 1:nrow(new)){
      if(is.na(new$accepted_name[i]) &
         length(strsplit(new$canonical_[i],' ')[[1]])>2){
        swap_name <- paste(word(new$canonical_[i],1,1),word(new$canonical_[i],3,3))
        name_match <- master[which(master$canonical==swap_name),]
        if(dim(name_match)[1]==1){
          if(name_match$accid[1]==0){
            match_rec <- name_match
            if(verbose){cat("+")}
          } else {
            match_rec <- master[which(master$id==name_match$accid[1]),]
            if(verbose){cat("*")}
          }
          new$accepted_name[i] <- match_rec$canonical[1]
          new$source[i] <- name_match$source[1]
          new$canonical_[i] <- swap_name
          new$method[i] <- "sub2sp"
        }
      }
    }
  }
  
  # Dropping subspecies
  if(nrow(new[which(is.na(new$accepted_name)),])>0){
    if(verbose){cat("\nTrying dropping Subspecies")}
    for(i in 1:nrow(new)){
      if(is.na(new$accepted_name[i]) &
         length(strsplit(new$canonical_[i],' ')[[1]])>2){
        drop_name <- word(new$canonical_[i],1,2)
        name_match <- master[which(master$canonical==drop_name),]
        if(dim(name_match)[1]==1){
          if(name_match$accid[1]==0){
            match_rec <- name_match
            if(verbose){cat("+")}
          } else {
            match_rec <- master[which(master$id==name_match$accid[1]),]
            if(verbose){cat("*")}
          }
          new$accepted_name[i] <- match_rec$canonical[1]
          new$source[i] <- name_match$source[1]
          new$canonical_[i] <- drop_name
          new$method[i] <- "sppdrop"
        }
      }
    }
  }
  
  if(nrow(new[which(is.na(new$accepted_name)),])>0){
    if(!missing(mastersource)){
      if(verbose){cat("\nFetching accepted names from remaining names")}
      master <- orig_master
      new_resolved <- new[which(!is.na(new$accepted_name)),]
      new <- new[which(is.na(new$accepted_name)),]
      new$id_<- master$id[match(new$canonical_, master$canonical)]
      new$accid_<- master$accid[match(new$canonical_, master$canonical)]
      new$newid_[which(new$accid_==0)] <- new$id_[which(new$accid_==0)]
      new$newid_[which(new$accid_!=0)] <- new$accid_[which(new$accid_!=0)]
      new$accepted_name <- master$canonical[match(new$newid_, master$id)]
      new$source <- master$source[match(new$id_, master$id)]
      new$method[which(is.na(new$method) & !is.na(new$accepted_name))] <- "direct2"
      new <- rbind(new_resolved,new)
    }
  }
  
  # Fuzzy matches
  if(fuzzymatch){
    if(nrow(new[which(is.na(new$accepted_name)),])>0){
      if(verbose){cat("\nTrying Fuzzy Matches")}
      names_matched <- NULL
      for(i in 1:nrow(new)){
        if(is.na(new$accepted_name[i]) & !is.na(new$canonical_[i])){
          if(guess_taxo_rank(new$canonical_[i])!="Genus or above"){
            if(verbose){cat(paste("\n ",new$canonical_[i],"- "))}
            if(new$canonical_[i] %in% names_matched$sname){
              fres <- names_matched[which(names_matched$sname==new$canonical_[i]),]
            } else {
              fres <- taxo_fuzzy_match(new$canonical_[i],master,dist=fuzzydist)
              names_matched <- rbind(names_matched,fres)
            }
            if(!is.null(fres)){
              new$canonical_[i] <- fres$canonical
              name_match <- master[which(master$canonical==fres$canonical),]
              if(verbose){cat(name_match$source[1])}
              if(dim(name_match)[1]==1){
                if(name_match$accid[1]==0){
                  match_rec <- name_match
                  if(verbose){cat(" +")}
                } else {
                  match_rec <- master[which(master$id==name_match$accid[1]),]
                  if(verbose){cat(" *")}
                }
                new$accepted_name[i] <- match_rec$canonical[1]
                new$source[i] <- name_match$source[1]
                new$method[i] <- "fuzzy"
              }
            } else {
              fres <- data.frame("canonical"=NA,
                                 "dist"=NA,
                                 "sname"=new$canonical_[i],
                                 stringsAsFactors = F)
              names_matched <- rbind(names_matched,fres)
            }
          }
        }
      }
    }
  }
  
  # Get higher taxonomy
  new$family <- master$family[match(new$canonical_, master$canonical)]
  new$subfamily <- master$subfamily[match(new$canonical_, master$canonical)]
  new$tribe <- master$tribe[match(new$canonical_, master$canonical)]
  
  # Match higher taxonomy rankes than species
  if(match_higher){
    if("genus" %!in% names(master)){
      if(verbose){cat("\nSetting up for higher taxonomy searches\n")}
      master <- melt_canonical(master,"canonical","genus","species","subspecies")
    }
    if("family" %!in% names(master)){
      message("family column missing in master")
    } else {
      new_resolved <- new[which(!is.na(new$accepted_name)),]
      new <- new[which(is.na(new$accepted_name)),]
      new$id_<- master$id[match(new$canonical_, master$family)]
      new$accid_<- master$accid[match(new$canonical_, master$family)]
      new$newid_[which(new$accid_==0)] <- new$id_[which(new$accid_==0)]
      new$newid_[which(new$accid_!=0)] <- new$accid_[which(new$accid_!=0)]
      new$accepted_name <- master$family[match(new$newid_, master$id)]
      new$source <- master$source[match(new$id_, master$id)]
      new$method[which(is.na(new$method) & !is.na(new$accepted_name))] <- "family"
      new$family <- master$family[match(new$newid_, master$id)]
      new <- rbind(new_resolved,new)
    }
    new_resolved <- new[which(!is.na(new$accepted_name)),]
    new <- new[which(is.na(new$accepted_name)),]
    new$id_<- master$id[match(new$canonical_, master$genus)]
    new$accid_<- master$accid[match(new$canonical_, master$genus)]
    new$newid_[which(new$accid_==0)] <- new$id_[which(new$accid_==0)]
    new$newid_[which(new$accid_!=0)] <- new$accid_[which(new$accid_!=0)]
    new$accepted_name <- master$genus[match(new$newid_, master$id)]
    new$source <- master$source[match(new$id_, master$id)]
    new$method[which(is.na(new$method) & !is.na(new$accepted_name))] <- "genus"
    new$family <- master$family[match(new$canonical_, master$genus)]
    new$subfamily <- master$subfamily[match(new$canonical_, master$genus)]
    new$tribe <- master$tribe[match(new$canonical_, master$genus)]
    new <- rbind(new_resolved,new)
  }
  
  # Cleanup and return data
  new <- new[which(new$mysrno_!=nrow(new)),]
  new <- new[order(new$mysrno_),]
  new <- new[,-which(names(new) %in% c("id_","accid_","newid_","mysrno_"))]
  if(!is.na(canonical)){
    new <- rename_column(new,"canonical_",canonical)
  }
  tab_stat <- plyr::count(new$method)
  if(prefix!=""){
    new <- rename_column(new,"accepted_name",paste(prefix,"accepted_name",sep = ""))
    new <- rename_column(new,"orig_canonical",paste(prefix,"orig_canonical",sep = ""))
    new <- rename_column(new,"source",paste(prefix,"source",sep = ""),silent=TRUE)
    new <- rename_column(new,"method",paste(prefix,"method",sep = ""))
    new <- rename_column(new,"family",paste(prefix,"family",sep = ""),silent=TRUE)
    new <- rename_column(new,"subfamily",paste(prefix,"subfamily",sep = ""),silent=TRUE)
    new <- rename_column(new,"tribe",paste(prefix,"tribe",sep = ""),silent=TRUE)
  }
  if(verbose){
    cat("\nDone\n\nStat:\n")
    print.data.frame(tab_stat)
  }
  return(new)
}
