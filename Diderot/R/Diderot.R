#! /bin/R
# Diderot 0.1 : Bibliographic Citation Network Analysis
# @2017 Christian Vincenot (christian@vincenot.biz)
# Released under the GNU General Public License v3. See https://www.gnu.org/licenses/gpl.html

# !!!!!!!!!!!!
# TODO: Changes to R 4.0.0 changed default value of stringsAsFactors 
# in data.frame() and read.table() from TRUE to FALSE. 
# Changed all empty calls to ", stringsAsFactors = TRUE". Double-check if problems occur.
# !!!!!!!!!!!!

# library(RCurl)
library(igraph)
library(stringi)
library(doParallel)
library(igraph)
library(graphics)
library(stats)
library(utils)
library(parallel)
library(splitstackshape)

get_date_from_doi <- function(doi, extract_date_from_doi) {
#  require(RCurl)
  tt<-doi
  if (extract_date_from_doi) {
    tt<-sub("(.*)([1-2][0-9][0-9][0-9]\\.[0-9][0-9])\\.(.*)", "\\2", doi)
  }
  if (tt == doi) {
    webpage<-tryCatch({
      webpage<-RCurl::getURL(sprintf("http://dx.doi.org/%s", doi), httpheader = c(Accept = "text/turtle"), .opts=RCurl::curlOptions(followlocation=TRUE,cookiefile="nosuchfile"))
      #print(webpage)
      tt<-sub("^.*?/date>.*?([1-2][0-9][0-9][0-9]-[0-9][0-9]?(-[0-9][0-9]?)?).*", "\\1", webpage)
      if (tt == webpage) tt<-NA
    }, 
    error = function(e) {
      print(paste("CAUGHT ERROR ",e))
      tt<-NA
      NA
    })
  } else {
    print(sprintf("[%s] Guessed date from DOI string: %s",doi, tt))
  }
  tt
}

# Retrieve citation title from full reference string using Freecite
# e.g. string : Udvarhelyi, I.S., Gatsonis, C.A., Epstein, A.M., Pashos, C.L., Newhouse, J.P. and McNeil, B.J. Acute Myocardial Infarction in the Medicare population: process of care and clinical outcomes. Journal of the American Medical Association, 1992; 18:2530-2536.
get_reference_title<-function(str) {
  begin<-"<span class=\"title\">"
  end<-"</span>"
  web<-RCurl::postForm("http://freecite.library.brown.edu/citations/create", citation=str, httpheader = c(Accept = "text/turtle"), .opts=curlOptions(followlocation=TRUE,cookiefile="nosuchfile"))
  m<-regexpr(begin,web)
  temp<-substr(web,m[1]+nchar(begin),1000)
  m<-regexpr(end,temp)
  temp<-substr(temp,0,m[1]-1)
  temp
}

create_bibliography <- function(corpora_files, labels, keywords, retrieve_pubdates=F, clean_refs=F, encoding=NULL) {
  # Standardize date to %Y-%m-%d format (i.e. %F)
  standard_date <- function(dat, fill_random_day, minYear=-Inf, maxYear=+Inf) {
    if (is.na(dat)) return(NA)
    tt<-as.Date(dat, format = "%F")
    if (is.na(tt)) {
      tt<-format(as.Date(paste(1, dat), "%d %Y-%m"), "%F") #Assign random day for %Y-%m format
      rep<-ifelse(fill_random_day,sprintf("%02d",round(stats::runif(1,1,28))),"00")
      tt<-sub("[0-9][0-9]$", rep, tt)
    } else tt<-format(tt, "%F") 
    # Check if date is within range
    year<-as.numeric(sub("^([1-2][0-9][0-9][0-9]).*", "\\1", tt))
    if (!is.na(year) && year < maxYear+1 & year > minYear-1) {tt
    } else NA
  }
  
  # Remove possible Byte-Order-Markers in Windows UTF files
  no_BOM<-function(string) {
    if (grepl(substring(string,1,3), '\xef\xbb\xbf'))
      return(substring(string,4)) else
        return(string)
  }
  
  tt_Final<-NULL
  for (i in 1:length(corpora_files)) {
    if (!is.null(encoding)) {
      tt<-utils::read.csv(corpora_files[i], stringsAsFactors=FALSE, fileEncoding=encoding)
    } else tt<-utils::read.csv(corpora_files[i], stringsAsFactors=FALSE)
    tt$Corpus<-labels[i];
    print(sprintf("File %s contains %d records", corpora_files[i], length(tt$Corpus)))
    #pause()
    
    colnames(tt)[1]<-no_BOM(colnames(tt)[1])
    
    if (clean_refs) {
      rr<-strsplit(tt$References, ";")
      for (j in 1:length(rr)) {
        r1<-unlist(rr[j])
        if (is.null(r1[1]) || is.na(r1[1])) {#print(paste("NULL VALUE !!",j, rr[j]))
        } else {
          r1_old<-r1
          r1<-sub(".*?\\., (.*?)\\([1-2][0-9][0-9][0-9].*", "\\1", r1_old)
          #r1<-sub(".*?., (.*?)\\([1-2][0-9][0-9][0-9].*", "\\1", r1)
          # We face a different type of reference (with year without parenthesis and the title between " ") hopefully...
          if (r1[1] == r1_old[1] || r1[1] == "") r1<-sub(".*\\\"(.*),\\\".*", "\\1", r1_old)
          
          # Yet another one (with title between year in parenthesis and comma)
          if (r1[1] == r1_old[1] || r1[1] == "")
            r1<-sub(".*\\([1-2][0-9][0-9][0-9]\\)\\s*?(.*?),.*", "\\1", r1_old)
          
          # Last one (with title between year in parenthesis+comma and period)
          if (r1[1] == r1_old[1] || r1[1] == "")
            r1<-sub(".*\\([1-2][0-9][0-9][0-9]\\),\\s*?(.*?)\\..*", "\\1", r1_old)
          
          # Last resort: take everything between year in parenthesis and comma (1) or end (2), OR take everything between year without parenthesis and comma (3)
          # (1)
          if (r1[1] == r1_old[1] || r1[1] == "") {
            r1<-sub(".*\\([1-2][0-9][0-9][0-9]\\)(\\s*?)(.*?),.*", "\\2", r1_old)
          }
          # (2)
          if (r1[1] == r1_old[1] || r1[1] == "") {
            r1<-sub(".*\\([1-2][0-9][0-9][0-9]\\)(\\s*)?(.*)", "\\2", r1_old)
          }
          # (3)
          if (r1[1] == r1_old[1] || r1[1] == "") {
            print(paste("Warning, spurrious record: ",unlist(rr[j])[1]," INDEX:", j))
            r1<-sub(".*\\.,(\\s*)(.*),.*", "\\2", r1_old)
          }
          
          #print(paste(r1[1],j))
          r1<-r1[r1 != ""]
          tt$References[j]<-paste(r1, collapse=';')
        }
      }
    }
    if (!is.null(tt_Final)) {tt_Final<-rbind(tt_Final,tt);#merge(tt_Final,tt, all=T);
    } else tt_Final<-tt
  }
  
  # For Science of Science (SCI2) Input
  colnames(tt_Final)[which(names(tt_Final) == "References")] <- "Cited References"
  colnames(tt_Final)[which(names(tt_Final) == "Title")] <- "Cite Me As"
  
  # Correct Corpuses for ABM/IBM
  # Correct Corpuses for mixed input
  tt_Final[tt_Final$Corpus == labels[1] & grepl(keywords[2], tt_Final$Author.Keywords, ignore.case = T), "Corpus"] <- paste(labels[1],"|", labels[2])
  tt_Final[tt_Final$Corpus == labels[1] & grepl(keywords[2], tt_Final$Index.Keywords, ignore.case = T), "Corpus"] <- paste(labels[1],"|", labels[2])
  tt_Final[tt_Final$Corpus == labels[1] & grepl(keywords[2], tt_Final$Abstract, ignore.case = T), "Corpus"]<- paste(labels[1],"|", labels[2])
  tt_Final[tt_Final$Corpus == labels[2] & grepl(keywords[1], tt_Final$Author.Keywords, ignore.case = T), "Corpus"]<- paste(labels[1],"|", labels[2])
  tt_Final[tt_Final$Corpus == labels[2] & grepl(keywords[1], tt_Final$Index.Keywords, ignore.case = T), "Corpus"]<- paste(labels[1],"|", labels[2])
  tt_Final[tt_Final$Corpus == labels[2] & grepl(keywords[1], tt_Final$Abstract, ignore.case = T), "Corpus"]<- paste(labels[1],"|", labels[2])
  
  #tt_Final_BACK<<-tt_Final
  
  #tt_Final[(duplicated(tolower(tt_Final[["Cite Me As"]]), fromLast=T) & duplicated(tolower(tt_Final[["Authors"]]),fromLast=T)),"Corpus"]<-paste(labels[1],"|", labels[2]
  
  # Remove duplicate entries based on title
  # Normalize case to avoid missing duplicates with "!duplicated"
  #tt_Final<-tt_Final[!duplicated(tolower(tt_Final[["Cite Me As"]])),]
  tt_dup_check<-tt_Final[c("Cite Me As", "Authors", "Corpus")]
  tt_dup_check[["Cite Me As"]]<-tolower(tt_dup_check[["Cite Me As"]])
  tt_dup_check[["Authors"]]<-tolower(tt_dup_check[["Authors"]])
  # First remove real duplicates (authors, title, corpus)
  tt_Final<-tt_Final[!duplicated(tt_dup_check),]
  tt_dup_check<-tt_dup_check[!duplicated(tt_dup_check),]
  tt_dup_check<-tt_dup_check[c("Cite Me As", "Authors")]
  # Identify joint citations, i.e. duplicates (authors, title)
  tt_Final[duplicated(tt_dup_check, fromLast=T), "Corpus"]<-paste(labels[1],"|", labels[2])
  tt_Final<-tt_Final[!duplicated(tt_dup_check,),]
  rm(tt_dup_check)
  
  #tt_Final<-tt_Final[!(duplicated(tolower(tt_Final[["Cite Me As"]])) & duplicated(tolower(tt_Final[["Authors"]]))),]
  
  # Retrieve dates
  if (retrieve_pubdates) {
    lng<-length(tt_Final$DOI)
    for (i in 1:lng) {
      print(paste("Retrieving date for ", tt_Final[i, "DOI"], " ", i, "/", lng))
      tt_Final[i,"raw_date"] <- get_date_from_doi(tt_Final[i, "DOI"], FALSE)
      tt_Final[i,"date"] <- standard_date(tt_Final[i,"raw_date"], TRUE, min(tt_Final$Year, na.rm = TRUE)-1, max(tt_Final$Year, na.rm = TRUE)+1)
      if (is.na(tt_Final[i,"date"]) & !is.na(tt_Final[i, "Year"])) tt_Final[i,"date"] <- sprintf("%s-%02d-%02d", tt_Final[i,"Year"], round(stats::runif(1,1,12)), round(stats::runif(1,1,28)))
    }
  }
  tt_Final
}


# Outputs the total number of references in the dataset
nb_refs<-function(db) {
  rr<-strsplit(db[["Cited References"]], ";")
  rr<-unlist(rr)
  res<-length(rr)
  print(sprintf("%d publications citing %d references", nrow(db), res))
  res
}

compute_Ji<-function(db, pubtitle, labels, from=-1, to=-1) {
  calculate_jr<-function(list1, list2, from, to) {
    dat<-data.frame(year=seq(from,to), stringsAsFactors = TRUE)
    dat<-merge(dat, data.frame(year=names(list1),as.numeric(list1), stringsAsFactors = TRUE), all=T)
    dat<-merge(dat, data.frame(year=names(list2),as.numeric(list2), stringsAsFactors = TRUE), all=T)
    colnames(dat)<-c("year","list1","list2")
    dat[is.na(dat$list1),"list1"]<-0
    dat[is.na(dat$list2),"list2"]<-0
    dat$list1<-cumsum(dat$list1)
    dat$list2<-cumsum(dat$list2)
    dat$Ji<-pmin(dat$list1, dat$list2)
    ret<-data.frame(seq(from,to), dat$Ji, stringsAsFactors = TRUE)
    colnames(ret)<-c("year", "val")
    ret
  }
  if (from<0) from<-min(db$Year)
  if (to<0) from<-max(db$Year)  
  cits<-db[grep(pubtitle, db[["Cited References"]], ignore.case=T),]
  IBcits<- cits[cits$Corpus == labels[1],]
  IByr<- table(IBcits$Year)
  ABcits<- cits[cits$Corpus == labels[2],]
  AByr<- table(ABcits$Year)
  calculate_jr(AByr, IByr, from, to)    
}

# Notes: (1) We create the graph using vertices WITHOUT names first, to avoid strange "invalid vertex names" error during edge creation.
#        (2) If an error happens in foreach thread, it will only be reported after all iterations are done. For debugging, swithc to for loop.
build_graph<-function(db, title="Cite Me As", year="Year", authors="Authors", ref="Cited References", 
                      set.title.as.name=F, attrs=NULL, verbose=F, makeCluster.type="PSOCK", nb.cores=NA, fine.check.threshold=1000, 
                      fine.check.nb.authors=3, small.year.mismatch=T, debug=F) {
  if (debug) options(error=utils::recover)
  start<-Sys.time()
#  require(doParallel)
  if (!is.na(nb.cores)) no_cores<-nb.cores else no_cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(no_cores, type=makeCluster.type, outfile="")
  doParallel::registerDoParallel(cl)
  on.exit({if (!is.null(cl)) parallel::stopCluster(cl)})
  
  dat<-data.frame(seq(1,length(db[[title]])),db[[title]], db[[authors]], db[[year]], as.character(db[[ref]]),stringsAsFactors=F)
  colnames(dat)<-c("id",title,authors,year,ref)
  grap<-graph.empty(n=length(dat$id), directed=T)
  grap<-set.vertex.attribute(grap, name="id", index=V(grap), value=db$id)
  
  refs<-strsplit(dat[[ref]], ";", fixed=T)
  
  rest<-NULL
  i<-0
  ##########
  #for (i in seq(10438,length(dat$id))) {
  rest<-foreach(i=1:length(dat$id)
                , .packages='stringi', .verbose = verbose) %dopar% {
                  #if (verbose) print(sprintf("[%d] ENTER",i))
                  neighs<-c()
                  line<-dat[i,]
                  if (nchar(line[[ref]]) < 1) {
                    if (verbose) print(sprintf("[%d] Skipping record '%s'",i,line[[title]]))
                    
                    ############
                    #rest<-append(rest,list(list(line$id,NULL)))
                    #next
                    ###########
                    return(list(line$id,NULL))
                  }
                  
                  if (verbose) print(sprintf("[%d] Processing record '%s'",i,line[[title]]))
                  
                  # Find family names of authors (ignoring prefix particles such as "de", "al", "van")
                  fauthors<-NULL
                  if (fine.check.nb.authors > 0) {
                    raw_authors<-strsplit(as.character(line[[authors]]),",")[[1]]
                    for (z in seq(1:min(length(raw_authors),fine.check.nb.authors))) {
                      iter<-raw_authors[[z]]
                      person<-strsplit(iter," ")[[1]]
                      if (length(person) > 1) { 
                        fauthor2<-person[[2]]
                        fauthor1<-person[[1]]
                        if (nchar(fauthor1)<4 & !stringi::stri_detect_fixed(fauthor2,".")) fauthors<-c(fauthors,fauthor2) else fauthors<-c(fauthors,fauthor1)
                      } else fauthors<-c(fauthors,person[[1]])
                    }
                    fauthors<-fauthors[fauthors != ""]
                    if (length(fauthors) == 0) fauthors <- NULL
                  }
                  temp_neighs<-dat[stringi::stri_detect_fixed(dat[[ref]], as.character(line[[title]]), case_insensitive=T), "id"]
                  
                  #if (verbose) print(sprintf("[%d] %d preliminary hits",i,length(temp_neighs)))
                  if (is.null(fauthors) | any(fauthors=="")) {
                    print(sprintf("ERROR: List of authors contains empty fields. List: %s", fauthors))
                  }
                  
                  if (nchar(as.character(line[[title]])) < fine.check.threshold) { 
                    for (entry in temp_neighs) {
                      if (verbose) print(sprintf("[%d] Checking record %d",i,entry))
                      cands<-stringi::stri_detect_fixed(refs[[entry]], as.character(line[[title]]), case_insensitive=T)
                      good<-cands  #OK?
                      cands<-refs[[entry]][cands]
                      
                      if (!is.na(line[[year]])) {
                        good<-stringi::stri_detect_fixed(cands, as.character(line[[year]]), case_insensitive=T)
                        if (small.year.mismatch) {
                          good <- good | stringi::stri_detect_fixed(cands, as.character(as.numeric(line[[year]])-1), case_insensitive=T) | 
                            stringi::stri_detect_fixed(cands, as.character(as.numeric(line[[year]])+1), case_insensitive=T)
                        }
                      }
                      
                      #if (verbose) print(sprintf("[%d] Authors [%s]",i,fauthors))
                      if (!is.null(fauthors)) for (aut in fauthors) {
                        if (!any(good)) break
                        #if (verbose) print(sprintf("[%d] Looking up authors [%s]",i,aut))
                        good<- good & stringi::stri_detect_fixed(cands, as.character(aut), case_insensitive=T)
                      }
                      
                      if (any(good)) neighs<-c(neighs,entry)
                      
                    }
                    cleaned<-length(temp_neighs)-length(neighs)
                    
                    if (verbose) if (cleaned > 0) {
                      print(sprintf("[%d] Cleaned %d out of %d hits",i,cleaned,length(temp_neighs)))
                    }
                  } else neighs<-temp_neighs
                  list(line$id,neighs)
                  ############
                  #rest<-append(rest,list(list(line$id,neighs)))
                }
  parallel::stopCluster(cl)
  cl<-NULL
  
  for (rec in rest) {
    li<-rec[[2]]
    if (length(li)>0) {
      ind<-rec[[1]]
      if (verbose) print(sprintf("Detected %d citations to vertex %s",length(li),ind))
      for (n in li) {
        if (verbose) print(sprintf("Creating edge from %s to %s", as.character(n), as.character(ind)))
        grap<-grap + edges(n,ind)
      }
    }
  }
  
  if (set.title.as.name) grap<-set.vertex.attribute(grap, name="name", index=V(grap), value=db[[title]])
  else grap<-set.vertex.attribute(grap, name="title", index=V(grap), value=db[[title]])
  
  for (at in attrs) grap<-set.vertex.attribute(grap, name=at, index=V(grap), value=db[[at]])
  
  stop<-Sys.time()
  print(sprintf("Graph built! Execution time: %.2f seconds.",stop-start))
  grap  
}

load_graph <- function(filename) {
  read.graph(file=filename, format="graphml")
}

save_graph <- function(gr, filename) {
  write.graph(gr, file=filename, format="graphml")
}

# Algorithm wrong if not computed over whole timerange (see TODO inline). Therefore, should be run over
# complete timerange to be considered. (Not done automatically yet, because -1 values for year should be tackled.)
precompute_heterocitation<-function(gr, labels, infLimitYear, supLimitYear) {
  write_to_graph<-T
  adjlist<-get.adjlist(gr, mode="out")
  authors<-get.vertex.attribute(gr, "Authors")
  attribs<-get.vertex.attribute(gr, "Corpus")
  attribs[grepl("\\|", attribs)]<-paste(labels[2],"|",labels[1])
  attribs<-factor(attribs)
  years<-as.numeric(get.vertex.attribute(gr, "Year"))
  
  problist<-rep(-2,length(adjlist)) #list()
  sameT<-0
  diffT<-0
  sameT1<-sameT2<-diffT1<-diffT2<-nbnodes1<-nbnodes2<-nbnodesall<-0
  results<-data.frame(seq(1,length(attribs)),#get.vertex.attribute(gr, "label"), #get.vertex.attribute(gr, "DOI"), 
                      years, attribs, stringsAsFactors = TRUE)#, authors)
  for (i in seq(1,length(adjlist))) {
    nodeType<-attribs[i]
    nodeYear<-years[i]
    neighs<-unlist(adjlist[i])
    #print(neighs)
    same<-0
    diff<-0
    
    if (!is.null(neighs) & length(neighs) > 0 & (length(years) == 0 || (!is.na(nodeYear) & nodeYear < supLimitYear & nodeYear >= infLimitYear))) {
      nbnodesall<-nbnodesall+1
      if (nodeType == labels[1]) {
        nbnodes1<-nbnodes1+1
      } else if (nodeType == labels[2]) nbnodes2<-nbnodes2+1
      
      if (!grepl("\\|", as.character(nodeType))) {        
        for (j in seq(1:length(neighs))) {
          nd<-neighs[j]
          if (nd != i & (length(years) == 0 || !is.na(years[nd]))){ #& years[nd] < supLimitYear) {
            if (attribs[nd] == nodeType) same=same+1
            else diff=diff+1
          }
        }
      } #else print(paste("Skipping ",as.character(nodeType), " ",length(neighs)))
    }
    
    results[i, "Same"]<-same
    results[i, "Diff"]<-diff
    if (same+diff > 0) {
      if (nodeType == labels[1]) {
        sameT1<-sameT1+same
        diffT1<-diffT1+diff
      } else if (nodeType == labels[2]) {
        sameT2<-sameT2+same
        diffT2<-diffT2+diff
      }
      sameT<-sameT+same
      diffT<-diffT+diff
      problist[i]<-diff/(same+diff)
    } else {
      problist[[i]]<--1
    }
    #if(same>0 & diff>0) print(i)
    V(gr)[i]$Sx<-problist[[i]]
    
  }
  
  problist<-unlist(problist)
  
  print(sprintf("Summary of the nodes considered for computation (%d-%d)",infLimitYear,supLimitYear-1))
  print(        "-------------------------------------------------------")
  print(sprintf("%s     %s     %s|%s", labels[1],labels[2],labels[1],labels[2]))
  print(sprintf("%d     %d     %d",nbnodes1,nbnodes2,nbnodesall-nbnodes1-nbnodes2))
  print("\n")
  print("Edges summary")
  print("-------------")
  print(sprintf("%s->%s/%s->Other: %d/%d => Proportion of %s->Other: %.3f", labels[1], labels[1], labels[1], sameT1, diffT1, labels[1], diffT1/(sameT1+diffT1)))
  print(sprintf("%s->%s/%s->Other: %d/%d => Proportion of %s->Other: %.3f", labels[2],labels[2],labels[2], sameT2, diffT2,labels[2], diffT2/(sameT2+diffT2)))
  print(sprintf("General Same/Other: %d/%d => Proportion of %.3f", sameT, diffT, diffT/(sameT+diffT)))
  print("\n")
  print("Heterocitation metrics")
  print("----------------------")
  #print(problist)
  #table(problist)
  print(paste("Sx ALL / ", labels[1], " / ", labels[2]))
  sx1<-mean(problist[problist>=0 & attribs == labels[1]])
  sx2<-mean(problist[problist>=0 & attribs == labels[2]])
  sxALL<-(nbnodes1*sx1+nbnodes2*sx2)/(nbnodes1+nbnodes2)
  print(sprintf("%.3f / %.3f / %.3f", sxALL, sx1, sx2))
  
  print(paste("Dx ALL / ", labels[1], " / ", labels[2]))
  #allBut1<-((nbnodesall-nbnodes1)/nbnodesall)
  #allBut2<-((nbnodesall-nbnodes2)/nbnodesall)
  joint<-data.frame(attribs, years, stringsAsFactors = TRUE)
  nbLab1<-0
  nbLab2<-0
  nbLabAll<-0
  allBut1<-0
  allBut2<-0
  
  # TODO CORRECT : nb and allBut should be computed over the whole dataset, not the time-restricted one
  # minYearDataset <- min(years)
  for (i in seq(infLimitYear, supLimitYear-1)) {
    ci<-i-infLimitYear+2
    nbLab1[ci]<-length(subset(joint, attribs == labels[1] & years == i, select="attribs")$attribs)+nbLab1[ci-1]
    nbLab2[ci]<-length(subset(joint, attribs == labels[2] & years == i, select="attribs")$attribs)+nbLab2[ci-1]
    nbLabAll[ci]<-length(joint[joint$years == i,"attribs"])+nbLabAll[ci-1]
    allBut1[ci]<-(nbLabAll[ci]-nbLab1[ci])/nbLabAll[ci]
    allBut2[ci]<-(nbLabAll[ci]-nbLab2[ci])/nbLabAll[ci]
  }
  for (i in 1:length(attribs)) {
    if (V(gr)[i]$Sx < 0) {
      V(gr)[i]$Dx <- V(gr)[i]$Sx <- NA
    } else {
      ci<-V(gr)[i]$Year-infLimitYear+2
      if (attribs[i] == labels[1]) {
        if (allBut1[ci] == 0) {
          V(gr)[i]$Dx<-0
        } else V(gr)[i]$Dx<-(V(gr)[i]$Sx-allBut1[ci]) / allBut1[ci]
      } else if (attribs[i] == labels[2]) {
        if (allBut2[ci] == 0) {
          V(gr)[i]$Dx<-0
        } else V(gr)[i]$Dx<-(V(gr)[i]$Sx-allBut2[ci]) / allBut2[ci]
      }
    }
  }
  scoreALL<-mean(V(gr)$Dx, na.rm=T)
  score1<-mean(V(gr)[V(gr)$Corpus==labels[1]]$Dx, na.rm=T)
  score2<-mean(V(gr)[V(gr)$Corpus==labels[2]]$Dx, na.rm=T)

  print(sprintf("%.3f / %.3f / %.3f", scoreALL, score1, score2))
  
  if (write_to_graph) {
    return(gr)
  } else return(c(sx1,sx2,sxALL,score1,score2,scoreALL,allBut1[supLimitYear-1],allBut2[supLimitYear-1],nbLab1[supLimitYear-1],nbLab2[supLimitYear-1],nbLabAll[supLimitYear-1]))
}

heterocitation<- function(gr, labels, infLimitYear, supLimitYear) {
  if (is.null(V(gr)$Sx)) {
    print(sprintf("Error: Please run precompute_heterocitation() on the whole study time range before running heterocitation on a smaller period."))
    return()  
  }
  sg<-induced.subgraph(gr, which(V(gr)$Year >= infLimitYear & V(gr)$Year < supLimitYear))
  sxALL<-mean(V(sg)$Sx, na.rm=T)
  sx1<-mean(V(sg)[V(sg)$Corpus==labels[1]]$Sx, na.rm=T)
  sx2<-mean(V(sg)[V(sg)$Corpus==labels[2]]$Sx, na.rm=T)
  
  scoreALL<-mean(V(sg)$Dx, na.rm=T)
  score1<-mean(V(sg)[V(sg)$Corpus==labels[1]]$Dx, na.rm=T)
  score2<-mean(V(sg)[V(sg)$Corpus==labels[2]]$Dx, na.rm=T)
  
  print(paste("Sx ALL / ", labels[1], " / ", labels[2]))
  print(sprintf("%.3f / %.3f / %.3f", sxALL, sx1, sx2))
  print(paste("Dx ALL / ", labels[1], " / ", labels[2]))
  print(sprintf("%.3f / %.3f / %.3f", scoreALL, score1, score2))
  
  return(c(sx1,sx2,sxALL,score1,score2,scoreALL,0,0,0,0,0))
}

plot_heterocitation_timeseries<-function(gr_arg, labels, mini=-1, maxi=-1, cesure=-1) {
  precompiled<-T
  func<-heterocitation
    
  if (length(which(V(gr_arg)$Year < 0)) > 0) {
    warning("Occurrence of negative years. Ignorning them.")
    gr<-induced.subgraph(gr_arg, which(V(gr_arg)$Year > 0))
  } else gr<-gr_arg
  
  
  years<-as.numeric(get.vertex.attribute(gr, "Year"))
  years<-years[!is.na(years)]
  sx1<-c()
  sx2<-c()
  sxALL<-c()
  dx1<-c()
  dx2<-c()
  dxALL<-c()
  allBut1<-c()
  allBut2<-c()
  yr<-c()
  
  if (mini < 1) realLimInf<-liminf<-min(years)
  else realLimInf<-liminf<-mini
  if (maxi < 1) limsup<-max(years)
  else limsup<-maxi
  
  if (cesure > 0) {
    yr<-cesure#c(sprintf("%d-%d",liminf, cesure))
    ret<-func(gr,labels,liminf,cesure+1)
    sx1<-c(ret[1])
    sx2<-c(ret[2])
    sxALL<-c(ret[3])
    dx1<-c(ret[4])
    dx2<-c(ret[5])
    dxALL<-c(ret[6])
    allBut1<-c(ret[7])
    allBut2<-c(ret[8])
    realLimInf<-cesure+1
    liminf<-cesure
  }
  
  for (i in realLimInf:limsup) {
    yr<-c(yr,i)
    ret<-func(gr,labels, i,i+1)
    sx1<-c(sx1,ret[1])
    sx2<-c(sx2,ret[2])
    sxALL<-c(sxALL,ret[3])
    dx1<-c(dx1,ret[4])
    dx2<-c(dx2,ret[5])
    dxALL<-c(dxALL,ret[6])
    allBut1<-c(allBut1,ret[7])
    allBut2<-c(allBut2,ret[8])
  }
  bc<-data.frame(yr, sx1,sx2,sxALL,dx1,dx2,dxALL, stringsAsFactors = TRUE)#,allBut1,allBut2)
  CEX<-2
  par(mfrow=c(1,2), mar=c(5,6,4,2)+0.1,mgp=c(4,1,0))
  plot(yr, sx1, #xaxt="n", ylim=c(0,0.3), 
       xlim=c(liminf,limsup), 
       xlab = "Year", ylab="Heterocitation share (Sx)", col="red", type="b", lty=2, cex=CEX, cex.lab=CEX*1.2, cex.axis=CEX)
  #axis(1, at=c(2005,2007,2009,2011,2013,2015,2017),labels=c("~2005","2007","2009","2011","2013","2015","2017"), cex.lab=CEX, cex.axis=CEX)
  points(yr, sx2, pch = 23, type="b", col="blue", lty=2, cex=CEX, cex.lab=CEX)
  lines(yr, sxALL, col="gray18", type="b", pch=24, cex=CEX)
  legend("topleft",c(labels[2],labels[1], "ALL"),
         col=c("blue","red", "gray18"),pch=c(1,23,24), cex=CEX)
  plot(yr, dx1,# xaxt="n",ylim=c(-1,-0.4),#ylim=c(-1,-0.5), 
       xlim=c(liminf,limsup), col="red", type="b", xlab = "Year", ylab="Heterocitation balance (Dx)", lty=2, cex=CEX, cex.lab=CEX*1.2, cex.axis=CEX)
  points(yr, dx2, pch = 23, type="b", col="blue", lty=2, cex=CEX, cex.lab=CEX)
  lines(yr, dxALL, col="gray18", type="b", pch=24, cex=CEX, cex.lab=CEX)
  return(bc)
}

plot_publication_curve<-function(gr,labels, k=1) {
  attribs<-get.vertex.attribute(gr, "Corpus")
  attribs[grepl("\\|", attribs)]<-"Both"#paste(labels[2],"|",labels[1])
  attribs<-factor(attribs)
  years<-as.numeric(get.vertex.attribute(gr, "Year"))
  print("Plotting publication trend curve")
  print("-------------")
  IBMCounts<-data.frame(table(years[grepl(labels[1],attribs)]), stringsAsFactors = TRUE)
  ABMCounts<-data.frame(table(years[grepl(labels[2],attribs)]), stringsAsFactors = TRUE)
  BothCounts<-data.frame(table(years[grepl("Both",attribs)]), stringsAsFactors = TRUE)
  ABMCounts$Var1<-as.numeric(levels(ABMCounts$Var1))[ABMCounts$Var1]
  IBMCounts$Var1<-as.numeric(levels(IBMCounts$Var1))[IBMCounts$Var1]
  BothCounts$Var1<-as.numeric(levels(BothCounts$Var1))[BothCounts$Var1]
  dat<-merge(ABMCounts,IBMCounts, by="Var1", all=T)
  dat<-merge(dat,BothCounts, by="Var1", all=T)
  colnames(dat)<-c("Year",labels[2],labels[1],"Both")
  dat[is.na(dat[[labels[1]]]),labels[1]]<-0 #"ABM"
  
  # Clean
  colnames(ABMCounts)<-c("Year","Publications per year")
  colnames(IBMCounts)<-c("Year","Publications per year")
  colnames(BothCounts)<-c("Year","Both")
  dat<-dat[dat$Year > 0,]
  IBMCounts<-IBMCounts[IBMCounts$Year>0,]
  ABMCounts<-ABMCounts[ABMCounts$Year>0,]
  BothCounts<-BothCounts[BothCounts$Year>0,]
  
 # write.csv(dat, "pubtrend.csv")
  
  
  mar.default <- c(5,4,4,2) + 0.1
  par(mar = mar.default + c(0, 4, 0, 0)) 
  plot(ABMCounts[-length(ABMCounts$Year),], #xlim=c(1990,2016.5), 
       type="b", col="blue", cex.lab=1.2*k, cex.axis=1.2*k, ylim=c(0,max(max(ABMCounts[["Publications per year"]]),max(IBMCounts[["Publications per year"]]))),
       xlab="Year",ylab="Publications/Year")
  lines(IBMCounts[-length(IBMCounts$Year),], #xlim=c(1990,2016.5), 
        col="red", type="b", pch=23)
  legend("topleft",c(labels[2],labels[1]),cex=k,
         col=c("blue","red"),pch=c(1,23), bty="n")
  par(mar.default)
  colnames(ABMCounts)<-c("Year",labels[2])
  colnames(IBMCounts)<-c("Year",labels[1])
#  plot(BothCounts[-length(BothCounts$Year),], #xlim=c(1990,2016.5), 
#       type="p", col="blue", cex.lab=1.2*k, cex.axis=1.2*k, ylim=c(0,max(max(BothCounts[["Both"]]),max(BothCounts[["Both"]]))),
#       xlab="Year",ylab="Publications/Year")
#  z<-stats::lm(BothCounts[-length(BothCounts$Year),]$Both ~ BothCounts[-length(BothCounts$Year),]$Year)
#  abline(z)
#  text(locator(), format("y=7.5*x-1.5  R^2=0.95"))
#  print(summary(z))
  dat$shareBoth<-dat$Both/(dat[[labels[1]]]+dat[[labels[2]]]+dat$Both)
  plot(dat$Year, dat$shareBoth, #ylim<-c(0,0.1), xlim=c(2000,2016.5), 
       col="red", type="l", pch=23, main=sprintf("Share of joint %s-%s papers (nb_%s.%s/nb_total)", labels[1], labels[2], labels[1], labels[2]))
  z<-stats::lm(dat$shareBoth ~ dat$Year)
  abline(z)
  print(summary(z))
  
  #plot(residuals(z))
  return(merge(merge(IBMCounts, ABMCounts, all=T), BothCounts, all=T))
}

compute_citation_ranking<-function(gr, labels, write_to_graph=F) {
  if (write_to_graph) {
    V(gr)$Count<-degree(gr,mode="in")
    return(gr)
  }
  attribs<-get.vertex.attribute(gr, "Corpus")
  attribs[grepl("\\|", attribs)]<-paste(labels[2],"|",labels[1])
  attribs<-factor(attribs)
  years<-as.numeric(get.vertex.attribute(gr, "Year"))
  results<-data.frame(get.vertex.attribute(gr, "Authors"), get.vertex.attribute(gr, "title"), #get.vertex.attribute(gr, "DOI"), 
                      years, Corpus=attribs, Count=degree(gr, mode="in"), stringsAsFactors = TRUE)
  results<-results[order(results$Count, decreasing=T),]
  colnames(results)<-c("Authors","Title","Year","Corpus","Count")
  results
}

compute_BC_ranking<-function(gr, labels, write_to_graph=F) {
  if (write_to_graph) {
    V(gr)$BC<-betweenness(gr,directed = T)
    return(gr)
  }
  attribs<-get.vertex.attribute(gr, "Corpus")
  attribs[grepl("\\|", attribs)]<-paste(labels[2],"|",labels[1])
  attribs<-factor(attribs)
  years<-as.numeric(get.vertex.attribute(gr, "Year"))
  results<-data.frame(get.vertex.attribute(gr, "Authors"), get.vertex.attribute(gr, "title"), #get.vertex.attribute(gr, "DOI"), 
                      years, Corpus=attribs, BC=betweenness(gr, directed=T), stringsAsFactors = TRUE)
  results<-results[order(results$BC, decreasing=T),]
  results
}

# TODO: add write_to_graph
compute_Ji_ranking<-function(gr, labels, infLimitYear, supLimitYear, write_to_graph=F){
  # Find most important nodes for fusion
  # i.e. based on citations
  
  adjlist<-get.adjlist(gr, mode="in")
  attribs<-get.vertex.attribute(gr, "Corpus")
  attribs[grepl("\\|", attribs)]<-paste(labels[2],"|",labels[1])
  attribs<-factor(attribs)
  years<-as.numeric(get.vertex.attribute(gr, "Year"))
  
  
  results<-data.frame(get.vertex.attribute(gr, "Authors"), get.vertex.attribute(gr, "title"), #get.vertex.attribute(gr, "DOI"), 
                      years, Corpus=attribs, stringsAsFactors = TRUE)
  for (i in seq(1,length(adjlist))) {
    nodeType<-attribs[i]
    nodeYear<-years[i]
    neighs<-unlist(adjlist[i])
    #print(neighs)
    
    if (!is.null(neighs) & length(neighs) > 0 & (length(years) == 0 || (!is.na(nodeYear) & nodeYear < supLimitYear & nodeYear >= infLimitYear))) {
      
      results[i, labels[1]] <- 0
      results[i, labels[2]] <- 0
      for (j in seq(1:length(neighs))) {
        nd<-neighs[j]
        #print(paste(i," ",attribs[nd],"vs",nodeType))
        if (nd != i & (length(years) == 0 || !is.na(years[nd])) & !grepl("\\|", attribs[nd])) { #& years[nd] < supLimitYear) {
          if (grepl(labels[1], attribs[nd])) results[i, labels[1]] <- results[i, labels[1]] + 1
          else if (grepl(labels[2], attribs[nd])) results[i, labels[2]] <- results[i, labels[2]] + 1
          else { # Both
            results[i, labels[1]] <- results[i, labels[1]] + 1
            results[i, labels[2]] <- results[i, labels[2]] + 1
          } 
        }
      }
      results[i, "min"] <- min(results[i, labels[1]], results[i, labels[2]])
    } 
  }
  
  if (write_to_graph) {
    V(gr)$Ji<-results$min
    return(gr)
  }
  
  res<-results[order(-results$min),]
  colnames(res)<-c("Authors","Title","Year","Corpus",sprintf("Cits from %s",labels[1]),sprintf("Cits from %s", labels[2]),"Ji")
  res
}

# Plot number of authors
plot_authors_count<-function(db) {
  tt<-data.frame(db$Authors,db$Year,db$Corpus, stringsAsFactors = TRUE)
  colnames(tt)<-c("Authors","Year","Corpus")
  tt<-splitstackshape::cSplit(tt,"Authors", sep=",", direction="long")
  data.table::setDF(tt)
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  tt$Authors<-trim(tt$Authors)
  tt<-tt[order(tt$Year),]
  
  # Total, counting each author at most once per year (i.e. for each year, how many different people authored an ACS publication?)
  tot<-tapply(tt$Authors,tt$Year, function(X) length(unique(X)))
  # Total, counting each author only EVER (i.e. for each year, how many new ACS authors have appeared?)
  tot_cumulative<-tt[!duplicated(tt$Authors),]
  tot_cumulative<-tapply(tot_cumulative$Authors,tot_cumulative$Year, length)
  #Very dirty
  final<-merge(data.frame(dimnames(tot),as.numeric(tot)),data.frame(dimnames(tot_cumulative),as.numeric(tot_cumulative), stringsAsFactors = TRUE))
  colnames(final)<-c("Year","Nb authors","New authors")
  final$Year<-as.numeric(unlist(levels(final$Year)))
  plot(final[,1:2], type="b", col="black")
  points(final[,c(1,3)], type="b", col="gray")
  legend("topleft",c("Cumulative count","New authors"),
         col=c("black","gray"),pch=c(1,1), bty="n")
  return(final)
}


# WARNING: Always use "which" to select nodes!! (Not filtering, which generates errors) 
compute_modularity<-function(gr, infLimitYear, supLimitYear) {
  selected<-which(V(gr)$Year < supLimitYear & V(gr)$Year >= infLimitYear & !grepl("\\|",V(gr)$Corpus))
  tt<-induced.subgraph(gr, selected)
  attribs<-get.vertex.attribute(tt, "Corpus")
  com<-as.numeric(factor(attribs))
  modularity(tt, com)
}

# Computes the Newmann modularity but only for the subgraph comprising
# nodes that are not ABM&IBM at the same time, and that are either:
# A. within the time window
# B. direct outgoing neighbors of nodes A (whatever their year tag) 
compute_custom_modularity<-function(gr, infLimitYear, supLimitYear) {
  #core<-V(gr)[Year < supLimitYear & Year >= infLimitYear & !grepl("\\|",Corpus)]
  core<-which(V(gr)$Year < supLimitYear & V(gr)$Year >= infLimitYear & !grepl("\\|",V(gr)$Corpus))
  neighs<-unlist(neighborhood(gr, 1, core, mode="out"))
  all<-c(core,neighs)
  all<-all[!duplicated(all)]
  subg<-induced.subgraph(gr,c(core,neighs))
  
  attribs<-get.vertex.attribute(subg, "Corpus")
  com<-as.numeric(factor(attribs))
  modularity(subg, com)
}

plot_modularity_timeseries<-function(gr_arg, mini=-1, maxi=-1, cesure=-1, window=1, modularity_function="normal") {
  if (modularity_function=="normal") { fun<-compute_modularity
  } else fun<- compute_custom_modularity
  if (length(which(V(gr_arg)$Year < 0)) > 0) {
    warning("Occurrence of negative years. Ignorning them.")
    gr<-induced.subgraph(gr_arg, which(V(gr_arg)$Year > 0))
  } else gr<-gr_arg
  
  years<-as.numeric(get.vertex.attribute(gr, "Year"))
  years<-years[!is.na(years)]
  yr<-c()
  ret<-c()
  
  if (mini < 1) realLimInf<-liminf<-min(years)
  else realLimInf<-liminf<-mini
  if (maxi < 1) limsup<-max(years)
  else limsup<-maxi
  
  if (cesure > 0) {
    yr<-cesure#c(sprintf("%d-%d",liminf, cesure))
    ret<-c(ret,fun(gr,liminf,cesure+1))
    realLimInf<-cesure+1
    liminf<-cesure
  }
  
  for (i in realLimInf:limsup) {
    yr<-c(yr,i)
    ret<-c(ret, fun(gr,i-(window-1),i+1))
  }
  bc<-data.frame(yr, ret, stringsAsFactors = TRUE)
  colnames(bc)<-c("Year","Modularity")
  plot(bc)
  return(bc)
}

# Use only on precompiled dataset !! 
heterocitation_authors<-function(gr, infLimitYear, supLimitYear, pub_threshold=0, remove_orphans=F, remove_citations_to_joint_papers=F) {
  # Returns avgSx, i.e. average the Sx values for publications of a given author, 
  # treat NA record as NA if Corpus == NA to render ABM/IBM joint paper or ignore the record if Corpus != NA (i.e. absence of references in the paper)  
  avgSxFun<-function(sxlist,corpuslist, default_joint=NA) {
    sum<-nb<-0
    for (i in seq(1:length(sxlist))) {
      if (is.na(corpuslist[i])) return(default_joint)
      if (!is.na(sxlist[i])) {
        sum<-sum + sxlist[i]
        nb<-nb +1
      }
    }
    if (nb>0) return(sum/nb)
    NA
  }
  
  if (is.null(V(gr)$Sx)) {
    print(sprintf("Error: Please run precompute_heterocitation() on the whole study time range before running heterocitation_authors on a smaller period."))
    return()
  }
  
  if (remove_orphans) { sg<-induced.subgraph(gr, which(V(gr)$Year >= infLimitYear & V(gr)$Year < supLimitYear & !is.na(V(gr)$Sx)))
  } else sg<-induced.subgraph(gr, which(V(gr)$Year >= infLimitYear & V(gr)$Year < supLimitYear))
  tt<-data.frame(Authors=V(sg)$Authors,Year=V(sg)$Year, Corpus=V(sg)$Corpus, Sx=V(sg)$Sx, Dx=V(sg)$Dx, stringsAsFactors = TRUE)
  tt<-splitstackshape::cSplit(tt,"Authors", sep=",", direction="long")
  data.table::setDF(tt)
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  tt$Authors<-trim(tt$Authors)
  # Rank by year
  tt<-tt[order(tt$Year),]
  print("Will turn Corpus into numerical values depending on levels:")
  print(levels(tt$Corpus))
  tt<-stats::aggregate(tt, by=list(tt$Authors), FUN=c)
  tt$Authors<-NULL
  tt$NbPubs<-lapply(tt$Sx, length)
  tt<-tt[tt$NbPubs >= pub_threshold,]
  #tt$avgSx<-lapply(tt$Sx, mean)  
  colnames(tt)[1]<-"Authors"
  
  for (i in seq(1:length(tt$Authors))) {
    #print(i)
    sx<-unlist(tt[i,"Sx"])
    dx<-unlist(tt[i,"Dx"])
    corpus<-unlist(tt[i,"Corpus"])
    backcorpus<-corpus
    if (remove_citations_to_joint_papers) corpus[corpus==3] <- NA
    else corpus[corpus==3] <- 1.5  #How to consider joint pubs for avgCorpus calc
    year<-unlist(tt[i,"Year"])
    
    tt[i, "nbNoJoints"] <- length(corpus[!is.na(corpus)])
    tt[i, "avgSx"] <- avgSxFun(sx,corpus)
    tt[i, "avgDx"] <- avgSxFun(dx,corpus)
    tt[i,"avgCorpus"] <- mean(corpus, na.omit=T)
    
    if (all(is.na(sx)) || all(is.na(year))) { tt[i,"coeffSx"] <- NA
    } else tt[i,"coeffSx"] <- stats::lm(sx~year,na.action=stats::na.omit)$coeff[2]
    
    if (all(is.na(dx)) || all(is.na(year))) { tt[i,"coeffDx"] <- NA
    } else tt[i,"coeffDx"] <- stats::lm(dx~year,na.action=stats::na.omit)$coeff[2]
    
    corpus <- backcorpus
    corpus[corpus==3] <- 1.5
    if (all(is.na(year))) { tt[i,"coeffCorpus"] <- NA
    } else tt[i,"coeffCorpus"] <- stats::lm(corpus~year,na.action=stats::na.omit)$coeff[2]
    
  }
  
  return(tt)
  
}

# Monte Carlo runs with random reassignment of node corpus to evaluate significance of heterocitation statistics
MC_baseline_distribution <- function(gr, labels, infYearLimit, supYearLimit, rep=20) {
  gr_loc<-gr
  res1<-c()
  res2<-c()
  resALL<-c()
  
  # Subset the graph to exclude orphans and speed up computation
  print(sprintf("NOTE: Removing %d orphans from the graph for increased performance",sum(degree(gr) == 0)))
  gr_loc <- delete.vertices(gr_loc, V(gr_loc)[degree(gr_loc)==0])
  print(paste("NOTE:",length(V(gr_loc)),"nodes left"))
  for(i in 1:rep) {
    attribs<-get.vertex.attribute(gr_loc, "Corpus")
    shuffled<-sample(attribs)
    V(gr_loc)$Corpus<-shuffled
    invisible(gr_loc<-precompute_heterocitation(gr_loc, labels, infYearLimit, supYearLimit))
    invisible(res<-heterocitation(gr_loc, labels, infYearLimit, supYearLimit))
    res1<-c(res1,res[4])
    res2<-c(res2,res[5])
    resALL<-c(resALL,res[6])
    print(sprintf("%f %f %f",res[4],res[5],res[6]))
  }
  hist(res1)
  hist(res2)
  hist(resALL)
  ret<-data.frame(res1, res2, resALL, stringsAsFactors = TRUE)
  colnames(ret)<-c(paste("Dx",labels[1]),paste("Dx",labels[2]), "Dx ALL")
  ret
}

significance_Dx <- function(value, control, normality_threshold=0.05) {
  pv<-NA
  # Test normality
  tn<-stats::shapiro.test(control)
  if (tn$p.value > normality_threshold) {
    print("Distribution is normal. Performing t-test.")
    # p-value calculation
    ttest<-stats::t.test(value-control)
    print(ttest)
    pv<-ttest$p.value
  }
  # Glass' effect size. 
  # See Glass, G.V., McGaw, B. and Smith, M.L. (1981) Meta-Analysis in Social Research. London: Sage.
  glass<-(value-mean(control))/stats::sd(control)
  print(paste("Glass' effect size:",glass))
  return(c(pv,glass))
}

#real_example<-function() {
#   keys<-c("individual-based model|individual based model|individual based\\s*?[;|,]|individual-based\\s*?[;|,]", "agent-based model|agent based model|agent-based\\s*?[;|,]|agent based\\s*?[;|,]") # Includes model/modeling/modelling
#   labels<-c("IBM","ABM")
#   db<-create_bibliography(corpora_files=c("IBMmerged.csv","ABMmerged.csv"), labels=labels, keywords=keys)
  # ### [1] "File IBMmerged.csv contains 3184 records"
  # ### [1] "File ABMmerged.csv contains 9641 records"
  # gr<-build_graph(db=db,small.year.mismatch=T,fine.check.nb.authors=2,attrs=c("Corpus","Year","Authors", "DOI"))
  # ### [1] "Graph built! Execution time: 1200.22 seconds."
  # save_graph(gr, "graph.graphml")
  # 
  # # Compute and plot modularity
  # compute_modularity(gr_sx, 1987, 2018)
  # ###[1] 0.3164805
  # plot_modularity_timeseries(gr_sx, 1987, 2018, window=1000)
  # 
  # # Compute and plot publication heterocitation
  # gr_sx<-precompute_heterocitation(gr,labels=labels,infLimitYear=1987, supLimitYear=2018)
  # ret<-p_value_Dx(gr_sx, labels, 1987, 2018, rep=100)
#   ###[1] "Summary of the nodes considered for computation (1987-2017)"
#   ###[1] "-----------------------------------------------------------"
#   ###[1] "IBM     ABM     IBM|ABM"
#   ###[1] "1928     5378     153"
#   ###[1]
#   ###[1] "Edges summary"
#   ###[1] "-------------"
#   ###[1] "IBM->IBM/IBM->Other 5583/1086 => Prop 0.163"
#   ###[1] "ABM->ABM/ABM->Other 16946/2665 => Prop 0.136"
#   ###[1] "General Same/Diff 22529/3751 => Prop 0.143"
#   ###[1]
#   ###[1] "Heterocitation metrics"
#   ###[1] "----------------------"
#   ###[1] "Sx ALL /  IBM  /  ABM"
#   ###[1] "0.127 / 0.137 / 0.124"
#   ###[1] "Dx ALL /  IBM  /  ABM"
#   ###[1] "-0.652 / -0.803 / -0.598"
#   heterocitation(gr_sx, labels=labels, 1987, 2005)
#   ###[1] "Sx ALL /  ABM  /  IBM"
#   ###[1] "0.047 / 0.214 / 0.007"
#   ###[1] "Dx ALL /  ABM  /  IBM"
#   ###[1] "-0.927 / -0.690 / -0.982"
#   plot_heterocitation_timeseries(gr_sx, labels=labels, mini=-1, maxi=-1, cesure=2005)
# 
#   # Compute author heterocitation
#   hetA<-heterocitation_authors(gr_sx, 1987, 2018, pub_threshold=4)
#   head(hetA[order(hetA$avgDx,decreasing=T),c(1)], n=10)
#   ### [1] "Ashlock D."     "Evora J."       "Hernandez J.J." "Hernandez M."   "Gooch K.J."     "Reinhardt J.W." "Ng K."         
#   ### [8] "Kazanci C."     "Senior A.M."    "Ariel G." 
#   
#   # Try to figure which publication are most impactful in terms of cross-fertilization
#   jir<-compute_Ji_ranking(gr_sx, labels=labels, 1987, 2018)
#   head(jir[,c(2,7)],n=3)
#   ###         Title                                                                           Ji
#   ### 758     A standard protocol for describing individual-based and agent-based models      200
#   ### 4437    Pattern-oriented modeling of agent-based complex systems: Lessons from ecology  134
#   ### 33      The ODD protocol: A review and first update                                     120
#   
# }

# test<-function() {
#   gr<-load_graph("ABM_IBM_2017.13.04.graphml")
#   gr_sx<-precompute_heterocitation(gr,labels=c("ABM","IBM"),infLimitYear=1987, supLimitYear=2018)
#   heterocitation(gr_sx, labels=c("ABM", "IBM"), 1987, 2005)
#   heterocitation(gr_sx, labels=c("ABM", "IBM"), 2005, 2018)
#   plot_heterocitation_timeseries(gr_sx, labels=c("ABM", "IBM"), mini=-1, maxi=-1, cesure=-1)
#   compute_citation_ranking(gr_sx, labels=c("ABM","IBM"))
#   compute_BC_ranking(gr_sx, labels=c("ABM","IBM"))
#   compute_Ji_ranking(gr_sx, labels=c("ABM","IBM"), 1987, 2018)
#   compute_modularity(gr_sx, 1987, 2018)
#   compute_custom_modularity(gr_sx, 1987, 2004)
#   compute_custom_modularity(gr_sx, 2005, 2016)
#   plot_modularity_timeseries(gr_sx, 1987, 2018, window=1000)
#   plot_modularity_timeseries(gr_sx, 1987, 2018, window=5)
#   plot_modularity_timeseries(gr_sx, 1987, 2018, window=1, modularity_function="custom")
#   heterocitation_authors(gr_sx, 1987, 2018, pub_threshold=4)
#   p_value_Dx(gr_sx, labels=c("ABM","IBM"), 1987, 2018)
# }

#package.skeleton(name="Diderot", code_files="Diderot.R")

# test_unit<-function(){
# # Generate corpora
# corp1<-data.frame(Authors=paste("Author",round(runif(45,1,20)),sep=""),Title=paste("Title",seq(1,45), sep=""), Year=round(runif(45,1990,2018)), References=NA,stringsAsFactors=F)
# corp2<-data.frame(Authors=paste("Author",round(runif(65,15,35)),sep=""),Title=paste("Title",seq(46,110), sep=""), Year=round(runif(65,1990,2018)), References=NA, stringsAsFactors=F)
# 
# len1<-length(corp1$Authors)
# len2<-length(corp2$Authors)
# lnall<-len1+len2
# 
# for (i in seq(1:lnall)) {
#   str<-""
#   for (j in seq(1:round(runif(1,1,4)))) {
#     rn<-round(runif(1,1,lnall))
#     if (rn > len1) { 
#       str<-sprintf("%s %s. %d. %s;", str, corp2$Authors[rn-len1], corp2$Year[rn-len1], corp2$Title[rn-len1])
#     } else str<-sprintf("%s %s. %d. %s;", str, corp1$Authors[rn], corp1$Year[rn], corp1$Title[rn])
#   }
#   if (i>len1) {
#     corp2$References[i-len1]<-str
#   } else corp1$References[i]<-str
# }
# 
# # Create joint references
# for (i in seq(1:5)) {
#   corp1<-rbind(corp1, corp2[runif(1,1,len2),])
#   corp2<-rbind(corp2, corp1[runif(1,1,len1),])
# }
# 
# # Add duplicate entry
# corp1<-rbind(corp1, corp1[1,])
# 
# tempfi1<-file.path(tempdir(),"corpus1.csv")
# tempfi2<-file.path(tempdir(),"corpus2.csv")
# write.csv(corp1, tempfi1)
# write.csv(corp2, tempfi2)
# 
# labels<-c("Corpus1","Corpus2")
# 
# # Build a bibliographical dataset from Scopus exports
# db<-create_bibliography(corpora_files=c(tempfi1,tempfi2), 
#                         labels=labels, keywords=NA)
# unlink(c(tempfi1, tempfi2))
# 
# # NB refs
# nb_refs(db)
# 
# # Build graph
# gr<-build_graph(db=db,small.year.mismatch=T, attrs=c("Corpus","Year","Authors"), nb.cores=1)
# save_graph(gr, "tt.graphml")
# unlink("tt.graphml")
# 
# # Publication curve
# plot_publication_curve(gr,labels)
# 
# # Compute BC
# compute_BC_ranking(gr, labels)
# 
# # Compute Citation Ranking
# compute_citation_ranking(gr, labels)
# 
# # Compute Modularity
# compute_modularity(gr, 1990, 2019)
# compute_custom_modularity(gr, 1990, 2019)
# plot_modularity_timeseries(gr, 1990, 2019)
# 
# # Heterocitation
# gr<-precompute_heterocitation(gr,labels, 1990, 2019)
# heterocitation(gr,labels, 1990, 2019)
# plot_heterocitation_timeseries(gr, labels, 1990, 2019)
# 
# # Author heterocitation
# heterocitation_authors(gr, 1990, 2019)
# 
# # Compute Ji
# compute_Ji(db, "Title1", labels, from=1990, to=2019)
# compute_Ji_ranking(gr, labels, 1990, 2019)
# 
# # Get date from DOI
# get_date_from_doi(doi="10.1016/j.procs.2010.04.250",extract_date_from_doi=T)
# }
# 
