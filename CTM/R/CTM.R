#' @title Term Document Matrix
#' @description Constructs Term-Document Matrix from Chinese Text Documents.
#'
#' @param doc The Chinese text document. A vector of Chinese strings.
#' @param weighting Available weighting function with matrix are binary, count, tf, tfidf. See details.
#' @param EngTermDeleted remove English from text documents.
#' @param NumTermDeleted remove Numbers from text documents.
#' @param shortTermDeleted Deltected short word when nchar <2.
#' @author Jim Liu,  Quan Gu
#' @details
#' This function run a Chinese word segmentation by jiebeR and build
#' term-document matrix, and there is four weighting function with matrix, and
#' "binary" means value can only be 1 if the term occurs, "count" means how many times the term occurs in a doc,
#'  "tf" means term frequency and "tfidf" means term frequency inverse document frequency.
#'
#'
#' @export
#' @import jiebaR
#' @import plyr
#' @examples
#' library(CTM)
#' a1 <- "hello taiwan"
#' b1 <- "world of tank"
#' c1 <- "taiwan weather"
#' d1 <- "local weather"
#' text1 <- t(data.frame(a1,b1,c1,d1))
#' tdm1 <- CTDM(doc = text1, weighting = "tfidf", EngTermDeleted = FALSE, shortTermDeleted = FALSE)



CTDM <- function(doc,weighting,EngTermDeleted = TRUE,NumTermDeleted = TRUE, shortTermDeleted = TRUE){
  ###jiebaR
  cutter <- jiebaR::worker()
  dataText <- as.matrix(doc)
  if(EngTermDeleted){dataText <- gsub(pattern ="[A-Za-z]" ,replacement = "",x = dataText)}
  if(NumTermDeleted){dataText <- gsub(pattern ="[0-9]" ,replacement = "",x = dataText)}

  segs <- sapply(dataText,USE.NAMES = F,FUN = function(x){
    temp=(cutter <= x)
    temp[nchar(temp)>0]
  })

  allTerms=unlist(segs)
  lens=sapply(segs,length)                               #count of terms in different id
  allIds=rep(1:length(segs),lens)
  segMatrix <- data.frame(id=allIds,term=allTerms,stringsAsFactors = F)
  ###plyr
  it_freq <- plyr::count(segMatrix,vars = c("id","term"))#count of different terms in each id, "it" means id_terms

  term <- unique(segMatrix$term)
  if(shortTermDeleted){
    term <- term[nchar(term)>=2]
  }

  dtm <- matrix(nrow = length(dataText),ncol = length(term),0)#likely to be sparse matrix, can use Matrix::Matrix

  if(weighting=="binary"){
    for(i in 1:length(term)){
      find1 <- it_freq$id[it_freq$term==term[i]]
      dtm[find1,i] <- 1
    }
  }

  if(weighting=="count"){
    for(i in 1:length(term)){
      find1 <- it_freq[it_freq$term==term[i],c("id","freq")]
      dtm[find1$id,i] <- find1$freq
    }
  }

  if(weighting=="tf"){
    for(i in 1:length(term)){
      find1 <- it_freq[it_freq$term==term[i],c("id","freq")]
      find2 <- lens[find1$id]
      dtm[find1$id,i] <- find1$freq/find2
    }
  }

  if(weighting=="tfidf"){
    n_row=nrow(dtm)
    for(i in 1:length(term)){
      find1 <- it_freq[it_freq$term==term[i],c("id","freq")]
      find2 <- lens[find1$id]
      dtm[find1$id,i] <- find1$freq/find2
      dtm[,i] <- dtm[,i]*log(n_row/(1+nrow(find1)))
    }
  }
  colnames(dtm) <- term
  rownames(dtm) <- paste("doc",1:length(doc))
  tdm=t(dtm)
  tdm
}




#' @title Document Term Matrix
#' @description Constructs Document-Term Matrix from Chinese Text Documents.
#'
#' @param doc The Chinese text document. A vector of Chinese strings.
#' @param weighting Available weighting function with matrix are binary, count, tf, tfidf. See details.
#' @param EngTermDeleted remove English from text documents.
#' @param NumTermDeleted remove Numbers from text documents.
#' @param shortTermDeleted Deltected short word when nchar <2.
#' @author Jim Liu,  Quan Gu
#' @details
#' This function run a Chinese word segmentation by jiebeR and build
#' document-term matrix, and there is four weighting function with matrix, and
#' "binary" means value can only be 1 if the term occurs, "count" means how many times the term occurs in a doc,
#'  "tf" means term frequency and "tfidf" means term frequency inverse document frequency.
#'
#'
#' @export
#' @import jiebaR
#' @import plyr
#' @examples
#' library(CTM)
#' a1 <- "hello taiwan"
#' b1 <- "world of tank"
#' c1 <- "taiwan weather"
#' d1 <- "local weather"
#' text1 <- t(data.frame(a1,b1,c1,d1))
#' dtm1 <- CTDM(doc = text1, weighting = "tfidf",EngTermDeleted = FALSE, shortTermDeleted = FALSE)


CDTM <- function(doc,weighting,EngTermDeleted = TRUE, NumTermDeleted = TRUE, shortTermDeleted =TRUE){
  ###jiebaR
  cutter <- jiebaR::worker()
  dataText <- as.matrix(doc)
  if(EngTermDeleted){dataText <- gsub(pattern ="[A-Za-z]" ,replacement = "",x = dataText)}
  if(NumTermDeleted){dataText <- gsub(pattern ="[0-9]" ,replacement = "",x = dataText)}

  segs <- sapply(dataText,USE.NAMES = F,FUN = function(x){
    temp=(cutter <= x)
    temp[nchar(temp)>0]
  })

  allTerms=unlist(segs)
  lens=sapply(segs,length)                               #count of terms in different id
  allIds=rep(1:length(segs),lens)
  segMatrix <- data.frame(id=allIds,term=allTerms,stringsAsFactors = F)
  ###plyr
  it_freq <- plyr::count(segMatrix,vars = c("id","term"))#count of different terms in each id, "it" means id_terms

  term <- unique(segMatrix$term)
  if(shortTermDeleted){
    term <- term[nchar(term)>=2]
  }

  dtm <- matrix(nrow = length(dataText),ncol = length(term),0)#likely to be sparse matrix, can use Matrix::Matrix

  if(weighting=="binary"){
    for(i in 1:length(term)){
      find1 <- it_freq$id[it_freq$term==term[i]]
      dtm[find1,i] <- 1
    }
  }

  if(weighting=="count"){
    for(i in 1:length(term)){
      find1 <- it_freq[it_freq$term==term[i],c("id","freq")]
      dtm[find1$id,i] <- find1$freq
    }
  }

  if(weighting=="tf"){
    for(i in 1:length(term)){
      find1 <- it_freq[it_freq$term==term[i],c("id","freq")]
      find2 <- lens[find1$id]
      dtm[find1$id,i] <- find1$freq/find2
    }
  }

  if(weighting=="tfidf"){
    n_row=nrow(dtm)
    for(i in 1:length(term)){
      find1 <- it_freq[it_freq$term==term[i],c("id","freq")]
      find2 <- lens[find1$id]
      dtm[find1$id,i] <- find1$freq/find2
      dtm[,i] <- dtm[,i]*log(n_row/(1+nrow(find1)))
    }
  }
  colnames(dtm) <- term
  rownames(dtm) <- paste("doc",1:length(doc))
  dtm
}





#' @title Term Count
#' @description Computing term count from text documents
#'
#' @param doc The Chinese text document.
#' @param EngTermDeleted remove English from text documents.
#' @param NumTermDeleted remove Numbers from text documents.
#' @param shortTermDeleted Deltected short word when nchar <2.
#' @author Jim Liu
#' @details
#' This function run a Chinese word segmentation by jiebeR and
#' compute term count from all these text document.
#'
#'
#' @export
#' @import jiebaR
#' @examples
#' library(CTM)
#' a1 <- "hello taiwan"
#' b1 <- "world of tank"
#' c1 <- "taiwan weather"
#' d1 <- "local weather"
#' text1 <- t(data.frame(a1,b1,c1,d1))
#' count1 <- termCount(doc = text1, EngTermDeleted=FALSE, shortTermDeleted = FALSE)


termCount <- function(doc,EngTermDeleted=TRUE,NumTermDeleted=TRUE,shortTermDeleted=TRUE){
  ###jiebaR
  cutter <- jiebaR::worker()
  cutfunc <- function(s){
    return(cutter <= s)
  }
  dataText <- as.matrix(doc)
  if(EngTermDeleted){dataText <- gsub(pattern ="[A-Za-z]" ,replacement = "",x = dataText)}
  if(NumTermDeleted){dataText <- gsub(pattern ="[0-9]" ,replacement = "",x = dataText)}
  res <- data.frame(table(unlist(lapply(dataText, cutfunc))))
  res[,1] <- as.character(res[,1])
  if(shortTermDeleted){
    res <- res[nchar(res[,1])>1,]
  }
  res <- res[order(-res$Freq),]
  res
}
