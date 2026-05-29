library(xlsx)
library(stopwords)
library(udpipe)
library(PersianStemmer)
library(lattice)
library(shiny)
library(shinythemes)
library(tm)
library(textmineR)
library(tidytext)
library(dplyr)
library(hwordcloud)
library(igraph)
library(stringr)
library(stringi)
library(tidyr)
library(glue)
library(ngram)
library(visNetwork)

utils::globalVariables(c("ID", "value","weight"))

stp0= stopwords::stopwords(language = "fa", source = "stopwords-iso")[-c(1:8)]

stp1<-c('\u0648\u06af\u0631\u0646\u0647',  '\u0627\u0631',  '\u0627\u064a\u0634',  '\u0622\u0646\u06af\u0647',  '\u06af',  '\u0628',  '\u0631',  '\u0632',  '\u0627',  '\u062f\u064a\u062f',  '\u062f\u064a\u062f\u0646',  '\u06af\u0631\u062f',  '\u06af\u0631',  '\u062f\u0631\u064a\u0646',  '\u0645\u0631\u0627',  '\u062a\u0631\u0627',  '\u06a9\u0632\u064a\u0646',  '\u0627\u0632\u064a\u0646',  '\u0646\u0631\u0648',  '\u0628\u0631\u0648',  '\u0632\u064a\u0646',  '\u0647\u0645\u064a',  '\u0628\u0633\u064a',  '\u067e\u0631',  '\u0622\u064a\u062f',  '\u0632\u0627\u0646',  '\u06a9\u0632',  '\u062f\u064a\u062f\u0647',  '\u062f\u064a\u062f\u0646',  '\u0648\u06af\u0631',  '\u0645\u06af\u0631',  '\u0628\u0633\u064a',  '\u062f\u06af\u0631',  '\u062f\u064a\u06af\u0631',  '\u0628\u0631\u0648',  '\u0627\u0644\u0627',  '\u067e\u064a',  '\u0632\u062f',  '\u06a9\u0627\u0646',  '\u06a9\u064a\u0646',  '\u0634\u0648',  '\u0648\u0644\u064a\u06a9\u0646',  '\u0634\u0648',  '\u0648\u0644\u064a\u06a9\u0646',  '\u0646\u0628\u0627\u0634\u062f',  '\u0622\u0646\u0686\u0647',  '\u0627\u0632\u0648',  '\u0627\u0648',  '\u0628\u062f\u064a\u0646',  '\u06af\u0641\u062a\u0627',  '\u0648\u0632',  '\u0645\u062b\u0627\u0644',  '\u0647',  '\u0646',  '\u0627\u0648\u0633\u062a',  '\u0647\u0631\u0686\u0647',  '\u0628\u06af\u0631\u062f')


stp2<-c('\u064a\u06a9\u064a','\u062f\u0627\u0631','\u0622\u0645\u062f','\u0647\u064a\u0686','\u0686\u0646\u064a\u0646','\u0627\u0646\u062f\u0631','\u062f\u0631\u064a\u0646','\u062f\u064a\u062f','\u0622\u064a\u062f','\u0647\u0645\u064a','\u0646\u0628\u0648\u062f','\u0628\u0627\u064a\u062f','\u0686\u0631\u0627','\u0632\u064a\u0646','\u0686\u0646\u0627\u0646','\u0686\u0646\u062f','\u0631\u0641\u062a','\u062f\u0627\u062f','\u06a9\u0633\u064a','\u0628\u0631\u0648','\u0632\u064a\u0631','\u0686\u064a\u0632','\u0628\u0627\u0634','\u062c\u0632','\u0627\u0632','\u0628\u0648\u062f','\u06a9\u0631\u062f','\u0627\u0632\u064a\u0646','\u0645\u06af\u0631','\u0645\u0631\u0627','\u06a9\u0648','\u06a9\u062c\u0627','\u06a9\u0631\u062f\u0647','\u062f\u06af\u0631','\u0628\u0633','\u0648\u06af\u0631','\u06a9\u064a','\u06a9\u0632','\u062a\u0648','\u0634\u0648','\u062f\u064a\u06af\u0631','\u062f\u0631','\u06a9\u0627\u0646','\u0627','\u06a9\u0646\u064a','\u0628\u0627','\u0631','\u0646\u062f\u0627\u0631\u062f','\u0646\u062f\u0627\u0631','\u0622\u0648\u0631\u062f','\u0622\u064a','\u0647\u0633\u062a','\u0627\u0644\u0627','\u06af\u0647','\u0628','\u0628\u06af\u0648','\u0686\u064a\u0633','\u0686\u06af\u0648\u0646\u0647','\u0622\u0646\u06af\u0647','\u0628\u0622\u062e\u0631','\u0628\u0645\u0627\u0646\u062f','\u0648\u0632','\u06af\u0641\u062a\u0627','\u0627\u0648\u0633','\u062f\u0647\u062f','\u06af','\u0627\u064a\u0634','\u0628\u062f\u064a\u0646','\u0645\u064a\u062f','\u0628\u0631',
        "\u06a9\u0646\u062f","\u06a9\u0646","\u0634\u0648\u062f","\u0634\u062f","\u062f\u0627\u0631\u062f","\u06a9\u0631\u062f","\u0628\u0648\u062f","\u0647\u0633\u062a","\u0647\u0633\u062a\u0645",
        "\u0647\u0633\u062a\u06cc\u0645","\u0647\u0633\u062a\u06cc\u062f","\u0647\u0633\u062a\u0646\u062f","\u0628\u0648\u062f\u0645","\u0628\u0648\u062f\u06cc","\u0628\u0648\u062f\u06cc\u0645","\u0628\u0648\u062f\u06cc\u062f",
        "\u0628\u0648\u062f\u06cc\u0645","\u06a9\u0631\u062f\u0645","\u06a9\u0631\u062f\u06cc","\u06a9\u0631\u062f\u06cc\u0645","\u06a9\u0631\u062f\u06cc\u062f","\u06a9\u0631\u062f\u0646\u062f","\u0634\u062f\u0645","\u0634\u062f\u06cc",
        "\u0634\u062f\u06cc\u0645","\u0634\u062f\u06cc\u062f","\u0634\u062f\u0646\u062f","\u062f\u0627\u0631\u0645","\u062f\u0627\u0631\u06cc","\u062f\u0627\u0631\u06cc\u0645","\u062f\u0627\u0631\u06cc\u062f",
        "\u062f\u0627\u0631\u0646\u062f","\u06a9\u0646\u0645","\u06a9\u0646\u06cc","\u06a9\u0646\u06cc\u0645","\u06a9\u0646\u06cc\u062f","\u06a9\u0646\u0646\u062f","\u06a9\u0646\u0646",
        "\u062f\u0627\u0631\u0646","\u0634\u062f\u0646","\u06a9\u0631\u062f\u0646","\u0628\u0648\u062f\u0646","\u0647\u0633\u062a\u0646",
        "\u06cc\u06a9","\u062f\u0648","\u0633\u0647","\u0686\u0647\u0627\u0631","\u067e\u0646\u062c","\u0634\u0634","\u0647\u0641\u062a","\u0647\u0634\u062a","\u0628\u0627\u0634\u0645","\u0628\u0627\u0634\u06cc",
        "\u0628\u0627\u0634\u062f","\u0628\u0627\u0634\u06cc\u0645","\u0628\u0627\u0634\u06cc\u062f","\u0628\u0627\u0634\u0646\u062f","\u062f\u0627\u0634\u062a\u0645","\u062f\u0627\u0634\u062a\u06cc","\u062f\u0627\u0634\u062a",
        "\u062f\u0627\u0634\u062a\u06cc\u0645","\u062f\u0627\u0634\u062a\u06cc\u062f","\u062f\u0627\u0634\u062a\u0646\u062f","\u062d\u062a\u06cc","\u062e\u0648\u062f")


stp=c(stp0,stp1,stp2)


TYPE1=c('\u0647\u0627\u06cc\u0645\u0627\u0646',
        '\u0647\u0627\u06cc\u062a\u0627\u0646',
        '\u0647\u0627\u06cc\u0634\u0627\u0646',
        '\u0647\u0627\u06cc\u0645\u0648\u0646',
        '\u0647\u0627\u06cc\u062a\u0648\u0646',
        '\u0647\u0627\u06cc\u0634\u0648\u0646',
        '\u0647\u0627\u0645\u0648\u0646',
        '\u0647\u0627\u062a\u0648\u0646',
        '\u0647\u0627\u0634\u0648\u0646',
        '\u0647\u0627\u0645\u0627\u0646',
        '\u0647\u0627\u062a\u0627\u0646',
        '\u0647\u0627\u0634\u0627\u0646',
        '\u0647\u0627\u06cc\u0645',
        '\u0647\u0627\u06cc\u062a',
        '\u0647\u0627\u06cc\u0634',
        '\u06cc\u0645\u0627\u0646',
        '\u06cc\u062a\u0627\u0646',
        '\u06cc\u0634\u0627\u0646',
        '\u06cc\u0645\u0648\u0646',
        '\u06cc\u062a\u0648\u0646',
        '\u06cc\u0634\u0648\u0646',
        '\u0647\u0627\u06cc\u06cc',
        '\u0645\u0648\u0646',
        '\u0634\u0648\u0646',
        '\u062a\u0648\u0646',
        '\u0645\u0627\u0646',
        '\u0634\u0627\u0646',
        '\u062a\u0627\u0646',
        '\u0647\u0627\u0645',
        '\u0647\u0627\u062a',
        '\u0647\u0627\u0634',
        '\u06cc\u0645',
        '\u06cc\u062f',
        '\u0646\u062f',
        '\u0627\u0646',
        '\u0627\u062a',
        '\u0627\u0634',
        '\u0627\u0645',
        '\u0647\u0627'
)

TYPE2=paste(' ',TYPE1,sep='')
TYPE.org=c(TYPE2[1:6],TYPE1[1:6],
           TYPE2[7:12],TYPE1[7:12],
           TYPE2[13:22],TYPE1[13:22],
           TYPE2[23:31],TYPE1[23:31],
           TYPE2[32:39],TYPE1[32:39])


left = function(text, num_char) {
  substr(text, 1, num_char)
}

right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}




#' Persian Text Normalization and Stemming
#'
#' This function normalizes Persian text by replacing specific characters
#' and applies stemming.
#'
#' @param x A character vector of Persian text.
#' @return Returns a character vector where each element is the normalized
#' and stemmed version of the corresponding element in the input vector.
#' Specifically, it performs character replacement and stemming on each
#' element of the input, thereby returning a vector of the same length
#' but with processed text. If an element cannot be processed, it will be
#' returned as NA in the output vector.
#' @importFrom stringi stri_replace_all_fixed
#' @importFrom PersianStemmer PerStem
#' @importFrom stats complete.cases
#' @export
#' @examples
#' \dontrun{
#'   text <- c("Persian text here")
#'   normalized_text <- f3(text)
#' }
f3<-function(x){
  X=c()
  x=stri_replace_all_fixed(x,'\u064a','\u06cc')
  L=length(x)
  for(i in 1:L){
    X[i]=PerStem(x[i],NoEnglish=TRUE, NoNumbers= TRUE,
                 NoStopwords=TRUE, NoPunctuation= TRUE,
                 StemVerbs = TRUE, NoPreSuffix= TRUE,
                 Context = TRUE,StemBrokenPlurals=TRUE,
                 Transliteration= F)
  }
  X1=X[complete.cases(X)]
  X1
}

#' Filter Data Frame by Document ID
#'
#' This function filters a data frame by the specified document ID.
#' If the ID is 0, the entire data frame is returned.
#'
#' @param UPIP A data frame with a column named 'doc_id'.
#' @param I An integer representing the document ID.
#' @return Returns a subset of the input data frame (`UPIP`) containing only
#' the rows where the 'doc_id' column matches the specified document ID `I`.
#' If `I` is 0, the function returns the entire data frame unmodified. The
#' output is a data frame with the same structure as the input but potentially
#' fewer rows, depending on the presence and frequency of the specified ID.
#' @export
#' @examples
#' data <- data.frame(doc_id = 1:5, text = letters[1:5])
#' filtered_data <- f5(data, 2)
f5<-function(UPIP,I){
  L=length(as.numeric(names(table(UPIP$doc_id))))
  if(I!=0){H<-UPIP[which(UPIP$doc_id==I),]}else{H<-UPIP}
  H
}


#' Extract Token Information from Data Frame
#'
#' This function extracts token, lemma, and part-of-speech (POS) tag information
#' from a given data frame and compiles them into a new data frame.
#'
#' @param UPIP A data frame containing columns 'token', 'lemma', and 'upos'
#'             for tokens, their lemmatized forms, and POS tags respectively.
#' @return Returns a new data frame with three columns: 'TOKEN', 'LEMMA', and
#' 'TYPE'. 'TOKEN' contains the original tokens from the 'token' column
#' of the input data frame. 'LEMMA' contains the lemmatized forms of
#' these tokens, as provided in the 'lemma' column. 'TYPE' contains POS
#' tags corresponding to each token, as provided in the 'upos' column.
#' The returned data frame has the same number of rows as the input
#' data frame, with each row representing the token, its lemma, and
#' its POS tag from the corresponding row of the input.
#' @export
#' @examples
#' data <- data.frame(token = c("running", "jumps"),
#'                    lemma = c("run", "jump"),
#'                    upos = c("VERB", "VERB"))
#' token_info <- f6(data)
f6<-function(UPIP){
  L=dim(UPIP)[1]
  TOKEN=c()
  LEMMA=c()
  TYPE=c()
  for(j in 1:L){
    TOKEN[j]=UPIP$token[j]
    LEMMA[j]=UPIP$lemma[j]
    TYPE[j]=UPIP$upos[j]}
  x=data.frame(TOKEN,LEMMA,TYPE)
  x
}


#' Extract and Count Specific Parts of Speech
#'
#' This function extracts tokens of a specified part of speech (POS)
#' from the given data frame and counts their frequency.
#'
#' @param UPIP A data frame with columns 'upos' (POS tags) and 'lemma' (lemmatized tokens).
#' @param type A string representing the POS to filter (e.g., 'NOUN', 'VERB').
#' @return Returns a data frame where each row corresponds to a unique lemma
#' of the specified POS type. The data frame has two columns: 'key',
#' which contains the lemma, and 'freq', which contains the frequency
#' count of that lemma in the data. The rows are ordered in decreasing
#' frequency of occurrence. This format is useful for quickly
#' identifying the most common terms of a particular POS in the data.
#' @importFrom udpipe txt_freq
#' @export
#' @examples
#' data <- data.frame(upos = c('NOUN', 'VERB'), lemma = c('house', 'run'))
#' noun_freq <- f7(data, 'NOUN')
f7<-function(UPIP,type){
  noune<-UPIP[UPIP$upos %in% "NOUN",]
  adp<-UPIP[UPIP$upos %in% "ADP",]
  adj<-UPIP[UPIP$upos %in% "ADJ",]
  verb<-UPIP[UPIP$upos %in% "VERB",]
  pron<-UPIP[UPIP$upos %in% "PRON",]
  adv<-UPIP[UPIP$upos %in% "ADV",]
  sconj<-UPIP[UPIP$upos %in% "SCONJ",]
  cconj<-UPIP[UPIP$upos %in% "CCONJ",]
  aux<-UPIP[UPIP$upos %in% "AUX",]
  det<-UPIP[UPIP$upos %in% "DET",]
  num<-UPIP[UPIP$upos %in% "NUM",]
  part<-UPIP[UPIP$upos %in% "PART",]
  intj<-UPIP[UPIP$upos %in% "INTJ",]
  if(type=='NOUN'){out.m=noune}
  if(type=='ADP'){out.m=adp}
  if(type=='ADJ'){out.m=adj}
  if(type=='VERB'){out.m=verb}
  if(type=='PRON'){out.m=pron}
  if(type=='ADV'){out.m=adv}
  if(type=='SCONJ'){out.m=sconj}
  if(type=='CCONJ'){out.m=cconj}
  if(type=='AUX'){out.m=aux}
  if(type=='DET'){out.m=det}
  if(type=='NUM'){out.m=num}
  if(type=='PART'){out.m=part}
  if(type=='INTJ'){out.m=intj}
  stats<- txt_freq(out.m$lemma)
  stats$key <- factor(stats$key,levels = rev(stats$key))
  stats
}


#' Convert to Data Frame
#'
#' This function converts the given object to a data frame.
#'
#' @param x An object to be converted into a data frame.
#' @return Returns a data frame with rows and columns corresponding to the
#' original object's structure. If `x` is a matrix, each column in the matrix
#' becomes a column in the data frame. If `x` is a list where all elements
#' are of the same length, each element of the list becomes a column in the
#' data frame. Attributes such as rownames, colnames, and dimnames (if any)
#' are preserved in the conversion.
#' @export
#' @examples
#' data <- ASDATA.FRAME(matrix(1:4, ncol = 2))
ASDATA.FRAME<-function(x){
  A=as.data.frame(x)
  A
}


#' Persian Suffix Modification
#'
#' This function modifies Persian words ending with 'Persian text here' suffix.
#'
#' @param v A character vector of Persian words.
#' @return Returns a character vector where each element corresponds to a word
#' from the input vector `v` with the specified suffix modified.
#' This results in a transformed vector where each word ending with
#' the specified suffix is altered. The length of the returned vector
#' matches the length of the input vector, and each word is modified
#' independently based on the presence of the specified suffix.
#' @export
#' @examples
#' \dontrun{
#'   words <- c("Persian text here")
#'   modified_words <- fungi(words)
#' }
fungi<-function(v){
  V=c()
  X=c()
  Y=unlist(strsplit(v,' '))
  for(i in 1:length(Y)){
    if(right(Y[i],2)=='\u06af\u06cc' && length(unlist(strsplit(Y[i],""))[-c(1,2)])>=2){
      X[i]=paste0(c(unlist(strsplit(substr(Y[i],1,nchar(Y[i])-2),"")),'\u0647'),
                  collapse = '')}else{X[i]=Y[i]}
  }
  paste0(X,collapse=' ')
}


#' Persian Suffix Modification for 'Persian text here' Suffix
#'
#' This function modifies Persian words ending with 'Persian text here' suffix.
#'
#' @param v A character vector of Persian words.
#' @return Returns a character vector where each element corresponds to a word
#' from the input vector `v` with the 'Persian text here' suffix modified.
#' This results in a transformed vector where each word ending with
#' the specified suffix is altered. The length of the returned vector
#' matches the length of the input vector, and each word is modified
#' independently based on the presence of the specified suffix.
#' @export
#' @examples
#' \dontrun{
#'   words <- c("Persian text here")
#'   modified_words <- fungan(words)
#' }
fungan<-function(v){
  V=c()
  X=c()
  Y=unlist(strsplit(v,' '))
  for(i in 1:length(Y)){
    if(right(Y[i],3)=='\u06af\u0627\u0646' && length(unlist(strsplit(Y[i],""))[-c(1,2,3)])>=2){
      X[i]=paste0(c(unlist(strsplit(substr(Y[i],1,nchar(Y[i])-3),"")),'\u0647'),
                  collapse = '')}else{X[i]=Y[i]}
  }
  paste0(X,collapse=' ')
}


#' Modify Persian Words Starting with 'Persian text here'
#'
#' This function modifies Persian words starting with the prefix 'Persian text here'.
#'
#' @param v A character vector of Persian words.
#' @return Returns a character vector where each element corresponds to a word
#' from the input vector `v` with the specified suffix modified.
#' This results in a transformed vector where each word ending with
#' the specified suffix is altered. The length of the returned vector
#' matches the length of the input vector, and each word is modified
#' independently based on the presence of the specified suffix.
#' @export
#' @examples
#'  \dontrun{
#'   words <- c("Persian text here")
#'   modified_words <- funmi(words)
#' }
funmi<-function(v){
  X=c()
  Y=unlist(strsplit(v,' '))
  for(i in 1:length(Y)){
    if(left(Y[i],2)=='\u0645\u06cc' && (length(unlist(strsplit(Y[i],""))[-c(1,2)])>=2 ||
                              length(unlist(strsplit(Y[i],""))[-c(1,2)])==0)){
      X[i]=paste0(c(unlist(strsplit(substr(Y[i],3,nchar(Y[i])),""))),
                  collapse = '')}else{X[i]=Y[i]}
  }
  paste0(X,collapse=' ')
}


#' General Persian Suffix Modification
#'
#' This function modifies Persian words based on a specified suffix type.
#'
#' @param v A character vector of Persian words.
#' @param type A character string representing the suffix type.
#' @return Returns a character vector where each element corresponds to a word
#' from the input vector `v` with the specified suffix type modified.
#' This results in a transformed vector where each word has been modified
#' to remove or alter the specified suffix. The length of the returned
#' vector matches the length of the input vector, and each word is
#' modified independently based on the specified suffix type.
#' @export
#' @examples
#' \dontrun{
#'   words <- c("Persian text here")
#'   modified_words <- fun.one.sums(words, "Persian text here")
#' }
fun.one.sums<-function(v,type){
  X=c()
  l=length(unlist(strsplit(type,'')))
  Y=unlist(strsplit(v,' '))
  for(i in 1:length(Y)){
    if(right(Y[i],l)==type && (length(unlist(strsplit(Y[i],""))[-c(1:l)])>=3 ||
                               length(unlist(strsplit(Y[i],""))[-c(1:l)])==0)){
      X[i]=paste0(c(unlist(strsplit(substr(Y[i],1,nchar(Y[i])-l),""))),
                  collapse = '')}else{X[i]=Y[i]}
  }
  paste0(X,collapse=' ')
}


#' Apply Suffix Modifications to Persian Words
#'
#' This function iteratively applies a series of suffix modifications to a vector of Persian words.
#'
#' @param v A character vector of Persian words.
#' @param TYPE A vector of suffix types for modification.
#' @return Returns a character vector where each element corresponds to a word
#' from the input vector `v` with all specified suffix modifications applied.
#' This results in a transformed vector where each word has been modified
#' according to the series of suffix types provided in `TYPE`. The length
#' of the returned vector matches the length of the input vector.
#' @export
#' @examples
#' \dontrun{
#'   words <- c("Persian text here")
#'   modified_words <- fun.all.sums(words, TYPE)
#' }
fun.all.sums=function(v,TYPE=TYPE.org){
  v.cur=v
  for(i in 1:length(TYPE)){
    v.up<-fun.one.sums(v.cur,TYPE[i])
    v.cur<-v.up
  }
  v.up
}


#' Persian Lemmatization
#'
#' This function performs lemmatization on a vector of Persian words.
#'
#' @param Y A character vector of Persian words.
#' @param TYPE A vector of suffix types for modification.
#' @return Returns a character vector where each element is the lemmatized
#' form of the corresponding element in the input vector `Y`.
#' Lemmatization involves removing inflectional endings and returning
#' the word to its base or dictionary form. The length of the returned
#' vector matches the length of the input vector, and each word is
#' lemmatized independently based on the specified suffix types in `TYPE`.
#' @export
#' @examples
#' \dontrun{
#'   words <- c("Persian text here")
#'   lemmatized_words <- LEMMA(words, TYPE)
#' }
LEMMA<-function(Y,TYPE=TYPE.org){
  Y1=c()
  for(i in 1:length(Y)){
    y1<-fungan(Y[i])
    y2<-fungi(y1)
    y3<-fun.all.sums(y2,TYPE)
    Y1[i]<-funmi(y3)
  }
  Y2<-Y1[complete.cases(Y1)]
  Y2
}


#' Calculate Pointwise Mutual Information (PMI)
#'
#' This function calculates the PMI for collocations in a given text data.
#'
#' @param x A data frame with columns 'token' and 'doc_id'.
#' @return Returns a data frame where each row represents a unique keyword
#' (collocation) in the input data. The data frame contains columns
#' such as 'keyword', representing the keyword, and 'pmi', representing
#' the PMI score of that keyword. Higher PMI scores indicate a stronger
#' association between the components of the collocation within the corpus.
#' @importFrom udpipe keywords_collocation
#' @export
#' @examples
#' data <- data.frame(token = c("word1", "word2"), doc_id = c(1, 1))
#' pmi_scores <- PMI(data)
PMI<-function(x){
  x$word <- tolower(x$token)
  st1 <- keywords_collocation(x = x,term="word",group="doc_id")
  st1$key <- factor(st1$keyword,levels =rev(st1$keyword))
  st1
}

#' Extract Bigram Information and Count Frequency
#'
#' This function processes a data frame containing bigrams and their frequency,
#' and creates a new data frame with separated words and their frequencies.
#'
#' @param tf.bigrams A data frame with bigram terms and their frequency.
#' @return A tibble data frame where each row represents a unique bigram from
#' the input data. The data frame contains three columns: 'word1' and
#' 'word2' representing the individual words in the bigram, and 'weight'
#' representing the frequency of the bigram in the corpus. This structure
#' facilitates further analysis of the bigram relationships and their
#' occurrences.
#' @export
#' @importFrom stringr str_split
#' @examples
#' tf_bigrams <- data.frame(term = c("hello_world", "shiny_app"),
#'                          term_freq = c(3, 2))
#' bigram_info <- FUNbigrams(tf_bigrams)
FUNbigrams <- function(tf.bigrams){
  bigrams<-tf.bigrams$term
  word1<-as.vector(sapply(bigrams,
                          function(x) unlist(str_split(x,'_'))[1]))
  word2<-as.vector(sapply(bigrams,
                          function(x) unlist(str_split(x,'_'))[2]))
  weight <- tf.bigrams$term_freq
  bi.gram.count <- tibble(word1,word2,weight)
  bi.gram.count
}


#' Scale a Numeric Vector
#'
#' This function scales a numeric vector by a specified lambda value.
#'
#' @param x A numeric vector.
#' @param lambda A numeric scaling factor.
#' @return A numeric vector where each element of the input vector 'x' is
#' divided by the scaling factor 'lambda'. This results in a scaled
#' version of the input vector.
#' @export
#' @examples
#' scaled_vector <- ScaleWeight(1:10, 2)
ScaleWeight <- function(x, lambda) {
  x / lambda
}

#' Set Graph Attributes
#'
#' This function sets various attributes for a given graph object, including
#' vertex degree and edge width.
#'
#' @param network A graph object.
#' @return The input graph object with added attributes: 'degree' for each vertex
#' and 'width' for each edge. These attributes enhance the graph's
#' visual representation and analytical capabilities.
#' @export
#' @importFrom igraph E<-
set.graph<-function(network){
  V(network)$degree <- strength(graph = network)
  E(network)$width <- E(network)$weight/max(E(network)$weight)
  network
}


#' Cluster a Graph and Extract Largest Component
#'
#' This function applies clustering to a graph and extracts the largest
#' connected component.
#'
#' @param network A graph object.
#' @return A list containing three elements: 'gr' with the largest connected
#' component of the graph, 'cl' with a data frame of nodes and their
#' cluster membership, and 'node.impo' with a data frame of node
#' importance measures like degree, closeness, and betweenness.
#' @export
#' @importFrom igraph clusters
#' @importFrom igraph V
#' @importFrom igraph V<-
#' @importFrom igraph induced_subgraph
#' @importFrom igraph strength
#' @importFrom igraph closeness
#' @importFrom igraph betweenness
#' @examples
#' \dontrun{
#'   # Assuming 'network' is a predefined graph object
#'   cluster.graph(network)
#' }
cluster.graph<-function(network){
  clusters(graph = network)
  # Select biggest connected component.
  V(network)$cluster <- clusters(graph = network)$membership
  cc.network <- induced_subgraph(
    graph = network,
    vids = which(V(network)$cluster == which.max(clusters(graph = network)$csize))
  )
  NOD<-names( clusters(graph = network)$membership)
  number.cluster<-as.vector( clusters(graph = network)$membership)
  cl<-data.frame(NOD,number.cluster)
  cl <- cl[order(cl$number.cluster),]

  node.impo.df <- tibble(
    word = V(cc.network)$name,
    degree = strength(graph = cc.network),
    closeness = closeness(graph = cc.network),
    betweenness = betweenness(graph = cc.network)
  )
  list('gr'=cc.network,'cl'=cl,'node.impo'=node.impo.df)
}


#' Plot Community Detection in a Graph
#'
#' This function applies community detection to a graph and plots the result.
#'
#' @param network A graph object.
#' @return A plot visualizing the graph with nodes colored according to their
#' community membership. The plot also displays the modularity score
#' as a sub-title, indicating the strength of the community structure.
#' @export
#' @importFrom igraph layout_with_fr
#' @importFrom igraph modularity
#' @importFrom igraph modularity_matrix
#' @importFrom glue glue
#' @examples
#' \dontrun{
#'   # Assuming 'network' is a predefined graph object
#'   # network <- make_graph("Zachary")
#'   Community.Detection.Plot(network)
#' }
Community.Detection.Plot <- function(network){
  coords = layout_with_fr(network)
  comm.det.obj=cluster_louvain(graph=network,
                               weights=E(network)$weight)
  modul= modularity(comm.det.obj)
  modul.matrix=modularity_matrix(network,membership(comm.det.obj))[1,]
  plot(network,
       vertex.color=membership(comm.det.obj),
       layout=coords,
       sub= glue('modularity: {modul}'))
}

#' Get Community Membership of a Graph
#'
#' This function applies community detection to a graph and returns the
#' membership information of each node.
#'
#' @param network A graph object.
#' @return A data frame where each row represents a node in the graph,
#' with columns for the node name and its corresponding community
#' membership number. This information is useful for understanding
#' the community structure within the graph.
#' @export
#' @importFrom igraph cluster_louvain
#' @importFrom igraph E
#' @importFrom igraph membership
#' @examples
#' \dontrun{
#'   network <- make_graph("Zachary")
#'   membership_info <- Community.Detection.Membership(network)
#'   print(membership_info)
#' }
Community.Detection.Membership <- function(network){
  coords = layout_with_fr(network)
  comm.det.obj=cluster_louvain(graph=network,
                               weights=E(network)$weight)
  nod<-names(membership(comm.det.obj))
  number.cluster<-as.vector(membership(comm.det.obj))
  membership.Community<-data.frame(nod,number.cluster)
  membership.Community[order(membership.Community$number.cluster,
                             decreasing = F),]
}


#' Create and Plot a Correlation Network
#'
#' This function creates a correlation network based on specified terms
#' and a threshold, and optionally plots it.
#'
#' @param dt A document-term matrix.
#' @param Terms A vector of terms to check for correlation.
#' @param threshold A numeric threshold for correlation.
#' @param pl A logical value to plot the network or not.
#' @return If 'pl' is TRUE, a plot of the correlation network is displayed,
#' highlighting the strength of associations between terms. If 'pl' is FALSE,
#' a data frame with correlation pairs and their corresponding weights is returned.
#' @export
#' @importFrom tm findAssocs
#' @importFrom igraph graph_from_data_frame
network.cor<-function(dt,Terms,threshold=.4,pl=TRUE){
  A=findAssocs(dt,terms = Terms,corlimit =threshold)
  a1=sapply(A,length)
  a2=as.vector(which(a1==0))
  if(length(a2)==0){A=A}else{A=A[-a2]}
  Terms=names(A)
  d.frame=data.frame('item1'=0,'item2'=0,'weight'=0)
  for(i in 1:length(Terms)){
    B=data.frame('item1'=NA,'item2'=NA,'weight'=A[[i]])
    B$item1=rep(Terms[i],dim(B)[1])
    B$item2=row.names(B)
    row.names(B)<-NULL
    d.frame=rbind(d.frame,B)
  }
  d.frame=d.frame[-1,]
  network=graph_from_data_frame(d.frame,directed = FALSE)
  networ.new<-set.graph(network)
  if(pl==TRUE){
    plot(
      networ.new,
      vertex.size = 1,
      vertex.label.color = 'blue',
      vertex.label.cex = 1,
      vertex.label.dist = 1,
      edge.color = 'gray',
      main = 'correlation Network',
      sub = glue('correlation Threshold: {threshold}'),
      alpha = 50)
  }else{d.frame}
}


#' User Interface for MadanText
#'
#' This function creates a user interface for the MadanText Shiny application.
#' It includes various input and output widgets for file uploads, text input,
#' and visualization.
#'
#' @return A Shiny UI object.
ui<-fluidPage(
  shinythemes::themeSelector(),
  titlePanel("MadanText"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1","Choose excel file",accept=c(".xlsx")),
      fileInput("file2","enter excel afinn",
                accept=c(".xlsx")),
      fileInput("file3","enter excel con1",
                accept=c(".xlsx")),
      fileInput("file4","enter excel con2",
                accept=c(".xlsx")),
      selectInput("horizontalInput","horizontal",
                  choices =c(TRUE,FALSE),selected = FALSE )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Document Lengths",
                 h1("Summary of Document Lengths"),
                 tableOutput("summary")),
        tabPanel("Token",
                 h1("Most Frequent of Tokens"),
                 br(),br(),
                 numericInput("MAX","MAX Frequancy",min=0,max=2000,
                              value = 20,step=5,width='30%'),
                 numericInput("X",
                              "Remove Any Tokens That Were In X Or Fewer Documents",
                              min=0,max=200,value = 0,width='30%'),
                 numericInput("R1","Remove Any Token You Desire",
                              min=0,max=20,value = 0,width='30%'),
                 dataTableOutput("tokenfrequent"),
                 selectizeInput(
                   "vec1"
                   , "Enter the vector you want to delete"
                   , choices = NULL
                   , multiple = TRUE
                   , options = list(create = TRUE),width='30%'
                 ),
                 selectizeInput(
                   "vec2"
                   , "Enter the old words vector"
                   , choices = NULL
                   , multiple = TRUE
                   , options = list(create = TRUE),width='30%'
                 ),
                 selectizeInput(
                   "vec3"
                   , "Enter the new words vector"
                   , choices = NULL
                   , multiple = TRUE
                   , options = list(create = TRUE),width='30%'
                 ),
                 downloadButton('download1',"Download the data EXCEL"),
                 downloadButton('download2',"Download the data CSV"),
                 br(),br(),
                 br(),br(),
                 h1("Bar Plot of Tokens"),
                 plotOutput("plot1"),
                 br(),br(),
                 h1("Bar Plot of Tokens on Documents "),
                 plotOutput("plot2"),
                 br(),br(),
                 h1("Word Cloud Token"),
                 br(),br(),
                 numericInput("Ncloud",
                              "MAX Frequancy for Word Cloud of Token",
                              min=20,max=1000,value=100,
                              step=10,width='30%'),
                 br(),br(),
                 hwordcloudOutput("shinytest", height = "500px"
                                  ,width="100%")
        ),
        tabPanel("Bigram",
                 h1("Most Frequent of Bigram"),
                 br(),br(),
                 numericInput("R2","Remove Any Bigram You Desire",
                              min=0,max=20,value = 0,width='30%'),
                 dataTableOutput("bigramfrequent"),
                 downloadButton('download3',"Download the data EXCEL"),
                 downloadButton('download4',"Download the data CSV"),
                 br(),br(),
                 h1("Bar Plot of Bigram"),
                 plotOutput("plot3"),
                 br(),br(),
                 h1("Bar Plot of Bigram on Documents "),
                 plotOutput("plot4"),
                 br(),br(),
                 h1("Word Cloud Bigram"),
                 br(),br(),
                 numericInput("Ncloud1",
                              "MAX Frequancy for Word Cloud of Bigram",
                              min=20,max=1000,value=100,
                              step=10,width='30%'),
                 br(),br(),
                 hwordcloudOutput("shinytest1", height = "500px"
                                  ,width="100%")
        ),
        tabPanel('NETWORK',
                 h1('EDG OF NETWORK'),
                 br(),br(),
                 numericInput("threshold",
                              "Remove co-occurrences less than the threshold value",
                              min=0,max=1000,value = 3,step=1,width='30%'),
                 dataTableOutput("table1.net"),
                 downloadButton('download1gr',"Download the data EXCEL"),
                 downloadButton('download2gr',"Download the data CSV"),
                 br(),br(),
                 h1('PLOT OF NETWORK'),
                 br(),br(),
                 plotOutput("plot1.net",height = "1000px"
                            ,width="100%"),
                 br(),br(),
                 h1('Connected components of a graph'),
                 br(),br(),
                 dataTableOutput("table2.net"),
                 downloadButton('download3gr',"Download the data EXCEL"),
                 downloadButton('download4gr',"Download the data CSV"),
                 br(),br(),
                 h1('plot Connected components'),
                 plotOutput("plot2.net",height = "1000px"
                            ,width="100%"),
                 br(),br(),
                 h1('Node Importance'),
                 br(),br(),
                 selectInput("Importance","Node Importance",
                             choices =c('degree','closeness','betweenness'),
                             selected = 'degree',width='30%' ),
                 dataTableOutput("table3.net"),
                 downloadButton('download6gr',"Download the data CSV"),
                 br(),br(),
                 h1('Community Detection'),
                 br(),br(),
                 h2('PLOT of Community Detection'),
                 br(),br(),
                 plotOutput("plot3.net",height = "1000px"
                            ,width="100%"),
                 br(),br(),
                 h2('membership of Community'),
                 br(),br(),
                 dataTableOutput("table4.net"),
                 downloadButton('download7gr',"Download the data EXCEL"),
                 downloadButton('download8gr',"Download the data CSV"),
                 br(),br(),
                 h2('Correlation two Words '),
                 br(),br(),
                 numericInput("thresholds.cor",
                              "Remove correlation less than the threshold value",
                              min=.1,max=1,value=.3,step=.1,width='30%'),
                 selectizeInput(
                   "Terms.cor"
                   , "Enter word for correlation"
                   , choices = NULL
                   , multiple = TRUE
                   , options = list(create = TRUE)
                   , width='30%'
                 ),
                 dataTableOutput("table5.net"),
                 downloadButton('download9gr',"Download the data EXCEL"),
                 downloadButton('download10gr',"Download the data CSV"),
                 br(),br(),
                 h2('PLOT of network  Correlation'),
                 br(),br(),
                 plotOutput("plot4.net",height = "1000px"
                            ,width="100%")
        ),
        tabPanel("persian stemmer and pos tagger",
                 h1('Table of frequancy of key (noun, verb,..)'),
                 br(),br(),
                 selectInput("poetry.poss","number poetry",
                             choices =c(0:500),selected = 1,width='30%'),
                 selectInput("type.poss"," SELECT key",
                             choices =c('NOUN','ADP','ADJ','VERB',
                                        'PRON','ADV','SCONJ','CCONJ',
                                        'AUX','DET','NUM','PART','INTJ'),
                             selected ='NOUN',width='30%'),
                 numericInput("MAX.poss","MAX Frequancy of key",
                              min=0,max=2000,value = 20,
                              step=5,width='30%'),
                 dataTableOutput("table1.poss"),
                 downloadButton('download8',"Download the data EXCEL"),
                 downloadButton('download9',"Download the data CSV"),
                 br(),br(),
                 h1('Bar chart of frequancy of key (noun, verb,..)'),
                 plotOutput("plot1.poss"),
                 br(),br(),
                 h1('Frequency table of each key (noun, verb,..)'),
                 br(),br(),
                 downloadButton('download10',"Download the data EXCEL"),
                 br(),br(),
                 downloadButton('download11',"Download the data CSV"),
                 dataTableOutput("table2.poss"),
                 br(),br(),
                 h1('Bar chart of Frequency of each key (noun, verb,..)'),
                 plotOutput("plot2.poss"),
                 br(),br(),
                 h1('cloud word plot of each key (noun, verb,..)'),
                 hwordcloudOutput("plot3.poss", height = "500px"
                                  ,width="100%"),
                 br(),br(),
                 h1(' stemmer and pos tagging (seraji algoritm)'),
                 br(),br(),
                 downloadButton('download12',"Download the data EXCEL"),
                 br(),br(),
                 downloadButton('download13',"Download the data CSV"),
                 br(),br(),
                 dataTableOutput("taggin.poss")
        )
      )
    )
  )
)


#' Server Logic for MadanText Shiny Application
#'
#' This function contains the server-side logic for the MadanText application.
#' It handles user inputs, processes data, and creates outputs to be displayed
#' in the UI.
#'
#' @param input List of Shiny inputs.
#' @param output List of Shiny outputs.
#' @importFrom shiny reactive renderTable renderPlot renderDataTable downloadHandler
#' @importFrom dplyr %>%
#' @importFrom stringi stri_replace_all_fixed
#' @importFrom xlsx read.xlsx
#' @importFrom tm removeWords
#' @importFrom tm as.TermDocumentMatrix
#' @importFrom tm weightTf
#' @importFrom dplyr tibble
#' @importFrom dplyr inner_join
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom tidyr replace_na
#' @importFrom textmineR CreateDtm
#' @importFrom textmineR TermDocFreq
#' @importFrom utils head
#' @importFrom graphics barplot
#' @importFrom hwordcloud renderHwordcloud
#' @importFrom hwordcloud hwordcloud
#' @importFrom udpipe udpipe_download_model
#' @importFrom udpipe udpipe_load_model
#' @importFrom udpipe udpipe_annotate
#' @importFrom lattice barchart
#' @importFrom tidytext unnest_tokens
#' @importFrom utils write.csv
#' @importFrom visNetwork toVisNetworkData
#' @importFrom xlsx write.xlsx
#' @return This function sets up the reactive environment and output elements in
#' the Shiny application. It does not return any value but modifies the Shiny app's
#' UI based on user inputs and reactive expressions. It returns a Shiny Server object.
#' @export
server<-function(input,output){
  options(shiny.maxRequestSize=30*1024^2)

  text12 <- reactive({
    inFile1 <- input$file1
    if (is.null(inFile1))
      return(NULL)
    tbl <- read.xlsx(inFile1$datapath,sheetIndex=1,header=T,
                     stringsAsFactors = FALSE,encoding="UTF-8")
    return(tbl)
  })
  AFINN<-reactive({
    inFile2 <- input$file2
    if (is.null(inFile2))
      return(NULL)
    afinn<-read.xlsx(inFile2$datapath,sheetIndex=1,header=T,
                     stringsAsFactors = FALSE,encoding="UTF-8")
    return(afinn)
  })
  CON1<-reactive({
    inFile3 <- input$file3
    if (is.null(inFile3))
      return(NULL)
    con1<-read.xlsx(inFile3$datapath,sheetIndex=1,header=T,
                    stringsAsFactors = FALSE,encoding="UTF-8")
    return(con1)
  })
  CON2<-reactive({
    inFile4 <- input$file4
    if (is.null(inFile4))
      return(NULL)
    con2<-read.xlsx(inFile4$datapath,sheetIndex=1,header=T,
                    stringsAsFactors = FALSE,encoding="UTF-8")
    return(con2)
  })
  text<-reactive({
    text12()[,2]
  })
  tweetID<-reactive({
    1:length(text())
  })
  TweetText1<-reactive({
    stri_replace_all_fixed(text(),CON1()[,1],CON1()[,2],
                           vectorize_all=F)
  })
  TweetText2<-reactive({
    removeWords(TweetText1(),stp)
  })
  #TweetText3.0.0=TweetText2
  TweetText3<-reactive({
    LEMMA(TweetText2())
  })
  TweetText4<-reactive({
    f3(TweetText3())
  })
  TweetText5<-reactive({
    removeWords(TweetText4(),input$vec1)
  })
  TweetText6<-reactive({
    stri_replace_all_fixed(TweetText5(),c(" ",input$vec2),
                           c(" ",input$vec3),
                           vectorize_all=F)
  })
  tweet<-reactive({
    tibble(ID=tweetID(),TEXT=TweetText6())
  })
  tweet_sentiment <-reactive({
    tweet() %>%
      unnest_tokens("word", "TEXT") %>%
      inner_join(AFINN())
  })
  sen<- reactive({
    tweet() %>%
      left_join(tweet_sentiment() %>%
                  group_by(ID) %>%
                  summarise(value = sum(value)),by = "ID") %>%
      replace_na(list(value = 0))
  })
  tweet.positive<-reactive({
    sen() %>%
      filter("value">0)
  })
  tweet.negative<-reactive({
    sen() %>%
      filter("value"<0)
  })
  tweet.neutral<-reactive({
    sen() %>%
      filter("value"==0)
  })
  n.positive<-reactive({
    dim(tweet.positive())[1]
  })
  n.negative<-reactive({
    dim(tweet.negative())[1]
  })
  n.neutral<-reactive({
    dim(tweet.neutral())[1]
  })
  TABLE<-reactive({
    table(c(
      rep("PosITIVE",n.positive()),
      rep("NEUTRAL",n.neutral()),
      rep("NEGATIVE",n.negative())
    ))
  })
  dtm1 <-reactive({
    CreateDtm(doc_vec = sen()$TEXT, # character vector of documents
              doc_names = sen()$ID, # document names, optional
              ngram_window = c(1, 2), # minimum and maximum n-gram length
              stopword_vec = c(stopwords::stopwords("en"), # stopwords from tm
                               stopwords::stopwords(source = "smart")), # this is the default value
              lower = TRUE, # lowercase - this is the default value
              remove_punctuation = TRUE, # punctuation - this is the default
              remove_numbers = TRUE, # numbers - this is the default
              verbose = FALSE, # Turn off status bar for this demo
              cpus = 2)
  })
  tf_mat1<- reactive({
    TermDocFreq(dtm = dtm1())
  })
  term1<-reactive({
    stri_replace_all_fixed(tf_mat1()$term,
                           CON2()[,1],CON2()[,2],vectorize_all=F)
  })
  dtm <- reactive({
    dtm1()[ , colSums(dtm1() > 0) > input$X ]
  })
  tf_mat <-reactive({
    tf_mat1()[ term1() %in% colnames(dtm()) , ]
  })



  ######################################################################

  frequent.tokens0<-reactive({
    if(input$R1<1){
      tf_mat()[ order(tf_mat()$term_freq,
                      decreasing = TRUE) , ]}
    else{
      tf_mat()[ order(tf_mat()$term_freq,
                      decreasing = TRUE) , ][-c(0:input$R1),]}
  })


  frequent.tokens.doc0<-reactive({
    if(input$R1<1){
      tf_mat()[ order(tf_mat()$doc_freq,
                      decreasing = TRUE) , ]}
    else{
      tf_mat()[ order(tf_mat()$doc_freq,
                      decreasing = TRUE) , ][-c(0:input$R1),]}
  })
  frequent.tokens<-reactive({
    head(frequent.tokens0(),input$MAX)
  })

  frequent.doc.tokens<-reactive({
    head(frequent.tokens.doc0(),input$MAX)
  })



  frequent.tokens.cloud<-reactive({
    head(frequent.tokens0(),input$Ncloud)
  })
  tf_bigrams1<-reactive({
    tf_mat()[ stringr::str_detect(tf_mat()$term, "_") , ]
  })
  tf_bigrams <- reactive({
    tf_bigrams1()[ tf_bigrams1()$term %in% colnames(dtm()) , ]
  })

  frequent.bigram0<-reactive({
    if(input$R2<1){
      tf_bigrams()[ order(tf_bigrams()$term_freq,
                          decreasing = TRUE) , ]}
    else{
      tf_bigrams()[ order(tf_bigrams()$term_freq,
                          decreasing = TRUE) , ][-c(0:input$R2),]}
  })
  frequent.bigram.doc0<-reactive({
    if(input$R2<1){
      tf_bigrams()[ order(tf_bigrams()$doc_freq,
                          decreasing = TRUE) , ]}
    else{
      tf_bigrams()[ order(tf_bigrams()$doc_freq,
                          decreasing = TRUE) , ][-c(0:input$R2),]}
  })
  frequent.bigram<-reactive({
    head(frequent.bigram0(),input$MAX)
  })

  frequent.doc.bigram<-reactive({
    head(frequent.bigram.doc0(),input$MAX)
  })

  frequent.bigram.cloud<-reactive({
    head(frequent.bigram0(),input$Ncloud1)
  })
  #####################################################
  #NETWORK
  #####################################################

  bi.gram.count<-reactive({
    FUNbigrams (frequent.bigram0())
  })

  network <-reactive({
    bi.gram.count() %>%
      filter(weight > input$threshold) %>%
      mutate(weight = ScaleWeight(x = weight, lambda = 2E3)) %>%
      graph_from_data_frame(directed = FALSE)
  })
  EDG<-reactive({
    toVisNetworkData(network())[[2]]
  })
  EDG1 <-reactive({
    EDG()[order(EDG()$weight,decreasing = TRUE),]
  })

  output$table1.net<-renderDataTable({
    EDG1()
  })
  output$download1gr <- downloadHandler(
    filename = function(){"theName1.xlsx"},
    content = function(fname){
      write.xlsx(EDG1(),row.names=F,fname)
    }
  )
  output$download2gr <- downloadHandler(
    filename = function(){"theName2.csv"},
    content = function(fname){
      write.csv(EDG1(),row.names=F,fname)
    }
  )

  network.new<-reactive({
    set.graph(network())
  })

  output$plot1.net <- renderPlot({
    plot(
      network.new(),
      vertex.color = 'lightblue',
      # Scale node size by degree.
      vertex.size = 10*V(network.new())$degree,
      vertex.label.color = 'blue',
      vertex.label.cex = 1.6,
      vertex.label.dist = 1.6,
      edge.color = 'gray',
      # Set edge width proportional to the weight relative value.
      edge.width = 3*E(network.new())$width ,
      main = 'Bigram Count Network',
      sub = glue('Weight Threshold: {input$threshold}'),
      alpha = 50
    )

  })


  clustr.gr<-reactive({
    cluster.graph(network.new())[[1]]
  })


  membership.gr<-reactive({
    cluster.graph(network())[[2]]
  })




  output$table2.net<-renderDataTable({
    membership.gr()
  })


  output$download3gr <- downloadHandler(
    filename = function(){"theName3.xlsx"},
    content = function(fname){
      write.xlsx(membership.gr(),row.names=F,fname)
    }
  )
  output$download4gr <- downloadHandler(
    filename = function(){"theName4.csv"},
    content = function(fname){
      write.csv(membership.gr(),row.names=F,fname)
    }
  )




  clustr.gr.new<-reactive({
    set.graph( clustr.gr())
  })

  output$plot2.net<-renderPlot({
    plot(
      clustr.gr.new(),
      vertex.color = 'blue',
      vertex.size = 20*V(clustr.gr.new())$degree,
      vertex.label.color = 'blue',
      vertex.label.cex = 1.6,
      vertex.label.dist = 1.6,
      edge.color = 'gray',
      edge.width = 3*E(clustr.gr.new())$width ,
      main = 'Bigram Count Network (Biggest Connected Component)',
      sub = glue('Weiight Threshold: {input$threshold}'),
      alpha = 50
    )
  })

  node.impo.df<-reactive({
    cluster.graph(clustr.gr.new())[[3]]
  })

  impo <- reactive({
    node.impo.df()[order(node.impo.df()[,input$Importance],
                         decreasing =TRUE ),]
  })

  output$table3.net<-renderDataTable({
    impo()
  })
  #output$download5gr <- downloadHandler(
  # filename = function(){"theName5.xlsx"},
  #content = function(fname){
  # write.xlsx(impo(),row.names=F,fname)
  # }
  #)

  output$download6gr <- downloadHandler(
    filename = function(){"theName6.csv"},
    content = function(fname){
      write.csv(impo(),row.names=F,fname)
    }
  )
  Community.plot<-reactive({
    #Community.Detection.Plot(network.new())
    Community.Detection.Plot(clustr.gr.new())
  })


  output$plot3.net <- renderPlot({
    Community.plot()
  })

  Community.membership<-reactive({
    #Community.Detection.Membership(network.new())
    Community.Detection.Membership(clustr.gr.new())
  })

  output$table4.net<-renderDataTable({
    Community.membership()
  })

  output$download7gr <- downloadHandler(
    filename = function(){"theName7.xlsx"},
    content = function(fname){
      write.xlsx( Community.membership(),row.names=F,fname)
    }
  )
  output$download8gr <- downloadHandler(
    filename = function(){"theName8.csv"},
    content = function(fname){
      write.csv( Community.membership(),row.names=F,fname)
    }
  )

  dtm1gr <-reactive({
    CreateDtm(doc_vec = sen()$TEXT, # character vector of documents
              doc_names = sen()$ID, # document names, optional
              ngram_window = c(1, 1), # minimum and maximum n-gram length
              stopword_vec = c(stopwords::stopwords("en"), # stopwords from tm
                               stopwords::stopwords(source = "smart")), # this is the default value
              lower = TRUE, # lowercase - this is the default value
              remove_punctuation = TRUE, # punctuation - this is the default
              remove_numbers = TRUE, # numbers - this is the default
              verbose = FALSE, # Turn off status bar for this demo
              cpus = 2)
  })


  dtmgr <- reactive({
    dtm1gr()[ , colSums(dtm1gr() > 0) > input$X ]
  })

  dt <-reactive({
    as.TermDocumentMatrix(t(dtmgr()),weighting=weightTf)
  })

  Terms.cor1<-reactive({
    if(length(input$Terms.cor)==0){
      word=tf_mat()$term[1]}else{word=input$Terms.cor}
  })
  correlation.words.a<-reactive({
    network.cor(dt(),Terms.cor1(),
                threshold=input$thresholds.cor,pl=FALSE)
  })

  output$table5.net<-renderDataTable({
    correlation.words.a()
  })

  output$download9gr <- downloadHandler(
    filename = function(){"theName9.xlsx"},
    content = function(fname){
      write.xlsx(correlation.words.a(),row.names=F,fname)
    }
  )
  output$download10gr <- downloadHandler(
    filename = function(){"theName10.csv"},
    content = function(fname){
      write.csv(correlation.words.a(),row.names=F,fname)
    }
  )

  output$plot4.net <- renderPlot({
    network.cor(dt(),Terms.cor1(),
                threshold=input$thresholds.cor,pl=TRUE)
  })
  ##########output1######################
  Document.Lengths <-reactive({
    data.frame(var=c("min","q25","median","mean","q75","max"),
               value=apply(as.data.frame(rowSums(dtm1()))
                           ,2,summary))
  })
  output$summary<-renderTable({
    Document.Lengths()
  })
  output$barplot<-renderPlot({
    barplot(TABLE(),col=c("red","yellow","green"))
  })
  output$plot1<-renderPlot({
    barplot(frequent.tokens()$term_freq,
            las = 2, names.arg =frequent.tokens()$term,
            col ="lightblue", main ="Most frequent words",
            horiz=input$horizontalInput)
  })
  output$plot2<-renderPlot({
    barplot(frequent.doc.tokens()$doc_freq,
            las = 2, names.arg =frequent.doc.tokens()$term,
            col ="lightblue", main ="Most frequent Tokens on Documents",
            horiz=input$horizontalInput)
  })
  output$plot3<-renderPlot({
    barplot(frequent.bigram()$term_freq,
            las = 2, names.arg =frequent.bigram()$term,
            col ="lightblue", main ="Most frequent Bigram",
            horiz=input$horizontalInput)
  })
  output$plot4<-renderPlot({
    barplot(frequent.doc.bigram()$doc_freq,
            las = 2, names.arg =frequent.bigram()$term,
            col ="lightblue", main ="Most frequent Bigram on Documents",
            horiz=input$horizontalInput)
  })
  output$shinytest <- renderHwordcloud({
    hwordcloud(text =frequent.tokens.cloud()$term,
               size=frequent.tokens.cloud()$term_freq,
               theme="darkunica")
  })
  output$shinytest1 <- renderHwordcloud({
    hwordcloud(text =frequent.bigram.cloud()$term,
               size=frequent.bigram.cloud()$term_freq,
               theme="darkunica")
  })
  output$table<-renderTable({
    TABLE()
  })
  output$tokenfrequent<-renderDataTable({
    frequent.tokens()
  })

  output$download1 <- downloadHandler(
    filename = function(){"thename1.xlsx"},
    content = function(fname){
      write.xlsx(frequent.tokens0(),row.names=F,fname)
    }
  )
  output$download2 <- downloadHandler(
    filename = function(){"thename2.csv"},
    content = function(fname){
      write.csv(frequent.tokens0(),row.names=F,fname)
    }
  )



  output$bigramfrequent<-renderDataTable({
    frequent.bigram()
  })

  output$download3 <- downloadHandler(
    filename = function(){"thename3.xlsx"},
    content = function(fname){
      write.xlsx(frequent.bigram(),row.names=F,fname)
    }
  )
  output$download4 <- downloadHandler(
    filename = function(){"thename4.csv"},
    content = function(fname){
      write.csv(frequent.bigram(),row.names=F,fname)
    }
  )



  output$summary<-renderTable({
    Document.Lengths()
  })
  output$SENTIMENTS<-renderTable({
    sen()
  })



  ########################poss tagging#########################
  model <-reactive({
    udpipe_download_model(language = "persian-seraji")
  })
  ud_farsi <-reactive({udpipe_load_model(model()$file_model)})
  x <- reactive({
    udpipe_annotate(ud_farsi(),doc_id =tweetID(),TweetText6())
  })


  #x <- reactive({
    #udpipe_annotate(ud_farsi,doc_id =tweetID(),TweetText3.2())
 # })
  stp_df1 <- reactive({
    as.data.frame(x())
  })
  stp_df2<-reactive({
    f5(stp_df1(),input$poetry.poss)
  })

  stats <-reactive({
    txt_freq(stp_df2()[,8])
  })

  stats.key <-reactive({
    factor(stats()[,1], levels = rev(stats()[,1]))
  })

  stats1<-reactive({
    f7(stp_df2(),input$type.poss)
  })
  POS.TAGG<-reactive({
    f6(stp_df2())
  })
  word1 <-reactive({
    tolower(x$token)
  })
  pmi1<-reactive({
    PMI(stp_df2())
  })
  output$plot1.poss<-renderPlot({
    barchart(stats.key() ~ freq, data = stats(), col = "cadetblue",
             main = "UPOS  frequency of occurrence",
             xlab = "Freq")
  })
  DATA.freq<-reactive({
    head(stats1(),input$MAX.poss)
  })
  output$table1.poss<-renderDataTable({
    stats()
  })

  output$download8 <- downloadHandler(
    filename = function(){"thename8.xlsx"},
    content = function(fname){
      write.xlsx(stats(),row.names=F,fname)
    }
  )
  output$download9 <- downloadHandler(
    filename = function(){"thename9.csv"},
    content = function(fname){
      write.csv(stats(),row.names=F,fname)
    }
  )
  output$plot2.poss<-renderPlot({
    barchart(key ~ freq, data =DATA.freq(),col = "cadetblue",
             main=paste0("Most occurring",input$type, collapse = ' ')
             ,xlab="Freq")
  })
  output$table2.poss<-renderDataTable({
    stats1()
  })

  output$download10 <- downloadHandler(
    filename = function(){"thename10.xlsx"},
    content = function(fname){
      write.xlsx(stats1(),row.names=F,fname)
    }
  )
  output$download11 <- downloadHandler(
    filename = function(){"thename11.csv"},
    content = function(fname){
      write.csv(stats1(),row.names=F,fname)
    }
  )

  output$plot3.poss <- renderHwordcloud({
    hwordcloud(text =DATA.freq()$key,
               size=DATA.freq()$freq,
               theme="darkunica")
  })
  output$taggin.poss<-renderDataTable({
    POS.TAGG()
  })

  output$download12 <- downloadHandler(
    filename = function(){"thename12.xlsx"},
    content = function(fname){
      write.xlsx(POS.TAGG(),row.names=F,fname)
    }
  )
  output$download13 <- downloadHandler(
    filename = function(){"thename13.csv"},
    content = function(fname){
      write.csv(POS.TAGG(),row.names=F,fname)
    }
  )
}
#' Run Shiny Application for MadanText
#'
#' This function runs the MadanText Shiny application which includes both
#' the user interface and server logic.
#'
#' @examples
#' shinyApp(ui, server)
shinyApp(ui,server)
