#' @title Taxolist to document
#' @description Converts a taxolist to a formatted document in html, pdf or
#' word document
#' @param taxolist taxolist
#' @param genus only process for specific genus. Default("") implying process 
#' all
#' @param family only process for specific family. Default("") implying process 
#' all
#' @param title List title you want to print in output header
#' @param addsource boolean If the source tag should be displayed. Default TRUE
#' @param mastersource source string for the master list
#' @param duplicatesyn boolean if synonyms should to displayed in their 
#' alphabetical sorted position too. Default TRUE
#' @param sourcecol vector of text color values for each source value
#' @param outformat output format one of "html_document", "word_document",
#' "odt_document", "rtf_document", "pdf_document". Default ("html_document")
#' @param outdir output directory for the document. Default temporary directory.
#' @param outfile output file name. Default ("taxolist.html")
#' @importFrom rmarkdown render
#' @importFrom stringr str_split
#' @return NULL Saves a document file
#' @details Converts a taxolist to a formatted document in html, pdf or
#' word document making it easy for taxonomist to read through the data
#' @family List functions
#' @examples
#' \donttest{
#'mytaxo <- data.frame("id" = c(1,2,3,4,5,6,7),
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
#' taxo2doc(mytaxo)
#' taxo2doc(mytaxo,source="My list")
#'}
#' @rdname taxo2doc
#' @export
#' 
taxo2doc <- function(taxolist=NULL,genus=NA,family=NA,
                     title="", addsource=TRUE, mastersource = "",
                     duplicatesyn=TRUE,
                     sourcecol=c("black","blue"),
                     outformat="html_document",
                     outdir=tempdir(),outfile="taxolist.html"){
  if(is.null(taxolist)){
    stop("No taxolist to process")
  }
  if("species" %!in% names(taxolist)){
    taxolist <- melt_canonical(taxolist,
                               canonical="canonical",
                               genus="genus",
                               species="species",
                               subspecies="subspecies")
  }
  tfile <- tempfile("taxo_", fileext = c(".rmd"))
  con <- file(tfile)
  sink(con, append=TRUE)
  if(title==""){
    cat(paste('---\ntitle: "Taxonomic list" \n'))
  } else {
    cat(paste('---\ntitle: "',title,'"\n'))
  }
  if(outformat=="word_document"){
    cat(paste("output:\n\tbookdown::word_document2:\n\t\ttoc: false\n\t\treference_docx: mytemplet.docx"))
  }
  cat("---\n\n")
  if(mastersource!=""){
    cat(paste('\n Master source of the list ',mastersource,'\n'))
  } 
  pcol <- "black"
  mytaxo <- taxolist
  if("source" %!in% names(mytaxo)) {
    mytaxo <- mytaxo[!duplicated(mytaxo$canonical),]
  } else {
    mytaxo <- cast_cs_field(mytaxo,"canonical","source")
  }
  mytaxo$family[which(is.na(mytaxo$family))] <- "-"
  if("author" %in% names(mytaxo)){
    mytaxo$author[which(is.na(mytaxo$author))] <- ""
  } else {
    mytaxo$author <- ""
  }
  mytaxo <- mytaxo[with(mytaxo, order(family,canonical)),]
  if(!is.na(genus)){
    mytaxo <-mytaxo[which(mytaxo$genus %in% genus),]
  }
  if(!is.na(family)){
    mytaxo <-mytaxo[which(mytaxo$family %in% family),]
  }
  cat(paste("  \n\n"))
  if(duplicatesyn){
    mytaxo_ac <- mytaxo
  } else {
    mytaxo_ac <- mytaxo[which(mytaxo$accid==0),]
  }
  
  if(nrow(mytaxo_ac)==0){
    cat(paste("# ",source,"has nothing to display  \n"))
    sink() 
    return()
  }
  fam <- ""
  index <- 1
  for(i in 1:nrow(mytaxo_ac)){
    if(mytaxo_ac$family[i]!=fam){
      cat(paste("  \n\n"))
      cat(paste("### Family: _",mytaxo_ac$family[i],"_  \n",sep=''))
      fam <- mytaxo_ac$family[i]
    }
    mytaxo_ac$author[i] <- str2ascii(mytaxo_ac$author[i])
    if(!is.empty(mastersource)){
      pcol <- set_pcol(mytaxo_ac$source[i],mastersource,sourcecol)
    } 
    if(mytaxo_ac$accid[i]==0){
      cat(printf_taxo(mytaxo_ac[i,],index,pcol,addsource,FALSE,outformat))
      index <- index + 1
    } else {
      cat(printf_taxo(mytaxo_ac[i,],0L,pcol,addsource,FALSE,outformat))
      cat(" = ")
      accrec <- mytaxo[which(mytaxo$id==mytaxo_ac$accid[i]),]
      if(nrow(accrec)>0){
        cat(printf_taxo(accrec[1,],0L,pcol,addsource,FALSE,outformat))
      } else {
        cat("<b>Orphan synonym</b><br>")
      }
    }
    if(nrow(mytaxo[which(mytaxo$accid==mytaxo_ac$id[i]),])>0){
      synlst <- mytaxo[which(mytaxo$accid==mytaxo_ac$id[i]),]
      for(j in 1:nrow(synlst)){
        synlst$author[j] <- str2ascii(synlst$author[j])
        if(!is.empty(mastersource)){
          pcol <- set_pcol(synlst$source[j],mastersource,sourcecol)
        } 
        cat(printf_taxo(synlst[j,],0L,pcol,addsource,TRUE,outformat))
      }
    }
  }
  sink() 
  
  rmarkdown::render(input=tfile,
                    output_format=outformat,
                    output_dir = outdir,
                    output_file = outfile)
  return(tfile)
}

printf_taxo <- function(rec=NULL,index=0,
                        color="black",
                        addsource=TRUE,
                        syn=TRUE,
                        outformat){
  dropnl <- FALSE
  if(rec$accid==0) {
    pstr <- ifelse(index>0,
                   paste(index," _",rec$canonical,"_ ",sep = ''),
                   paste(" _",rec$canonical,"_ ",sep = ''))
  } else {
    if(syn){
      pstr <- paste("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;= _",
                    rec$canonical,"_ ",sep = '')
    } else {
      pstr <- paste("->&nbsp;_", rec$canonical,"_ ",sep = '')
      dropnl <- TRUE
    }
  }
  if(!is.empty(rec$author)){
    rec$author <- str2ascii(rec$author)
    pstr <- paste(pstr,rec$author,sep = '')
  }
  if(addsource){
    if(!is.empty(rec$source)){
      pstr <- paste(pstr," [",rec$source,"]",sep = '')
    }
  }
  if(dropnl){
    return(paste(colFmt(pstr,color,outformat)," "))
  } else {
    return(paste(colFmt(pstr,color,outformat),"  \n"))
  }
}

colFmt <- function(x,color,outputFormat) {
  if(outputFormat  %in% c('latex')) {
    ret <- paste("\\textcolor{",color,"}{",x,"}",sep="")
  } else if(outputFormat %in% c('html_document')) {
    ret <- paste("<font color='",color,"'>",x,"</font>",sep="")
  } else if(outputFormat %in% c('word_document')) {
    ret <- paste(':::{custom-style="',color,'"}\n',x,'\n:::\n',sep="")
  } else{
    ret <- x
  }
  return(ret)
}

set_pcol <- function(source,mastersource,
                     sourcecol=c("black","red")){
  if(mastersource %in% trimws(str_split(source,",")[[1]])){
    pcol <- sourcecol[1] #"black"
  } else {
    pcol <- sourcecol[2] # "red"
  }
  return(pcol)
}