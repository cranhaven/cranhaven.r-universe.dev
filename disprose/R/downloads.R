#'
#'Get GenInfo Identifier numbers
#'
#'Retrieves NCBI sequence identifiers (GIs) for given organism name or taxon identifier.
#'
#'@param org.name character; scientific name or taxon identifier (written as "txid0000") of the organism/taxon.
#'@param db character; NCBI database for search. See \link[rentrez]{entrez_dbs}() for possible values.
#'@param n.start integer; download starting value. Default is 1.
#'@param n.stop integer; download finishing value. Default is NULL, which provides retrieval of all available GIs.
#'@param step integer; download increment value.
#'@param return.vector logical; whether to return GI numbers as character vector (another variant is list of vectors).
#'@param check.result logical; check if download was done correctly.
#'@param term character; search query.
#'@param temp.dir character; name and path of directory for downloaded temporary files (only for "Windows" OS)
#'@param delete.temp logical; delete downloaded files (only for "Windows" OS, does not delete directory).
#'@param gis.list list of previously downloaded GIs vectors.
#'@param verbose logical; show messages
#'
#'@details
#'This function sends the query to NCBI database and returns sequence identifiers according to the query. By default the
#'query is organism, so the function returns GI numbers for all sequences that are associated with the requested organism.
#'For example, if \code{org.name = "Homo sapiens"} the function will download GI numbers for all sequences that answer the query
#'"Homo sapiens[Organism]". For any other query use parameter \code{term}.
#'
#'The function downloads GI numbers by piecemeal, by several pieces in one block. The size of the block is defined by parameter
#'\code{step}. It is useful if by any reason the download was interrupted, so later it is possible to reload only
#'the missing blocks without the need to reload the entire amount of data. By default, all available GI numbers are downloaded,
#' but you may also choose start and finish notes by specifying the parameters \code{n.start} and \code{n.stop}. The numeration starts with 1, not 0.
#' At the end the resulting list of blocks (list of character vectors) is unlisted into one character vector. You may prevent this by setting
#' \code{return.vector = FALSE}. Also, regardless of \code{return.vector} settings, the list of blocks is returned if the download was somehow compromised.
#'
#'If download was corrupted you may use \code{get_GIs_fix()} function to reload the missing block. The corrupted list of blocks
#'should be set in \code{gis.list} parameter. You may also check and reload data when \code{get_GIs()} function is running
#'by specifying \code{check.result = TRUE}.
#'
#'The function checks for user's OS type. For Windows temporal files are created while downloading,
#'so \code{temp.dir} and \code{delete.temp} parameters should be set. This helps to solve the
#'"routines:SSL23_GET_SERVER_HELLO:tlsv1 alert protocol version" problem by using \code{curl} instead of \code{RCurl}.
#'However it slows down the function.If there is no \code{temp.dir} directory, it will be
#'created and will not be removed (only temporal files will be deleted if \code{delete.temp = TRUE}).
#'
#'In progress the functions turn off and on scientific notation.
#'
#'@return
#'\code{get_GIs()} returns character vector of GI numbers. If \code{return.vector = FALSE} or there are missing data,
#'list of character vectors is returned.
#'
#'\code{get_GIs_fix()} returns list of character vectors.
#'
#'
#'@examples
#'gi.list<-get_GIs(org.name="txid9606", db="nucleotide",
#'                 n.start=1, n.stop=3, step=1,
#'                 return.vector = FALSE, check.result=TRUE,
#'                 temp.dir = tempdir(),  delete.temp=TRUE)
#'
#'@author Elena N. Filatova
#'@name get_GIs
NULL
#'
#'@describeIn get_GIs Retrieves NCBI sequence identifiers (GIs) for given organism name or taxon identifier.
#'@export

get_GIs<-function(org.name, db, n.start = 1, n.stop = NULL, step = 99999,
                  return.vector = TRUE, check.result = FALSE, term = NULL,
                  temp.dir=NULL, delete.temp = FALSE, verbose = TRUE){
  # test package dependencies
  if (!requireNamespace("reutils", quietly = TRUE)) { stop("Package \"reutils\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("rentrez", quietly = TRUE)) { stop("Package \"rentrez\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("XML", quietly = TRUE)) { stop("Package \"XML\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("curl", quietly = TRUE)) { stop("Package \"curl\" needed for this function to work. Please install it.", call. = FALSE)}
  # options
  old.ops<-options()# turn off scientific notation
  on.exit(options(old.ops), add=T)
  options(scipen = 999)
  #check setup for Windows
  if (.Platform$OS.type == "windows" & is.null(temp.dir) == TRUE) {stop ("Set directory for temporal files")}
  # variables for query
  n.start<-n.start-1 # numeration starts with zero
  if (is.null(term)==TRUE){term<-paste(org.name, "[Organism]", sep="")}
  if (is.null(n.stop)==TRUE){n.stop<-try(rentrez::entrez_search(db=db, term=term, retmax=0)$count)}
  if (inherits (n.stop, "try-error") ){stop("Could not connect to server. Please try again.")}
  if(n.stop==0){stop("No sequences found")} # wrong term
  n.stop<-n.stop-1 # numeration starts with zero
  sequ<-seq(n.start, n.stop, step) # cutting query blocks
  if (verbose) message ("Downloading GIs for ", n.stop-n.start+1, " sequences in ", length(sequ), " blocks") # start message
  res<-vector( mode = "list", length = length(sequ))
  # for linux
  if (.Platform$OS.type != "windows"){
    for (i in 1:length(sequ)){bind<-tryCatch(reutils::esearch(db=db, term=term, retstart=sequ[i], retmax=step),
                                             error=function(err) NA)
    res[[i]]<-tryCatch(reutils::uid(bind), error=function(err) NA)
    if(verbose) cat(sequ[i]+step, "GIs downloaded\r")}}  # download message
  # for windows
  if (.Platform$OS.type == "windows"){
    if (exists(temp.dir) == FALSE){dir.create(temp.dir)}
    for (i in 1:length(sequ)){
      try({
        suppressWarnings(URL<-reutils::getUrl(reutils::esearch(db=db, term=term, retstart=sequ[i], retmax=step)))
        curl::curl_download(URL, destfile=paste0(temp.dir, "\\gis", i))
        if (verbose)cat(sequ[i]+step, "GIs downloaded\r") # download message
        gifile<-XML::xmlToList(paste0(temp.dir, "\\gis", i))
        res[[i]]<-as.character(unlist(gifile[[which(names(gifile)=="IdList")]])) # take IdList from file and unlist it
      })
      if (delete.temp == TRUE & file.exists(paste0(temp.dir, "\\gis", i))){
        file.remove(paste0(temp.dir, "\\gis", i))}
    }
    #check try-error
    for (j in 1:length(res)){if (class(res[[j]])[1]=="try-error" | is.null(res[[j]])){res[[j]]<-NA}}
  }
  # check.result == TRUE
  if (check.result==TRUE){res<-get_GIs_fix(gis.list=res, org.name=org.name, db=db, n.start=n.start+1,
                                           n.stop=n.stop+1, step=step, temp.dir = temp.dir, delete.temp = delete.temp,
                                           verbose = verbose)}

  #check empty blocks and return the result
  check<-lapply(X=res, FUN=is.na)
  ifelse(sum(unlist(check))>0,
         {if (verbose) message ("There are some empty downloads, the result is returned as list")
           return(res)},
         {ifelse (return.vector==TRUE,
                  {if (verbose) message("Everything seems fine, the result is returned as vector")
                    res<-unlist(res);return(res)},
                  {if (verbose) message ("Everything seems fine, the result is returned as list")
                    return(res)})})
}

#' @describeIn get_GIs Checks the downloads and tries to retrieve the compromised data.
#' @export

get_GIs_fix<-function(gis.list, org.name, db, n.start = 1, n.stop = NULL, step = 99999,
                      term = NULL, temp.dir = NULL, delete.temp = FALSE, verbose = TRUE){
  # test package dependencies
  if (!requireNamespace("reutils", quietly = TRUE)) { stop("Package \"reutils\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("rentrez", quietly = TRUE)) { stop("Package \"rentrez\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("XML", quietly = TRUE)) { stop("Package \"XML\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("curl", quietly = TRUE)) { stop("Package \"curl\" needed for this function to work. Please install it.", call. = FALSE)}
  # options
  old.ops<-options()# turn off scientific notation
  on.exit(options(old.ops), add=T)
  options(scipen = 999)
  if (verbose) message ("Checking mistakes in ", length(gis.list), " blocks") # start message
  n.start<-n.start-1 # numeration starts with zero
  if (is.null(term)==TRUE){term<-paste(org.name, "[Organism]", sep="")}
  if (is.null(n.stop)==TRUE){n.stop<-try(rentrez::entrez_search(db=db, term=term, retmax=0)$count)}
  if (inherits(n.stop, "try-error")){stop("Could not connect to server. Please try again.")}
  if(n.stop==0){stop("No sequences found")} # wrong term
  n.stop<-n.stop-1 # numeration starts with zero
  sequ<-seq(n.start, n.stop, step) # cutting query blocks
  #check try-error
  for (j in 1:length(gis.list)){if (class(gis.list[[j]])[1]=="try-error"){gis.list[[j]]<-NA}}
  check<-lapply(X=gis.list, FUN=is.na) # check empty
  if (sum(unlist(check))==0){if (verbose) message ("No mistakes found")}else{
    for (i in 1:length(gis.list)){
      if (sum(check[[i]])>0){
        # for linux
        if (.Platform$OS.type != "windows"){
          bind<-tryCatch(reutils::esearch(db=db, term=term, retstart=sequ[i], retmax=step),
                         error=function(err) NA)
          gis.list[[i]]<-tryCatch(reutils::uid(bind), error=function(err) NA)}
        # for windows
        if (.Platform$OS.type == "windows"){
          if (exists(temp.dir) == FALSE){dir.create(temp.dir)}
          try({
            suppressWarnings(URL<-reutils::getUrl(reutils::esearch(db=db, term=term, retstart=sequ[i], retmax=step)))
            curl::curl_download(URL, destfile=paste0(temp.dir, "\\gis", i))
            gifile<-XML::xmlToList(paste0(temp.dir, "\\gis", i))
            gis.list[[i]]<-as.character(unlist(gifile[[which(names(gifile)=="IdList")]])) # take IdList from file and unlist it
          })
          if(class(gis.list[[i]])[1]=="try-error" | is.null(gis.list[[i]])){gis.list[[i]]<-NA}
          if (delete.temp == TRUE  & file.exists(paste0(temp.dir, "\\gis", i))){
            file.remove(paste0(temp.dir, "\\gis", i))}}
        # check the new gis.list [[i]] and final messages
        if (sum(is.na(gis.list[[i]]))==0){if (verbose) message ("Block ", i, " is fixed")}
        if (sum(is.na(gis.list[[i]]))>0){if (verbose) message ("Block ", i, " is not fixed")}
      }}}
  return (gis.list)
}
#'
#'Get NCBI sequence record
#'
#'Retrieves information about sequences from NCBI records for given organism name or taxon identifier.
#'
#'@param org.name character; scientific name or taxon identifier (written as "txid0000") of the organism/taxon.
#'@param db character; NCBI database for search. See \link[rentrez]{entrez_dbs}() for possible values.
#'@param n.start integer; download starting value. Default is 1.
#'@param n.stop integer; download finishing value. Default is NULL, which provides retrieval of all available GIs.
#'@param step integer; download increment value. Maximum is 500.
#'@param return.dataframe integer; whether to return information as structured data frame (another variant is list of lists).
#'@param check.result logical; check if download was done correctly.
#'@param term character; search query.
#'@param web.history previously saved web_history object for use in calls to the NCBI. New web.history is created if none is provided.
#'@param info.list list of previously downloaded records.
#'@param unlist logical; unlist result before transforming (only recommended if \code{step > 1}).
#'@param verbose logical; show messages
#'
#'
#'@details
#'This function sends the query to NCBI database and returns sequence records according to the query. By default the
#'query is organism, so the function returns data of all sequences that are associated with the requested organism.
#'For example, if \code{org.name = "Homo sapiens"} the function will download data for all records that answer the query
#'"Homo sapiens[Organism]". For any other query use parameter \code{term}.
#'
#'The function downloads records by piecemeal, by several pieces in one block. The size of the block is defined by parameter
#'\code{step}. It is useful if by any reason the download was interrupted, so later it is possible to reload only
#'the missing blocks without the need to reload the entire amount of data. By default, all available records are downloaded,
#' but you may also choose start and finish points by specifying the parameters \code{n.start} and \code{n.stop}. The numeration starts with 1, not 0.
#' At the end the resulting list of blocks (list of lists if \code{step > 1}) is unlisted into one data frame that contains information about record GI, UID,
#' caption, source database, organism, strain etc. You may prevent this by setting \code{return.dataframe = FALSE}.
#' Also, regardless of \code{return.dataframe} settings, the list of blocks is returned if the download was somehow compromised.
#' Optionally, you can turn the resulting list into data frame later using the function \code{info_listtodata()}.
#' Note that in this case, if parameter \code{info.list} was inherited from \code{get_seq_info()} function,
#' the result must be unlisted first (use \code{unlist = TRUE}).
#'
#'If download was corrupted you may use \code{get_seq_info()} function to reload the missing block. The corrupted list of blocks
#'should be set in \code{info.list} parameter. You may also check and reload data when \code{get_seq_infos()} function is running
#'by specifying \code{check.result = TRUE}.
#'
#'In progress the functions turn off and on scientific notation.
#'
#'@return
#'\code{get_seq_info()} returns data frame that contains most of sequence information from NCBI records.
#'If \code{return.dataframer = FALSE} or there are missing data, list of lists is returned. List contains full information
#'from NCBI records.
#'
#'\code{get_seq_info_fix()} returns list of lists.
#'
#'\code{info_listtodata()} returns data frame.
#'
#'@examples
#' info.dataframe <- get_seq_info (org.name = "txid9606", db = "nucleotide", n.start = 1,
#'                                n.stop = 10, step = 5, return.dataframe = TRUE,
#'                                check.result = TRUE)
#'
#'@author Elena N. Filatova
#'@name get_seq_info
NULL
#'
#'@describeIn get_seq_info Retrieves NCBI sequence records for given organism name or taxon identifier.
#'@export

get_seq_info <- function (org.name, db, n.start = 1, n.stop = NULL, step = 500,
                          return.dataframe = FALSE, check.result = FALSE, term = NULL, verbose = TRUE){
  # test package dependencies
  if (!requireNamespace("rentrez", quietly = TRUE)) { stop("Package \"rentrez\" needed for this function to work. Please install it.", call. = FALSE)}
  # options
  old.ops<-options()# turn off scientific notation
  on.exit(options(old.ops), add=T)
  options(scipen = 999)
  if (is.null(term)==TRUE){term<-paste(org.name, "[Organism]", sep="")} # search term
  web.search<- try(rentrez::entrez_search(db=db, term=term, use_history=TRUE))
  if (class(web.search)[1] == "try-error"){stop("Could not connect to server. Please try again.")}
  if (is.null(n.stop)==TRUE) {n.stop<-web.search$count}
  if(n.stop==0){stop("No sequences found")} # wrong term
  n.start<-n.start-1 ; n.stop<-n.stop-1 # numeration starts with zero
  if (verbose) message (web.search$count, " sequences found, downloading records for sequences from ", n.start+1, " to ", n.stop+1)
  sequ<-seq(n.start, n.stop, step)
  res<-vector( mode = "list", length = length(sequ))
  for( seq_start in sequ){
    res[[match(seq_start, sequ)]]<-tryCatch(rentrez::entrez_summary(db=db, web_history=web.search$web_history, retmax=step, retstart=seq_start),
                                            error=function(err) NA)
    if (verbose) cat(seq_start+step, "notes downloaded\r")
  }
  if (check.result==TRUE){res<-get_seq_info_fix(info.list = res, web.history = web.search, db=db, n.start=n.start+1, n.stop=n.stop+1,
                                                step=step, verbose = verbose)}
  #check empty blocks and return the result
  check<-lapply(X=res, FUN=is.na)
  ifelse(sum(unlist(check))>0,
         {if (verbose) message ("There are some empty downloads, the result is returned as list")
           return(res)}, # return list with mistake
         {ifelse (return.dataframe==FALSE,
                  {if (verbose) message ("Everything seems fine, the result is returned as list")
                    return(res)}, #return list as set
                  {if (verbose) message ("Everything seems fine, transferring result list to data frame") # transfer list to data frame
                    # for step = 1 no unlisting
                    if (step==1){
                      res.data<-info_listtodata(info.list = res, unlist=FALSE, verbose = verbose)
                    } else{res.data<-info_listtodata(info.list = res, unlist=TRUE, verbose = verbose)}
                    return(res.data)})})
}
#'@describeIn get_seq_info Checks the downloads and tries to retrieve the compromised data.
#'@export
#'
get_seq_info_fix<-function(info.list, web.history = NULL, org.name = NULL, db,
                           n.start = 1, n.stop = NULL, step = 500, term = NULL, verbose = TRUE){
  # test package dependencies
  if (!requireNamespace("rentrez", quietly = TRUE)) { stop("Package \"rentrez\" needed for this function to work. Please install it.", call. = FALSE)}
  # options
  if (verbose) message ("Checking mistakes in ", length(info.list), " blocks") # start message
  old.ops<-options()# turn off scientific notation
  on.exit(options(old.ops), add=T)
  options(scipen = 999)
  if (is.null(web.history)==TRUE){ # make history if none available
    if (is.null(term)==TRUE){term<-paste(org.name, "[Organism]", sep="")} # search term
    web.history<- try(rentrez::entrez_search(db=db, term=term, use_history=TRUE))}
  if (class(web.history)[1] == "try-error"){stop("Could not connect to server. Please try again.")}
  if (is.null(n.stop)==TRUE) {n.stop<-web.history$count}
  if(n.stop==0){stop("No sequences found")} # wrong term
  n.start<-n.start-1 ; n.stop<-n.stop-1 # numeration starts with zero
  sequ<-seq(n.start, n.stop, step)
  #check - NAs in list
  check<-lapply(X=info.list, FUN=is.na) # check empty
  if (sum(unlist(check))==0){if (verbose) message ("No mistakes found")}else{
    for (i in 1:length(info.list)){
      if (sum(check[[i]])>0){ # if there is mistake in block i
        info.list[[i]]<-tryCatch(rentrez::entrez_summary(db=db, web_history=web.history$web_history, retmax=step, retstart=sequ[i]),
                                 error=function(err) NA)
        #check the new info.list [[i]] and final messages
        if (sum(is.na(info.list[[i]]))==0){if (verbose) message ("Block ", i, " is fixed")}
        if (sum(is.na(info.list[[i]]))>0){if (verbose) message ("Block ", i, " is not fixed")}
      }
    }}
  return (info.list)
}
#'@describeIn get_seq_info Transforms downloaded list into data frame.
#'@export
#'
info_listtodata<-function(info.list, unlist = TRUE, verbose = TRUE){
  save.list<-info.list # save list for possible problems to return result as list
  if (unlist==TRUE){ #unlisting
    info.list2<-list(); for (i in 1:length(info.list)){info.list2<-append(info.list2, info.list[[i]])}
    info.list<-info.list2}
  # function to checl if field exists and make vector
  make.vec <- function (info.list, field){
    res <- c()
    if (field %in% names (info.list[[1]])) {res<-unlist(lapply(info.list, '[', field))
    } else {res<-rep (NA, length(info.list))}
    return (res)}
  uid<-make.vec(info.list, "uid")
  gi<-as.numeric(make.vec(info.list, "gi"))
  GB_AcNum<-make.vec(info.list, "caption")
  createdate<-make.vec(info.list, "createdate")
  updatedate<-make.vec(info.list, "updatedate")
  source_db<-make.vec(info.list,  "sourcedb")
  organism<-make.vec(info.list, "organism")
  title<-make.vec(info.list,  "title")
  strain<-make.vec(info.list, "strain")
  taxid<-make.vec(info.list,  "taxid")
  length<-make.vec(info.list, "slen")
  biomol<-make.vec(info.list,  "biomol")
  moltype<-make.vec(info.list, "moltype")
  genome<-make.vec(info.list,  "genome")
  complete <- make.vec(info.list, "completeness")
  geneticcode<-make.vec(info.list, "geneticcode")
  strand <- make.vec(info.list, "strand")
  #subinfo If several takes first
  subtype<-c(); subname<-c(); host<-c(); country<-c(); isolation_source<-c(); collection_date<-c()
  for (i in 1:length(info.list)){
    subtype<-info.list[[i]]$subtype; subtype<-strsplit(subtype, "|", fixed=T)[[1]]
    subname<-info.list[[i]]$subname; subname<-strsplit(subname, "|", fixed=T)[[1]]
    ifelse(("host" %in% subtype)==TRUE, host[i]<-subname[[which(subtype=="host")[1]]], host[i]<-NA)
    ifelse(("country" %in% subtype)==TRUE, country[i]<-subname[[which(subtype=="country")[1]]], country[i]<-NA)
    ifelse(("isolation_source" %in% subtype)==TRUE, isolation_source[i]<-subname[[which(subtype=="isolation_source")[1]]], isolation_source[i]<-NA)
    ifelse(("collection_date" %in% subtype)==TRUE, collection_date[i]<-subname[[which(subtype=="collection_date")[1]]], collection_date[i]<-NA)
  }
  res.data<-try(data.frame(uid, gi, GB_AcNum, createdate, updatedate, source_db, organism, title, strain, taxid,
                           length, biomol, moltype, genome, complete, geneticcode, strand,
                           host, country, isolation_source, collection_date), silent=TRUE )# genome info data
  if(is.data.frame(res.data)){if  (verbose){message("The result is returned as data frame")}
    rownames(res.data)<-NULL
    return(res.data)}
  if (is.data.frame(res.data) == FALSE){if  (verbose){
    message("It seems there are some problems with metadata format, the result is returned as list")}
    return(save.list)}

}

#'
#'Get nucleotide sequences from NCBI
#'
#'Retrieves nucleotide sequences from NCBI for given identification numbers.
#'
#'@param ids vector of NCBI sequences' identification numbers: GenBank accession numbers, GenInfo identifiers (GI) or Entrez unique identifiers (UID)
#'@param db character; NCBI database for search. See \link[rentrez]{entrez_dbs}() for possible values
#'@param check.result logical; check if download was done correctly
#'@param return character; sequence returned object; possible values are "vector", "data.frame" and "fasta"
#'@param fasta.file character; FASTA file name and path, only used if \code{return = "fasta"}
#'@param exclude.from.download logical; ignore some sequences while downloading
#'@param exclude.var vector that is used to define which sequences should be ignored, only used if \code{exclude.from.download = TRUE}.
#'@param exclude.pattern value that matches to \code{exclude.var} and marks unwanted sequences, only used if \code{exclude.from.download = TRUE}
#'@param exclude.fixed logical; match \code{exclude.pattern} as is, only used if \code{exclude.from.download = TRUE}.
#'@param res.data data.frame; data frame of nucleotide ids and previously downloaded sequences
#'@param verbose logical; show messages
#'
#'@details
#'Master records (for example, in WGS-project) do not contain any nucleotide.
#'They might be excluded from download with \code{exclude.from.download} parameters.
#'However this has no affect and such ids do not have to be excluded when loading.
#'
#'If writing FASTA to existing FASTA file, sequences are appended.
#'
#'@return
#'If \code{return = "vector"} function returns vector of nucleotide sequences,
#' \code{return = "data.frame"} - data frame with nucleotide ids and nucleotide sequences,
#' \code{return = "fasta"} - writes FASTA file, no data returned.
#'
#' @examples
#' ids<-c(2134240466, 2134240465, 2134240464)
#' fasta.file<-tempfile()
#' get_seq_for_DB (ids = ids, db = "nucleotide", check.result = TRUE,
#'                 return = "fasta", fasta.file = fasta.file, exclude.from.download=FALSE)
#' file.remove(fasta.file)
#'
#'@author Elena N. Filatova
#'@name get_seq_for_DB
NULL
#'
#'@describeIn get_seq_for_DB Retrieves NCBI nucleotide sequences for given identification numbers.
#'@export
get_seq_for_DB <- function (ids, db, check.result = FALSE, return="data.frame", fasta.file = NULL,
                            exclude.from.download = FALSE, exclude.var, exclude.pattern, exclude.fixed = TRUE,
                            verbose = TRUE){
  # test package dependencies
  if (!requireNamespace("rentrez", quietly = TRUE)) { stop("Package \"rentrez\" needed for this function to work. Please install it.", call. = FALSE)}
  # check fasta file
  if (return=="fasta" & is.null(fasta.file)==TRUE){stop ("Set FASTA file name and path")} # fasta file name
  if (return=="fasta"){ if (file.exists(fasta.file)==TRUE){ warning ("Note that FASTA file already exists. Adding sequences to file.")}}
  #check return type
  if (return!="vector" & return!="data.frame" & return!="fasta"){stop("Choose return object")}
  # exclude
  res.data<-data.frame(ids, seqs=rep(NA, length(ids)))
  if (exclude.from.download==TRUE){ # exclude shit
    exclude.n<-grep(exclude.pattern, exclude.var, fixed=exclude.fixed)
    res.data$seqs[exclude.n]<-"no sequence"}
  #download
  for (i in 1:length(ids)) {
    if (is.na(res.data$seqs[i])==TRUE){
      try(res.data$seqs[i]<-rentrez::entrez_fetch(db=db, id=ids[i], rettype="fasta"))
      if (verbose) cat(i, "sequences downloaded\r")}
  }
  #checking mistakes
  if (check.result==TRUE){res.data<-get_seq_for_DB_fix(res.data=res.data, db=db, verbose = verbose)}
  #returning
  if (return=="vector"){return(res.data$seqs)}
  if(return=="data.frame"){return(res.data)}
  if (return=="fasta"){
    #check if there is no NAs
    fa_seq<-res.data$seqs
    if (exclude.from.download==TRUE){noseq.n<-grep("no sequence", fa_seq); fa_seq<-fa_seq[-noseq.n]} # delete no sequence data
    write(x=fa_seq, file=fasta.file, append = TRUE)
    if (verbose) message ("FASTA file is in ", fasta.file)
  }
}

#'@describeIn get_seq_for_DB Checks the downloads and tries to retrieve the compromised data.
#'@export

get_seq_for_DB_fix <- function(res.data, db, verbose = TRUE){
  if (verbose) message ("checking mistakes")
  # test package dependencies
  if (!requireNamespace("rentrez", quietly = TRUE)) { stop("Package \"rentrez\" needed for this function to work. Please install it.", call. = FALSE)}
  for (i in 1:nrow(res.data)){
    if (is.na(res.data$seqs[i])==FALSE){ # NA try function from get_seqs_for_id returns in mistake; "\n" - is no seq (master record)
      if (res.data$seqs[i]=="no sequence" | res.data$seqs[i]=="\n"){
        if (verbose) message ("note ", i, " - ", res.data$ids[i], " no sequence")
      } else { if (verbose) message ("note ", i, " - ", res.data$ids[i], " sequence downloaded")}
    } else{
      try(res.data$seqs[i]<-rentrez::entrez_fetch(db=db, id=res.data$ids[i], rettype="fasta"))
      if (is.null(res.data$seqs[i]) == TRUE) {res.data$seqs[i]<-NA}
      if (is.na(res.data$seqs[i])==FALSE) {
        if (verbose) message ("note ", i, " - ", res.data$ids[i], " sequence fixed")
      } else {if (verbose) message ("note ", i, " - ", res.data$ids[i], " sequence not fixed, try again")}
    }
  }
  return (res.data)
}

#' Assigns master record's id to all project records
#'
#' The function assigns the project master record's NCBI access number to all records that belong to the project.
#'
#' @param data data frame; contains information about sequence records.
#' @param ac.num.var character; data frame variable that contains sequence accession numbers.
#' @param title.var character; data frame variable that contains sequence titles.
#' @param db.var character; data frame variable that contains source data base names.
#' @param type character; type of the project which records should be united with one accession number.
#' At the moment \code{"shotgun"} is the only possible value which corresponds to the whole genome shotgun sequencing project with shotgun technology.
#' @param order logical; rearrange a data frame in alphabetical order of accession numbers (highly recommended).
#' @param new.titles logical; add new titles according to new access numbers.
#'
#' @details
#' The function looks through all records in a data frame.
#' If the record belongs to the project (for example, WGS-project), the function assigns the project master record's NCBI access number to this record.
#' If the record is not related to any project, it retains its own accession number.
#'
#' It is highly recommended to arrange the data in alphabetical order of accession numbers,
#'  since the first record among similar ones is determined as master record.
#'
#'@return
#'If \code{new.titles = FALSE} data frame with old and new access numbers is returned.
#'
#'If \code{new.titles = TRUE} data frame with old and new access numbers and new titles is returned.
#'
#'@examples
#'# Example with sequences from WGS-project of Chlamydia pneumoniae genome
#'data (meta.target) #load metadata of target sequences with GenBank identificators
#'meta.target.new.ids <- unite_NCBI_ac.nums (data = meta.target,
#'                                           ac.num.var = meta.target$GB_AcNum,
#'                                           title.var = meta.target$title,
#'                                           db.var = meta.target$source_db,
#'                                           type = "shotgun", order = TRUE,
#'                                           new.titles = TRUE)
#'
#' @author Elena N. Filatova
#' @name unite_NCBI_ac.nums
#' @export
unite_NCBI_ac.nums <- function (data, ac.num.var, title.var, db.var, type = "shotgun",
                                order = TRUE, new.titles = FALSE){
  #provide master record is always first
  if(order==FALSE) {warning ("Don't forget to order your NCBI accession numbers alphabetically")} else {
    od.nums<-order(ac.num.var)
    data<-data[od.nums,];title.var<-title.var[od.nums]; db.var<-db.var[od.nums];ac.num.var<-ac.num.var[od.nums]} # AcNum is last!!
  shotgun.nums<-grep(type, title.var) # define type rows (which are shotgun records)
  ac.nums.uniq<-rep("same", length(ac.num.var)); ac.nums.uniq[shotgun.nums]<-type # create new ac.num vector
  for (i in 1:length(ac.num.var)){ # loop every ac.nums.uniq and
    if (ac.nums.uniq[i]==type){ # if it is shortgun define its type and assign master record number. It is first so it is master record
      change.acnumber<-ac.num.var[i] # take master records number
      pattern<-"" # define serial part which is in all one project records
      if (nchar(ac.num.var[i])==11 & db.var[i]=="refseq") {pattern<-substr(x=ac.num.var[i], start = 1, stop = 5)} # refseq
      if (nchar(ac.num.var[i])==15 & db.var[i]=="refseq") {pattern<-substr(x=ac.num.var[i], start = 1, stop = 7)} # refseq
      if (nchar(ac.num.var[i])==18 & db.var[i]=="refseq") {pattern<-substr(x=ac.num.var[i], start = 1, stop = 9)} # refseq
      if (nchar(ac.num.var[i])==15 & db.var[i]=="insd") {pattern<-substr(x=ac.num.var[i], start = 1, stop = 6)} #insd
      if (nchar(ac.num.var[i])==13 & db.var[i]=="insd") {pattern<-substr(x=ac.num.var[i], start = 1, stop = 4)} #insd
      if (nchar(ac.num.var[i])==12 & db.var[i]=="insd") {pattern<-substr(x=ac.num.var[i], start = 1, stop = 4)} #insd
      if (nchar(ac.num.var[i])==8 & db.var[i]=="insd") {pattern<-substr(x=ac.num.var[i], start = 1, stop = 2)} #insd
      if (nchar(pattern)==0 & order==FALSE){stop("Unknown NCBI accession number format at row ", i)} # stop if AcNum format is unknown
      if (nchar(pattern)==0 & order==TRUE){stop("Unknown NCBI accession number format at row ", i, ". Note that rows have been sorted alphabetically by accession number")}
      nums.change<-grep(pattern, ac.num.var, fixed=TRUE) # choose all rows that are in one project
      ac.nums.uniq[nums.change]<-change.acnumber} # change their numbers for master record number
    if (ac.nums.uniq[i]=="same"){ac.nums.uniq[i]<-ac.num.var[i]}# if it is not shortgun, leave its old AcNum
  }
  # so we get ACNums vector with all records of one shotgun project get one master record AcNumber
  #returning
  if (new.titles==FALSE){ data.res<-cbind.data.frame(old.id=ac.num.var, new.id=ac.nums.uniq)}
  if (new.titles==TRUE){
    new.titles<-c(); for (i in 1:length(ac.nums.uniq)){
      new.titles[i]<- title.var[ac.num.var==ac.nums.uniq[i]]}
    data.res<-cbind.data.frame(old.id=ac.num.var, new.id=ac.nums.uniq, new.title=new.titles)
    return(data.res)}
}


#' Annotate probes
#'
#' Get genome annotation for oligonucleotide sequence
#'
#' @param source character; genome annotation source. Possible values are:
#' \code{"data.frame"} (from data frame), \code{"giff"} (from GIFF file),
#' \code{"load"} (download from NCBI with \link[biomartr]{getGFF} function)
#' @param ann.data genome annotation data frame
#' @param gff.path character; .gff file name and path
#' @param org.name character; the scientific name of the organism of interest
#' @param db character; database from which the genome shall be retrieved; possible values are \code{"refseq", "genbank", "ensembl"}
#' @param refs logical; download genome if it isn't marked in the database as either a reference or a representative genome
#' @param probe.id.var vector of probes' identification numbers
#' @param probe.start.var,probe.stop.var integer; vector of probes' start and end coordinates
#' @param file.annot character; resulting annotation file name and path
#' @param save.format character; format of resulting annotation file; possible values are \code{"txt", "csv"}
#' @param sep character;  field separator string
#' @param return character; returned object; possible values are: \code{"annotation"} (annotation data frame),
#' \code{"resume"} (annotation attributes only), \code{"add.resume"} (user's data frame with added annotation attributes)
#' @param priority character; vector of sequence ontology types that should be returned in resume in the first place
#' @param data,data.probe.id.var users data frame and probes' identification variable in it (used if \code{return = "add.resume"})
#' @param delete.downloads logical; delete files that were downloaded from NCBI
#' @param verbose logical; show messages
#'
#'@details
#'This function uses \code{boimartr} genome annotation retrieval instruments. See \link[biomartr]{getGFF} for details.
#'If retrieval is not available, GFF file may be used.
#'
#'This function creates annotation ".txt" or ".csv" file. By default file is created in working directory.
#'Optionally function returns annotation resume, i.e. annotation attribute for specified sequence ontology (SO).
#'Priorities of SOs are set by user in \code{priopity} parameter.
#'For example, if \code{priopity = c("CDS", "gene", "region")}, the function returns resume for "CDS" SO, if there are none - for
#'"gene" CO etc.
#'If there are several attributes meet \code{priority}, the first annotation attribute is returned.
#'If none of \code{priority} COs found, the first annotation attribute is returned.
#'
#'Number of found annotations are indicated in returned data (\code{"ann.n" column}).
#'
#'@return
#'Annotation data frame, or annotation attributes, or user's data frame with added annotation attributes. Also annotation file is created.
#'
#'@examples
#' path<-tempdir()
#' dir.create(path) # create temporal directory
#' data(ann.data) # load genome annotation data frame
#' annotation<-annotate_probes(source = "data.frame", ann.data = ann.data,
#'                 probe.id.var = 1:5,
#'                 probe.start.var = c (1, 100, 200, 300, 400),
#'                 probe.stop.var = c (99, 199, 299, 399, 499),
#'                 file.annot = paste0(path, "/annotation.txt"), save.format = "txt",
#'                 return = "resume")
#' file.remove(paste0(path, "/annotation.txt")) # delete files
#' unlink(path, recursive = TRUE)
#'
#' @author Elena N. Filatova
#' @name annotate_probes
#' @export
annotate_probes<-function(source = "data.frame", ann.data=NULL, gff.path=NULL,
                          org.name, db = "refseq", refs = TRUE,
                          probe.id.var, probe.start.var, probe.stop.var,
                          file.annot = NULL, save.format = "txt", sep = ";", return = "add.resume",
                          priority = c ("CDS", "gene", "region"), data, data.probe.id.var,
                          delete.downloads = FALSE, verbose = TRUE){
  # test package dependencies
  if (source != "data.frame") {if (!requireNamespace("biomartr", quietly = TRUE)) { stop("Package \"biomartr\" needed for this function to work. Please install it.", call. = FALSE)}}
  if (!requireNamespace("utils", quietly = TRUE)) { stop("Package \"utils\" needed for this function to work. Please install it.", call. = FALSE)}
  #check return and file format
  if (return!="annotation" & return!="resume" & return!="add.resume"){stop("Choose return parameter")}
  if (save.format!="txt" & save.format!="csv"){stop("Choose save.format parameter")}
  if (source!="data.frame" & source!="giff" & source!="load") {stop ("Choose annotation source")}
  #check dataframe or gff files
  if (source == "data.frame"){if (is.null(ann.data) == TRUE) {stop ("Set annotation dataframe")}}
  if (source == "giff"){
    if (is.null(gff.path) == TRUE){stop("Set .gff file name and path")}
    if (file.exists(gff.path)==FALSE){stop("There is no such GFF file")}}
  #get result file path
  if (is.null(file.annot)==TRUE){stop ("Set annotation file name and path")}
  if (file.exists(file.annot)){warning ("Note that ", file.annot, " file already exists. Overwritting file.")} # if file exists already
  #get reference genome table
  if (source =="load"){ # load from NCBI
    annot.path<-try(biomartr:: getGFF(db=db, organism=org.name, reference = refs))
    annot.table<-try(biomartr::read_gff(annot.path))
    if (class(annot.table)[1] == "try-error"){stop("Could not download annotation files. Please try again.")}}
  if (source =="giff"){annot.table<-biomartr::read_gff(gff.path)} # from giff file
  if (source =="data.frame"){annot.table<-ann.data} # from data.frame
  starts<-annot.table$start; stops<-annot.table$end
  #get references for each probe - by start and stop
  annot.data<-data.frame()
  for (i in 1:length(probe.id.var)){
    #get row numbers
    z.start<-probe.start.var[i]; z.stop<-probe.stop.var[i]
    n.starts<-which(starts<=z.start & stops>=z.start)
    n.stops<-which(stops<=z.stop & stops>=z.stop)
    nums<-unique(n.starts, n.stops)
    #get annotation data
    rows<-data.frame(); rows<-annot.table[nums,]
    rows<-cbind.data.frame("probe.id"=rep(probe.id.var[i], nrow(rows)), rows[,1:2], "ann.n"=rep(nrow(rows), nrow(rows)), rows[, 3:ncol(rows)])
    annot.data<-rbind.data.frame(annot.data, rows)}
  #save annot.data
  if (save.format=="txt"){utils::write.table(x=annot.data, file=file.annot, sep=sep)}
  if (save.format=="csv"){file.annot=paste(file.annot, ".csv", sep="")
  utils::write.table(x=annot.data, file=file.annot, sep=sep, row.names = FALSE)}
  if (verbose) message ("Annotations are saved in ", file.annot, " file.")
  #delete ncbi downloads
  if (delete.downloads==TRUE){
    file.name<-paste(getwd(), "/_ncbi_downloads", sep="")
    unlink(file.name, recursive = TRUE) }
  #return
  if (return=="annotation"){return(annot.data)} else{
    resume<-data.frame() # make resume by priority
    for (i in 1:length(probe.id.var)){
      data.small<-annot.data[annot.data$probe.id==probe.id.var[i],]
      types<-data.small$type; Type<-NULL
      for (j in 1:length(priority)){if (sum(types==priority[j])>0) {Type<-priority[j]; break()}}
      #return resume
      if (is.null(Type)==TRUE){resume[i,]<-data.small[1,]} else{
        data.small<-data.small[data.small$type==Type,]
        resume<-rbind.data.frame(resume, data.small[1,])} } # first row
    if (return=="resume"){return(resume)}
    if(return=="add.resume"){
      data.ret<-data.frame()
      for (i in 1:length(data.probe.id.var)){data.ret<-rbind.data.frame(data.ret, resume[resume$probe.id==data.probe.id.var[i],])}
      colnames(data.ret)<-paste("ann.", colnames(data.ret), sep="")
      resume<-cbind.data.frame(data, data.ret[2:ncol(data.ret)])
      return(resume)} }
}

#'Read GISAID sequence file
#'
#'Get metadata and nucleotide sequence from GISAID files
#'
#'@param dir.path character; directory name and path
#'@param return character; type of returned object; possible values are:
#'\code{"info"} (sequence metadata), \code{"seq"} (nucleotide sequences), \code{"both"} (both of them).
#'@param seq.return character; sequence returned object;  possible values are "vector", "data.frame" and "fasta"
#'@param fasta.file character; FASTA file name and path, only used if \code{return = "fasta"}
#'@param verbose logical; show messages
#'
#'@details
#'This function works with downloaded from GISAID "Input for the Augur pipeline" archives
#'(with "metadata.tsv" and "sequences.fasta" files).
#'Archives must be unzipped before usage.
#'All extracted from GISAID archive files must be in one directory.
#'
#'If \code{return = "seq"}, serial numbers are used as sequence identification numbers.
#'
#'Metadata is transformed into data frame of the same format as \link{get_seq_info} function does.
#'Sequences are transformed into data type of the same format as \link{get_seq_for_DB} function does.
#'
#'@return
#'List of length two, where first is metadata and second is nucleotide sequence.
#'If \code{return = "info"} or \code{return = "seq"} only first or second element is returned.
#'
#'@examples
#'\dontrun{
#'# First download some sequences' archives from GISAID (https://www.gisaid.org/)
#'# unzip them and put into "gisaidfiles" directory
#'
#'res <- get_GA_files (dir.path = "gisaidfiles", return = "info")
#'res <- get_GA_files (dir.path = "gisaidfiles", return = "seq", seq.return = "data.frame")
#'res <- get_GA_files (dir.path = "gisaidfiles", return ="both", seq.return = "fasta")
#'}
#'
#' @author Elena N. Filatova
#' @name get_GA_files
#' @export
get_GA_files <- function (dir.path,  return = "both", seq.return = "data.frame", fasta.file = NULL, verbose = TRUE){
  # test package dependencies
  if (!requireNamespace("utils", quietly = TRUE)) { stop("Package \"utils\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("seqinr", quietly = TRUE)) { stop("Package \"seqinr\" needed for this function to work. Please install it.", call. = FALSE)}
  #run
  res.list<-list("info"=NA, "sequenes"=NA) #future return
  #check return seqs and return
  if (seq.return!="vector" & seq.return!="data.frame" & seq.return!="fasta"){stop("Choose seq.return object")}
  if (return!="info" & return!="seq" & return!="both"){stop("Choose return parameter")}
  if (seq.return == "fasta") {
    if(is.null(fasta.file)==TRUE){stop ("Set fasta file name and path")} # fasta file name
    if (file.exists(fasta.file)){warning ("Note that FASTA file already exists. Adding sequences to file.")}}
  # get info
  if (return=="info" | return=="both"){
    meta.files<-list.files(dir.path, pattern="metadata.tsv")
    Meta.Data<-data.frame()
    for (k in 1:length(meta.files)){
      meta<-utils::read.csv(paste(dir.path, "/", meta.files[k], sep=""), header=F, sep="\n")
      meta.h<-strsplit(x=meta[1,], split = "\t", fixed=T)
      meta.h<-meta.h[[1]]
      meta.i<-strsplit(x=meta[2:nrow(meta),], split = "\t", fixed=T)
      meta.i.vec<-c()
      for (i in 1:length(meta.i)){meta.i.vec<-c(meta.i.vec, meta.i[[i]])}
      meta.data<-matrix(meta.i.vec, nrow=length(meta.i), ncol=length(meta.i[[1]]), byrow = T)
      colnames(meta.data)<-meta.h
      Meta.Data<-rbind(Meta.Data, meta.data)}
    #make info data
    GB_AcNum<-Meta.Data$genbank_accession; for (i in 1:length(GB_AcNum)){if(GB_AcNum[i]=="?"){GB_AcNum[i]=Meta.Data$gisaid_epi_isl[i]}}
    info<-data.frame(uid=Meta.Data$gisaid_epi_isl, gi= Meta.Data$gisaid_epi_isl, GB_AcNum=GB_AcNum,
                     createdate=Meta.Data$date_submitted, updatedate=Meta.Data$date_submitted,
                     source_db=rep("gisaid", nrow(Meta.Data)), organism=Meta.Data$virus,
                     title=Meta.Data$strain, strain=Meta.Data$strain, taxid=rep(NA, nrow(Meta.Data)),
                     length=Meta.Data$length, biomol=Meta.Data$segment, moltype=rep(NA, nrow(Meta.Data)),
                     genome=Meta.Data$segment, complete=rep(NA, nrow(Meta.Data)), geneticcode=rep(NA, nrow(Meta.Data)),
                     strand=rep(NA, nrow(Meta.Data)), host=Meta.Data$host, country=paste(Meta.Data$country, Meta.Data$location, sep=": "),
                     isolation_source=rep(NA, nrow(Meta.Data)), collection_date=Meta.Data$date)
    res.list[[1]]<-info} # return info
  #02 get fastas
  if (return=="seq" | return=="both"){
    fasta.files<-list.files(dir.path, pattern="sequences.fasta")
    seq.list<-list()
    for (i in 1:length(fasta.files)){seq.list[[i]]<-seqinr::read.fasta(file=paste(dir.path, "/", fasta.files[i], sep=""), as.string = TRUE)}
    seqs<-unlist(seq.list)
    #make seq data
    if (return=="both"){ids<-info$uid};  if (return=="seq"){ids<-1:length(seqs)}
    seqs<-paste(">", ids, " _ ", names(seqs), "\n", seqs, sep="")
    Seq.Data<-data.frame(ids=ids, seqs=seqs)
    # return fastas
    if (seq.return=="vector"){res.list[[2]]<-Seq.Data$seqs}
    if(seq.return=="data.frame"){res.list[[2]]<-Seq.Data}
    if (seq.return=="fasta"){
      write(x=Seq.Data$seqs, file=fasta.file, append = TRUE)
      res.list[[2]]<-fasta.file
      if (verbose) message ("FASTA file is in ", fasta.file)}}
  #return
  if(return=="info"){return(res.list[[1]])}
  if(return=="seq"){return(res.list[[2]])}
  if(return=="both"){return(res.list)}}


