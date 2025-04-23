# ------------------------------------------------------------------------------
# Allele clusters functions
# The functions in this scripts are for generating the allele clusters from a given reference set

#' @include piglet.R
#' @include utils.R
NULL


## TODO: decide if a class is needed

# ------------------------------------------------------------------------------
#' zenodoArchive
#' @docType class
#' @return Object of \code{R6Class} for modelling an zenodoArchive for ASC cluster files
#' @format \code{R6Class} object.
#'
#' @examples
#' \donttest{
#'   zenodo_archive <- zenodoArchive$new(
#'      doi = "10.5281/zenodo.7401189"
#'   )
#'
#'   # view available version ins the archive
#'   archive_versions <- zenodo_archive$get_versions()
#'
#'   # Getting the available files in the latest zenodo archive version
#'   files <- zenodo_archive$get_version_files()
#'
#'   # downloading the first file from the latest archive version
#'   zenodo_archive$download_zenodo_files()
#' }
#' @export
zenodoArchive <- R6::R6Class(
  "zenodoArchive",
  list(
    #' @field doi zenodoArchive doi, NULL is not supplied
    doi = NULL,
    #' @field all_versions zenodoArchive if to return all versions, `true` when not specified
    all_versions = "true",
    #' @field sort zenodoArchive how to sort the records, `mostrecent` when not specified
    sort = "mostrecent",
    #' @field page zenodoArchive which page to pull in query, `1` when not specified
    page = 1,
    #' @field size zenodoArchive how many records per page,  `20` when not specified
    size = 20,
    #' @field zenodoVersions zenodoArchive doi available version, a storing variable.
    zenodoVersions = NULL,
    #' @field zenodoQuery zenodoArchive doi version query, a storing variable.
    zenodoQuery = NULL,
    #' @field download_file zenodoArchive doi downloads files, a storing variable.
    download_file = NULL,
    #' @field download_url zenodoArchive doi downloads urls, a storing variable.
    download_url = NULL,
    
    
    #' @description initializes the zenodoArchive
    #' @param doi A zenodo doi. To retrieve all records supply a concept doi (a generic doi common to all versions).
    #' @param page Which page to query. Default is 1
    #' @param size How many records per page. Default is 20
    #' @param all_versions If to return all concept doi versions. If `true` returns all, if `false` returns the latest. Default is `ture`
    #' @param sort Which sorting to apply on the records. Default is `mostrecent`. Possible sortings "bestmatch", "mostrecent", "-mostrecent" (ascending), "version", "-version" (ascending).
    initialize = function(doi,
                          page = 1,
                          size = 20,
                          all_versions = "true",
                          #c("true","false"),
                          sort = "mostrecent") {
      # c("bestmatch","mostrecent","-mostrecent","version","-version")
      stopifnot(is.character(doi), length(doi) == 1)
      stopifnot(is.character(all_versions),
                length(all_versions) == 1,
                any(all_versions %in% c("true", "false")))
      stopifnot(is.character(sort),
                length(sort) == 1,
                any(
                  sort %in% c(
                    "bestmatch",
                    "mostrecent",
                    "-mostrecent",
                    "version",
                    "-version"
                  )
                ))
      stopifnot(is.numeric(page), length(page) == 1)
      stopifnot(is.numeric(size), length(size) == 1)
      self$doi <- doi
      self$all_versions <- all_versions
      self$sort <- sort
      self$size <- size
      self$page <- page
      self$zenodoVersions <- NULL
      self$zenodoQuery <- NULL
      self$download_file <- NULL
      self$download_url <- NULL
    },
    #' @description cleans the doi record for query
    #' @param doi  The zenodo archive doi
    #' @return the clean doi
    clean_doi = function(doi = self$doi) {
      gsub("10.5281/zenodo.", "", doi, fixed = T)
    },
    #' @description Query the zenodo archive according to the initial parameters.
    #' @param ...   Excepts the self created by `initialize`
    #' @return a list with the query values.
    zenodo_query = function(...) {
      url <-
        sprintf(
          'https://zenodo.org/api/records/?q=conceptrecid:%s&all_versions=%s&sort=%s&page=%s&size=%s',
          self$clean_doi(),
          self$all_versions,
          self$sort,
          self$page,
          self$size
        )
      version_query <- jsonlite::read_json(url)
      
      if (length(version_query$hits$hits) == 0) {
        # try querying the doi. If succeeded doi supplied is not conceptrecid, if fails then doi is incorect
        url <-
          sprintf(
            'https://zenodo.org/api/records/?q=%s&all_versions=%s&sort=%s&page=%s&size=%s',
            self$clean_doi(),
            self$all_versions,
            self$sort,
            self$page,
            self$size
          )
        
        version_query <- jsonlite::read_json(url)
        
        stopifnot("doi supplied does not match any zenodo record" = length(version_query$hits$hits) !=
                    0)
        
        message(
          sprintf(
            "doi supplied is not an 'all versions doi'\nfor viewing all of the archive records change the doi to:%s",
            version_query$hits$hits[[1]]$conceptdoi
          )
        )
      }
      self$zenodoQuery <- version_query
      self$zenodoQuery
    },
    #' @description Extract all concept doi available versions.
    #' @param ...   Excepts the self created by `initialize`
    #' @return a data.frame of the available versions.
    get_versions = function(...) {
      req <-
        if (is.null(self$zenodoQuery))
          self$zenodo_query()
      else
        self$zenodoQuery
      self$zenodoVersions <-
        do.call(rbind,
                sapply(req$hits$hits, function(x)
                  data.frame(
                    doi = x$metadata$doi,
                    version = x$metadata$version,
                    date = x$metadata$publication_date
                  ), simplify = F))
      self$zenodoVersions
    },
    #' @description get the chosen doi archive version available files
    #' @param version which archive version files to get. Default to latest. To see all available version use `get_versions`
    #' @return a list of the available files in the archive version.
    get_version_files = function(version = "latest") {
      versions <-
        if (is.null(self$zenodoVersions))
          self$get_versions()
      else
        self$zenodoVersions
      
      doi_version <-
        if (version == "latest")
          versions$doi[rev(order(versions$date))][1]
      else
        versions$doi[self$zenodoVersions$version == version]
      
      query <-
        jsonlite::fromJSON(paste0(
          "https://zenodo.org/api/records/",
          self$clean_doi(doi_version)
        ))
      self$download_url <- query$files$links$self
      self$download_file <- basename(query$files$key)
      self$download_file
    },
    #' @description get the chosen doi archive version available files
    #' @param file If supplied, downloads the specific file from the archive.
    #' @param path The output folder for saving the archive files. Default is to a temporary directory.
    #' @param version which archive version files to get. Default to latest. To see all available version use `get_versions`
    #' @param get_file_path Logical (FALSE by default). Do you want to return the path for the file downloaded.
    #' @param all_files Logical (FALSE by default). Do you want to download all files in the archive.
    #' @return If `get_file_path` is TRUE, the function returns the path to the archive file
    #' @param quite     Logical (FALSE by default). Do you want to suppress informative messages
    download_zenodo_files = function(file = NULL,
                                     path = tempdir(),
                                     version = "latest",
                                     all_files = F,
                                     get_file_path = F,
                                     quite = F) {
      if (is.null(self$download_url)) {
        invisible(self$get_version_files(version))
        url <- self$download_url
      } else{
        url <- self$download_url
      }
      if (!is.null(file)) {
        if (is.na(match(file, self$download_file))) {
          message(paste0("Input file is not found in chosen version "))
          if (!is.null(self$download_file))
            message(paste0(
              "The available file are: ",
              paste0(self$download_file, collapse = ",")
            ))
          stop()
        }
        
        url <- grep(file, url, fixed = T, value = T)
      } else{
        file <- self$download_file
      }
      if (all_files) {
        lapply(1:length(file), function(i)
          download.file(url = url[1], file.path(path, file[i])), quite = quite)
        
        if (get_file_path)
          file.path(path, file)
      } else{
        download.file(url[1],  file.path(path, file[1]), quite = quite)
        if (get_file_path)
          file.path(path, file[1])
      }
      
      
    }
  )
)

#' Retrieving allele similarity clusters Zenodo archive
#' 
#' A wrapper function for `zenodoArchive`, download the most recent allele similarity clusters and thresholds from the zenodo archive.
#' The clusters and thresholds are based on \url{https://yaarilab.github.io/IGHV_reference_book/}
#' At the moment only available for human IGHV reference set.
#'
#' @param doi       The doi for the archive to download. Default is the IGHV set.
#' @param path      The output folder for saving the archive files. Default is to a temporary directory.
#' @param get_file  Logical (FALSE by default). Do you want to return the path for the file downloaded.
#' @param quite     Logical (FALSE by default). Do you want to suppress informative messages
#'
#' @return
#'
#' If get_file is TRUE, the function returns the path to the archive file
#'
#' @examples
#' 
#' \donttest{
#' recentAlleleClusters(doi="10.5281/zenodo.7401189")
#' }
#'
#' @export
recentAlleleClusters <-
  function(doi = "10.5281/zenodo.7401189",
           path,
           get_file = FALSE,
           quite = FALSE) {
    if (missing(path)) {
      path <- tempdir()
      if (!quite)
        message(paste0("Files will be downloaded to tmp directory: ", path))
    }
    
    zenodo_archive <- zenodoArchive$new(doi = doi)
    output <-
      zenodo_archive$download_zenodo_files(path = path,
                                           quite = quite,
                                           get_file_path = get_file)
    
    if (get_file)
      return(output)
  }

#' Extracts the allele cluster table from the archive file.
#'
#' @param archive_file            A path to the asc archive file. Default is null. (see details)
#'
#' @details
#'
#' For downloading the latest archive file with the updated allele cluster table, use the function \code{recentAlleleClusters}.
#'
#' @return
#'
#' Returns the allele cluster table.
#'
#' The table columns:
#' `new_allele` - the ASC given allele name
#' `func_group` - the ASC cluster number
#' `imgt_allele` - the original IUIS/IMGT allele name
#' `thresh` - the allele threshold for ASC-based genotype inference
#' `amplicon_length` - is the original length of the reference set.
#' 
#' @examples
#' 
#' \donttest{
#' asc_archive <- recentAlleleClusters(doi="10.5281/zenodo.7429773", get_file = TRUE)
#'
#' allele_cluster_table <- extractASCTable(archive_file = asc_archive)
#' }
#' 
#'
#' @export

extractASCTable <- function(archive_file = NULL) {
  . <- NULL
  
  if (!is.null(archive_file)) {
    master <- as.character(unzip(archive_file, list = TRUE)$Name)
    allele_cluster_table <-
      read.delim(unz(
        archive_file,
        grep("asc_alleles_table.tsv", master, value = T)
      ))
  }
  
  setDT(allele_cluster_table)
  allele_cluster_table <- allele_cluster_table[, .("imgt_allele" = unlist(strsplit(get("imgt_allele"),"/"))), by = setdiff(names(allele_cluster_table), "imgt_allele")]
  
  return(allele_cluster_table)
}

#' Converts IGHV germline set to ASC germline set.
#'
#' @param allele_cluster_table    The allele cluster table.
#' @param germline                An IGHV germline set with matching names to the "imgt_allele" column in the allele_cluster_table.
#'
#' @return
#'
#' Returns the IGHV germline set with the ASC allele names.
#'
#' @examples
#' 
#' # preferably obtain the latest ASC cluster table
#' # asc_archive <- recentAlleleClusters(doi="10.5281/zenodo.7429773", get_file = TRUE)
#'
#' # allele_cluster_table <- extractASCTable(archive_file = asc_archive)
#'
#' data(HVGERM)
#'
#' # example allele similarity cluster table
#' data(allele_cluster_table)
#'
#' asc_germline <- germlineASC(allele_cluster_table, germline = HVGERM)
#'
#' 
#'
#' @export
germlineASC <- function(allele_cluster_table, germline) {
  . <- NULL
  
  setDT(allele_cluster_table)
  if (any(grepl("/", allele_cluster_table$imgt_allele))) {
    allele_cluster_table <- allele_cluster_table[, .("imgt_allele" = unlist(strsplit(get("imgt_allele"),"/"))), by = setdiff(names(allele_cluster_table), "imgt_allele")]
  }
  
  germline_asc <- germline[allele_cluster_table$imgt_allele]
  
  asc_alleles <-
    setNames(allele_cluster_table$new_allele,
             allele_cluster_table$imgt_allele)
  
  names(germline_asc) <- asc_alleles[names(germline_asc)]
  
  germline_asc <- germline_asc[!duplicated(germline_asc)]
  
  return(germline_asc)
}


# ------------------------------------------------------------------------------

#' Assign allele similarity clusters
#' 
#' \code{assignAlleleClusters} uses the allele clusters annotation to change the preliminary allele
#' assignments to the new annotations before inferring a genotype.
#'
#' @param data                  data.frame in AIRR format, containing V allele calls from a single subject and the sample IMGT-gapped V(D)J sequences under seq.
#' @param v_call                name of the V allele call column. Default is `v_call`
#' @param alleleClusterTable    A data.frame of the allele clusters new annotations relative to the original reference set. See details.
#'
#' @return
#' A modified input \code{data.frame} with the new assigned
#'
#'@examples
#'
#'
#' # preferably obtain the latest ASC cluster table
#' # asc_archive <- recentAlleleClusters(doi="10.5281/zenodo.7429773", get_file = TRUE)
#'
#' # allele_cluster_table <- extractASCTable(archive_file = asc_archive)
#'
#' # example allele similarity cluster table
#' data(allele_cluster_table)
#'
#' # loading TIgGER AIRR-seq b cell data
#' data <- tigger::AIRRDb
#'
#' asc_data <- assignAlleleClusters(data, allele_cluster_table)
#'
#'
#' @export
assignAlleleClusters <-
  function(data, alleleClusterTable, v_call = "v_call") {
    . <- NULL
    
    setDT(alleleClusterTable)
    if (any(grepl("/", alleleClusterTable$imgt_allele))) {
      alleleClusterTable <- alleleClusterTable[, .("imgt_allele" = unlist(strsplit(get("imgt_allele"),"/"))), by = setdiff(names(alleleClusterTable), "imgt_allele")]
    }
    
    # set the dictionary
    germline_set <-
      setNames(alleleClusterTable$new_allele,
               alleleClusterTable$imgt_allele)
    
    # switch the assignments
    data[[v_call]] <- sapply(data[[v_call]], function(x) {
      calls <- unlist(strsplit(x, ","))
      calls <- germline_set[calls]
      calls <- calls[!duplicated(calls)]
      paste0(calls, collapse = ",")
    }, USE.NAMES = F)
    
    return(data)
  }

# ------------------------------------------------------------------------------

#' Allele based genotype inference
#' 
#' \code{inferGenotypeAllele} infer an individual's genotype based on the allele-base method.
#' The method utilize the allele specific threshold to determine the presence of an allele in the genotype.
#' More specifically, based on the allele frequency, repertoire depth, and the specific allele threshold, a confidence level (Z score) is calculated 
#' for the presence of the allele in the genotype. The user can select the confidence level for the genotype inference.
#'
#' @param data                     data.frame in AIRR format, containing allele calls from a single subject and the sample IMGT-gapped V(D)J sequences under seq.
#' @param allele_threshold_table   A data.frame of the alleles and their thresholds. 
#' @param call                     name of the V,D, or J allele call column, i.e v_call, d_call, j_call. Default is `v_call`
#' @param asc_annotation           Logical (FALSE by default). Are the allele calls annotated with the allele similarity clusters.
#' @param translate_to_asc         For V allele calls, collapse identical allele for the genotype inference. Default is FALSE.
#' @param single_assignment        if TRUE, the method only considers sequence with single assignment for the genotype inference.
#' @param germline_db              named vector of sequences containing the germline sequences named in V allele calls and the alleleClusterTable. Only required if find_unmutated is TRUE.
#' @param find_unmutated           if TRUE, use germline_db to find which samples are unmutated. Not needed if V allele calls only represent unmutated samples.
#' @param seq                      name of the column in data with the aligned, IMGT-numbered, V(D)J nucleotide sequence. Default is sequence_alignment.
#' @param default_allele_threshold The default allele threshold for the genotype inference, in case the allele threshold is not in the `allele_threshold_table`. Default is 1e-04.
#' @param quiet                    Logical (TRUE by default). Do you want to suppress informative messages
#' @return
#' A a data.frame with the inferred V genotype. The table contains the following columns:
#' 
#' - allele: The alleles in the `allele_threshold_table`.
#' - counts: The number of reads for each alleles.
#' - depth: The total number of reads in the genotype (Sum of counts).
#' - threshold: The population driven allele thresholds for genotype presence.
#' - z_score: The confidence level for the presence of the allele in the genotype.
#' - asc_allele: If `translate_to_asc` is true, the asc allele value from allele_threshold_table.
#'
#' @details
#'
#' In naive repertoires, allele calls where more than one assignment is assigned is rare. Hence, in case the data represents the naive repertoire of a subject
#' it is recommended to use the `find_unmutated=TRUE` option, to remove mutated sequences. For non-naive population, the allele calls in cases of multiple assignment
#' are treated as belonging to all groups.
#'
#' @seealso
#'
#' \link{inferAlleleClusters} will infer the allele clusters based on a supplied V reference set and set the default allele threshold of 1e-04.
#' See \link{recentAlleleClusters} to obtain the latest version of the IGHV allele clusters and the naive population based allele threshold.
#'
#'@examples
#'
#'
#' # loading TIgGER AIRR-seq b cell data
#' data <- tigger::AIRRDb
#'
#' # allele threshold table
#' data(allele_threshold_table)
#'
#' data(HVGERM)
#'
#' # inferring the genotype
#' genotype <- inferGenotypeAllele(
#' data = data,
#' allele_threshold_table = allele_threshold_table,
#' germline_db = HVGERM, find_unmutated=TRUE)
#' 
#' # filter alleles with z_score >= 0 
#' 
#' head(genotype[genotype$z_score >= 0,])
#'
#' @export
# Parts are adapted from tigger::inferGenotype
inferGenotypeAllele <-
  function(data,
           allele_threshold_table=NULL,
           call = "v_call",
           asc_annotation = FALSE,
           single_assignment = FALSE,
           translate_to_asc = FALSE,
           germline_db = NA,
           find_unmutated = FALSE,
           seq = "sequence_alignment",
           default_allele_threshold = 1e-04,
           quiet = TRUE) {
    . = NULL
    .. = NULL
    
    # only clean the allele calls if they don't start with IG/TR
    if (any(!grepl("IG|TR", data[[call]]))) {
      allele_calls <- clean_allele_calls(data[[call]])
    }else{
      allele_calls <- data[[call]]
    }
    
    unique_calls <- unique(unlist(allele_calls))
    allele_calls <- sapply(allele_calls, function(x) paste0(x, collapse = ","))
    segment <- unique(substr(unique_calls,4,4))
    ## check that the allele calls are in the supplied allele_threshold_table
    if(is.null(allele_threshold_table)){
      ## load the default allele threshold table
      data(allele_threshold_table, envir = environment())
    }
    
    if(!"tag" %in% names(allele_threshold_table)){
      allele_threshold_table[,"tag":=substr(get("allele"),4,4)]
    }
    
    asc_col <- "allele"
    if(asc_annotation){
      asc_col <- "asc_allele"
    }
    
    allele_threshold_table <- allele_threshold_table[get("tag") %in% segment]
    match <- unique_calls %in% allele_threshold_table[[asc_col]]
    if (!all(match)) {
      for(allele in unique_calls[!match]) {
        if(!quiet) warning(paste0("The allele call ", allele, " is not in the allele threshold table. Using the default threshold: ", default_allele_threshold))
        allele_threshold_table <- rbind(allele_threshold_table, 
                                    data.frame(
                                      "tag" = substr(allele,4,4),
                                      "allele" = allele,
                                      "asc_allele" = allele,
                                      "threshold" = default_allele_threshold
                                      ))
      }
    }
    
    allele_threshold_table[,"genotyped_allele":=get(asc_col)]
    
    base_count <- 1/length(unique(allele_threshold_table[["genotyped_allele"]]))
    
    ## check unmutated
    if (find_unmutated) {
      if (is.na(germline_db[1])) {
        stop("germline_db needed if find_unmutated is TRUE")
      }
      allele_calls <-
        tigger::findUnmutatedCalls(allele_calls, as.character(data[[seq]]),
                                   germline_db)
      if (length(allele_calls) == 0) {
        stop("No unmutated sequences found! Set 'find_unmutated' to 'FALSE'.")
      }
    }
    
    genotype_dt <- data.table::data.table(
      "genotyped_allele" = allele_calls)
    
    if(single_assignment){
      genotype_dt <- genotype_dt[!grepl(",",get("genotyped_allele")),]    
    }
    
    genotype_dt[,"multiple":=1/(stringi::stri_count_fixed(get("genotyped_allele"), ",")+1)]
    genotype_dt[,"nrow":=1:.N]
    genotype_dt <- genotype_dt[, .("genotyped_allele" = unlist(strsplit(get("genotyped_allele"), ","))), by = mget(c("multiple", "nrow"))]
    genotype_dt[,"single_assignments":=sum(get("multiple")==1), by = .(get("genotyped_allele"))]
    genotype_dt[,"multiple_assignments_wheight":=ifelse(get("single_assignments") == 0,
                                                        base_count,
                                                        get("single_assignments") / length(unique(allele_threshold_table$allele))),
                by = mget(c("genotyped_allele", "nrow"))]
    genotype_dt[get("multiple")==1, "multiple_assignments_wheight":=1]
    genotype_dt[,"fraction":=get("multiple")*get("multiple_assignments_wheight")]
    genotype_dt <- genotype_dt[,.("count" = sum(get("fraction"))), by = mget(c("genotyped_allele"))]
    genotype_dt <- merge(allele_threshold_table, genotype_dt, by = c("genotyped_allele"), all.x = T, all.y=F)
    genotype_dt[,"gene" := alakazam::getGene(
      get("genotyped_allele"),
      first = F,
      collapse = T,
      strip_d = F
    )]
    ## if translate_to_asc, then collapse similar alleles by asc.
    final_columns <- c("gene","genotyped_allele", "count", "depth", "threshold", "z_score")
    if(translate_to_asc){
      genotype_dt <- genotype_dt[,.(
        "gene" = paste0(unique(get("gene")),collapse = "/"),
        "genotyped_allele" = paste0(get("genotyped_allele"),collapse = "/"),
        "count" = sum(get("count"), na.rm = T),
        "threshold" = min(get("threshold"))
      ), by =.(get("asc_allele"))]
      final_columns <- c("gene","genotyped_allele", "count", "depth", "threshold", "z_score")
    }
    ## add base counts
    genotype_dt[is.na(get("count")), "count" := base_count]
    genotype_dt[, "depth" := sum(get("count"))]
    
    ## get the z_score
    genotype_dt[, "z_score" := z_score(get("count"), get("depth"), get("threshold"))]
    genotype_dt <- genotype_dt[, .SD, .SDcols = final_columns]
    names(genotype_dt)[2] <- "allele"
    return(genotype_dt)
}

# ------------------------------------------------------------------------------

#' Allele similarity cluster based genotype inference Testing function
#' 
#' \code{inferGenotypeAllele_asc} infer an individual's genotype based on the allele-base method.
#' The method utilize the allele specific threshold to determine the presence of an allele in the genotype.
#' More specifically, the absolute frequency of each allele is calculated and checked against the threshold.
#'
#' @param data                     data.frame in AIRR format, containing V allele calls from a single subject and the sample IMGT-gapped V(D)J sequences under seq.
#' @param alleleClusterTable       A data.frame of the allele similarity clusters thresholds.
#' @param v_call                   name of the V allele call column. Default is `v_call`
#' @param single_assignment        if TRUE, the method only considers sequence with single assignment for the genotype inference.
#' @param germline_db              named vector of sequences containing the germline sequences named in V allele calls and the alleleClusterTable. Only required if find_unmutated is TRUE.
#' @param find_unmutated           if TRUE, use germline_db to find which samples are unmutated. Not needed if V allele calls only represent unmutated samples.
#' @param seq                      name of the column in data with the aligned, IMGT-numbered, V(D)J nucleotide sequence. Default is sequence_alignment.
#' @param confidence_level         The confidence level on which to filter the inferred genotype alleles. Default is NULL, meaning filtering only based on allele threshold.
#' @param default_allele_threshold The default allele threshold for the genotype inference, in case the allele threshold is not in the `alleleClusterTable`. Default is 1e-04.
#'
#' @return
#' A a data.frame with the inferred V genotype. The table contains the following columns:
#' 	|gene            | alleles             | imgt_alleles          | counts              | absolute_fraction     | absolute_threshold               | genotyped_alleles    | genotype_imgt_alleles|
#'  |----------------|---------------------|-----------------------|---------------------|-----------------------|----------------------------------|----------------------|----------------------|
#'  | allele cluster | the present alleles | the imgt nomenclature | the number of reads | the absolute fraction | the population driven allele     | the alleles which    | the imgt nomenclature|
#'  |                | in the repertoire   | of the alleles        | for each alleles    | of the alleles        | thresholds for genotype presence | entered the genotype | of the alleles 		  |
#'
#' @details
#'
#' In naive repertoires, allele calls where more than one assignment is assigned is rare. Hence, in case the data represents the naive repertoire of a subject
#' it is recommended to use the `find_unmutated=TRUE` option, to remove mutated sequences. For non-naive population, the allele calls in cases of multiple assignment
#' are treated as belonging to all groups.
#'
#' @seealso
#'
#' \link{inferAlleleClusters} will infer the allele clusters based on a supplied V reference set and set the default allele threshold of 1e-04.
#' See \link{recentAlleleClusters} to obtain the latest version of the IGHV allele clusters and the naive population based allele threshold.
#'
#'@examples
#'
#'
#' # loading TIgGER AIRR-seq b cell data
#' data <- tigger::AIRRDb
#'
#' # preferably obtain the latest ASC cluster table
#' # asc_archive <- recentAlleleClusters(doi="10.5281/zenodo.7429773", get_file = TRUE)
#'
#' # allele_cluster_table <- extractASCTable(archive_file = asc_archive)
#'
#' # example allele similarity cluster table
#' data(allele_cluster_table)
#'
#' data(HVGERM)
#'
#' # reforming the germline set
#' asc_germline <- germlineASC(allele_cluster_table, germline = HVGERM)
#'
#' # assigning the ASC alleles
#' asc_data <- assignAlleleClusters(data, allele_cluster_table)
#'
#' # inferring the genotype
#' asc_genotype <- inferGenotypeAllele_asc(
#' data = asc_data,
#' alleleClusterTable = allele_cluster_table,
#' germline_db = asc_germline, find_unmutated=TRUE)
#' 
#' @export
inferGenotypeAllele_asc <- function(data,
         alleleClusterTable,
         v_call = "v_call",
         single_assignment = FALSE,
         germline_db = NA,
         find_unmutated = FALSE,
         seq = "sequence_alignment",
         confidence_level = NULL,
         default_allele_threshold = 1e-04) {
  . = NULL
  
  allele_calls = clean_allele_calls(data[[v_call]])
  
  ## check that the allele calls are in the supplied alleleClusterTable
  unique_calls <- unique(unlist(allele_calls))
  match <- unique_calls %in% alleleClusterTable$new_allele
  if (!all(match)) {
    
    for(allele in unique_calls[!match]) {
      warning(paste0("The allele call ", allele, " is not in the alleleClusterTable. Using the default threshold: ", default_allele_threshold))
      
      alleleClusterTable <- rbind(alleleClusterTable, 
                                  data.frame(new_allele = allele,
                                             func_group = strsplit(allele,'[*]')[[1]][1], 
                                             imgt_allele = allele,
                                             thresh = default_allele_threshold))
    }
    
  }
  
  ## check unmutated
  if (find_unmutated) {
    if (is.na(germline_db[1])) {
      stop("germline_db needed if find_unmutated is TRUE")
    }
    allele_calls <-
      tigger::findUnmutatedCalls(allele_calls, as.character(data[[seq]]),
                                 germline_db)
    if (length(allele_calls) == 0) {
      stop("No unmutated sequences found! Set 'find_unmutated' to 'FALSE'.")
    }
  }
  
  geno_V <- data.table::data.table(v_call = allele_calls)
  
  geno_V <-
    geno_V[, "gene" := alakazam::getGene(
      get("v_call"),
      first = F,
      collapse = T,
      strip_d = F
    )]
  
  # removing multiple gene assignments
  geno_V <- geno_V[!grepl(",", get("gene"))]
  
  # clean the allele class
  geno_V[, "v_allele" :=
           gsub(
             "(IG[HKL][VDJADEGMC]|TR[ABDG])[A-Z0-9\\(\\)]+[-/\\w]*[*]",
             "",
             get("v_call"),
             perl = T
           )]
  
  ## get single assignments
  if (single_assignment) {
    geno_V <- geno_V[!(grepl(",", get("v_allele")))]
    geno_V <- geno_V[!is.na(get("v_allele"))]
    geno_V[, "n_row_sub" := .N]
    geno_V[, "frac" := 1]
    geno_V_fraction <-
      geno_V[, .("absolute_fraction" = round(sum(get("frac")) / unique(get("n_row_sub")), 8),
                 "count" = sum(get("frac"))), by = list(get("gene"), get("v_allele"))]
  } else{
    ### distribute the multiple assignments for non naive sequences
    geno_V <- geno_V[!is.na(get("v_allele"))]
    geno_V[, "n_row_sub" := .N]
    geno_V[, "frac" := 1]
    geno_V <-
      geno_V[, .("count" = sum(get("frac"))), by = mget(c("gene", "v_allele", "n_row_sub"))]
    
    n_row_sub <- unique(geno_V$n_row_sub)
    
    geno_V_fraction <- c()
    #### code from TIgGER inferGentoype
    for (g in unique(geno_V$gene)) {
      ac <- geno_V[get("gene") == g, get("v_allele")]
      t_ac <-
        setNames(geno_V[get("gene") == g, get("count")], geno_V[get("gene") == g, get("v_allele")]) # table of allele calls
      potentials <-
        unique(unlist(strsplit(names(t_ac), ","))) # potential alleles
      
      regexpotentials <-
        paste(gsub("\\*", "\\\\*", potentials), "$", sep = "")
      regexpotentials <-
        paste(regexpotentials, gsub("\\$", ",", regexpotentials), sep =
                "|")
      tmat <-
        sapply(regexpotentials, function(x)
          grepl(x, names(t_ac), fixed = FALSE))
      
      if (length(potentials) == 1 | length(t_ac) == 1) {
        seqs_expl <-
          t(as.data.frame(apply(t(as.matrix(tmat)), 2, function(x)
            x *
              t_ac)))
        rownames(seqs_expl) <- names(t_ac)[1]
      } else{
        seqs_expl <- as.data.frame(apply(tmat, 2, function(x)
          x *
            t_ac))
      }
      #       seqs_expl = as.data.frame(apply(tmat, 2, function(x) x*t_ac))
      colnames(seqs_expl) <- potentials
      # Add low (fake) counts
      sapply(colnames(seqs_expl), function(x) {
        if (sum(rownames(seqs_expl) %in% paste(x)) == 0) {
          seqs_expl <<- rbind(seqs_expl, rep(0, ncol(seqs_expl)))
          
          rownames(seqs_expl)[nrow(seqs_expl)] <<- paste(x)
          seqs_expl[rownames(seqs_expl) %in% paste(x), paste(x)] <<-
            0.01
          
        }
      })
      
      # Build ratio dependent allele count distribution of multi assigned reads
      seqs_expl_single <-
        seqs_expl[grep(',', rownames(seqs_expl), invert = T),]
      
      seqs_expl_multi <-
        seqs_expl[grep(',', rownames(seqs_expl), invert = F),]
      if (is.null(nrow(seqs_expl_multi))) {
        seqs_expl_multi <- t(as.data.frame(seqs_expl_multi))
        rownames(seqs_expl_multi) <-
          grep(',',
               rownames(seqs_expl),
               invert = F,
               value = T)
      }
      
      if (!is.null(nrow(seqs_expl_single))  &&
          nrow(seqs_expl_single) != 0 &&
          nrow(seqs_expl_single) != nrow(seqs_expl)) {
        if (nrow(seqs_expl_multi) > 1) {
          seqs_expl_multi <-
            seqs_expl_multi[order(nchar(row.names(seqs_expl_multi))),]
        }
        sapply(1:nrow(seqs_expl_multi), function(x) {
          genes <- unlist(strsplit(row.names(seqs_expl_multi)[x], ','))
          
          counts <-
            seqs_expl_single[rownames(seqs_expl_single) %in% genes, genes]
          counts <- colSums(counts)
          counts_to_distribute <- seqs_expl_multi[x, genes]
          
          new_counts <-
            counts + ((counts_to_distribute * counts) / sum(counts))
          for (i in 1:length(new_counts)) {
            gene_tmp <- names(new_counts)[i]
            seqs_expl_single[rownames(seqs_expl_single) %in% gene_tmp, gene_tmp] <<-
              new_counts[i]
          }
        })
      }
      
      seqs_expl <-
        if (is.null(nrow(seqs_expl_single)) ||
            nrow(seqs_expl_single) == 0) {
          seqs_expl
        } else{
          seqs_expl_single
        }
      seqs_expl <- round(seqs_expl)
      if (sum(rowSums(seqs_expl) == 0) != 0) {
        seqs_expl <- seqs_expl[rowSums(seqs_expl) != 0, ]
      }
      
      allele_tot <-
        sort(apply(seqs_expl, 2, sum), decreasing = TRUE)
      
      gene_table <-
        data.table::data.table(
          "gene" = g,
          "v_allele" = names(allele_tot),
          "count" = allele_tot,
          "n_row_sub" = n_row_sub
        )
      gene_table <- gene_table[get("count") != 0]
      
      geno_V_fraction <- dplyr::bind_rows(geno_V_fraction, gene_table)
    }
    ############
    geno_V_fraction <-
      geno_V_fraction[, .("absolute_fraction" = round(get("count") / unique(get("n_row_sub")), 8),
                          "count" = get("count")), by = mget(c("gene", "v_allele"))]
    
  }
  
  ## add original allele and cut off
  geno_V_fraction[, "v_call" := paste0(get("gene"), "*", get("v_allele"))]
  
  if (any(duplicated(alleleClusterTable$new_allele))) {
    alleleClusterTable <- setDT(alleleClusterTable)
    
    alleleClusterTable <-
      alleleClusterTable[, .("imgt_allele" = paste0(sort(unlist(unique(mget(
        c("imgt_allele")
      )))), collapse = "/")), by = mget(names(alleleClusterTable)[names(alleleClusterTable) !=
                                                                    "imgt_allele"])]
  }
  
  alleles_clusters <-
    setNames(alleleClusterTable$imgt_allele,
             alleleClusterTable$new_allele)
  geno_V_fraction[, "v_call_or" := alleles_clusters[get("v_call")]]
  
  
  na_id <- which(is.na(geno_V_fraction$v_call_or))
  if (length(na_id) != 0)
    geno_V_fraction$v_call_or[na_id] <-
    sapply(na_id, function(i) {
      new_allele <- geno_V_fraction$v_call[i]
      closest <- strsplit(geno_V_fraction$v_call[i], "_")[[1]][1]
      or_allele <- alleles_clusters[closest]
      paste0(or_allele, gsub(closest, "", new_allele, fixed = T))
    })
  
  allele_cluster_threshold <-
    setNames(as.numeric(alleleClusterTable$thresh), alleleClusterTable$new_allele)
  geno_V_fraction <-
    geno_V_fraction[, "absolute_thresh" := allele_cluster_threshold[get("v_call")]]
  
  z_score <- function(Nai, N, Tai) (Nai - Tai*N) / sqrt(Tai*N*(1-Tai))
  
  geno_V_fraction <-
    geno_V_fraction[, "genotype_confidence" := z_score(Nai=get("count"),N=n_row_sub,Tai=get("absolute_thresh"))]
  
  na_id <- which(is.na(geno_V_fraction$absolute_thresh))
  if (length(na_id) != 0)
    geno_V_fraction$absolute_thresh[na_id] <-
    sapply(na_id, function(i) {
      new_allele <- geno_V_fraction$v_call[i]
      closest <- strsplit(geno_V_fraction$v_call[i], "_")[[1]][1]
      allele_cluster_threshold[closest]
    })
  
  ## check if allele is above thresh.
  
  geno_V_fraction[, "above_thresh" := get("absolute_fraction") >= get("absolute_thresh")]
  
  if(!is.null(confidence_level)) {
    geno_V_fraction <- geno_V_fraction[, "above_confidence" := get("genotype_confidence") >= confidence_level]
  }else{
    geno_V_fraction <- geno_V_fraction[, "above_confidence" := TRUE]
  }
  
  sortBy <- c('gene', 'absolute_fraction')
  sortType <- c(1, -1)
  data.table::setorderv(geno_V_fraction, sortBy, sortType)
  genoV <-
    geno_V_fraction[, .(
      "alleles" = paste0(get("v_allele"), collapse = ","),
      "imgt_alleles" = paste0(get("v_call_or"), collapse = ","),
      "counts" = paste0(get("count"), collapse = ","),
      "absolute_fraction" = paste0(round(get(
        "absolute_fraction"
      ), 7), collapse = ","),
      "absolute_threshold" = paste0(formatC(get(
        "absolute_thresh"
      ), format = "f"), collapse = ","),
      "genotype_confidence" = paste0(formatC(get(
        "genotype_confidence"
      ), format = "f"), collapse = ","),
      "genotyped_alleles" = paste0(get("v_allele")[get("above_thresh") &
                                                     get("above_confidence")], collapse = ","),
      "genotyped_imgt_alleles" = paste0(get("v_call_or")[get("above_thresh") &
                                                           get("above_confidence")], collapse = ",")
    ), by = mget(c("gene"))]
  
  
  # geno <- as.data.frame(genotype, stringsAsFactors = FALSE)
  # if (find_unmutated == TRUE) {
  #   seqs <- genotypeFasta(geno, germline_db)
  #   dist_mat <- seqs %>% sapply(function(x) sapply((getMutatedPositions(seqs,
  #                                                                       x)), length)) %>% as.matrix
  #   rownames(dist_mat) <- colnames(dist_mat)
  #   for (i in 1:nrow(dist_mat)) {
  #     dist_mat[i, i] = NA
  #   }
  #   same <- which(dist_mat == 0, arr.ind = TRUE)
  #   if (nrow(same) > 0) {
  #     for (r in 1:nrow(same)) {
  #       inds <- as.vector(same[r, ])
  #       geno[getGene(rownames(dist_mat)[inds][1]), ]$note <- paste(rownames(dist_mat)[inds],
  #                                                                  collapse = " and ") %>% paste("Cannot distinguish",
  #                                                                                                .)
  #     }
  #   }
  # }
  # rownames(geno) <- NULL
  
  return(genoV)
}
