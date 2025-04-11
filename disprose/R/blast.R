#'
#' Builds local database for BLAST
#'
#' Builds a BLAST database with local sequences using FASTA file.
#'
#'@param makeblastdb.way character; name and path to makeblastdb executable file
#'@param fasta.way character; name and path to FASTA file
#'@param db.way character; name and path to local BLAST database
#'@param db.type character; type of BLAST database
#'@param db.title character; BLAST data base title
#'@param delete.fasta logical; delete FASTA file
#'@param verbose logical; show messages
#'
#'@details
#'This function is using BLAST applications. BLAST+ (UNIX or Windows) should be installed.
#'
#'@return
#'The function creates local BLAST data base. No additional data is returned.
#'
#'@examples
#'\dontrun{
#'# This function is using BLAST applications. BLAST+ should be installed.
#'# FASTA file with sequences for local data base should be downloaded first (see get_seq_for_DB ())
#'path <- tempdir()
#'dir.create (path)
#'# load metadata for target sequences of Chlamydia pneumoniae
#'(meta.target)
#'# load sequences, it will take about 3 minutes
#'get_seq_for_DB (ids = meta.target$gi, db = "nucleotide", check.result = TRUE,
#'               return="fasta", fasta.file = paste0 (path, "/seq.fasta"), verbose = TRUE)
#'# create local data base, it will take 0.235217 seconds
#'make_blast_DB (makeblastdb.way = "D:/Blast/blast-2.11.0+/bin/makeblastdb.exe",
#'               fasta.way = paste0 (path, "/seq.fasta"), db.title = "Cl_pneumoniae",
#'               db.way = paste0 (path, "/DB"), db.type = "nucl", delete.fasta = FALSE)
#'# delete FASTA file (also can set delete.fasta = TRUE)
#'file.remove (paste0 (path, "/seq.fasta"))
#'}
#'
#'@references
#' Camacho C., Coulouris G., Avagyan V. et al. (2009). BLAST+: architecture and applications. BMC Bioinformatics 10, 421.
#' \url{https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-10-421}.
#'@author Elena N. Filatova
#'@name make_blast_DB
#'@export
make_blast_DB <- function (makeblastdb.way, fasta.way, db.way, db.type = "nucl", db.title,
                           delete.fasta = FALSE, verbose = TRUE){
  in.par<-paste("-in", fasta.way, sep=" ")
  title.par<-paste("-title", db.title, sep=" ")
  out.par<-paste("-out", db.way, sep=" ")
  dbtype.par<-paste("-dbtype", db.type, sep=" ")
  comm<-paste(makeblastdb.way, in.par, dbtype.par,  title.par, out.par, sep=" ")
  system(comm)
  if (delete.fasta==TRUE){file.remove(fasta.way);
  if (verbose) message ("FASTA file is removed")}
}

#' Local BLAST
#'
#'Perform nucleotide BLAST with local database
#'
#'@param probe.var character; query - vector of nucleotide sequences
#'@param probe.id.var vector of identification numbers for query sequences
#'@param fasta.way character; name and path to FASTA file
#'@param blastn.way character; name and path to blastn executable file
#'@param db.way character; name and path to local BLAST database
#'@param out.way character; name and path to blastn output file
#'@param mc.cores integer; number of processors for parallel computation (not supported on Windows)
#'@param add.query.info logical; add query nucleotide sequence and its length to result
#'@param temp.db character; temporal SQLite database name and path
#'@param delete.files logical; delete created FASTA and output files
#'@param eval integer; expect value for saving hits
#'@param ws integer; length of initial exact match
#'@param reward integer; reward for a nucleotide match
#'@param penalty integer; penalty for a nucleotide mismatch
#'@param gapopen integer; cost to open a gap
#'@param gapextend integer; cost to extend a gap
#'@param maxtargetseqs integer; number of aligned sequences to keep
#'@param verbose logical; show messages
#'
#'@details
#'For this function BLAST+ executables (blastn) must be installed and local nucleotide database must be created.
#'
#' While working, the function creates blastn input FASTA file and output file. If files exist already, they will be overwritten.
#' Those files could be deleted by \code{delete.files = TRUE} parameter.
#'
#' If no \code{probe.id.var} is provided, query sequences are numbered in order, starting with 1.
#'
#' Query cover is query coverage per HSP (as a percentage)
#'
#' If \code{add.query.info = TRUE} function saves data in temporal SQLite database.
#' Function will stop if same database already exists, so deleting temporal database
#' (by setting \code{delete.files = TRUE}) is highly recommended.
#'
#' "no lines available in input" error is returned when there are no BLAST results matching the specified parameters. Adjust BLAST parameters.
#'
#' @return
#' Data frame with BLAST alignments: query sequence id, start and end of alignment in query, subject GI, accession, title and taxon id,
#' start and end of alignment in subject, length of alignment, number of mismatches and gaps, number of identical matches,
#' raw score, bit score, expect value and query cover.
#' If \code{add.result.info = TRUE}, query sequence and its length are also added to data frame.
#'
#' @examples
#'\dontrun{
#'# This function is using BLAST applications. BLAST+ should be installed.
#'# Local nucleotide database should be created
#'# Local database of target sequences of Chlamydia pneumoniae was created
#'# in temporal directory previously (see make_blast_DB () function)
#'path <- tempdir()
#'dir.create (path)
#'#set probes for local BLAST
#'probes <- c ("catctctatttcggtagcagctcc", "aaagtcatagaaaagcctgtagtcgc",
#'             "ccttcttctcgaactctgaagtacact", "aaaaaaaaaaaaaaaaa", "acacacacacacaac")
#'blast.raw <- blast_local(probe.var = probes, probe.id.var = NULL,
#'                         fasta.way = paste0 (path, "/blast.fasta"),
#'                         blastn.way = "D:/Blast/blast-2.11.0+/bin/blastn.exe",
#'                         db.way = paste0 (path, "/DB"),
#'                         out.way = paste0 (path, "/blast.out"),
#'                         mc.cores=1, add.query.info = TRUE, temp.db = paste0 (path, "/temp.db"),
#'                         delete.files = TRUE, eval = 1, maxtargetseqs = 200)
#'}
#'
#' @references
#' Camacho C., Coulouris G., Avagyan V. et al. (2009). BLAST+: architecture and applications. BMC Bioinformatics 10, 421.
#' \url{https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-10-421}.
#' @author Elena N. Filatova
#' @name blast_local
#' @export
blast_local <- function (probe.var, probe.id.var = NULL, fasta.way = NULL,
                         blastn.way = NULL, db.way = NULL, out.way = NULL,
                         mc.cores=1, add.query.info = FALSE, temp.db = NULL, delete.files = FALSE,
                         eval = 1000, ws = 7, reward = 1, penalty = -3, gapopen = 5, gapextend = 2, maxtargetseqs = 500,
                         verbose = TRUE){
  # test package dependencies
  if (!requireNamespace("seqinr", quietly = TRUE)) { stop("Package \"seqinr\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("utils", quietly = TRUE)) { stop("Package \"utils\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("DBI", quietly = TRUE)) { stop("Package \"DBI\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("RSQLite", quietly = TRUE)) { stop("Package \"RSQLite\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("parallel", quietly = TRUE)) { stop("Package \"parallel\" needed for this function to work. Please install it.", call. = FALSE)}
  #check temporal database
  if (add.query.info == TRUE){
    if (is.null(temp.db)==TRUE){stop ("Set temp.db name and path")}
    if (file.exists(temp.db)){stop(temp.db, " database already exists. Delete it or change database name.")}}
  #check blast ways and make probe fasta
  if (is.null(blastn.way)==TRUE){stop("Set blastn.way name and path")}
  if (is.null(db.way)==TRUE){stop("Set db.way name and path")}
  if (is.null(out.way)==TRUE){stop("Set out.way name and path")}
  if (is.null(probe.id.var)==TRUE){probe.id.var=1:length(probe.var)} # probe id
  if (is.null(fasta.way)==TRUE){stop ("Set fasta.way name and path")} # fasta way
  if (file.exists(fasta.way)){warning ("Note that ", fasta.way, " file already exists. It will be overwritten.")}
  seq_fasta.list<-list();  for (i in 1:length(probe.var)){seq_fasta.list[[i]]<-probe.var[i]}
  seqinr::write.fasta(seq_fasta.list, names=probe.id.var, file.out=fasta.way, open="w", as.string = FALSE) # write fasta file
  #blast comand
  db<-paste("-db", db.way, sep=" ") # database place
  query<-paste("-query", fasta.way, sep=" ") # query fasta file
  threads<-paste("-num_threads", mc.cores, sep=" ") # parallel
  params<-paste("-word_size", ws, "-dust no -evalue", eval, "-reward", reward, "-penalty", penalty,
                "-gapopen", gapopen, "-gapextend", gapextend, "-max_target_seqs", maxtargetseqs, sep=" ") # blastn params
  tabs<-"-outfmt \"6 qseqid qstart qend sgi sacc stitle staxid sstart send length mismatch gaps nident score bitscore evalue qcovhsp\"" # output format
  out<-paste("-out", out.way, sep=" ") # result file
  if (file.exists(out.way)){warning ("Note that ", out.way, " file already exists. It will be overwritten.")}
  comm<-paste(blastn.way, db, query, threads, params, tabs, out, sep=" ") # comand
  # blast
  if (verbose) message ("BLAST in progress")
  system(comm); result<-utils::read.csv(out.way, sep="\t", quote = "", header = FALSE)
  colnames(result)<-c("Qid", "Qstart", "Qend", "Rgi", "Racc", "Rtitle", "Rtaxid", "Rstart", "Rend", "alig.length", "mismatch", "gaps",
                      "ident.number", "score", "bitscore", "Evalue", "Qcover")
  #adding probe, probe.length, Qcover
  if (add.query.info==TRUE){
    if (verbose) message ("Adding query information. Writing temporal database")
    #write temp db
    probe.data<-data.frame("id"=probe.id.var, "probe"=probe.var)
    num.del<-duplicated(probe.data) # check unique
    if (sum(num.del)>0){probe.data<-probe.data[!num.del,]}
    write_to_DB(database=temp.db, data=probe.data, table="probe")
    index_DB(database = temp.db, table = "probe", index.unique = TRUE, index.column.name = "id")
    #get probes sequence
    get.info.one<-function(res.id){
      if (inherits(res.id, "character")){res.id<-paste("\"", res.id, "\"", sep="")} # add quotes for characters
      conn <- DBI::dbConnect (RSQLite::SQLite (), temp.db)
      on.exit(DBI::dbDisconnect (conn), add=T)
      query=paste("SELECT probe FROM probe WHERE id =", res.id, sep=" ")
      probe<-as.character(DBI::dbGetQuery(conn, query))
      return(probe)}
    probe<-parallel::mclapply(X=result$Qid, FUN=function(x) get.info.one(res.id=x), mc.cores=mc.cores)
    probe<-unlist(probe)
    # count probe length and qcover
    probe.length<-c(); for (i in 1:length(probe)){probe.length[i]<-nchar(probe[i])}
    result<-cbind.data.frame(probe, probe.length, result) }
  # delete work files
  if (delete.files==TRUE){file.remove(file=fasta.way); file.remove(file=out.way);
    if(add.query.info==TRUE){file.remove(file=temp.db)}
    if (verbose) message ("FASTA, output files and temporal database are deleted")} # delete files
  return(result)}

#' Complement BLAST result
#'
#' Provides subjects' GenInfo Identifiers if BLAST alignment result does not contain one.
#'
#'@param blast.result data frame; BLAST alignment result
#'@param AcNum.column.name,GI.column.name character; name of column with subject
#'accession numbers and GenInfo Identifier numbers from BLAST result data frame
#'@param delete.version logical; remove version suffix from subject accession number
#'@param version.sep character; accession number and version suffix separator (a dot for NCBI accession numbers)
#'@param add.gi character; table with linked accession and GI numbers is taken from
#'SQLite database (\code{"DB"}) or data frame (\code{"DF"})
#'@param add.gi.df data frame with table (used if \code{add.gi = "DF"})
#'@param temp.db character; temporal SQLite database name and path
#'@param delete.temp logical; delete created temporal SQLite database
#'@param add.gi.db,add.gi.table,add.gi.ac.column.name,add.gi.gi.column.name SQLite database name and path,
#'table name and name of columns with accession and GI numbers (used if \code{add.gi = "DB"})
#'@param mc.cores integer; number of processors for parallel computation (not supported on Windows)
#'@param ac.num.var vector of accession numbers
#'@param verbose logical; show messages
#'
#' @details
#'BLAST alignment, performed with local database, may not contain subject GI information. Also subject accession may contain version suffix.
#'This can make it difficult to analyze the results further. This function adds subject GI and removes subject accession version suffix.
#'
#' To add GI GenInfo Identifiers table with them linked to accession numbers must be provided as data frame or SQLite database table.
#' \code{add.gi.df} must be a data frame with column one - accession numbers, column two - GenInfo Identifier numbers.
#' If \code{add.gi = "DF"} temporal SQLite database is created.
#'
#' SQLite database table with accession and GI numbers should not contain duplicated rows.
#' It is also highly recommended to index accession numbers' variable in database.
#'
#' \code{delete.version} executes in the first step, so if you use this option accession numbers
#' in \code{add.gi} table must not contain version suffix.
#'
#'\code{AcNum.column.name}, \code{GI.column.name}, \code{add.gi.ac.column.name} and \code{dd.gi.gi.column.name}
#'must be column names exactly as in data frame.
#'
#'@return
#' \code{blast.result} data frame with added GI and deleted accession version suffix.
#'
#' @examples
#' path <- tempdir()
#' dir.create (path)
#' # load raw blast results
#' data (blast.raw)
#' #load meta.target with result (targets' sequences) GI and Acc.nums
#' data (meta.target)
#' blast.fill <- fill_blast_results(blast.result = blast.raw, delete.version = TRUE,
#'                                  add.gi = "DF", add.gi.df = meta.target[, c("GB_AcNum", "gi")],
#'                                  temp.db = paste0 (path, "/temp.db"), delete.temp = TRUE)
#'
#' @author Elena N. Filatova
#' @name fill_blast_result
NULL
#'
#'@describeIn fill_blast_result Provides subjects' Genbank Identifiers if BALST alignment result does not contain one
#' @export
fill_blast_results<-function(blast.result, AcNum.column.name="Racc", GI.column.name="Rgi",
                             delete.version = FALSE, version.sep = ".",
                             add.gi="DB", add.gi.df, temp.db = NULL, delete.temp = FALSE,
                             add.gi.db = NULL, add.gi.table = NULL,
                             add.gi.ac.column.name="AC", add.gi.gi.column.name="GI",
                             mc.cores=1, verbose = TRUE){
  # test package dependencies
  if (!requireNamespace("DBI", quietly = TRUE)) { stop("Package \"DBI\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("RSQLite", quietly = TRUE)) { stop("Package \"RSQLite\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("parallel", quietly = TRUE)) { stop("Package \"parallel\" needed for this function to work. Please install it.", call. = FALSE)}
  #  check parameters
  if (add.gi!="DB" & add.gi!="DF"){stop("Choose add.gi parameter")}
  if (add.gi == "DF"){
    if (is.null(temp.db)==TRUE){stop ("Set temp.db name and path")}
    if (file.exists(temp.db)){stop(paste(temp.db, " database already exists. Delete it or change database name.", sep=" "))}}
  # Racc and Rgi column nums
  cols<-colnames(blast.result)
  acc.num<-which(cols==AcNum.column.name)
  gi.num<-which(cols==GI.column.name)
  #delete AcNum version
  if (delete.version == TRUE){
    if (verbose) message ("Deleting AcNum version")
    Racc<-delete_AcNum_version(ac.num.var = blast.result[, acc.num], version.sep=version.sep, mc.cores = mc.cores)
    blast.result[, acc.num]<-Racc  }
  #add GIs from dataframe
  if (verbose) message ("Adding gi")
  if (add.gi=="DF"){
    if (verbose) message ("Writing temporal database")
    num.dup<-duplicated(add.gi.df)
    if (sum(num.dup)>0){add.gi.df<-add.gi.df[!num.dup,]} # check add.gi.df unique
    colnames(add.gi.df)<-c("AC", "GI")
    #write temp.db
    write_to_DB(database=temp.db, data=add.gi.df, table="ac_gi")
    index_DB(database = temp.db, table = "ac_gi", index.unique = TRUE, index.column.name = "AC")
    add.gi.gi.column.name="GI"; add.gi.ac.column.name="AC"}
  #one result func
  gi.one<-function(ac, database, table){
    if (inherits(ac, "character")){ac<-paste("\"", ac, "\"", sep="")} # add quotes for characters
    conn <- DBI::dbConnect (RSQLite::SQLite (), database)
    on.exit(DBI::dbDisconnect (conn), add=T)
    query=paste("SELECT", add.gi.gi.column.name, "FROM", table, "WHERE", add.gi.ac.column.name, "=", ac, sep=" ")
    gi<-DBI::dbGetQuery(conn, query)[1,1]
    return(gi)}
  if (add.gi=="DB"){
    gi.list<-parallel::mclapply(X=blast.result[, acc.num],
                                FUN=function(x) gi.one(ac=x, database=add.gi.db, table=add.gi.table), mc.cores=mc.cores)}
  if (add.gi=="DF"){
    gi.list<-parallel::mclapply(X=blast.result[, acc.num],
                                FUN=function(x) gi.one(ac=x, database=temp.db, table="ac_gi"), mc.cores=mc.cores)
    if (delete.temp==TRUE){if (verbose) message ("Temporal database is deleted")
      file.remove(temp.db)}}
  blast.result[, gi.num]<-unlist(gi.list)
  return(blast.result)}

#'@describeIn fill_blast_result Remove accession version suffix
#'@export
delete_AcNum_version <- function (ac.num.var, version.sep=".", mc.cores = 1){
  # test package dependencies
  if (!requireNamespace("parallel", quietly = TRUE)) { stop("Package \"parallel\" needed for this function to work. Please install it.", call. = FALSE)}
  # run
  racc.list<-parallel::mclapply(X=ac.num.var, FUN=function(x) strsplit(x=x, split=version.sep, fixed=TRUE)[[1]][1], mc.cores = mc.cores)
  result<-unlist(racc.list)
  return(result)}

#' Summarize BLAST result
#'
#' Summarize aligned, not aligned and undesirably aligned sequences
#'
#' @param sum.aligned character; summarize specific or not specific alignments; possible values are
#' \code{"sp"} (aligned and not aligned specific subjects) and \code{"nonsp"} (aligned non specific subjects)
#' @param blast.probe.id.var vector of query identification numbers from BLAST result data
#' @param blast.res.id.var,blast.res.title.var vector of subject identification numbers and titles from BLAST result data
#' @param reference.id.var,reference.title.var vector of identification numbers and titles of
#' specific sequences that should be or might be aligned
#' @param titles logical; include titles in alignment reports
#' @param add.blast.info logical; add other BLAST results
#' @param data.blast.info data frame; additional BLAST result from BLAST result data
#' @param check.blast.for.source logical; delete queries that are not aligned with one obligatory sequence
#' @param source identification number of obligatory sequence for alignment
#' @param switch.ids logical; use different identification numbers for BLAST result's subjects
#' @param switch.table data frame; table of old and new identification numbers (and new titles) linked by row
#' @param mc.cores integer; number of processors for parallel computation (not supported on Windows)
#' @param digits integer; number of decimal places to round the result
#' @param sep character; the field separator character
#' @param temp.db character; temporal SQLite database name and path
#' @param delete.temp.db logical; delete temporal SQLite database afterwards
#' @param return character; returned object; possible values are \code{"list"} (list of data frames with alignment
#' summary and report for each probe) and \code{"summary"} (data frame with summary for all probes is returned
#' and alignment reports are written into files or SQLite database tables)
#' @param write.alignment character; write alignment reports into files (\code{"file"}) or SQLite database tables (\code{"DB"};
#' used if (\code{return = "summary"}))
#' @param alignment.db,alignment.table.sp.aligned,alignment.table.sp.not.aligned,alignment.table.nonsp character;
#' SQLite database name and path, tables names (used if \code{write.alignment = "DB"})
#' @param change.colnames.dots logical; change dots to underscore in data frame column names
#' (used if \code{write.alignment = "DB"})
#' @param file.sp.aligned,file.sp.not.aligned,file.nonsp character; file names and path (used if \code{write.alignment = "file"})
#' @param verbose logical; show messages
#'
#'@details
#'This function works with data frame created by \link[disprose]{blast_local} function.
#'It takes BLAST results, divides aligned subjects on specific (that should be aligned)
#' and non specific (that should not be aligned) according to \code{reference}) values.
#'Function summarizes amount of aligned and not aligned specific subjects and amount of aligned non specific subjects.
#'
#'When \code{sum.aligned = "sp"} aligned and not aligned specific subjects are summarized and
#'\code{reference.id.var} and \code{reference.title.var} should contain sequences that it is necessary to align with.
#'When \code{sum.aligned = "nonsp"} aligned non specific subjects are summarized and
#' \code{reference.id.var} should contain sequences that may be aligned (that are not considered as non specific),
#' no titles needed.
#'
#' When \code{return = "summary"}, function returns summary (amount of aligned and not aligned subjects) and writes
#' sorted alignments (alignment report) in file (\code{write.alignment = "file"}) or SQLite database (\code{write.alignment = "DB"}).
#' Usually only subjects' ids and (optionally) titles are returned, but you may add as many BLAST results as you like
#' with \code{add.blast.info} and \code{data.blast.info} parameters.
#' If you add some BLAST results, all alignments will present in alignment report,
#' if not -  duplicated subjects will be deleted.
#'
#' By default result tables in database (if \code{write.alignment = "DB"}) are
#' "sp_aligned", "sp_not_aligned" and "nonsp",
#' Results are written by appending, so if files or tables already exist, data will be added into them.
#'

#' If subjects identification numbers in BLAST result data differ from those in \code{reference.id.var}
#' you may use \code{switch.ids = TRUE} to change BLAST ids into new according to \code{switch.table}.
#' \code{switch.table} must be a data frame with column one - old ids, column two - new ids and (optionally)
#' column three - new titles. Do not use dots in column names.
#'
#' When \code{check.blast.for.source = TRUE} probes that are non blasted for one special subject
#' (usually the sequence that was cut for probes) are deleted.
#' No \code{check.blast.for.source} is performed if \code{sum.aligned = "nonsp"}.
#' Check for source is performed after the possible \code{id.switch}, so \code{source} should be identification number of
#' same type as \code{reference}.
#'
#' Probe identification number must be character variable.
#'
#' If alignment report is written into database, probe identification variable is indexed in all tables.
#' Also it is highly recommended to set \code{change.colnames.dots = TRUE} to change possible dots to underscore
#' within result data frame's column names and avoid further mistakes.
#'
#'While working function saves data in temporal SQLite database.
#'Function will stop if same database already exists, so deleting temporal database is highly recommended.
#'
#'@return List of data frames with alignment summary and report for each probe or
#' data frame with summary for all probes (alignment reports are written into files or SQLite database tables).
#'
#'@examples
#'path <- tempdir()
#'dir.create (path)
#'# load blast results with subject accession numbers
#'data(blast.fill)
#'#load metadata of all Chlamydia pneumoniae sequences - they are subjects that
#'# do not count as nonspecific and may be aligned
#'data(meta.all)
#'# load metadata with target Chlamydia pneumoniae sequences - they are specific subjects
#'# that must be aligned
#'# make new accession numbers to count all WGS sequences as one (see unite_NCBI_ac.nums ())
#'meta.target.new.ids <- unite_NCBI_ac.nums (data = meta.target,
#'                                           ac.num.var = meta.target$GB_AcNum,
#'                                           title.var = meta.target$title,
#'                                           db.var = meta.target$source_db,
#'                                           type = "shotgun", order = TRUE,
#'                                           new.titles = TRUE)
#'# summarize blast results, count aligned specific subjects with "switch ids" option
#'# (WGS sequences are counted as one). Add query cover information.
#'blast.sum.sp <- summarize_blast_result (sum.aligned = "sp",
#'                                        blast.probe.id.var = blast.fill$Qid,
#'                                        blast.res.id.var = blast.fill$Racc,
#'                                        blast.res.title.var = blast.fill$Rtitle,
#'                                        reference.id.var = meta.target.new.ids$new.id,
#'                                        reference.title.var = meta.target.new.ids$new.title,
#'                                        titles = TRUE,
#'                                        add.blast.info = TRUE,
#'                                        data.blast.info = data.frame(Qcover = blast.fill$Qcover),
#'                                        switch.ids = TRUE, switch.table = meta.target.new.ids,
#'                                        temp.db = paste0 (path, "/temp.db"), delete.temp.db = TRUE,
#'                                        return = "summary", write.alignment = "DB",
#'                                        alignment.db = paste0 (path, "/alig.db"))
#'# summarize nonspecific alignments (that are not in meta.all dataframe)
#'blast.sum.nonsp <- summarize_blast_result (sum.aligned = "nonsp",
#'                                           blast.probe.id.var = blast.fill$Qid,
#'                                           blast.res.id.var = blast.fill$Racc,
#'                                           blast.res.title.var = blast.fill$Rtitle,
#'                                           reference.id.var = meta.all$GB_AcNum,
#'                                           reference.title.var = meta.all$title,
#'                                           titles = TRUE, switch.ids = FALSE,
#'                                           add.blast.info = TRUE,
#'                                           data.blast.info = data.frame(Qcover = blast.fill$Qcover),
#'                                           temp.db = paste0 (path, "/temp.db"),
#'                                           delete.temp.db = TRUE,
#'                                           return = "summary", write.alignment = "DB",
#'                                           alignment.db = paste0 (path, "/alig.db"))
#'# all specific targets are aligned
#'sp.aligned <- read_from_DB(database = paste0 (path, "/alig.db"), table = "sp_aligned")
#'# no targets that are not aligned
#'sp.not.aligned <- read_from_DB(database = paste0 (path, "/alig.db"), table = "sp_not_aligned")
#'# No nonspecific alignments
#'nonsp <- read_from_DB(database = paste0 (path, "/alig.db"), table = "nonsp")
#'file.remove (paste0 (path, "/alig.db"))
#'
#' @author Elena N. Filatova
#' @name summarize_blast_result
#' @export
#'
summarize_blast_result <- function(sum.aligned  ="sp", blast.probe.id.var, blast.res.id.var, blast.res.title.var,
                                  reference.id.var, reference.title.var, titles = FALSE,
                                  add.blast.info = FALSE, data.blast.info, check.blast.for.source = FALSE, source = NULL,
                                  switch.ids = FALSE, switch.table, mc.cores = 1, digits = 2, sep = ";",
                                  temp.db = NULL, delete.temp.db = TRUE, return = "summary", write.alignment = "DB",
                                  alignment.db = NULL, alignment.table.sp.aligned = NULL, alignment.table.sp.not.aligned = NULL,
                                  alignment.table.nonsp = NULL, change.colnames.dots = TRUE,
                                  file.sp.aligned = NULL, file.sp.not.aligned = NULL, file.nonsp = NULL,
                                  verbose = TRUE){
  # test package dependencies
  if (!requireNamespace("utils", quietly = TRUE)) { stop("Package \"utils\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("DBI", quietly = TRUE)) { stop("Package \"DBI\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("RSQLite", quietly = TRUE)) { stop("Package \"RSQLite\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("parallel", quietly = TRUE)) { stop("Package \"parallel\" needed for this function to work. Please install it.", call. = FALSE)}
  #check settings
  if (verbose) message ("Checking settings")
  if (add.blast.info == TRUE){if (is.data.frame(data.blast.info) == FALSE){stop("data.blast.info must be a data frame")}}
  if (write.alignment!="DB" & write.alignment!="file"){stop("Choose write.alignment parameter")}
  if (return!="summary" & return!="list"){stop("Choose return parameter")}
  if (sum.aligned!="sp" & sum.aligned!="nonsp"){stop("Choose sum.aligned parameter")}
  if (check.blast.for.source==TRUE & length(source)!=1){stop("Set source parameter")}
  #temporary blast db
  if (is.null(temp.db)==TRUE){stop("Set temp.db name and path")}
  if (file.exists(temp.db)){stop(paste(temp.db, " database already exists. Delete it or change database name.", sep=" "))}
  #write.alignment=DB
  if(write.alignment=="DB" & return=="summary"){
    if (is.null(alignment.db)==TRUE){stop(" Set alignment.db name and path")}
    if (file.exists(alignment.db)){warning (alignment.db, " database already exists. Adding tables to database.")}
    if (sum.aligned=="sp"){
      if (is.null(alignment.table.sp.aligned)==TRUE){alignment.table.sp.aligned<-"sp_aligned"}
      if (alignment.table.sp.aligned %in% list_DB(alignment.db)==TRUE){
        warning ("Note that table ", alignment.table.sp.aligned, " already exists in database ", alignment.db, ". Appending rows to table.")}
      if (is.null(alignment.table.sp.not.aligned)==TRUE){alignment.table.sp.not.aligned<-"sp_not_aligned"}
      if (alignment.table.sp.not.aligned %in% list_DB(alignment.db)==TRUE){
        warning ("Note that table ", alignment.table.sp.not.aligned, " already exists in database ", alignment.db, ". Appending rows to table.")}
    }
    if (sum.aligned=="nonsp"){
      if (is.null(alignment.table.nonsp)==TRUE){alignment.table.nonsp<-"nonsp"}
      if (alignment.table.nonsp %in% list_DB(alignment.db)==TRUE){
        warning ("Note that table ", alignment.table.nonsp, " already exists in database ", alignment.db, ". Appending rows to table.")}
    }}
  #write.alignment=file
  if(write.alignment=="file" & return=="summary"){
    if (sum.aligned=="sp"){
      if (is.null(file.sp.aligned)==TRUE){stop(" Set file.sp.aligned name and path")}
      if (file.exists(file.sp.aligned)){warning ("Note that ", file.sp.aligned, " file already exists. Adding lines to file.")}
      if (is.null(file.sp.not.aligned)==TRUE){stop(" Set file.sp.not.aligned name and path")}
      if (file.exists(file.sp.not.aligned)){warning ("Note that ", file.sp.not.aligned, " file already exists. Adding lines to file.")}
    }
    if (sum.aligned=="nonsp"){
      if (is.null(file.nonsp)==TRUE){stop(" Set file.nonsp name and path")}
      if (file.exists(file.nonsp)){warning ("Note that ", file.nonsp, " file already exists. Adding lines to file.")}
    }}
  ### start
  #blasted probes
  probes=unique(blast.probe.id.var)
  # SP SP SP SP
  if (sum.aligned=="sp"){
    if (verbose) message ("Summarizing specific alignment")
    if (verbose) message ("Create temporal database")
    #temp db
    data.db<-cbind.data.frame("probe"=blast.probe.id.var, "ids"=blast.res.id.var)
    if (titles==TRUE){data.db<-cbind.data.frame(data.db, "titles"=blast.res.title.var)}
    if (add.blast.info==TRUE){data.db<-cbind.data.frame(data.db, data.blast.info)}
    if (switch.ids==TRUE){
      if (verbose) message ("Switching blast results' ids")
      if(titles==TRUE){colnames(switch.table)<-c("old", "new", "title")} else {colnames(switch.table)<-c("old", "new")} # make colnames
      write_to_DB(database = temp.db, data = switch.table, table = "switch")
      index_DB(database = temp.db, table = "switch", index.unique = FALSE, index.column.name = "old")
      switch.one<-function(id, return=c("newid", "title")){
        conn <- DBI::dbConnect (RSQLite::SQLite (), temp.db)
        on.exit(DBI::dbDisconnect (conn), add=T)
        if (inherits(id, "character")){id<-paste("\"", id, "\"", sep="")} # add quotes for characters
        query=paste("SELECT * FROM switch WHERE old =", id, sep=" ")
        new<-DBI::dbGetQuery(conn, query)
        if (return=="newid"){return(new$new)};if (return=="title"){return(new$title)} }
      new<-parallel::mclapply(X=data.db$ids, FUN=function(x) switch.one(id=x, return="newid"), mc.cores=mc.cores)
      data.db$ids<-unlist(new)
      if (titles==TRUE){
        new<-parallel::mclapply(X=data.db$ids, FUN=function(x) switch.one(id=x, return="title"), mc.cores=mc.cores)
        data.db$titles<-unlist(new)}}
    write_to_DB(database = temp.db, data = data.db, table = "blast")
    index_DB(database = temp.db, table = "blast", index.unique = c(FALSE, FALSE), index.column.name = c("probe", "ids"))
    # references db
    refs<-cbind.data.frame("ids"=reference.id.var);
    if (titles==TRUE){refs<-cbind.data.frame(refs, "titles"=reference.title.var)}
    nums.rep<-duplicated(refs) # delete duplicates (for switch cases)
    if (sum(nums.rep)>0){refs<-refs[!nums.rep,]}
    write_to_DB(database = temp.db, data=refs, table="refs")
    index_DB(database = temp.db, table="refs", index.unique = TRUE, index.column.name = "ids")
    #get data
    sp.all<-nrow(refs)
    one.probe.sp.al<-function(probe){
      conn <- DBI::dbConnect (RSQLite::SQLite (), temp.db) # connect
      on.exit(DBI::dbDisconnect (conn), add=T)
      query<-paste("SELECT ids FROM blast WHERE probe = \"", probe, "\"", sep="") # all result nums for one probe
      did.get<-DBI::dbGetQuery(conn, query)
      did.get<-did.get[,1] # got numbers --- choose only sp. from them
      if (inherits(did.get, "character")){did.get<-paste("\"", did.get, "\"", sep="")} # add quotes for characters
      did.get.paste<-paste(did.get, collapse=", ")
      query1<-paste("SELECT ids FROM refs WHERE ids IN (", did.get.paste, ")", sep="")
      query2<-paste("SELECT * FROM blast WHERE probe = \"", probe, "\" AND ids in (", query1, ")", sep="")
      blast.one<-DBI::dbGetQuery(conn, query2)
      # in case 0 rows returned
      if (nrow(blast.one)==0){
        add.row<-matrix(rep(NA, ncol(blast.one)), nrow=1); colnames(add.row)<-colnames(blast.one)
        return=rbind(blast.one, add.row)
        return=cbind.data.frame("probe.id"=probe, "sp.all.n"=sp.all,
                                "sp.aligned.n"=0,
                                "sp.aligned.percent"=0,
                                "sp.not.aligned.n"=NA,
                                "nonsp.n"=NA,
                                "type"="sp.aligned", return[,2:ncol(blast.one)])
      }else{
        # more than 0 rows returned
        #count sp.aligned
        if (titles==TRUE){count.one<-blast.one[,1:3]} else{count.one<-blast.one[,1:2]}
        nums.dupl<-duplicated(count.one[,2]) # delete dupls by result id
        if(sum(nums.dupl)>0){count.one<-count.one[!nums.dupl,]}
        sp.aligned.n<-nrow(count.one)
        sp.aligned.percent<-sp.aligned.n*100/sp.all; sp.aligned.percent<-round(sp.aligned.percent, digits=digits)
        #return
        if (add.blast.info==TRUE){
          return=cbind.data.frame("probe.id"=blast.one$probe, "sp.all.n"=rep(sp.all, nrow(blast.one)), # added data - all rows are left
                                  "sp.aligned.n"=rep(sp.aligned.n, nrow(blast.one)),
                                  "sp.aligned.percent"=rep(sp.aligned.percent, nrow(blast.one)),
                                  "sp.not.aligned.n"=rep(NA, nrow(blast.one)),
                                  "nonsp.n"=rep(NA, nrow(blast.one)),
                                  "type"=rep("sp.aligned", nrow(blast.one)), blast.one[,2:ncol(blast.one)])
        }else{
          return=cbind.data.frame("probe.id"=count.one$probe, "sp.all.n"=rep(sp.all, nrow(count.one)), # no added data - delete repeats
                                  "sp.aligned.n"=rep(sp.aligned.n, nrow(count.one)),
                                  "sp.aligned.percent"=rep(sp.aligned.percent, nrow(count.one)),
                                  "sp.not.aligned.n"=rep(NA, nrow(count.one)),
                                  "nonsp.n"=rep(NA, nrow(count.one)),
                                  "type"=rep("sp.aligned", nrow(count.one)), count.one[,2:ncol(count.one)]) }
      }
      return(return)}
    if (verbose) message ("Summarizing aligned specific subjects")
    list.sp.aligned<-parallel::mclapply(X=probes, FUN=function(x) one.probe.sp.al(probe=x), mc.cores=mc.cores)
    list.names<-c(); for (i in 1:length(list.sp.aligned)){list.names[i]<-list.sp.aligned[[i]]$probe.id[1]}
    names(list.sp.aligned)<-list.names # names
    one.probe.sp.not.al<-function(probe){
      conn <- DBI::dbConnect (RSQLite::SQLite (), temp.db) # connect
      on.exit(DBI::dbDisconnect (conn), add=T)
      query<-paste("SELECT ids FROM blast WHERE probe = \"", probe, "\"", sep="")
      blast.one<-DBI::dbGetQuery(conn, query)
      did.get<-unique(blast.one[,1])
      # query all but them
      if (inherits(did.get, "character")){did.get<-paste("\"", did.get, "\"", sep="")} # add quotes for characters
      did.get.paste<-paste(did.get, collapse=", ")
      query<-paste("SELECT * FROM refs WHERE NOT ids IN (", did.get.paste, ")",sep="")
      didnot.get<-DBI::dbGetQuery(conn, query)
      if (nrow(didnot.get)==0){
        add.row<-matrix(rep(NA, ncol(didnot.get)), nrow=1); colnames(add.row)<-colnames(didnot.get)
        return=rbind(didnot.get, add.row)
        return=cbind.data.frame("probe.id"=probe, "sp.all.n"=sp.all,
                                "sp.aligned.n"=NA,
                                "sp.aligned.percent"=NA,
                                "sp.not.aligned.n"=0,
                                "nonsp.n"=NA,
                                "type"="sp.not.aligned", return)
        if (add.blast.info==TRUE){# add columns if add.blast==TRUE
          add.data<-matrix(rep(NA, (ncol(data.blast.info))), nrow = 1, ncol=ncol(data.blast.info))
          colnames(add.data)<-colnames(data.blast.info)
          return<-cbind.data.frame(return, add.data)}
      }else{
        return<-cbind.data.frame("probe.id"=rep(probe, nrow(didnot.get)), "sp.all.n"=rep(sp.all, nrow(didnot.get)),
                                 "sp.aligned.n"=rep(NA, nrow(didnot.get)),
                                 "sp.aligned.percent"=rep(NA, nrow(didnot.get)),
                                 "sp.not.aligned.n"=rep(nrow(didnot.get), nrow(didnot.get)),
                                 "nonsp.n"=rep(NA, nrow(didnot.get)),
                                 "type"=rep("sp.not.aligned", nrow(didnot.get)), didnot.get)
        if (add.blast.info==TRUE){# add columns if add.blast==TRUE
          add.data<-matrix(rep(NA, (ncol(data.blast.info)*nrow(didnot.get))), nrow = nrow(didnot.get), ncol=ncol(data.blast.info))
          colnames(add.data)<-colnames(data.blast.info)
          return<-cbind.data.frame(return, add.data)}}
      return(return)}
    if (verbose) message ("Summarizing not aligned specific subjects")
    list.sp.not.aligned<-parallel::mclapply(X=probes, FUN=function(x) one.probe.sp.not.al(probe=x), mc.cores=mc.cores)
    list.names<-c(); for (i in 1:length(list.sp.not.aligned)){list.names[i]<-list.sp.not.aligned[[i]]$probe.id[1]} # names
    names(list.sp.not.aligned)<-list.names
    # delete not blasted for source
    if (check.blast.for.source==TRUE){
      blasted<-c(); for (i in 1:length(list.sp.aligned)){blasted[i]<-source %in% list.sp.aligned[[i]]$ids}
      num.del<-which(blasted==FALSE)
      if (length(num.del)>0){ list.sp.aligned<-list.sp.aligned[-num.del]; list.sp.not.aligned<-list.sp.not.aligned[-num.del]}
      if (verbose) message (length(num.del), " probes are not blasted for source and deleted")}
    # return
    # DELETE temp
    if (delete.temp.db==TRUE){if (verbose) message  ("Temporal database is deleted")
      file.remove(temp.db)} else{
        if (verbose) message ("Temporal database is saved in ", temp.db)
    }
    if (return=="list"){list.ret<-list("sp.aligned"=list.sp.aligned, "sp.not.aligned"=list.sp.not.aligned);
    if (verbose) message ("Result is returned as list"); return(list.ret)}
    if (return=="summary"){
      #make sum.data - summary
      data1<-data.frame(); data2<-data.frame()
      for (i in 1:length(list.sp.aligned)){
        data1<-rbind.data.frame(data1, list.sp.aligned[[i]][1,1:4])
        data2<-rbind.data.frame(data2, list.sp.not.aligned[[i]][1,c(1,5,6)])}
      if (sum(data1$probe.id==data2$probe.id)==nrow(data1)){sum.data<-cbind.data.frame(data1, data2[,2:3])
      } else{sum.data<-unite_two_DF(data1 = data1, data1.shared.var = data1$probe.id, data1.shared.column.num = 1,
                                    data2 = data2, data2.shared.var = data2$probe.id, data2.shared.column.num = 1)}
      colnames(sum.data)<-c("probe.id", "sp.all.n", "sp.aligned.n", "sp.aligned.percent", "sp.not.aligned.n", "nonsp.n")
      #write data into file or DB
      if (write.alignment=="file"){
        if (verbose) message ("Result is returned as summary. writing full results in files ", file.sp.aligned,
                    " and ", file.sp.not.aligned)
        for (i in 1:length(list.sp.aligned)){
          utils::write.table(x = list.sp.aligned[[i]], file = file.sp.aligned, append = T, sep=sep, row.names = F,
                      col.names=!file.exists(file.sp.aligned))
          utils::write.table(x = list.sp.not.aligned[[i]], file = file.sp.not.aligned, append = T, sep=sep, row.names = F,
                      col.names=!file.exists(file.sp.not.aligned))}}
      if (write.alignment=="DB"){
        if (verbose) message ("Result is returned as summary. writing full results in database ", alignment.db,
                    " and tables ", alignment.table.sp.aligned, " , ", alignment.table.sp.not.aligned)
        if (change.colnames.dots==TRUE){if (verbose) message ("Dots in column names are changed into underscore")}
        for (i in 1:length(list.sp.aligned)){
          #change colnames dots to underscore
          if (change.colnames.dots==TRUE){
            colnames(list.sp.aligned[[i]])<-gsub(x=colnames(list.sp.aligned[[i]]), pattern=".", replacement = "_", fixed=T)
            colnames(list.sp.not.aligned[[i]])<-gsub(x=colnames(list.sp.not.aligned[[i]]), pattern=".", replacement = "_", fixed=T)}
          write_to_DB(database = alignment.db, data=list.sp.aligned[[i]], table=alignment.table.sp.aligned, append=TRUE, verbose = F)
          write_to_DB(database = alignment.db, data=list.sp.not.aligned[[i]], table=alignment.table.sp.not.aligned, append=TRUE, verbose = F)}
        index_DB(database = alignment.db, table = alignment.table.sp.aligned, index.unique = FALSE, # index probe ids
                 index.column.name = colnames(list.sp.aligned[[1]])[1])
        index_DB(database = alignment.db, table = alignment.table.sp.not.aligned, index.unique = FALSE,
                 index.column.name = colnames(list.sp.not.aligned[[1]])[1])}
      return (sum.data)}
  } # here ends sp block
  # NONSP NONSP NONSP NONSP
  if (sum.aligned=="nonsp"){
    if (verbose) message ("Summarizing non specific alignment")
    if (verbose) message ("Create temporal database")
    #temp db
    data.db<-cbind.data.frame("probe"=blast.probe.id.var, "ids"=blast.res.id.var)
    if (titles==TRUE){data.db<-cbind.data.frame(data.db, "titles"=blast.res.title.var)}
    if (add.blast.info==TRUE){data.db<-cbind.data.frame(data.db, data.blast.info)}
    if (switch.ids==TRUE){
      if(titles==TRUE){colnames(switch.table)<-c("old", "new", "title")} else {colnames(switch.table)<-c("old", "new")} # make colnames
      write_to_DB(database = temp.db, data = switch.table, table = "switch")
      index_DB(database = temp.db, table = "switch", index.unique = FALSE, index.column.name = "old")
      switch.one<-function(id, return=c("newid", "title")){
        conn <- DBI::dbConnect (RSQLite::SQLite (), temp.db)
        on.exit(DBI::dbDisconnect (conn), add=T)
        if (inherits(id, "character")){id<-paste("\"", id, "\"", sep="")} # add quotes for characters
        query=paste("SELECT * FROM switch WHERE old =", id, sep=" ")
        new<-DBI::dbGetQuery(conn, query)
        if (return=="newid"){return(new$new)};if (return=="title"){return(new$title)} }
      new<-parallel::mclapply(X=data.db$ids, FUN=function(x) switch.one(id=x, return="newid"), mc.cores=mc.cores)
      data.db$ids<-unlist(new)
      if (titles==TRUE){
        new<-parallel::mclapply(X=data.db$ids, FUN=function(x) switch.one(id=x, return="title"), mc.cores=mc.cores)
        data.db$titles<-unlist(new)}} # - no need for switch in nonsp
    write_to_DB(database = temp.db, data = data.db, table = "blast2")
    index_DB(database = temp.db, table = "blast2", index.unique = c(FALSE, FALSE), index.column.name = c("probe", "ids"))
    # references db
    refs<-cbind.data.frame("ids"=reference.id.var);
    nums.rep<-duplicated(refs) # delete duplicates (for switch cases - no need in nonsp)
    if (sum(nums.rep)>0){refs<-refs[!nums.rep,]}
    write_to_DB(database = temp.db, data=refs, table="refs2")
    index_DB(database = temp.db, table="refs2", index.unique = TRUE, index.column.name = "ids")
    #get data
    one.probe.nonsp<-function(probe){
      conn <- DBI::dbConnect (RSQLite::SQLite (), temp.db) # connect
      on.exit(DBI::dbDisconnect (conn), add=T)
      query<-paste("SELECT ids FROM blast2 WHERE probe = \"", probe, "\"", sep="") # all result nums for one probe
      did.get<-DBI::dbGetQuery(conn, query)
      did.get<-did.get[,1] # got numbers --- choose only nonsp. from them
      if (inherits(did.get, "character")){did.get<-paste("\"", did.get, "\"", sep="")} # add quotes for characters
      did.get.paste<-paste(did.get, collapse=", ")
      query1<-paste("SELECT ids FROM refs2 WHERE ids IN (", did.get.paste, ")", sep="")
      query2<-paste("SELECT * FROM blast2 WHERE probe = \"", probe, "\" AND NOT ids in (", query1, ")", sep="")
      blast.one<-DBI::dbGetQuery(conn, query2)
      # in case 0 rows returned
      if (nrow(blast.one)==0){
        add.row<-matrix(rep(NA, ncol(blast.one)), nrow=1); colnames(add.row)<-colnames(blast.one)
        return=rbind(blast.one, add.row)
        return=cbind.data.frame("probe.id"=probe, "sp.all.n"=NA,
                                "sp.aligned.n"=NA,
                                "sp.aligned.percent"=NA,
                                "sp.not.aligned.n"=NA,
                                "nonsp.n"=0,
                                "type"="nonsp", return[,2:ncol(blast.one)])
      }else{
        # more than 0 rows returned
        #count nonsp aligned
        if (titles==TRUE){count.one<-blast.one[,1:3]} else{count.one<-blast.one[,1:2]}
        nums.dupl<-duplicated(count.one[,2]) # delete dupls by result id
        if(sum(nums.dupl)>0){count.one<-count.one[!nums.dupl,]}
        nonsp.n<-nrow(count.one)
        #return
        if (add.blast.info==TRUE){
          return=cbind.data.frame("probe.id"=blast.one$probe, "sp.all.n"=rep(NA, nrow(blast.one)), # added data - all rows are left
                                  "sp.aligned.n"=rep(NA, nrow(blast.one)),
                                  "sp.aligned.percent"=rep(NA, nrow(blast.one)),
                                  "sp.not.aligned.n"=rep(NA, nrow(blast.one)),
                                  "nonsp.n"=rep(nonsp.n, nrow(blast.one)),
                                  "type"=rep("nonsp", nrow(blast.one)), blast.one[,2:ncol(blast.one)])
        }else{
          return=cbind.data.frame("probe.id"=count.one$probe, "sp.all.n"=rep(NA, nrow(count.one)), # no added data - delete repeats
                                  "sp.aligned.n"=rep(NA, nrow(count.one)),
                                  "sp.aligned.percent"=rep(NA, nrow(count.one)),
                                  "sp.not.aligned.n"=rep(NA, nrow(count.one)),
                                  "nonsp.n"=rep(nonsp.n, nrow(count.one)),
                                  "type"=rep("nonsp", nrow(count.one)), count.one[,2:ncol(count.one)]) }
      }
      return(return)  }
    if (verbose) message ("Summarizing aligned non specific subjects")
    list.nonsp<-parallel::mclapply(X=probes, FUN=function(x) one.probe.nonsp(probe=x), mc.cores=mc.cores)
    list.names<-c(); for (i in 1:length(list.nonsp)){list.names[i]<-list.nonsp[[i]]$probe.id[1]} # names
    names(list.nonsp)<-list.names
    # return
    # DELETE temp
    if (delete.temp.db==TRUE){if (verbose) message ("Temporal database is deleted")
      file.remove(temp.db)} else{
        if (verbose) message ("Temporal database is saved in ", temp.db)
    }
    if (return=="list"){if (verbose) message ("Result is returned as list"); return(list.nonsp)}
    if (return=="summary"){
      sum.data<-data.frame(); for (i in 1:length(list.nonsp)){sum.data<-rbind.data.frame(sum.data, list.nonsp[[i]][1, 1:6])}
      if (write.alignment=="file"){
        if (verbose) message ("Result is returned as summary. writing full results in file ", file.nonsp)
        for (i in 1:length(list.nonsp)){utils::write.table(x=list.nonsp[[i]], file=file.nonsp, append = T,
                                                           sep=sep, row.names = F, colnames!=file.exists(file.nonsp))}}
      if (write.alignment=="DB"){
        if (verbose) message ("Result is returned as summary. writing full results in database ", alignment.db,
                    " and table ", alignment.table.nonsp)
        if (change.colnames.dots==TRUE){if (verbose) message ("Dots in column names are changed into underscore")}
        for (i in 1:length(list.nonsp)){
          #change colnames dots to underscore
          if (change.colnames.dots==TRUE){
            colnames(list.nonsp[[i]])<-gsub(x=colnames(list.nonsp[[i]]), pattern=".", replacement = "_", fixed=T)}
          write_to_DB(database = alignment.db, data=list.nonsp[[i]], table=alignment.table.nonsp, append=TRUE, verbose = F)}
        index_DB(database = alignment.db, table = alignment.table.nonsp, index.unique = FALSE, # index probe ids
                 index.column.name = colnames(list.nonsp[[1]])[1])}
      return(sum.data)}
  } # here ends nonsp block
}

