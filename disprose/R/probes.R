
#' Cut probes
#'
#' Generate probes from nucleotide reference sequences
#'
#'@param ref.seq.from.file logical; read reference sequences from file (\code{TRUE}) or download them from NCBI data base (\code{FALSE}).
#'@param ref.seq.id identification number of reference nucleotide sequences. Only used when \code{ref.seq.from.file = FALSE}.
#' GenBank accession numbers, GenInfo identifiers (GI) or Entrez unique identifiers (UID) may be used.
#'@param ref.seq.db character; NCBI database for search. See \link[rentrez]{entrez_dbs} for possible values.
#' Only used when \code{ref.seq.from.file = FALSE}.
#'@param fasta.file character; FASTA file name and path, only used when \code{ref.seq.from.file = TRUE}.
#'@param delete.fasta logical; delete FASTA file.
#'@param start,stop integer; number of first and last nucleotide of the reference sequence's segment that should be cut into probes.
#'All sequence is used by default.
#'@param start.correction logical; count probes' start and stop nucleotides relatively to the specified segment (\code{FALSE})
#'or to the whole sequence (\code{TRUE}). Only used if \code{start>1}.
#'@param size integer; vector of probe size
#'@param delete.incomplete logical; remove probes that contain undeciphered nucleotides
#'@param delete.identical logical; remove identical (duplicated) probes
#'@param give.probes.id logical; add probes' identification numbers
#'@param mc.cores integer; number of processors for parallel computation (not supported on Windows)
#'@param verbose logical; show messages
#'
#'@details
#'This function takes nucleotide sequences and cut them on segments (probes) of given size.
#'Sequences might be downloaded from given FASTA file or from NCBI data bases.
#'In the latter case, FASTA file is created.
#'If desired, FASTA file can be deleted after.
#'
#'Not all sequence must be cut on probes, you may define needed segment by \code{start} and \code{stop} parameters.
#'Note that in this case probes' start and stop nucleotides would be counted relatively to the specified segment (\code{start.correction = FALSE})
#' or to the whole sequence (\code{start.correction = TRUE}).
#'
#'Undeciphered nucleotides are the one that are indicated by "rywsmkhbvdn" symbols.
#'
#'Probes' identification numbers are created by adding numeric indexes to reference sequence's identification number.
#'
#'See \link{cut_string}, \link{delete_duplicates_DF} and \link{make_ids} for details.
#'
#'@return Data frame with probe id (optionally), sequence id, probe size, start and stop nucleotide, sequence.
#'
#'@examples
#'path <- tempdir()
#'dir.create (path)
#'# download and save as FASTA "Chlamydia pneumoniae B21 contig00001,
#'# whole genome shotgun sequence" (GI = 737435910)
#'if (!requireNamespace("rentrez", quietly = TRUE)) {
#'stop("Package \"rentrez\" needed for this function to work. Please install it.", call. = FALSE)}
#'reference.string <- rentrez::entrez_fetch(db = "nucleotide", id = 737435910,
#'                                          rettype="fasta")
#'write( x= reference.string, file = paste0 (path, "/fasta"))
#'probes <- cut_probes (ref.seq.from.file = TRUE, fasta.file = paste0(path, "/fasta"),
#'                      delete.fasta = TRUE, start = 1000, stop = 1500,
#'                      start.correction = FALSE, size = c(400, 500),
#'                      delete.incomplete = FALSE,
#'                      delete.identical = FALSE, give.probes.id = TRUE, mc.cores = 1)
#'unlink (path, recursive = TRUE)
#'
#' @author Elena N. Filatova
#' @name cut_probes
#' @export
cut_probes <- function (ref.seq.from.file = FALSE, ref.seq.id, ref.seq.db, fasta.file = NULL, delete.fasta = FALSE,
                     start = 1, stop = NULL, start.correction = FALSE, size=24:32,
                     delete.incomplete = FALSE, delete.identical = FALSE, give.probes.id = FALSE, mc.cores = 1, verbose = TRUE){
  # test package dependencies
  if (!requireNamespace("seqinr", quietly = TRUE)) { stop("Package \"seqinr\" needed for this function to work. Please install it.", call. = FALSE)}
  if (ref.seq.from.file == TRUE){if (!requireNamespace("rentrez", quietly = TRUE)) { stop("Package \"rentrez\" needed for this function to work. Please install it.", call. = FALSE)}}
  if (!requireNamespace("dplyr", quietly = TRUE)) { stop("Package \"dplyr\" needed for this function to work. Please install it.", call. = FALSE)}
  if (delete.incomplete==TRUE) {if (!requireNamespace("stringr", quietly = TRUE)) { stop("Package \"stringr\" needed for this function to work. Please install it.", call. = FALSE)}}
  #download reference sequence
  if (is.null(fasta.file)==TRUE){stop ("Set fasta.file name and path")} # fasta file name
  if (ref.seq.from.file==FALSE){ # read from NCBI if there is no fasta
    if (file.exists(fasta.file) == TRUE) {warning (paste0("FASTA ", fasta.file, " already exists. New sequences will be added to file."))}
    if (verbose) message ("Downloading and writing reference sequence to ", fasta.file)
    for (i in 1:length(ref.seq.id)){reference<-try(rentrez::entrez_fetch(db=ref.seq.db, id=ref.seq.id[i], rettype="fasta"))
    if (class(reference)[1] == "try-error"){stop("Could not download sequence. Please try again.")}
    write(x=reference, file=fasta.file, append = T)}}
  #read fasta file and make ids
  ref.seq.list<-seqinr::read.fasta(file=fasta.file, as.string = TRUE)
  ref.seq.vector<-as.character(ref.seq.list)
  ref.ids<-names(ref.seq.list)
  if (delete.fasta==TRUE){if (verbose) message ("FASTA file is deleted")
    file.remove(fasta.file)}
  #start and stop nucleotides
  if(is.null(stop)==TRUE){stop=nchar(ref.seq.vector)}
  ref.seq.vector<-substr(ref.seq.vector, start, stop)
  # cutting
  zond.list<-lapply(X=ref.seq.vector, FUN=function(x) cut_string(string = x, size=size))
  #list to data
  id<-c();  for (i in 1:length(zond.list)){ id.min<-rep(ref.ids[i], nrow(zond.list[[i]]));id<-c(id, id.min) } # id vector for dataframe
  zond.data<-dplyr::bind_rows(zond.list, .id = NULL)
  zond.data<-cbind.data.frame(seq.id=id, zond.data) # data with sequence id
  #delete RW - unknown nucleotides
  if(delete.incomplete==TRUE){if (verbose) message  ("Checking for probes with undeciphered nucleotides")
    nums<-stringr::str_detect(zond.data$zond, "[rywsmkhbvdn]")
    if (sum(nums)!=0){if (verbose) message (sum(nums), " probes are deleted")
      zond.data<-zond.data[-nums,]
    } else{if (verbose) message ("There are no probes with undeciphered nucleotides")} }
  #delete identical probes
  if (delete.identical==TRUE){if (verbose) message ("Checking for identical probes")
    zond.data<-delete_duplicates_DF(data=zond.data, duplicated.var = zond.data$segment, exact = TRUE, stay = "first", mc.cores = mc.cores)}
  # Add name probes
  if (give.probes.id==TRUE){if (verbose) message  ("Adding probes ids")
    zond.id<-make_ids(var=zond.data$seq.id, sep="_"); zond.data<-cbind.data.frame(zond.id, zond.data)}
  # add start and stop correction if not all sequence is used
  if(start.correction==TRUE){
    seqs<-unique(zond.data$seq.id)
    if(length(start)<length(seqs)){start<-rep(start, length(seqs))}
    if(length(stop)<length(seqs)){stop<-rep(stop, length(seqs))}
    for (i in 1:length(seqs)){
      nums<-which(zond.data$seq.id==seqs[i])
      zond.data$start[nums]<-zond.data$start[nums]+start[i]-1
      zond.data$stop[nums]<-zond.data$stop[nums]+start[i]-1 }}
  #colnames
  colnames(zond.data)<-c("probe.id", "seq.id", "size", "start", "stop", "probe")
  return(zond.data)
}

#' Calculate physical and chemical properties
#'
#' Calculates GC-content, detects several nucleotides in a row, calculates minimum folding energy and melting temperature for oligonucleotide probes.
#'
#'@param probe.var character; vector of nucleotide probes
#'@param trim,trim.gc,trim.rep,trim.mfe,trim.tm logical; whether to select results that meet the criterion
#'@param GCmin,GCmax numeric; minimum and maximum value of GC-content (percent, used if \code{trim = TRUE})
#'@param nucl.pattern character; vector of nucleotide pattern
#'@param n.crit integer; minimal amount of nucleotide pattern's repeats in a row to detect
#'@param RNAfold.path character; name and path to RNAfold executable file
#'@param temperature numeric; folding design temperature
#'@param MFEmin numeric; maximum value of folding energy (used if \code{trim = TRUE})
#'@param TD.params character; vector of length 4, contains designation for four tables with thermodynamic values
#'(nn_table - thermodynamic NN values, tmm_table - thermodynamic values for terminal mismatches,
#'imm_table -  thermodynamic values for internal mismatches, de_table - thermodynamic values for dangling ends).
#'See \link[TmCalculator]{Tm_NN} for details.
#'@param TMmin,TMmax numeric; minimum and maximum value of melting temperature (used if \code{trim = TRUE})
#'@param digits integer; number of decimal places to round the result
#'@param mc.cores integer; number of processors for parallel computation (not supported on Windows)
#'@param MFE.files.dir character; directory for RNAfold input and output files
#'@param delete.MFE.files logical; delete RNAfold input and output files
#'@param add.to.data,data logical; add result vector to specified data frame (used unconditionally if \code{trim = TRUE})
#'@param verbose logical; show messages
#'@param Na numeric; millimolar concentration of Na, default is 50 (used for \code{count_TM} function)
#'@param K numeric; millimolar concentration of K, default is 0 (used for \code{count_TM} function)
#'@param Tris numeric; millimolar concentration of Tris, default is 0 (used for \code{count_TM} function)
#'@param Mg numeric; millimolar concentration of Mg, default is 0 (used for \code{count_TM} function)
#'@param dNTPs numeric; millimolar concentration of dNTPs, default is 0 (used for \code{count_TM} function)
#'
#'@details
#' GC-content trimming selects results that are between \code{GCmin} and \code{GCmax} (inclusive).
#' Nucleotides' amount trimming deletes probes that contain \code{n.crit} or more of same nucleotides (pattern) in a row.
#' Minimum folding energy trimming selects results that are equal or more than \code{MFEmin}.
#' Melting temperature trimming selects results that are between \code{TMmin} and \code{TMmax} (inclusive).
#'
#' This function is using ViennaRNA service to count minimum folding energy. ViennaRNA Package (UNIX or Windows) must be installed.
#' While counting MFE, working directory is set to \code{MFE.files.dir} and input and output files
#' for ViennaRNA ("seq_in" and "seq_out") are created in the working directory.Afterwards the working directory is changed back to user's setting.
#' If no \code{MFE.files.dir} exists it is created and is not deleted even if \code{delete.MFE.files = TRUE}.
#'
#' Melting temperature is counted with \link[TmCalculator]{Tm_NN} function. Indication of thermodynamic values must be provided.
#' By default they are: nn_table = "DNA_NN4", tmm_table = "DNA_TMM1", imm_table = "DNA_IMM1", de_table = "DNA_DE1".
#'
#'@return
#'If \code{trim = FALSE}, \code{count_PhCh} function returns data frame with GC-count (\code{GC.percent}),
#'nucleotide repeats (\code{repeats}, TRUE/FALSE), minimum folding energy (\code{MFE}) and melting temperature (\code{TM}) columns.
#' If \code{trim = TRUE}, \code{count_PhCh} function returns provided data frame with attached four columns
#' and rows selected according to values \code{GCmin, GCmax, n.crit, MFEmin, TMmin, TMmax}.
#'
#' If \code{trim.gc= FALSE}, \code{count_GC} function returns \code{GC.percent} vector or data with attached \code{GC.percent} column (when \code{add.to.data = TRUE}).
#' If \code{trim.gc = TRUE}, \code{count_GC} function returns provided data frame with attached \code{GC.percent} column and rows selected according to \code{GCmin, GCmax} values.
#'
#' If \code{trim.rep = FALSE}, \code{count_REP} function returns \code{repeats} vector (logical; TRUE/FALSE - there are/there are no nucleotide repeats) or data with attached \code{repeats} column (when \code{add.to.data = TRUE}).
#' If \code{trim.rep = TRUE}, \code{count_REP} function returns provided data frame with attached \code{repeats} column and rows selected according to \code{n.crit} value.
#'
#' If \code{trim.mfe = FALSE}, \code{count_MFE} function returns \code{MFE} vector or data with attached \code{MFE} column (when \code{add.to.data = TRUE}).
#' If \code{trim.mfe = TRUE}, \code{count_MFE} function returns provided data frame with attached \code{MFE} column and rows selected according to \code{MFEmin} value.
#'
#' If \code{trim.tm = FALSE}, \code{count_TM} function returns \code{TM} vector or data with attached \code{TM} column (when \code{add.to.data = TRUE}).
#' If \code{trim.tm = TRUE}, \code{count_TM} function returns provided data frame with attached \code{TM} column and rows selected according to \code{TMmin, TMmax} values.
#'
#'@examples
#'probes <- data.frame (ids = 1:3,  probes = c ("acacacacacaca", "aaaaagggggtttttccccc",
#'                                              "atgcgctagctcagc"))
#'probes <- count_GC (probe.var = probes$probes, trim.gc = FALSE, GCmin = 40, GCmax = 60,
#'                    add.to.data = TRUE, data = probes)
#'
#'probes <- count_REP (probe.var = probes$probes, trim.rep = FALSE, n.crit = 5,
#'                     add.to.data = TRUE, data = probes)
#'\dontrun{
#'# This function is using ViennaRNA service. ViennaRNA Package must be installed.
#'MFE.files.dir <- tempdir()
#'probes <- count_MFE (probe.var = probes$probes, RNAfold.path = "D:/Vienna/RNAfold.exe",
#'                     temperature = 40, trim.mfe = FALSE, MFEmin = 0,
#'                     MFE.files.dir = MFE.files.dir, delete.MFE.files = TRUE,
#'                     add.to.data = TRUE, data = probes, mc.cores = 1)
#'unlink (MFE.files.dir, recursive = TRUE)
#'}
#'probes <- count_TM (probe.var = probes$probes, TD.params = NULL, trim.tm = FALSE,
#'                    TMmin = 55, TMmax = 60, add.to.data = TRUE, data = probes,
#'                    digits = 4, mc.cores = 1)
#'# All in one command
#'\dontrun{
#'# This function is using ViennaRNA service. ViennaRNA Package must be installed.
#'MFE.files.dir <- tempdir()
#'probes2 <- count_PhCh (probe.var = probes$probes, trim = FALSE,
#'                       nucl.pattern = c ("a", "t", "g", "c"), n.crit = 5,
#'                       MFE.files.dir = MFE.files.dir, delete.MFE.files = TRUE,
#'                       RNAfold.path = "D:/Vienna/RNAfold.exe", temperature = 40,
#'                       TD.params = NULL, digits = 3, mc.cores = 1,
#'                       data = probes)
#'unlink (MFE.files.dir, recursive = TRUE)
#'}
#'
#'@references
#' Lorenz R., Stephan H.B., Höner zu Siederdissen C. et al. (2011). ViennaRNA Package 2.0. Algorithms for Molecular Biology, 6, 1.
#' \url{https://almob.biomedcentral.com/articles/10.1186/1748-7188-6-26}.
#'
#'@author Elena N. Filatova
#'@name count_PhCh
NULL
#'
#'@describeIn count_PhCh Calculates GC.percent, detects several nucleotides in a row, calculates minimum folding energy and melting temperature
#'@export

count_PhCh <- function (probe.var, trim = FALSE, data, digits = 4, mc.cores = 1, MFE.files.dir = NULL, delete.MFE.files = FALSE,
                     GCmin = 40, GCmax = 60, nucl.pattern = c ("a", "t", "g", "c"), n.crit = 5, RNAfold.path, temperature = 40, MFEmin = -3,
                     TD.params = NULL, TMmin = 55, TMmax = 60, verbose = TRUE,
                     Na = 50, K = 0, Tris = 0, Mg = 0, dNTPs=0){
  # test package dependencies
  if (!requireNamespace("parallel", quietly = TRUE)) { stop("Package \"parallel\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("stringr", quietly = TRUE)) { stop("Package \"stringr\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("utils", quietly = TRUE)) { stop("Package \"stringr\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("TmCalculator", quietly = TRUE)) { stop("Package \"TmCalculator\" needed for this function to work. Please install it.", call. = FALSE)}
  # run
  if (verbose) message  ("Counting CG rate")
  GC.percent<-count_GC(probe.var = probe.var, trim.gc = FALSE, mc.cores=mc.cores, add.to.data = FALSE, digits=digits)
  if(trim==TRUE){nums<-which(GC.percent>=GCmin & GC.percent<=GCmax)
  probe.var<-probe.var[nums]; GC.percent<-GC.percent[nums]; data<-data[nums,]
  data<-cbind.data.frame(data, GC.percent)}
  if (verbose) message ("Counting nucleotide repeats")
  repeats<-count_REP(probe.var = probe.var, trim.rep = FALSE, nucl.pattern = nucl.pattern,
                     n.crit = n.crit, mc.cores=mc.cores, add.to.data = FALSE)
  if(trim==TRUE){nums<-which(repeats==FALSE)
  probe.var<-probe.var[nums]; repeats<-repeats[nums]; data<-data[nums,]
  data<-cbind.data.frame(data, repeats)}
  if (verbose) message  ("Counting MFE")
  MFE<-count_MFE(probe.var = probe.var, RNAfold.path = RNAfold.path, temperature = temperature, trim.mfe = FALSE, add.to.data = FALSE,
                 MFE.files.dir = MFE.files.dir, delete.MFE.files = delete.MFE.files,
                 mc.cores = mc.cores, digits = digits, verbose = verbose)
  if(trim==TRUE){nums<-which(MFE>=MFEmin)
  probe.var<-probe.var[nums]; MFE<-MFE[nums]; data<-data[nums,]
  data<-cbind.data.frame(data, MFE)}
  if (verbose) message ("Counting melting temperature")
  TM<-count_TM(probe.var = probe.var, TD.params = TD.params, trim.tm = FALSE, add.to.data = FALSE, digits=digits,
               mc.cores = mc.cores, verbose = verbose, Na = Na, K = K, Tris = Tris, Mg = Mg, dNTPs=dNTPs)
  if(trim==TRUE){nums<-which(TM>=TMmin & TM<=TMmax)
  probe.var<-probe.var[nums]; TM<-TM[nums]; data<-data[nums,]
  data<-cbind.data.frame(data, TM)}
  #return
  if(trim==TRUE){data.res<-data}else{data.res<-data.frame(GC.percent,repeats, MFE, TM)}
  return(data.res)}

#'@describeIn count_PhCh Calculates GC-content (percent)
#'@export
count_GC <- function (probe.var, trim.gc = FALSE, GCmin = 40, GCmax = 60, mc.cores = 1, add.to.data = FALSE, data, digits = 4){
  # test package dependencies
  if (!requireNamespace("stringr", quietly = TRUE)) { stop("Package \"stringr\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("parallel", quietly = TRUE)) { stop("Package \"parallel\" needed for this function to work. Please install it.", call. = FALSE)}
  # run
  get_GC<-function(zond){GC.percent<-sum(stringr::str_count(zond, c("g", "c")))/nchar(zond)} # GCcount fun for 1 zond
  list<-parallel::mclapply(X=probe.var, FUN=function(x) get_GC(zond=x), mc.cores = mc.cores)
  GC.percent<-unlist(list);GC.percent<-GC.percent*100; GC.percent<-round(GC.percent, digits = digits);
  if(trim.gc==FALSE){ifelse(add.to.data==FALSE, return(GC.percent), { # no trimming. return vector or data
    data.res<-cbind.data.frame(data, GC.percent); return(data.res) })}
  if(trim.gc==TRUE){ # trimming results
    nums<-which(GC.percent>=GCmin & GC.percent<=GCmax)
    GC.percent<-GC.percent[nums]; data<-data[nums,]
    data.res<-cbind.data.frame(data, GC.percent)
    return(data.res)}}

#'@describeIn count_PhCh Detects several nucleotides in a row
#'@export
count_REP <- function (probe.var, trim.rep = FALSE,  nucl.pattern = c ("a", "t", "g", "c"),
                       n.crit = 5, mc.cores = 1, add.to.data = FALSE, data){
  # test package dependencies
  if (!requireNamespace("parallel", quietly = TRUE)) { stop("Package \"parallel\" needed for this function to work. Please install it.", call. = FALSE)}
  # run
  pattern.crit <- c(); for (i in 1:length(nucl.pattern)) {
    pattern.crit [i]<- paste (rep (nucl.pattern[i], n.crit), collapse = "")}
  get_rep<-function(zond){
    grep.c<-c();
    for (i in 1:length(pattern.crit)){grep.c[i]<-grepl(pattern.crit[i], zond, ignore.case =TRUE)}
    Rep.sum<-sum(grep.c)}
  list<-parallel::mclapply(X=probe.var, FUN=function(x) get_rep(zond=x), mc.cores=mc.cores)
  Rep.sum<-unlist(list)
  repeats<-c(); for (i in 1:length(Rep.sum)){repeats[i]<-Rep.sum[i]>0} # TRUE - there are repeats
  if(trim.rep==FALSE){ifelse(add.to.data==FALSE, return(repeats), { # no trimming. return vector or data
    data.res<-cbind.data.frame(data, repeats); return(data.res) })}
  if(trim.rep==TRUE){
    nums<-which(repeats==FALSE)
    repeats<-repeats[nums]; data<-data[nums,]
    data.res<-cbind.data.frame(data, repeats)
    return(data.res)}}

#'@describeIn count_PhCh Calculates minimum folding energy
#'@export
count_MFE <- function (probe.var, RNAfold.path, temperature = 40, trim.mfe = FALSE, MFEmin = -3, add.to.data = FALSE, data,
                       MFE.files.dir=NULL, delete.MFE.files = FALSE, mc.cores = 1, digits = 4, verbose = TRUE){
  # test package dependencies
  if (!requireNamespace("parallel", quietly = TRUE)) { stop("Package \"parallel\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("utils", quietly = TRUE)) { stop("Package \"utils\" needed for this function to work. Please install it.", call. = FALSE)}
  # test files directory
  if (is.null (MFE.files.dir) == TRUE) {stop ("Set MFE.files.dir parameter")}
  #change work directory
  old<-getwd()
  if (file.exists(MFE.files.dir)== FALSE){dir.create(MFE.files.dir)}
  setwd(MFE.files.dir)
  on.exit(setwd(old))
  #test that mc.cores < amount of probes
  if (mc.cores>length(probe.var)){stop("Number of cores are bigger than number of probes. Ajust mc.cores")}
  # divide probe.var on mc.cores
  vect.len<-length(probe.var);  one.peace<-floor(vect.len/(mc.cores))
  v.start<-seq(from=1, to=vect.len, by=one.peace); v.start<-v.start[1:mc.cores]
  v.stop<-v.start+one.peace-1; v.stop[mc.cores]<-vect.len; v.stop<-v.stop[1:mc.cores]
  iis<-1:mc.cores # this will go to ::mclapply
  # RNAfold params and command
  file.in<-paste(getwd(), "/seq_in", iis, sep="") # files in
  if(.Platform$OS.type == "windows") {file.out<-paste("--outfile=seq_out", iis, sep="") # files out for Windows and Unix
  } else {file.out<-paste("--outfile=", paste(getwd(), "/seq_out", iis, sep=""), " --filename-delim=/", sep="")}
  temp.full<-paste("--temp=", temperature, sep="") #temperature attribute
  params<-"--noPS" #addition attribute for not writing plots
  comand<-paste(RNAfold.path, file.in, temp.full, file.out, params)
  # run RNAfold for every iis
  get_mfe<-function(i){
    zond<-probe.var[v.start[i]:v.stop[i]] # write part of probe.var into file
    if (file.exists(file.in[i])){warning(paste("Note that", file.in[i], "file already exists. Adding sequences to",  file.in[i], "file.", sep=" "))} # check if there are files already
    utils::write.table(zond, file=file.in[i], quote=FALSE, row.names = FALSE, col.names = FALSE)
    system(comand[i])} # run RNAfold
  parallel::mclapply(X=iis, FUN<-function(x) get_mfe(i=x), mc.cores=mc.cores) # run RNAfold for all iis and get bunch of out files
  if (verbose) message ("seq_in and seq_out files are created in the working directory")
  #get MFE from out files
  #function for getting MFE from file text
  split_func<-function(x){ mfe<-as.numeric(gsub( ")", "", strsplit(x, split=" (", fixed=TRUE)[[1]][2]));return(mfe)} # divide all in 2 parts [by " ("], take the 2nd part. Delete the bracket and write as numeric
  #get MFE
  MFE<-c(); for (i in 1:length(iis)){
    dat.min<-data.frame(); mfe.min<-c()
    dat.min<-utils::read.csv(file=paste(getwd(), "/seq_out", iis[i], sep=""), header=FALSE) #read file
    nums<-seq(from=2, to=nrow(dat.min), by=2); dat.min<-dat.min[nums,] # take each 2nd line
    mfe.min<-lapply(X=dat.min, FUN=function(x) split_func(x=x)) # get MFE from text
    mfe.min<-unlist(mfe.min)
    MFE<-c(MFE, mfe.min)}
  MFE<-round(MFE, digits=digits)
  # delete in and out files
  if (delete.MFE.files==TRUE) {if (verbose) message  ("seq_in and seq_out files are deleted")
    file.remove(file=file.in); file.remove(file=paste(MFE.files.dir, "/seq_out", iis, sep=""))}
  #return
  if(trim.mfe==FALSE){ifelse(add.to.data==FALSE, return(MFE), { # no trimming. return vector or data
    data.res<-cbind.data.frame(data, MFE); return(data.res) })}
  if(trim.mfe==TRUE){
    nums<-which(MFE>=MFEmin)
    MFE<-MFE[nums]; data<-data[nums,]
    data.res<-cbind.data.frame(data, MFE)
    return(data.res)}
}

#'@describeIn count_PhCh Calculates melting temperature
#'@export
count_TM <- function (probe.var, TD.params = NULL, trim.tm = FALSE, TMmin = 55, TMmax = 60,
                      add.to.data = FALSE, data, digits = 4, mc.cores = 1, verbose = TRUE,
                      Na = 50, K = 0, Tris = 0, Mg = 0, dNTPs=0){
  # test package dependencies
  if (!requireNamespace("parallel", quietly = TRUE)) { stop("Package \"parallel\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("TmCalculator", quietly = TRUE)) { stop("Package \"TmCalculator\" needed for this function to work. Please install it.", call. = FALSE)}
  # TD parameters
  if (is.null(TD.params)==TRUE){if (verbose) message ("Setting thermodynamic parameters. Default is: nn_table = DNA_NN4, tmm_table = DNA_TMM1,imm_table = DNA_IMM1, de_table = DNA_DE1")
    TD.params=c("DNA_NN4", "DNA_TMM1", "DNA_IMM1", "DNA_DE1")
  } else {
    if (length(TD.params)!=4){warning ("There must be four thermodynamic values in TD.param. Setting nn_table = DNA_NN4, tmm_table = DNA_TMM1, imm_table = DNA_IMM1, de_table = DNA_DE1")
      TD.params=c("DNA_NN4", "DNA_TMM1", "DNA_IMM1", "DNA_DE1")}
    if(class(TD.params)!="character"){warning ("Mistake in thermodynamic values in TD.param. Setting nn_table = DNA_NN4, tmm_table = DNA_TMM1, imm_table = DNA_IMM1, de_table = DNA_DE1")
      TD.params=c("DNA_NN4", "DNA_TMM1", "DNA_IMM1", "DNA_DE1")}}
  # run Tm
  TM.list<-parallel::mclapply(X=probe.var, FUN=function(x) TmCalculator::Tm_NN(ntseq=x, nn_table = TD.params[1], tmm_table = TD.params[2],
                                                                               imm_table = TD.params[3], de_table = TD.params[4],
                                                                               Na = Na, K = K, Tris = Tris, Mg = Mg, dNTPs = dNTPs), mc.cores=mc.cores)
  TM<-c(); for (i in 1:length(TM.list)){TM[i]<-TM.list[[i]][[1]]} # get TM from list
  TM<-round(TM, digits=digits)
  #return
  if(trim.tm==FALSE){ifelse(add.to.data==FALSE, return(TM), { # no trimming. return vector or data
    data.res<-cbind.data.frame(data, TM); return(data.res) })}
  if(trim.tm==TRUE){
    nums<-which(TM>=TMmin & TM<=TMmax)
    TM<-TM[nums]; data<-data[nums,]
    data.res<-cbind.data.frame(data, TM)
    return(data.res)}
}

#' Add adapters to probes
#'
#' Add set of adapters to oligonucleotide probes
#'
#' @param probe.id.var vector of probes' identification numbers
#' @param probe.var character; character; vector of nucleotide probes
#' @param ad.len integer; vector of adapter length
#' @param ad.nucl character; vector of adapter nucleotides
#' @param end integer; probe's end for adapter attachment. Possible values are 3 and 5.
#' @param mc.cores integer; number of processors for parallel computation (not supported on Windows)
#' @param digits integer; number of decimal places to round the result (MFE)
#' @param return character; returned object; possible values are: \code{"vector"} (vector of nucleotide probes with added adapters),
#' \code{"dataframe"} (data frame with probes, adapters and their characteristics),
#' \code{"add"} (user's data frame with added data of probes, adapters and their characteristics)
#' @param data,data.probe.id.var user's data frame and it's variable with probes identification numbers (used if \code{return = "add"})
#' @param count.mfe logical; count minimum folding energy for probes with adapters
#' @param RNAfold.path,temperature,trim.mfe,MFEmin,MFE.files.dir,delete.MFE.files used if \code{count.mfe = TRUE};
#' see \link{count_MFE}[disprose] for details
#' @param verbose logical; show messages
#'
#' @details
#' \code{ad.len} parameter indicates number of \code{ad.nucl} repeats.
#' For example, with \code{ad.len =5} for \code{ad.nucl = "t"} adapter will be \code{"ttttt"} and for
#' \code{ad.nucl = "ac"} adapter will be \code{"acacacacac"}.
#'
#' \code{ad.len}, \code{ad.nucl} and \code{end} might be vectors of any length.
#' All possible variants of adapters will be added to probes and tested.
#'
#' For MFE counting ViennaRNA Package (UNIX or Windows) must be installed. see \link{count_MFE}[disprose] for details
#'
#'@return
#' Vector of nucleotide probes with added adapters, or data frame with probes, adapters and their characteristics,
#'  or user's data frame with added data of probes, adapters and their characteristics.
#'
#'@examples
#'probes <- data.frame (ids = 1:3,  probes = c ("acacacacacaca", "aaaaagggggtttttccccc",
#'                                              "atgcgctagctcagc"))
#'ad.data <- add_adapters(probe.var = probes$probes, probe.id.var = probes$ids,
#'                        ad.len = c(5, 8), ad.nucl = c("t", "dt"), end = c(3, 5),
#'                        count.mfe = FALSE, mc.cores = 1, digits = 4,
#'                        return = "dataframe", data = probes, data.probe.id.var = probes$ids)
#'
#'@author Elena N. Filatova
#'@name add_adapters
#'@export
add_adapters <- function (probe.id.var, probe.var, ad.len, ad.nucl = "t", end = c (3,5), mc.cores = 1, digits = 4,
                          return = "dataframe", data, data.probe.id.var, count.mfe = FALSE,
                          RNAfold.path, temperature = 40, trim.mfe = FALSE, MFEmin = 0,
                          MFE.files.dir = NULL, delete.MFE.files = FALSE, verbose = TRUE){
  #check return
  if (return!="vector" & return!="dataframe" & return!="add"){stop("Choose return parameter")}
  #create all possible adaptors
  ad.data<-expand.grid(ad.len, ad.nucl, end); ad.data$Var2<-as.character(ad.data$Var2)
  adaptor<-c(); for (i in 1:nrow(ad.data)){adaptor[i]<-paste(rep(ad.data$Var2[i], ad.data$Var1[i]), collapse = "")}
  ad.data<-cbind.data.frame(end=ad.data$Var3, adaptor, ad.length=ad.data$Var1)
  #add adaptors to probe
  probe.data<-data.frame()
  for (i in 1:length(probe.var)){
    probe.id<-rep(probe.id.var[i], nrow(ad.data))
    probe=rep(probe.var[i], nrow(ad.data))
    probe.ad<-c()
    for (j in 1:nrow(ad.data)){
      if (ad.data$end[j]==3){probe.ad[j]<-paste(c(probe.var[i], ad.data$adaptor[j]), collapse="")}
      if (ad.data$end[j]==5){probe.ad[j]<-paste(c(ad.data$adaptor[j], probe.var[i]), collapse="")}   }
    data.one<-cbind.data.frame(probe.id, probe, adaptor=ad.data$adaptor, ad.length=ad.data$ad.length, ad.end=ad.data$end, probe.ad)
    probe.data<-rbind.data.frame(probe.data, data.one)}
  #count MFE
  if (count.mfe==TRUE){
    if (verbose) message ("Counting MFE")
    probe.data<-count_MFE(probe.var=probe.data$probe.ad, RNAfold.path = RNAfold.path, temperature = temperature,
                          trim.mfe = trim.mfe, MFEmin = MFEmin, add.to.data = TRUE, data=probe.data,
                          MFE.files.dir = MFE.files.dir, delete.MFE.files = delete.MFE.files, mc.cores = mc.cores, digits = digits)
    colnames(probe.data)<-c(colnames(probe.data)[1:6], "ad.MFE")}
  #returning
  if(return=="vector"){res<-probe.data$probe.ad}
  if(return=="dataframe"){res<-probe.data}
  if(return=="add"){
    if (nrow (probe.data) != nrow (data)) {
      res<-probe.data
      warning ("There are more than 1 adapter variant. Cannot attach adaptors to data frame. Return result data only.")
    } else {
      probes<-unique(data$probe.id)
      res<-data.frame()
      for (i in 1:length(probes)){
        data.row<-data[data.probe.id.var==probes[i],]
        ad.rows<-probe.data[probe.data$probe.id==probes[i],]
        data.row<-rbind(data.row, data.row[rep(1, (nrow(ad.rows)-1)), ])
        res.one<-cbind.data.frame(data.row, ad.rows)
        res<-rbind.data.frame(res, res.one)  }}}
  return(res)}
