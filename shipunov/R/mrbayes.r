MrBayes <- function(x, file="", nst=6, rates="invgamma", ngammacat=4,
 nruns=2, ngen=1e+06, printfreq=100, samplefreq=10,
 nchains=4, savebrlens="yes", temp=0.2, burnin=10,
 contype="allcompat", run=FALSE,
 simple=TRUE, exec="mb-mpi", method="dna") {                           # three new options
 .write.nex <- function(x, file="", interleave=60, taxblock=FALSE) { # taken from the old ips (v.0.0.7)
  if (is(x, "alignment")) stop ("Sequence alignment must be of class 'DNAbin'")
  str2cha <- function(x) unlist(strsplit(x, ""))
  if (is(x, "DNAbin")) datatype <- "dna"
  if (is(x, "dist")) datatype <- "distances"
  if (is(x, "data.frame")) datatype <- "standard"
  missing <- if("?" %in% x[[1]]) "?" else "N"
  ntax <-  if (datatype == "distances") attr(x, "Size") else dim(x)[[1]]
  nchar <- dim(x)[[2]]
  taxnames <- if (datatype == "distances") labels(x) else rownames(x)
  header <- c("#NEXUS", paste("\n[created by ips on ", date(), "]\n", sep = ""))
  # TAXA BLOCK
  if (taxblock) {
   tb <- c("begin taxa;", paste("\tdimensions ntax=", ntax,";", sep = ""),
    "\ttaxlabels",  paste("\t", taxnames, sep = ""), ";\n")
  }
  # taxon names of same length
  len <- nchar(taxnames)
  mlen <- max(len)
  len <- mlen - len + 1
  foo <- function(x){
   x <- rep(" ", x)
   paste(x, collapse = "")
  }
  ws <- lapply(len, foo)
  taxnames <- paste(taxnames, ws, sep = "")
  # indices of partitions
  int <- if (!is.numeric(interleave)) "" else " interleave"
  if (!interleave) interleave <- nchar
   nbpart <- ceiling(nchar/interleave)
   pt <- matrix(nrow = nbpart, ncol = 2)
   pt[1, ] <- c(1, interleave)
   if (nbpart > 1)
    for (i in 2:(dim(pt)[1])) {
     pt[i, ] <- c(pt[i - 1, 2] + 1, pt[i - 1, 2] + interleave)
     pt[nbpart, 2] <- nchar
     }
  # assemble matrix
  m <- "matrix"
  for (i in seq(along = pt[, 1])) {
   sm <- as.character(x[, pt[i, 1]:pt[i, 2]])
   if (is.null(dim(sm))) sm <- as.matrix(sm, ncol = 1)
   sm <- apply(sm, 1, paste, collapse = "")
   sm <- paste(taxnames, sm)
   m <- c(m, sm)
  }
  m <- c(m, ";\nend;")
  # write DATA BLOCK
  if (datatype == "dna" || datatype == "standard") {
   if (datatype == "standard" && !identical(x[1, 1],
   round(x[1, 1])))
   datatype <- "continuous"
   if (!datatype == "standard") dt <- datatype
   else dt <- paste(datatype, " symbols=\"",
   paste(unique(unlist(x)), collapse = " "), "\"", sep = "")
   data <- if (taxblock) "characters" else "data"
   db <- c(paste("begin ", data, ";", sep = ""),
    paste("\tdimensions ntax=", ntax, " nchar=", nchar, ";", sep = ""),
    paste("\tformat datatype=", dt, " missing=", missing, " gap=-", int, ";", sep = ""), m)
  }
  # assemble NEXUS file:
  if (taxblock) nex <- c(header, tb, db) else nex <- c(header, db)
  # write NEXUS file
  if ( file == "" ) {
  cat(nex, sep = "\n")
  invisible(nex)
  } else {
  write(nex, file = file)
  }
 }
 method <- match.arg(method, choices=c("dna", "mixed"))
 if (method == "dna") {
 if (!inherits(x, "DNAbin"))
  stop("object 'x' is not of class 'DNAbin'")
 bayes <- c("\nbegin mrbayes;", paste("\tlset nst=", nst,
  " rates=", rates, " ngammacat=", ngammacat, ";", sep=""),
  paste("\tmcmc nruns=", nruns, " ngen=", as.integer(ngen),
   " printfreq=", printfreq, " samplefreq=", samplefreq,
   " nchains=", nchains, " savebrlens=", savebrlens,
   " temp=", temp, ";", sep=""), paste("\tsumt filename=",
   file, " burnin=", burnin, " contype=", contype,
   if(simple) { " conformat=simple;" },                                # 'simple format' allows to import node labels
   sep=""), "end;")
 if (file == "") {
  nexus <- .write.nex(x, interleave=FALSE)
  nexus <- c(nexus, bayes)
  cat(bayes, sep="\n")
 }
 else {
  nexus <- .write.nex(x, file="", interleave=FALSE)
  nexus <- c(nexus, bayes)
  write(nexus, file=file)
 }
 if (run) {
  if (.Platform$OS.type == "unix") {
   system(paste(exec, file, "| tee -a", paste0(file, ".out")))         # use 'exec' and view _and_ save output (into specific files)
  }
  else {
   system(paste("mrbayes ", file, ".bayes", sep=""))
  }
  tr <- ape::read.nexus(paste(file, ".con.tre", sep=""))
 }
 }
 if (method == "mixed") {
 ntax <- dim(x)[1]
 ncha <- dim(x)[2] # stantard data should be after DNA
 p2 <- min(which(x[1, ] %in% c("0", "1")))
 datatype <- paste("datatype=mixed(DNA:1-", p2-1, ",Standard:", p2, "-", ncha, ")", sep="")
 if ((p2-1) == 0) datatype <- paste("datatype=mixed(Standard:", p2, "-", ncha, ")", sep="")
 nexus <- vector(length=ntax + 11)
 nexus[1] <- "#NEXUS"
 nexus[c(2, 6, 8, ntax + 11)] <- ""
 nexus[3] <- "begin data;"
 nexus[4] <- paste("\tdimensions ntax=", ntax, " nchar=",
  ncha, ";", sep="")
 nexus[5] <- paste("\tformat", datatype, "missing=N gap=-;")
 nexus[7] <- "matrix"
 for (i in 1:ntax) {
  s <- paste(x[i, ], collapse="")
  s <- paste(rownames(x)[i], toupper(s))
  nexus[i + 8] <- s
 }
 nexus[ntax + 9] <- ";"
 nexus[ntax + 10] <- "end;"
 bayes <- vector(length=4)
 bayes[1] <- "begin mrbayes;"
 bayes[2] <- paste("\tlset nst=", nst, " rates=", rates, " ngammacat=",
  ngammacat, ";", sep="")
 bayes[3] <- paste("\tmcmc nruns=", nruns, " ngen=", as.integer(ngen),
  " printfreq=", printfreq, " samplefreq=", samplefreq,
  " nchains=", nchains, " savebrlens=", savebrlens, " temp=",
  temp, ";", sep="")
 bayes[4] <- paste("\tsumt filename=",
   file, " burnin=", burnin, " contype=", contype,
   if(simple) { " conformat=simple;" },                                # 'simple format' allows to import node labels
   "end;", sep="")
 nexus <- c(nexus, bayes)
 write(nexus, file)
 if (run) {
  if (.Platform$OS.type == "unix") {
   system(paste(exec, file, "| tee -a", paste0(file, ".out")))         # use 'exec' and view _and_ save output (into specific files)
  }
  else {
   system(paste("mrbayes ", file, ".bayes", sep=""))
  }
  tr <- ape::read.nexus(paste(file, ".con.tre", sep=""))
 }
 }
 return(tr)
}
