#' Read DNA/RNA Sequences in FASTA Format
#'
#' Read DNA/RNA Sequences in FASTA Format
#'
#' This function reads DNA/RNA sequences in FASTA format.
#'
#' @param file The name of the file which the sequences in fasta format are
#'             to be read from. If it does not contain an absolute or
#'             relative path, the file name is relative to the current
#'             working directory, \code{\link{getwd}}.
#'             The default here is to read the \code{example.fasta} file which
#'             is present in the \code{protseq} directory of the protr package.
#'
#' @param legacy.mode If set to \code{TRUE}, lines starting with a semicolon ';'
#'                    are ignored. Default value is \code{TRUE}.
#' @param seqonly If set to \code{TRUE}, only sequences as returned without
#'                attempt to modify them or to get their names and
#'                annotations (execution time is divided approximately
#'                by a factor 3). Default value is \code{FALSE}.
#'
#' @return The result character vector
#'
#' @keywords read FASTA
#'
#' @aliases readFASTA
#'
#' @note Note 
#'
#' @author Min-feng Zhu <\email{wind2zhu@@163.com}>
#'
#' @export readFASTA
#'
#' @references
#' Pearson, W.R. and Lipman, D.J. (1988)
#' Improved tools for biological sequence comparison.
#' \emph{Proceedings of the National Academy of Sciences
#' of the United States of America}, \bold{85}: 2444-2448
#'
#' @examples
#' x = readFASTA(system.file('dnaseq/hs.fasta', package = 'rDNAse'))


readFASTA = function (file, legacy.mode = TRUE, seqonly = FALSE) 
{
  lines = readLines(file)  
  
  if (legacy.mode) {
    comments = grep("^;", lines)
    if (length(comments) > 0) {
      lines = lines[-comments]
    }
  }
  
  ind = which(substr(lines, 1L, 1L) == ">")
  nseq = length(ind)
  
  if (nseq == 0) 
    stop ("no line starting with a > character found")
  
  start = ind + 1
  end = ind - 1
  end = c(end[-1], length(lines))
  sequences = lapply(seq_len(nseq), function(i) paste(lines[start[i]:end[i]], 
                                                       collapse = ""))
  sequences = lapply(sequences, toupper)
  dnacheck = sapply(sequences, function(i) all(strsplit(i, 
                                            split = "")[[1]]%in%c("A","G","C","T")))
  sequences = sequences[dnacheck]
  if (seqonly)  
      return(sequences)
  nomseq = lapply(seq_len(nseq), function(i) {
    firstword = strsplit(lines[ind[i]], " ")[[1]][1]
    substr(firstword, 2, nchar(firstword))
  })
  names(sequences) = nomseq[dnacheck]
  return(sequences)
}


