#' Create a header for a _rc file of popoolation2
#'
#' Creates a header for files in the _rc format of the \code{popoolation2}
#' software. This header can be applied to a matrix as column names.
#'
#' Please note that the first 9 columns are a default output of the
#' \code{popoolation2} software and thus this functions maintains the same
#' names.
#'
#' @param nPops is an integer specifying how many different populations exist in
#'   the _rc file.
#'
#' @return a character vector with the column names for a _rc popoolation2 file.
#'
#' @examples createHeader(nPops = 10)
#'
#' @export
createHeader <- function(nPops) {

  # create a vector with the information that is always present in the _rc files of popoolation2
  info <- c("chr", "pos", "rc", "allele_count", "allele_states", "deletion_sum", "snp_type",
            "major_alleles(maa)", "minor_alleles(mia)")

  # create a vector with the name of the columns containing the number of major allele reads for all the populations
  major <- paste0('maa_', 1:nPops)
  # create a vector with the name of the columns containing the number of minor allele reads for all the populations
  minor <- paste0('mia_', 1:nPops)

  # combine all of the previous vectors into a final vector - this is the header for the popoolation2 data
  header <- c(info, major, minor)
  # output the header
  header
}


#' Import a single file containing data in \code{popoolation2} format
#'
#' Load a file that is in the _rc format of the \code{popoolation2} software.
#'
#' This function will import a single file containing data in the _rc format.
#' Note that this function will remove all non biallelic sites and sites where
#' the sum of deletions in all populations is not zero.
#'
#' The first 9 columns of the matrix contain general information about the data
#' and the number of major-allele reads for each population starts on the 10th
#' column. Thus, the 10th column contains the number of major-allele reads for
#' the first population, the 11th column contains the number of major-allele
#' reads for the second population and so on. Thus if, for example, you wish to
#' import the data for the 5th and 6th population, then you should define the
#' \code{pops} input as \code{pops = c(5, 6)}. This will result in keeping only
#' the first 9 columns of the matrix plus the 15th and 16th columns and the
#' corresponding columns with the number of minor-allele reads for those
#' populations.
#'
#' @param file is a character string indicating the path to the file you wish to
#'   import.
#' @param pops is a vector with the index of the populations that should be
#'   imported. Defaults to NA, meaning that data is imported for all
#'   populations.
#' @param header is a character vector containing the names for the columns. If
#'   set to NA (default), no column names will be added to the output.
#' @param remove is a character vector where each entry is a name of a contig to
#'   be removed. These contigs are, obviously, removed from the imported
#'   dataset. If NA (default), all contigs will be kept in the output.
#'
#' @return a matrix with general information about the data in the first 9
#'   columns and the number of major and minor allele reads for the required
#'   populations in the remaining columns. If an header was supplied then the
#'   matrix will also contain column names as defined by the \code{header}
#'   input.
#'
#' @keywords internal
#'
#' @export
importData <- function(file, pops = NA, header = NA, remove = NA) {

  # import the data
  if(inherits(file, "character"))
    data <- utils::read.table(file = file, header = FALSE, stringsAsFactors = FALSE)
  else
    data <- file

  # remove all sites with more than two alleles
  data <- data[data[, 4] == 2, ]

  # remove all sites where deletion_sum is not equal to zero
  data <- data[data[, 6] == 0, ]

  # remove the row names of the matrix
  rownames(data) <- NULL

  # if a vector containing the column names was supplied, add those names to the columns of the matrix
  if (any(is.na(header)) == FALSE)
    colnames(data) <- header

  # if we do not wish to import all populations, import only those of interest
  # first, we need to check how many populations exist in the complete dataset
  # the first 9 columns contain information and so the first population is in the 10th column
  nPops <- ncol(data[10:ncol(data)])/2
  # we divide by two because for each population we have one column for the major allele and another for the minor allele

  # we add a 9 to the pops vector to skip over the first 9 columns that only contain extra information
  # the dataset has nPops populations - we add nPops + 9 to get to the columns containing the minor alleles information
  if(any(is.na(pops)) == FALSE)
    data <- data[, c(1:9, pops + 9, pops + (nPops + 9))]

  # if the remove input is not an NA
  # then it should be a vector containing the name of the contigs associated with removes
  if (any(is.na(remove)) == FALSE) {
    # check if any of the contigs in this file are present in the vector with the name of the contigs associated with removes
    toremove <- data[, 1] %in% remove
    # remove those entries (rows) from the matrix with the data
    data <- data[!toremove, ]
  }

  # check if any matrix entry is empty
  empty <- apply(data, MARGIN = c(1, 2), FUN = function(x) x == "")
  # sum the rows to find out if any row contains empty entries
  empty <- rowSums(empty) != 0
  # if any row contains empty entries
  if(sum(empty) != 0) # remove those rows from the matrix
    data <- data[!empty, ]

  # output the matrix with the dataset
  data
}


#' Remove sites with incorrect depths of coverage
#'
#' This functions checks if the sum of the number of major and minor allele
#' reads of a given population is equal to the total depth of coverage of that
#' population.
#'
#' This verification is performed for all the populations included in the
#' dataset. Any site where this verification fails for any of the populations is
#' removed from the dataset. More precisely, if the sum of the number of major
#' and minor allele reads of one population is not equal to the coverage of that
#' population, then that site is removed from the data for all the populations.
#'
#' @param nPops is an integer indicating the total number of different
#'   populations in the dataset.
#' @param info is a matrix containing information about the dataset. This matrix
#'   might contain several columns including one with the reference contig
#'   (chromosome), the position of the SNP in the reference contig and the
#'   reference character of the SNP. Note that each row of the matrix should be
#'   a different SNP and each column a different type of information.
#' @param major is a matrix with the reference character of the major allele.
#'   Each column of the matrix should be a different population and each row a
#'   different SNP.
#' @param minor is a matrix with the reference character of the minor allele.
#'   Each column of the matrix should be a different population and each row a
#'   different SNP.
#' @param rMajor is a matrix with the number of major allele reads. Each column
#'   of the matrix should be a different population and each row a different
#'   SNP.
#' @param rMinor is a matrix with the number of minor allele reads. Each column
#'   of the matrix should be a different population and each row a different
#'   SNP.
#' @param coverage is a matrix with the total number of reads i.e. the depth of
#'   coverage. Each column of the matrix should be a different population and
#'   each row a different SNP.
#'
#' @return a list with the following elements:
#'
#'   \item{info}{a matrix with the general information about the dataset. Each
#'   row of this matrix corresponds to a different site.}
#'
#'   \item{major}{a matrix with the reference character of the major allele.
#'   Each column of this matrix corresponds to a different population and each
#'   row to a different site.}
#'
#'   \item{minor}{a matrix with the reference character of the minor allele.
#'   Each column of this matrix corresponds to a different population and each
#'   row to a different site.}
#'
#'   \item{rMajor}{a matrix with the number of major-allele reads. Each row of
#'   this matrix is a different site and each column a different population.}
#'
#'   \item{rMinor}{a matrix with the number of minor-allele reads. Each row of
#'   this matrix is a different site and each column a different population.}
#'
#'   \item{coverage}{a matrix with the total coverage. Each row of this matrix
#'   is a different site and each column a different population.}
#'
#'   Each of those matrices is similar to the corresponding input but without
#'   any sites where the total depth of coverage does not match the sum of the
#'   major and minor allele reads.
#'
#' @keywords internal
#'
#' @export
checkCoverage <- function(nPops, info, major, minor, rMajor, rMinor, coverage) {

  # check if the number of reads with the major and the minor allele is not equal to the total depth of coverage
  eval.dp <- rMajor + rMinor == coverage
  # if the previous statement is true for all populations, then the sum of each row should be nPops
  # if this is not true, this means that the number of reads (major + minor) is different from the total depth of coverage
  eval.dp <- rowSums(eval.dp) == nPops

  # remove those sites from the matrix containing the number of reads with the major allele
  rMajor <- rMajor[eval.dp, ]
  # from the matrix containing the number of reads with the minor allele
  rMinor <- rMinor[eval.dp, ]
  # from the matrix with the depth of coverage
  coverage <- coverage[eval.dp, ]
  # from the matrix with the information about the nucleotides of the minor allele
  minor <- minor[eval.dp, ]
  # and, finally, from the matrix containing the information about the nucleotides of the major allele
  major <- major[eval.dp, ]
  # also, remove the same sites from the matrix with the info
  info <- info[eval.dp, ]

  # create a list with the various matrices - this is the output of the function
  list(info = info, major = major, minor = minor, rMajor = rMajor, rMinor = rMinor, coverage = coverage)
}


#' Remove sites with missing data
#'
#' This functions checks if there is any population with an "N" as the reference
#' character for the major allele.
#'
#' This verification is performed for all the populations included in the
#' dataset. Any site where this verification fails for any of the populations is
#' removed from the dataset. More precisely, if a single population has an "N"
#' as the reference character of their major allele, then that site is removed
#' from the data for all the populations.
#'
#' @param info is a matrix containing information about the dataset. This matrix
#'   might contain several columns including one with the reference contig
#'   (chromosome), the position of the SNP in the reference contig and the
#'   reference character of the SNP. Note that each row of the matrix should be
#'   a different SNP and each column a different type of information.
#' @param major is a matrix with the reference character of the major allele.
#'   Each column of the matrix should be a different population and each row a
#'   different SNP.
#' @param minor is a matrix with the reference character of the minor allele.
#'   Each column of the matrix should be a different population and each row a
#'   different SNP.
#' @param rMajor is a matrix with the number of major allele reads. Each column
#'   of the matrix should be a different population and each row a different
#'   SNP.
#' @param rMinor is a matrix with the number of minor allele reads. Each column
#'   of the matrix should be a different population and each row a different
#'   SNP.
#' @param coverage is a matrix with the total number of reads i.e. the depth of
#'   coverage. Each column of the matrix should be a different population and
#'   each row a different SNP.
#'
#' @return a list with the following elements:
#'
#'   \item{info}{a matrix with the general information about the dataset. Each
#'   row of this matrix corresponds to a different site.}
#'
#'   \item{major}{a matrix with the reference character of the major allele.
#'   Each column of this matrix corresponds to a different population and each
#'   row to a different site.}
#'
#'   \item{minor}{a matrix with the reference character of the minor allele.
#'   Each column of this matrix corresponds to a different population and each
#'   row to a different site.}
#'
#'   \item{rMajor}{a matrix with the number of major-allele reads. Each row of
#'   this matrix is a different site and each column a different population.}
#'
#'   \item{rMinor}{a matrix with the number of minor-allele reads. Each row of
#'   this matrix is a different site and each column a different population.}
#'
#'   \item{coverage}{a matrix with the total coverage. Each row of this matrix
#'   is a different site and each column a different population.}
#'
#'   Each of those matrices is similar to the corresponding input but without
#'   any sites where any of the populations has an "N" as the reference
#'   character for the major allele.
#'
#' @keywords internal
#'
#' @export
checkMissing <- function(info, major, minor, rMajor, rMinor, coverage) {

  # check if there is any population with a "N" as the allele information
  # this means that there are no reads for that population
  eval.N <- major == "N"

  # if there are no populations with an "N", then the row sum of the previous TRUE/FALSE matrix should produce a zero
  # values different from zero, mean that there is at least one population with a "N"
  eval.N <- rowSums(eval.N) == 0

  # remove those sites from the matrix containing the information about the nucleotide of the major allele
  major <- major[eval.N, ]
  # from the the matrix containing the information about the nucleotide of the minor allele
  minor <- minor[eval.N, ]
  # from the matrix containing the number of reads with the major allele
  rMajor <- rMajor[eval.N, ]
  # from the matrix containing the number of reads with the minor allele
  rMinor <- rMinor[eval.N, ]
  # from the matrix with the depth of coverage
  coverage <- coverage[eval.N, ]
  # also, remove the same sites from the matrix with the info
  info <- info[eval.N, ]

  # create a list with the various matrices - this is the output of the function
  list(info = info, major = major, minor = minor, rMajor = rMajor, rMinor = rMinor, coverage = coverage)
}


#' Check if the major allele is the same in all populations
#'
#' Checks if the reference character of the major allele is the same in all
#' populations present in the dataset.
#'
#' When working with two populations, the reference character of the major
#' allele is compared between the two populations. If they are not the same,
#' this function switches the major and minor reference character of the second
#' population i.e. the original reference character of the major allele of the
#' second population is now the reference character of the minor allele of the
#' second population. It also switches the number of reads so that the number of
#' major-allele reads is now the number of minor-allele reads and vice-versa.
#' When working with four populations, and if the major reference character is
#' not the same at all populations, this function checks if any of the reference
#' characters appears in three populations. If it does, then the major and minor
#' reference character and reads are switched in the remaining population. If
#' not, then the reference character and reads of two random populations are
#' switched.
#'
#' Finally, for both datasets with two or four populations, this function checks
#' if the total number of major-allele reads, across all populations and after
#' the switch, is larger than the total number of minor-allele reads. If this is
#' not true, then we switch the major and minor allele so that the more frequent
#' one corresponds to the major allele.
#'
#' @param nPops is an integer indicating the total number of different
#'   populations in the dataset.
#' @param major is a matrix with the reference character of the major allele.
#'   Each column of the matrix should be a different population and each row a
#'   different SNP.
#' @param minor is a matrix with the reference character of the minor allele.
#'   Each column of the matrix should be a different population and each row a
#'   different SNP.
#' @param rMajor is a matrix with the number of major allele reads. Each column
#'   of the matrix should be a different population and each row a different
#'   SNP.
#' @param rMinor is a matrix with the number of minor allele reads. Each column
#'   of the matrix should be a different population and each row a different
#'   SNP.
#'
#' @return a list with four named entries:
#'
#'   \item{major}{a matrix with the reference character of the major allele.
#'   Each column of this matrix corresponds to a different population and each
#'   row to a different site.}
#'
#'   \item{minor}{a matrix with the reference character of the minor allele.
#'   Each column of this matrix corresponds to a different population and each
#'   row to a different site.}
#'
#'   \item{rMajor}{a matrix with the number of major-allele reads. Each row of
#'   this matrix is a different site and each column a different population.}
#'
#'   \item{rMinor}{a matrix with the number of minor-allele reads. Each row of
#'   this matrix is a different site and each column a different population.}
#'
#'   Each of those matrices is similar to the corresponding input but with the
#'   major and minor allele switched when appropriate.
#'
#' @keywords internal
#'
#' @export
checkMajor <- function(nPops, major, minor, rMajor, rMinor) {

  # when the dataset is comprised of two populations
  if(nPops == 2) {

    # check if the nucleotide corresponding to the major allele in the first population is the same in the second population
    eval <- major[, 1] != major[, 2]
    # create two temporary matrices - one containing the nucleotide of the major allele for each population and across all sites
    # and another containing the nucleotide of the minor allele
    tempMaj <- major; tempMin <- minor
    # at the sites where the nucleotide of the major allele is not the same for all populations
    # copy the nucleotide of the minor allele of the second population
    # and paste in the column containing the information about the nucleotide of the major allele for the second population
    major[eval, 2] <- tempMin[eval, 2]
    # then, replace the nucleotide of the minor allele for the second population, with the nucleotide of the major allele
    minor[eval, 2] <- tempMaj[eval, 2]

    # create two temporary matrices - one containing the reads with the major allele for each population and across all sites
    # and another with the reads with the minor allele
    tempRmaj <- rMajor; tempRmin <- rMinor

    # replace the number of reads at the major matrix, for the second population, with the reads at the minor matrix
    rMajor[eval, 2] <- tempRmin[eval, 2]
    # replace the number of reads at the minor matrix, for the second population, with the reads at the major matrix
    rMinor[eval, 2] <- tempRmaj[eval, 2]

  } else {

    # find out which nucleotides are in each row of the major matrix
    # and the index of each of those nucleotides
    tmp <- apply(X = major, MARGIN = 1, function(row) split(seq_along(row), row))

    # check what are the indices that need to change
    eval <- which(sapply(tmp, length) == 2)

    # if there are any indices that need changing
    if(length(eval) != 0) {

      # get only the cases with 2 alleles that are different across pops
      tmp <- tmp[eval]

      # order the list - so that at each entry, the most common nucleotide appears first
      tmp <- lapply(tmp, function(x) x[order(-lengths(x))])

      # get the index of columns of pops we will change
      indextochange <- lapply(tmp, "[[", 2)

      # create two temporary matrices - one containing the nucleotide of the major allele for each population and across all sites
      # and another containing the nucleotide of the minor allele
      tempMaj <- major; tempMin <- minor

      # the following line will create the rows with the correct values
      # to be more specific, this will create the rows of the major matrix and replace the values that need to be changed with
      # the values from the minor matrix
      changed.rows <- lapply(1:length(indextochange), FUN = function(i)
        replace(x = major[eval[i], ], list = indextochange[[i]], values = tempMin[eval[i], indextochange[[i]]]))

      # replace the rows that need to be changed in the major matrix with the correct rows created in the previous step
      major[eval, ] <- do.call(rbind, changed.rows)

      # create the rows of the minor matrix and replace the values that need to be changed with the values from the major matrix
      changed.rows <- lapply(1:length(indextochange), FUN = function(i)
        replace(x = minor[eval[i], ], list = indextochange[[i]], values = tempMaj[eval[i], indextochange[[i]]]))

      # replace the rows that need to be changed in the minor matrix with the correct rows created in the previous step
      minor[eval, ] <- do.call(rbind, changed.rows)

      # create two temporary matrices - one containing the reads with the major allele for each population and across all sites
      # and another with the reads with the minor allele
      tempRmaj <- rMajor; tempRmin <- rMinor

      # create the rows of the matrix with the reads for the major allele and replace the values that need to be changed
      # with the values from the matrix containing the reads with the minor allele
      changed.rows <- lapply(1:length(indextochange), FUN = function(i)
        replace(x = rMajor[eval[i], ], list = indextochange[[i]], values = tempRmin[eval[i], indextochange[[i]]]))

      # replace the number of reads at the major matrix with the reads at the minor matrix
      rMajor[eval, ] <- do.call(rbind, changed.rows)

      # create the rows of the matrix with the reads for the minor allele and replace the values that need to be changed
      # with the values from the matrix containing the reads with the major allele
      changed.rows <- lapply(1:length(indextochange), FUN = function(i)
        replace(x = rMinor[eval[i], ], list = indextochange[[i]], values = tempRmaj[eval[i], indextochange[[i]]]))

      # replace the number of reads at the minor matrix with the reads at the major matrix
      rMinor[eval, ] <- do.call(rbind, changed.rows)
    }
  }

  # create the final matrices containing the information about the nucleotides of the major and minor alleles
  fmajor <- major; fminor <- minor

  # create two temporary matrices - one containing the reads with the major allele for each population and across all sites
  # and another with the reads with the minor allele
  tempRmaj <- rMajor; tempRmin <- rMinor

  # perform a final evaluation to check if the total number or reads for each site
  # is bigger in the matrix containing the "major" allele reads or in the matrix containing the "minor"
  evalf <- rowSums(rMajor) < rowSums(rMinor)
  # at the sites where the number of reads for the "minor" allele is bigger than the number of reads for the "major" allele
  # replace those rows with the rows from the matrix of the "minor" allele
  rMajor[evalf, ] <- tempRmin[evalf, ]
  # replace those same rows in the matrix with the reads of the "minor" allele
  rMinor[evalf, ] <- tempRmaj[evalf, ]

  # replace also those rows in the matrix containing the information about the nucleotides
  fmajor[evalf, ] <- minor[evalf, ]; fminor[evalf, ] <- major[evalf, ]

  # create a list with the various matrices - this is the output of the function
  list(major = fmajor, minor = fminor, rMajor = rMajor, rMinor = rMinor)
}


#' Import and clean a single file containing data in \code{popoolation2} format
#'
#' Imports data for two or four populations from a single file containing data
#' in the _rc format. The data is then split so that the number of major-allele
#' reads, minor-allele reads, total depth of coverage and remaining relevant
#' information are kept on separate matrices.
#'
#' The information in the _rc format is stored in a x/y format, where x
#' represents the observed reads and the y is the coverage. The initial step of
#' this function splits this string to separate the number of reads from the
#' total coverage. Then, the number of major plus minor allele reads is compared
#' to the total coverage and sites where both values are not equal are removed
#' from the dataset. Additionally, sites where any of the populations has an "N"
#' as the reference character of their major allele, are removed from the data.
#' This function also ensures that the major allele is the same and the most
#' frequent across all populations. Finally, if the \code{min.minor} input is
#' supplied, sites where the total number of minor-allele reads is below the
#' specified number, will be removed from the data set.
#'
#' Note also that all non biallelic sites and sites where the sum of deletions
#' in all populations is not zero will be removed from the dataset. Although
#' this function can only import 2 or 4 populations at the time, it is possible
#' to define which two or four populations to import. For instance, if we define
#' the first population as the first column for which we have data in the x/y
#' format, then you could wish to import the data for the 5th and 6th
#' populations, defined as the populations in the 6th and 7th columns. To do so,
#' you should define the \code{pops} input as \code{pops = c(5, 6)}.
#'
#' @param file is a character string indicating the path to the file you wish to
#'   import.
#' @param pops is a vector with the index of the populations that should be
#'   imported. This function works for two or four populations and so this
#'   vector must have either length 2 or 4.
#' @param header is a character vector containing the names for the columns. If
#'   set to NA (default), no column names will be added to the output.
#' @param remove is a character vector where each entry is a name of a contig to
#'   be removed. These contigs are, obviously, removed from the imported
#'   dataset. If NA (default), all contigs will be kept in the output.
#' @param min.minor what is the minimum allowed number of reads with the minor
#'   allele across all populations? Sites where this threshold is not met are
#'   removed from the data. The default (NA) means that no sites will be removed
#'   because of their number of minor-allele reads.
#'
#' @return a list with the following elements:
#'
#'   \item{rMajor}{a matrix with the number of major-allele reads. Each row of
#'   this matrix is a different site and each column a different population.}
#'
#'   \item{rMinor}{a matrix with the number of minor-allele reads. Each row of
#'   this matrix is a different site and each column a different population.}
#'
#'   \item{coverage}{a matrix with the total coverage. Each row of this matrix
#'   is a different site and each column a different population.}
#'
#'   \item{info}{a data frame with 5 different columns containing: the contig
#'   name, the SNP position, the reference character of the SNP and the
#'   reference character of the major and minor allele for each of the
#'   populations. Each row of this data frame corresponds to a different site}
#'
#' @examples
#' # load the data from one rc file
#' data(rc1)
#' # clean and organize the data in this single file
#' cleanData(file = rc1, pops = 7:10)
#'
#' @export
cleanData <- function(file, pops, header = NA, remove = NA, min.minor = NA) {

  # get the number of pops
  nPops <- length(pops)

  # this functions is intended to clean and import the data for two or four populations at a time
  if(!(nPops %in% c(2, 4)))
    stop("This function should would be used when working with two or four populations")

  # import the data
  geno <- importData(file = file, pops = pops, header = header, remove = remove)

  # get the sequence, for all populations, of the major allele
  major <- geno[, 8]
  # and the sequence of the minor allele
  minor <- geno[, 9]

  # split the string and keep only the information for the populations we wish to look into
  major <- t(sapply(X = major, FUN = function(seq) unlist(strsplit(seq, ""))[pops], USE.NAMES = FALSE))
  # do the same for the sequence of the minor allele
  minor <- t(sapply(X = minor, FUN = function(seq) unlist(strsplit(seq, ""))[pops], USE.NAMES = FALSE))

  # store the reads into a single matrix
  tempReads <- geno[, (9 + 1):ncol(geno), drop = FALSE]

  # create an info output - containing the name of the contig and the position of the SNP
  info <- geno[, 1:3]

  # The information about the number of reads is stored in a x/y format
  # where x represents the observed reads and the y is the coverage
  # This will split the information using the "/" as the separation
  splitReads <- apply(tempReads, MARGIN = 2, function(col) as.numeric(unlist(strsplit(col, "/"))))

  # Check if the split was correctly done - if so, the result should have twice as many rows as the input
  if(nrow(splitReads) != (2*nrow(tempReads))) {
    stop("error! Spliting the string according to a /. Check if all values are separated by a / symbol.")
  }

  # the result of strsplit is a matrix where each entry is coded as two rows
  # for instance 53/68 will appear in two rows as 53 68
  # thus, the number of reads is in the odd rows and the depth of coverage is in the even rows

  # get the number of reads
  nReads <- splitReads[seq(1, nrow(splitReads), by = 2), , drop = FALSE]
  # get the depth of coverage
  dp <- splitReads[seq(2, nrow(splitReads), by = 2), , drop = FALSE]

  # get the number of reads with the major allele
  rMajor <- nReads[, c(1:nPops)]
  # and the number of reads with the minor allele
  rMinor <- nReads[, c((nPops + 1):ncol(nReads))]

  # then, obtain the depth of coverage for each population and across all sites
  # since the depth of coverage should be the same for the major and minor allele, get the info from the first nPops columns
  dp <- dp[, c(1:nPops)]

  # check if the number of major plus minor allele reads is equal to the depth of coverage
  # and remove sites where the sum of the major and minor allele reads is not equal to the coverage
  temp <- checkCoverage(nPops = nPops, info = info, major = major, minor = minor, rMajor = rMajor, rMinor = rMinor, coverage = dp)
  # the previous step created a list - get the information and the character strings of the major and minor alleles
  info <- temp[["info"]]; major <- temp[["major"]]; minor <- temp[["minor"]]
  # get the number of major and minor allele reads and the depth of coverage
  rMajor <- temp[["rMajor"]]; rMinor <- temp[["rMinor"]]; dp <- temp[["coverage"]]

  # check if there is any population with a "N" as the major allele information
  # this means that there are no reads for that population - those sites should be removed from the data
  temp <- checkMissing(info = info, major = major, minor = minor, rMajor = rMajor, rMinor = rMinor, coverage = dp)
  # the previous step created a list - get the information and the character strings of the major and minor alleles
  info <- temp[["info"]]; major <- temp[["major"]]; minor <- temp[["minor"]]
  # get the number of major and minor allele reads and the depth of coverage
  rMajor <- temp[["rMajor"]]; rMinor <- temp[["rMinor"]]; dp <- temp[["coverage"]]

  # check if the major allele is the same in all populations and if it is the most frequent
  temp <- checkMajor(nPops = nPops, major = major, minor = minor, rMajor = rMajor, rMinor = rMinor)
  # the previous step created a list - get the character strings of the major and minor alleles
  major <- temp[["major"]]; minor <- temp[["minor"]]
  # get the number of major and minor allele reads
  rMajor <- temp[["rMajor"]]; rMinor <- temp[["rMinor"]]

  # add the information about the nucleotide of the major and minor allele - for each population and site
  info <- cbind(info, major = do.call(paste, c(data.frame(major)[1:ncol(major)], sep = "")),
                minor = do.call(paste, c(data.frame(minor)[1:ncol(minor)], sep = "")))

  # if the min.minor input is not NA - then it should be an integer
  # representing the minimum number of reads with the minor allele that we should observe across all populations
  if (is.na(min.minor) == FALSE) {
    # find out in which rows the total sum of the reads with the minor allele is below the threshold
    toremove <- rowSums(rMinor) < min.minor
    # remove those rows from the various matrices containing the data
    rMajor <- rMajor[!toremove, ]; rMinor <- rMinor[!toremove, ]; dp <- dp[!toremove, ]; info <- info[!toremove, ]
  }

  # create the output of the function
  output <- list(rMajor = rMajor, rMinor = rMinor, coverage = dp, info = info)

  # output the result of the function
  output
}


#' Filter the data by the frequency of the minor allele
#'
#' Computes the frequency of the minor allele across all populations and removes
#' sites where that frequency is below a certain threshold.
#'
#' The frequency of the minor allele is computed by dividing the total number of
#' minor-allele reads at each site and across all populations by the total
#' coverage of that site. The total coverage is obtained by adding the depth of
#' coverage of each population at each site. If a threshold is supplied, the
#' computed frequency is compared to that threshold and sites where the
#' frequency is below the threshold are removed from the dataset. If no
#' threshold is supplied, the threshold is assumed to be \code{1/total
#' coverage}, meaning that a site should have, at least, one minor-allele read.
#'
#' @param rMajor is a matrix containing the number of observed major-allele
#'   reads. Each row of the matrix should be a different site and each column
#'   should contain information for a single population
#' @param rMinor is a matrix containing the number of observed minor-allele
#'   reads. Each row of the matrix should be a different site and each column
#'   should contain information for a single population
#' @param coverage is a matrix containing the total depth of coverage. Each row
#'   of the matrix should be a different site and each column should contain
#'   information for a single population
#' @param info is a data frame containing the remaining relevant information,
#'   such as the contig name and the position of each SNP. Each row of the
#'   matrix should be a different site.
#' @param threshold is the maximum allowed frequency for the major allele. Sites
#'   where the allelic frequency is above this threshold are removed from the
#'   data.
#'
#' @return a list with the following elements:
#'
#'   \item{rMajor}{a matrix with the number of major-allele reads. Each row of
#'   this matrix is a different site and each column a different population.}
#'
#'   \item{rMinor}{a matrix with the number of minor-allele reads. Each row of
#'   this matrix is a different site and each column a different population.}
#'
#'   \item{coverage}{a matrix with the total coverage. Each row of this matrix
#'   is a different site and each column a different population.}
#'
#'   \item{info}{a data frame with 5 different columns containing: the contig
#'   name, the SNP position, the reference character of the SNP and the
#'   reference character of the major and minor allele for each of the
#'   populations. Each row of this data frame corresponds to a different site}
#'
#'   The `rMajor`, `rMinor` and `coverage` are similar to the corresponding
#'   input but without any sites where the frequency of the minor-allele is
#'   below a certain threshold.
#'
#' @keywords internal
#'
#' @export
filterData <- function(rMajor, rMinor, coverage, info, threshold = NA) {

  # the first step is obtaining the total coverage for each of the sites
  # i.e. the total number of reads across all populations for a site
  totalCoverage <- rowSums(coverage)

  # now we need to find the total coverage - across all populations - of the minor frequency allele
  minor <- rowSums(rMinor)

  # compute the frequency of the least common allele
  frequency <- minor/totalCoverage

  # if no threshold is supplied as input for the function
  if(is.na(threshold))
    # assume that the threshold at each site is 1/total number of reads at that site
    threshold <- 1/totalCoverage

  # find out which sites are below the threshold
  # i.e. entries where the allelic frequency of the least common allele is below the required level
  toremove <- frequency < threshold

  # if there are sites where the frequency of the minor frequency allele is below a certain threshold
  if (length(toremove) != 0) {
    # remove those rows from the matrix containing the depth of coverage
    coverage <- coverage[!toremove, , drop = FALSE]
    # remove those rows from the matrix containing the number of reads for the major allele
    rMajor <- rMajor[!toremove, , drop = FALSE]
    # remove those rows from the matrix containing the number of reads for the minor allele
    rMinor <- rMinor[!toremove, , drop = FALSE]
    # finally, remove those rows from the matrix containing other information - i.e contig name, etc
    info <- info[!toremove, , drop = FALSE]
  }

  # if no single site meets the criteria - i.e. no site has a frequency above the required threshold
  # stop the function and output an NA
  if(any(c(length(coverage), length(rMajor)) == 0))
    stop(return(NA))

  # otherwise, output the matrices with the various types of read numbers
  # without the sites that did not meet the threshold criteria
  list(rMajor = rMajor, rMinor = rMinor, coverage = coverage, info = info)
}


#' Organize information by contigs - for a single data file
#'
#' Organize the information of a single _rc file into different entries for each
#' contig.
#'
#' This function removes all monomorphic sites from the dataset. Monomorphic
#' sites are those where the frequency for all populations is 1 or 0. Then, the
#' name of each contig is used to organize the information in a per contig
#' basis. Thus, each output will be organized by contig. For example, the list
#' with the number of minor-allele reads will contain several entries and each
#' of those entries is a different contig.
#'
#' If the filter input is set to TRUE, this function also filters the data by
#' the frequency of the minor-allele. If a threshold is supplied, the computed
#' frequency is compared to that threshold and sites where the frequency is
#' below the threshold are removed from the dataset. If no threshold is
#' supplied, the threshold is assumed to be \code{1/total coverage}, meaning
#' that a site should have, at least, one minor-allele read.
#'
#' @param data is a list with four different entries. The entries should be
#'   named as "rMajor", "rMinor", "coverage" and "info". The \code{rMajor} entry
#'   should be a matrix containing the number of observed major-allele reads.
#'   The \code{rMinor} entry should be a matrix containing the number of
#'   observed minor-allele reads. The \code{coverage} entry should be a matrix
#'   containing the total depth of coverage. The \code{info} entry should be a
#'   matrix or a data frame containing the remaining relevant information, such
#'   as the contig name and the position of each SNP. Each row of these matrices
#'   should be a different site and each column should be a different
#'   population.
#' @param nPops is an integer indicating the total number of different
#'   populations in the dataset.
#' @param filter is a logical switch, either TRUE or FALSE. If TRUE, then the
#'   data is filtered by the frequency of the minor allele and if FALSE, that
#'   filter is not applied.
#' @param threshold is the minimum allowed frequency for the minor allele. Sites
#'   where the allelic frequency is below this threshold are removed from the
#'   data.
#'
#' @return a list with six named entries:
#'
#'   \item{freqs}{a list with the allele frequencies, computed by dividing the
#'   number of minor-allele reads by the total coverage. Each entry of this list
#'   corresponds to a different contig. Each entry is a matrix where each row is
#'   a different site and each column is a different population.}
#'
#'   \item{positions}{a list with the positions of each SNP. Each entry of this
#'   list is a vector corresponding to a different contig.}
#'
#'   \item{range}{a list with the minimum and maximum SNP position of each
#'   contig. Each entry of this list is a vector corresponding to a different
#'   contig.}
#'
#'   \item{rMajor}{a list with the number of major-allele reads. Each entry of
#'   this list corresponds to a different contig. Each entry is a matrix where
#'   each row is a different site and each column is a different population.}
#'
#'   \item{rMinor}{a list with the number of minor-allele reads. Each entry of
#'   this list corresponds to a different contig. Each entry is a matrix where
#'   each row is a different site and each column is a different population.}
#'
#'   \item{coverage}{a list with the total coverage. Each entry of this list
#'   corresponds to a different contig. Each entry is a matrix where each row is
#'   a different site and each column is a different population.}
#'
#' @examples
#' # load the data from one rc file
#' data(rc1)
#'
#' # clean and organize the data in this single file
#' mydata <- cleanData(file = rc1, pops = 7:10)
#'
#' # organize the information by contigs
#' prepareFile(data = mydata, nPops = 4)
#'
#' @export
prepareFile <- function(data, nPops, filter = FALSE, threshold = NA) {

  # if filter equals yes then this function also performs a filtering step
  # removing all sites where the frequency of the minor allele is below a certain threshold
  if (filter == TRUE)
    data <- filterData(rMajor = data[["rMajor"]], rMinor = data[["rMinor"]], coverage = data[["coverage"]], info = data[["info"]],
                       threshold)

  # how big is each contig? We should look at this for the whole data - prior to removing sites without variability
  info <- data[["info"]]

  # what contigs are present in this dataset?
  nContigs <- unique(info[, 1])

  # this creates a matrix with the minimum and maximum position for each contig
  # now we know how big is each contig - prior to removing monomorphic sites
  rangeContig <- sapply(nContigs, FUN = function(x) range(info[info[, 1] == x, 2]))

  # get the number of reads for the major frequency allele
  rMajor <- data[["rMajor"]]
  #  get the number of reads for the minor frequency allele
  rMinor <- data[["rMinor"]]
  # get the depth of coverage
  dp <- data[["coverage"]]

  # calculate allelic frequency as the ratio between observed minor-allele reads for each population and the depth of coverage
  freqs <- rMinor / dp

  # remove sites without variability - these are the sites where the frequency is equal to 1 or 0 in all pops
  # keep only the sites where the sum of the frequencies of all populations is not the same as the number of populations
  # if the frequency for each population is 1, then the sum of frequencies will be equal to the number of populations (nPops)
  toremove <- rowSums(freqs) == nPops | rowSums(freqs) == 0

  # if there are sites to remove then do that
  if(sum(toremove) != 0) {

    # remove those sites from matrix with the number of reads - for the major allele
    rMajor <- rMajor[!toremove, , drop = FALSE]
    # remove those sites from matrix with the number of reads - for the minor allele
    rMinor <- rMinor[!toremove, , drop = FALSE]
    # remove those sites from matrix with the depth of coverage
    dp <- dp[!toremove, , drop = FALSE]
    # remove those sites from matrix with the additional information
    info <- info[!toremove, , drop = FALSE]
    # remove those sites from matrix with the allelic frequency
    freqs <- freqs[!toremove, , drop = FALSE]
  }

  # we can also now update the nContigs variable - in case some contigs did not contain polymorphic sites
  nContigs <- unique(info[, 1])

  # get the allele frequencies of each contig into an entry of a list
  freqContigs <- lapply(nContigs, FUN = function(contig) freqs[info[, 1] == contig, , drop = FALSE])

  # get the number of reads per contig - for the major allele
  majorContigs <- lapply(nContigs, FUN = function(contig) rMajor[info[, 1] == contig, , drop = FALSE])

  # get the number of reads per contig - for the minor allele
  minorContigs <- lapply(nContigs, FUN = function(contig) rMinor[info[, 1] == contig, , drop = FALSE])

  # get the coverage per contig
  dpContigs <- lapply(nContigs, FUN = function(contig) dp[info[, 1] == contig, , drop = FALSE])

  # get the info per contig
  infoContig <- lapply(nContigs, FUN = function(contig) info[info[, 1] == contig, , drop = FALSE])

  # transform the list containing the minimum and maximum position of each contig into a list where each entry is a single contig
  # first, get the names of all the contigs stored on this list
  tempnames <- colnames(rangeContig)
  # then, convert each column of each list entry (i.e. each column of a matrix) into a list entry
  rangeContig <- lapply(seq_len(ncol(rangeContig)), function(i) rangeContig[, i])
  # add the contig name to each entry of the list
  names(rangeContig) <- tempnames
  # keep only the range of the contigs that were kept after removing monomorphic sites
  rangeContig <- rangeContig[names(rangeContig) %in% nContigs]

  # get the vector of position for each kept contig
  posContigs <- lapply(infoContig, function(contig) as.numeric(contig[, 2]))

  # add the contig name to each entry of the other lists - frequencies, positions and major reads
  names(freqContigs) <- nContigs; names(posContigs) <- nContigs; names(majorContigs) <- nContigs
  # and minor reads and depth of coverage
  names(minorContigs) <- nContigs; names(dpContigs) <- nContigs

  # prepare the output of the function
  list(freqs = freqContigs, positions = posContigs, range = rangeContig, rMajor = majorContigs,
       rMinor = minorContigs, coverage = dpContigs)
}


#' Organize information by contig - for multiple data files
#'
#' Organize the information of multiple _rc files into different entries for
#' each contig.
#'
#' This function removes all monomorphic sites from the dataset. Monomorphic
#' sites are those where the frequency for all populations is 1 or 0. Then, the
#' name of each contig is used to organize the information in a per contig
#' basis. Thus, each output will be organized by contig. For example, the list
#' with the number of minor-allele reads will contain several entries and each
#' of those entries is a different contig.
#'
#' If the filter input is set to TRUE, this function also filters the data by
#' the frequency of the minor-allele. If a threshold is supplied, the computed
#' frequency is compared to that threshold and sites where the frequency is
#' below the threshold are removed from the dataset. If no threshold is
#' supplied, the threshold is assumed to be \code{1/total coverage}, meaning
#' that a site should have, at least, one minor-allele read.
#'
#' @param data is a list with four different entries. The entries should be
#'   named as "rMajor", "rMinor", "coverage" and "info". The \code{rMajor} entry
#'   should be a matrix containing the number of observed major-allele reads.
#'   The \code{rMinor} entry should be a matrix containing the number of
#'   observed minor-allele reads. The \code{coverage} entry should be a matrix
#'   containing the total depth of coverage. The \code{info} entry should be a
#'   matrix or a data frame containing the remaining relevant information, such
#'   as the contig name and the position of each SNP. Each row of these matrices
#'   should be a different site and each column should be a different
#'   population.
#' @param nPops is an integer indicating the total number of different
#'   populations in the dataset.
#' @param filter is a logical switch, either TRUE or FALSE. If TRUE, then the
#'   data is filtered by the frequency of the minor allele and if FALSE, that
#'   filter is not applied.
#' @param threshold is the minimum allowed frequency for the minor allele. Sites
#'   where the allelic frequency is below this threshold are removed from the
#'   data.
#'
#' @return a list with six named entries:
#'
#'   \item{freqs}{a list with the allele frequencies, computed by dividing the
#'   number of minor-allele reads by the total coverage. Each entry of this list
#'   corresponds to a different contig. Each entry is a matrix where each row is
#'   a different site and each column is a different population.}
#'
#'   \item{positions}{a list with the positions of each SNP. Each entry of this
#'   list is a vector corresponding to a different contig.}
#'
#'   \item{range}{a list with the minimum and maximum SNP position of each
#'   contig. Each entry of this list is a vector corresponding to a different
#'   contig.}
#'
#'   \item{rMajor}{a list with the number of major-allele reads. Each entry of
#'   this list corresponds to a different contig. Each entry is a matrix where
#'   each row is a different site and each column is a different population.}
#'
#'   \item{rMinor}{a list with the number of minor-allele reads. Each entry of
#'   this list corresponds to a different contig. Each entry is a matrix where
#'   each row is a different site and each column is a different population.}
#'
#'   \item{coverage}{a list with the total coverage. Each entry of this list
#'   corresponds to a different contig. Each entry is a matrix where each row is
#'   a different site and each column is a different population.}
#'
#' @examples
#' # load the data from two rc files
#' data(rc1, rc2)
#' # combine both files into a single list
#' mydata <- list(rc1, rc2)
#'
#' # clean and organize the data for both files
#' mydata <- lapply(mydata, function(i) cleanData(file = i, pops = 7:10))
#'
#' # organize the information by contigs
#' prepareData(data = mydata, nPops = 4)
#'
#' @export
prepareData <- function(data, nPops, filter = FALSE, threshold = NA) {

  # if filter equals yes then this function also performs a filtering step
  # removing all sites where the frequency of the minor allele is below a certain threshold
  if (filter == TRUE) {
    data <- lapply(data, function(entry)
      filterData(rMajor = entry[["rMajor"]], rMinor = entry[["rMinor"]], coverage = entry[["coverage"]],
                 info = entry[["info"]], threshold))
  }

  # how big is each contig? We should look at this for the whole data - prior to removing sites without variability
  info <- lapply(data, "[[", "info")

  # what contigs are present in this dataset?
  nContigs <- lapply(info, function(entry) unique(entry[, 1]))

  # create a matrix with the minimum and maximum position for each contig
  rangeContig <- mapply(FUN = function(contigs, info)  sapply(contigs, FUN = function(x)
    range(info[info[, 1] == x, 2 , drop = FALSE])), nContigs, info, SIMPLIFY = FALSE)
  # now we know how big is each contig - prior to removing monomorphic sites

  # get the number of reads for the major frequency allele - combine the information into a single list
  rMajor <- lapply(data, function(entry) entry$rMajor)
  #  get the number of reads for the minor frequency allele - combine the information into a single list
  rMinor <- lapply(data, function(entry) entry$rMinor)
  # get the depth of coverage - combine the information into a single list
  dp <- lapply(data, function(entry) entry$coverage)
  # get the remaining information - combine the information into a single list
  info <- lapply(data, function(entry) entry$info)

  # calculate allelic frequency as the ratio between observed minor-allele reads for each population and the depth of coverage
  freqs <- lapply(data, function(entry) entry$rMinor/ entry$coverage)

  # remove sites without variability - these are the sites where the frequency is equal to 1 or 0 in all pops
  # keep only the sites where the sum of the frequencies of all populations is not the same as the number of populations
  # if the frequency for each population is 1, then the sum of frequencies will be equal to the number of populations (nPops)
  toremove <- lapply(freqs, function(freqs) rowSums(freqs) == nPops | rowSums(freqs) == 0)

  # if there are contigs to remove then do that
  if (any(sapply(toremove, sum)) != 0) {

    # create a vector with the index of the entries where we need to remove sites
    index <- which(sapply(toremove, sum) != 0)

    # with this loop we can iterate over all the entries where we need to remove sites
    for(i in 1:length(index)) {

      # get the index of the entry where sites need to be removed
      temp <- index[i]

      # get the row index of the sites to remove
      remove <- toremove[[temp]]

      # remove those sites from matrix with the number of reads - for the major allele
      rMajor[[temp]] <- rMajor[[temp]][!remove, , drop = FALSE]
      # remove those sites from matrix with the number of reads - for the minor allele
      rMinor[[temp]] <- rMinor[[temp]][!remove, , drop = FALSE]
      # remove those sites from matrix with the depth of coverage
      dp[[temp]] <- dp[[temp]][!remove, , drop = FALSE]
      # remove those sites from matrix with the additional information
      info[[temp]] <- info[[temp]][!remove, , drop = FALSE]
      # remove those sites from matrix with the allelic frequency
      freqs[[temp]] <- freqs[[temp]][!remove, , drop = FALSE]
    }
  }

  # we can also now update the nContigs variable - in case some contigs did not contain polymorphic sites
  nContigs <- lapply(info, function(entry) unique(entry[, 1]))

  # roughly the same line of code can be used to save the SNPs of each contig into an entry of a list
  freqContigs <- mapply(FUN = function(contigs, freqs, info)  sapply(contigs, FUN = function(x)
    freqs[info[, 1] == x, , drop = FALSE]), nContigs, freqs, info, SIMPLIFY = FALSE)
  # transform the previous into a list where each entry is a single contig
  freqContigs <- unlist(freqContigs, recursive = FALSE)

  # get the number of reads per contig - for the major allele
  majorContigs <- mapply(FUN = function(contigs, rMajor, info)  sapply(contigs, FUN = function(x)
    rMajor[info[, 1] == x, , drop = FALSE]), nContigs, rMajor, info, SIMPLIFY = FALSE)
  # transform the previous into a list where each entry is a single contig
  majorContigs <- unlist(majorContigs, recursive = FALSE)

  # get the number of reads per contig - for the minor allele
  minorContigs <- mapply(FUN = function(contigs, rMinor, info)  sapply(contigs, FUN = function(x)
    rMinor[info[, 1] == x, , drop = FALSE]), nContigs, rMinor, info, SIMPLIFY = FALSE)
  # transform the previous into a list where each entry is a single contig
  minorContigs <- unlist(minorContigs, recursive = FALSE)

  # get the coverage per contig
  dpContigs <- mapply(FUN = function(contigs, dp, info)  sapply(contigs, FUN = function(x)
    dp[info[, 1] == x, , drop = FALSE]), nContigs, dp, info, SIMPLIFY = FALSE)
  # transform the previous into a list where each entry is a single contig
  dpContigs <- unlist(dpContigs, recursive = FALSE)

  # transform each entry of the info list into a matrix
  info <- lapply(info, as.matrix)
  # get the info per contig
  infoContig <- mapply(FUN = function(contigs, info) sapply(contigs, FUN = function(x)
    info[info[, 1] == x, , drop = FALSE]), nContigs, info, SIMPLIFY = FALSE)
  # transform the previous into a list where each entry is a single contig
  infoContig <- unlist(infoContig, recursive = FALSE)
  # get the vector of position for each kept contig
  posContigs <- lapply(infoContig, function(contig) as.numeric(contig[, 2]))
  # convert each entry back into a data frame
  infoContig <- lapply(infoContig, function(contig) data.frame(contig))

  # transform the list containing the minimum and maximum position of each contig
  # into a list where each entry is a single contig
  # first, get the names of all the contigs stored on this list
  tempnames <- unlist(lapply(rangeContig, colnames))
  # then, convert each column of each list entry (i.e. each column of a matrix) into a list entry
  rangeContig <- sapply(rangeContig, function(x) lapply(seq_len(ncol(x)), function(i) x[, i]))
  # transform the previous into a list where each entry is a single contig
  rangeContig <- unlist(rangeContig, recursive = FALSE)
  # add the contig name to each entry of the list
  names(rangeContig) <- tempnames
  # keep only the range of the contigs that were kept after removing monomorphic sites
  rangeContig <- rangeContig[names(rangeContig) %in% unlist(nContigs)]

  # prepare the output of the function
  list(freqs = freqContigs, positions = posContigs, range = rangeContig, rMajor = majorContigs,
       rMinor = minorContigs, coverage = dpContigs)
}


#' Import multiple files containing data in PoPoolation2 format
#'
#' Imports multiple files containing data in PoPoolation2 format and organize
#' that information into different entries for each contig.
#'
#' The data from two or four populations is split so that the number of
#' major-allele reads, minor-allele reads, total depth of coverage and remaining
#' relevant information are kept on separate list entries. Sites where the sum
#' of the major and minor allele reads does not match the total coverage and
#' sites where any population has an "N" as the reference character of their
#' major allele, are removed from the data. This function also ensures that the
#' major allele is the same and the most frequent across all populations. Note
#' also that all non biallelic sites and sites where the sum of deletions in all
#' populations is not zero will be removed from the dataset.
#'
#' If the \code{min.minor} input is supplied, sites where the total number of
#' minor-allele reads is below the specified number, will be removed from the
#' data set. Alternatively, if the filter input is set to TRUE, data will be
#' filtered by the frequency of the minor-allele. If a threshold is supplied,
#' the computed frequency is compared to that threshold and sites where the
#' frequency is below the threshold are removed from the dataset. If no
#' threshold is supplied, the threshold is assumed to be \code{1/total
#' coverage}, meaning that a site should have, at least, one minor-allele read.
#'
#' Finally, the name of each contig is used to organize the information in a per
#' contig basis. Thus, each output will be organized by contig. For example, the
#' list with the number of minor-allele reads will contain several entries and
#' each of those entries is a different contig.
#'
#' @param path is a character string indicating the path to the folder where the
#'   data you wish to import is located.
#' @param pops is a vector with the index of the populations that should be
#'   imported. This function works for two or four populations and so this
#'   vector must have either length 2 or 4.
#' @param files is an integer or a numeric vector with the index of the files
#'   you wish to import.
#' @param header is a character vector containing the names for the columns. If
#'   set to NA (default), no column names will be added to the output.
#' @param remove is a character vector where each entry is a name of a contig to
#'   be removed. These contigs are, obviously, removed from the imported
#'   dataset. If NA (default), all contigs will be kept in the output.
#' @param min.minor what is the minimum allowed number of reads with the minor
#'   allele across all populations? Sites where this threshold is not met are
#'   removed from the data.
#' @param filter is a logical switch, either TRUE or FALSE. If TRUE, then the
#'   data is filtered by the frequency of the minor allele and if FALSE, that
#'   filter is not applied.
#' @param threshold is the minimum allowed frequency for the minor allele. Sites
#'   where the allelic frequency is below this threshold are removed from the
#'   data.
#'
#' @return a list with six named entries:
#'
#'   \item{freqs}{a list with the allele frequencies, computed by dividing the
#'   number of minor-allele reads by the total coverage. Each entry of this list
#'   corresponds to a different contig. Each entry is a matrix where each row is
#'   a different site and each column is a different population.}
#'
#'   \item{positions}{a list with the positions of each SNP. Each entry of this
#'   list is a vector corresponding to a different contig.}
#'
#'   \item{range}{a list with the minimum and maximum SNP position of each
#'   contig. Each entry of this list is a vector corresponding to a different
#'   contig.}
#'
#'   \item{rMajor}{a list with the number of major-allele reads. Each entry of
#'   this list corresponds to a different contig. Each entry is a matrix where
#'   each row is a different site and each column is a different population.}
#'
#'   \item{rMinor}{a list with the number of minor-allele reads. Each entry of
#'   this list corresponds to a different contig. Each entry is a matrix where
#'   each row is a different site and each column is a different population.}
#'
#'   \item{coverage}{a list with the total coverage. Each entry of this list
#'   corresponds to a different contig. Each entry is a matrix where each row is
#'   a different site and each column is a different population.}
#'
#' @examples
#' # this function should be used to import your data
#' # you should include the path to the folder your PoPoolation2 data is
#'
#' # this creates a variable with the path for the toy example data
#' mypath <- system.file('extdata', package = 'poolABC')
#'
#' # an example of how to import data for two populations from all files
#' importContigs(path = mypath, pops = c(8, 10))
#'
#' # to remove contigs from the data
#' importContigs(path = mypath, pops = c(8, 10), remove = "Contig1708")
#'
#' @seealso
#' For more details see the poolABC vignette:
#' \code{vignette("poolABC", package = "poolABC")}
#'
#' @export
importContigs <- function(path, pops, files = NA, header = NA, remove = NA, min.minor = NA,
                          filter = FALSE, threshold = NA) {

  # get the number of Pops to be imported
  nPops <- length(pops)

  # create a vector with the name of all the files on the folder with the data
  if(all(is.na(files))) {

    # if files is set to NA, import all files
    files <- list.files(path = path, pattern = "_rc")

  } else {

    # import just the required files
    files <- list.files(path = path, pattern = "_rc")[files]
  }

  # store the current working directory on a variable
  currentwd <- getwd()
  # ensure that we get back to the current working directory
  on.exit(setwd(currentwd))

  # then navigate to the folder where the data is saved
  setwd(path)

  # import and clean the data using the cleanData function
  data <- lapply(1:length(files), function(index)
    cleanData(file = files[index], pops = pops, header = header, remove = remove, min.minor = min.minor))

  # revert the working directory back to the original (where the project is located)
  setwd(currentwd)

  # use the prepareData function to further clean (remove monomorphic sites) and split the data:
  # separate the allelic frequencies from the depth of coverage, etc
  tempData <- prepareData(data, nPops = nPops, filter, threshold)

  # split the list so that each important element is a single list
  freqs <- tempData[[1]]; positions <- tempData[[2]]; range <- tempData[[3]]; rMajor <- tempData[[4]];
  rMinor <- tempData[[5]]; coverage <- tempData[[6]]

  # remove contigs with no information
  sizeContig <- sapply(freqs, length)
  # find contigs to remove
  toremove <- sizeContig == 0
  # if there are contigs to remove then do that
  if(sum(toremove) != 0) {
    # remove these contigs from the various lists
    freqs <- freqs[!toremove]; positions <- positions[!toremove]; range <- range[!toremove]; rMajor <- rMajor[!toremove]
    rMinor <- rMinor[!toremove]; coverage <- coverage[!toremove]
  }

  # combine the lists with information about SNP frequencies, positions, contig range and depth of coverage
  output <- list(freqs = freqs, positions = positions, range = range, rMajor = rMajor,
                 rMinor = rMinor, coverage = coverage)

  # output the list containing all the relevant information
  output
}


#' Remove sites, according to their coverage, from real data
#'
#' Removes sites that have too many or too few reads from the dataset.
#'
#' The `minimum` and `maximum` inputs define, respectively, the minimum and
#' maximum allowed coverage for the dataset. The coverage of each population at
#' each site is compared with those threshold values and any site, where the
#' coverage of at least one population is below or above the user defined
#' threshold, is completely removed from the dataset.
#'
#' @param nPops is an integer representing the total number of populations in
#'   the dataset.
#' @param data is a dataset containing information about real populations. This
#'   dataset should have lists with the allelic frequencies, the position of the
#'   SNPs, the range of the contig, the number of major allele reads, the number
#'   of minor allele reads and the depth of coverage.
#' @param minimum the minimum depth of coverage allowed i.e. sites where the
#'   depth of coverage of any population is below this threshold are removed.
#' @param maximum he maximum depth of coverage allowed i.e. sites where the
#'   depth of coverage of any population is above this threshold are removed.
#'
#' @return a list with the following elements:
#'
#'   \item{freqs}{a list with the allele frequencies, computed by dividing the
#'   number of minor-allele reads by the total coverage. Each entry of this list
#'   corresponds to a different contig. Each entry is a matrix where each row is
#'   a different site and each column is a different population.}
#'
#'   \item{positions}{a list with the positions of each SNP. Each entry of this
#'   list is a vector corresponding to a different contig.}
#'
#'   \item{range}{a list with the minimum and maximum SNP position of each
#'   contig. Each entry of this list is a vector corresponding to a different
#'   contig.}
#'
#'   \item{rMajor}{a list with the number of major-allele reads. Each entry of
#'   this list corresponds to a different contig. Each entry is a matrix where
#'   each row is a different site and each column is a different population.}
#'
#'   \item{rMinor}{a list with the number of minor-allele reads. Each entry of
#'   this list corresponds to a different contig. Each entry is a matrix where
#'   each row is a different site and each column is a different population.}
#'
#'   \item{coverage}{a list with the total coverage. Each entry of this list
#'   corresponds to a different contig. Each entry is a matrix where each row is
#'   a different site and each column is a different population.}
#'
#'   This output is identical to the `data` input, the only difference being the
#'   removal of sites with too many or too few reads.
#'
#' @examples
#' # load the data from one rc file
#' data(rc1)
#'
#' # clean and organize the data in this single file
#' mydata <- cleanData(file = rc1, pops = 7:10)
#'
#' # organize the information by contigs
#' mydata <- prepareFile(data = mydata, nPops = 4)
#'
#' # remove sites with less than 10 reads or more than 180
#' remove_realReads(nPops = 4, data = mydata, minimum = 10, maximum = 180)
#'
#' @export
remove_realReads <- function(nPops, data, minimum, maximum) {

  # check if the data is in the correct input format
  if(any(names(data) != c("freqs", "positions", "range", "rMajor", "rMinor", "coverage")))
    stop("The data is not in the correct format! Please check")

  # get the number of contigs in the dataset
  nContigs <- length(data[["coverage"]])

  # get the name of the contigs
  contigs <- names(data[["coverage"]])

  # find out which sites are below or above a certain coverage threshold
  tokeep <- lapply(data[["coverage"]], function(contig) contig >= minimum & contig <= maximum)

  # to find out which sites, i.e. which rows, should be kept
  # we need to find the rows where the previous evaluates to true in all the populations
  tokeep <- lapply(tokeep, function(contig) rowSums(contig) == nPops)

  # remove sites below or above the coverage threshold
  # 1- from the list containing the information about the depth of coverage
  data[["coverage"]] <- lapply(1:nContigs, function(contig)
    data[["coverage"]][[contig]][tokeep[[contig]], , drop = FALSE])
  # 2 - from the list containing the allele frequencies
  data[["freqs"]] <- lapply(1:nContigs, function(contig)
    data[["freqs"]][[contig]][tokeep[[contig]], , drop = FALSE])
  # 3- from the list containing the number of reads for the major allele
  data[["rMajor"]] <- lapply(1:nContigs, function(contig)
    data[["rMajor"]][[contig]][tokeep[[contig]], , drop = FALSE])
  # 4- from the list containing the number of reads for the minor allele
  data[["rMinor"]] <- lapply(1:nContigs, function(contig)
    data[["rMinor"]][[contig]][tokeep[[contig]], , drop = FALSE])
  # 5- from the vector containing the position of the SNPs
  data[["positions"]] <- lapply(1:nContigs, function(contig)
    data[["positions"]][[contig]][tokeep[[contig]]])

  # we need to remove empty list elements i.e. contigs that have no sites left after the filtering
  # first, get the list elements to remove
  toremove <- sapply(data[["coverage"]], nrow) == 0
  # if there are any contigs to remove
  if(sum(toremove) != 0) {

    # remove those elements from the lists
    # 1 - from the list containing the information about the depth of coverage
    data[["coverage"]] <- data[["coverage"]][!toremove]
    # 2 - from the list containing the allele frequencies
    data[["freqs"]] <- data[["freqs"]][!toremove]
    # 3- from the list containing the number of reads for the major allele
    data[["rMajor"]] <- data[["rMajor"]][!toremove]
    # 4- from the list containing the number of reads for the minor allele
    data[["rMinor"]] <- data[["rMinor"]][!toremove]
    # 5- from the list containing the range of the contigs
    data[["range"]] <- data[["range"]][!toremove]
    # 6- and from the vector containing the position of the SNPs
    data[["positions"]] <- data[["positions"]][!toremove]
  }

  # then, we should also remove these contigs from the vector containing the names of the contigs
  contigs <- contigs[!toremove]
  # we can check to see if the names of the contigs kept in the range list are the same as the names in this vector
  if(identical(names(data[["range"]]), contigs) == FALSE)
    stop("There is something wrong with the names of the contigs")

  # then, we just need to add the names of the contigs to the various list entries
  names(data[["rMajor"]]) <- contigs; names(data[["coverage"]]) <- contigs; names(data[["freqs"]]) <- contigs;
  names(data[["rMinor"]]) <- contigs; names(data[["positions"]]) <- contigs

  # output the dataset
  data
}


#' Remove sites using quantiles of the depth of coverage
#'
#' Removes sites that have too many or too few reads from the dataset.
#'
#' The 25% and the 75% quantiles of the coverage distribution is computed for
#' each population in the dataset. Then, the lowest 25% quantile across all
#' populations is considered the minimum depth of coverage allowed. Similarly,
#' the highest 75% quantile across all populations is considered the maximum
#' depth of coverage allowed. The coverage of each population at each site is
#' compared with those threshold values and any site, where the coverage of at
#' least one population is below or above that threshold, is completely removed
#' from the dataset.
#'
#' @param nPops is an integer representing the total number of populations in
#'   the dataset.
#' @param data is a dataset containing information about real populations. This
#'   dataset should have lists with the allelic frequencies, the position of the
#'   SNPs, the range of the contig, the number of major allele reads, the number
#'   of minor allele reads and the depth of coverage.
#'
#' @return a list with the following elements:
#'
#'   \item{freqs}{a list with the allele frequencies, computed by dividing the
#'   number of minor-allele reads by the total coverage. Each entry of this list
#'   corresponds to a different contig. Each entry is a matrix where each row is
#'   a different site and each column is a different population.}
#'
#'   \item{positions}{a list with the positions of each SNP. Each entry of this
#'   list is a vector corresponding to a different contig.}
#'
#'   \item{range}{a list with the minimum and maximum SNP position of each
#'   contig. Each entry of this list is a vector corresponding to a different
#'   contig.}
#'
#'   \item{rMajor}{a list with the number of major-allele reads. Each entry of
#'   this list corresponds to a different contig. Each entry is a matrix where
#'   each row is a different site and each column is a different population.}
#'
#'   \item{rMinor}{a list with the number of minor-allele reads. Each entry of
#'   this list corresponds to a different contig. Each entry is a matrix where
#'   each row is a different site and each column is a different population.}
#'
#'   \item{coverage}{a list with the total coverage. Each entry of this list
#'   corresponds to a different contig. Each entry is a matrix where each row is
#'   a different site and each column is a different population.}
#'
#'   This output is identical to the `data` input, the only difference being the
#'   removal of sites with too many or too few reads.
#'
#' @examples
#' # load the data from one rc file
#' data(rc1)
#'
#' # clean and organize the data in this single file
#' mydata <- cleanData(file = rc1, pops = 7:10)
#'
#' # organize the information by contigs
#' mydata <- prepareFile(data = mydata, nPops = 4)
#'
#' # remove sites according to the coverage quantile
#' remove_quantileReads(nPops = 4, data = mydata)
#'
#' @export
remove_quantileReads <- function(nPops, data) {

  # check if the data is in the correct input format
  if(any(names(data) != c("freqs", "positions", "range", "rMajor", "rMinor", "coverage")))
    stop("The data is not in the correct format! Please check")

  # get the number of contigs in the dataset
  nContigs <- length(data[["coverage"]])

  # get the name of the contigs
  contigs <- names(data[["coverage"]])

  # get the coverage of each of the populations
  coverage <- lapply(1:nPops, function(pop) lapply(data[["coverage"]], function(contig) contig[, pop, drop = FALSE]))
  # convert the coverage of each population into a single vector
  coverage <- lapply(coverage, function(pop) unname(unlist(pop)))

  # calculate the 25% and the 75% quantile of the coverage distribution for each of the populations
  quantiles <- lapply(coverage, function(pop) stats::quantile(pop, probs = c(0.25, 0.75)))
  # get the minimum depth of coverage from the quantiles - i.e. which of the 25% quantiles is the lowest
  minimum <- min(unlist(quantiles))
  # get the maximum depth of coverage from the quantiles - i.e. which of the 75% quantiles is the highest
  maximum <- max(unlist(quantiles))

  # find out which sites are below or above a certain coverage threshold
  tokeep <- lapply(data[["coverage"]], function(contig) contig >= minimum & contig <= maximum)

  # to find out which sites, i.e. which rows, should be kept
  # we need to find the rows where the previous evaluates to true in all the populations
  tokeep <- lapply(tokeep, function(contig) rowSums(contig) == nPops)

  # remove sites below or above the coverage threshold
  # 1- from the list containing the information about the depth of coverage
  data[["coverage"]] <- lapply(1:nContigs, function(contig)
    data[["coverage"]][[contig]][tokeep[[contig]], , drop = FALSE])
  # 2 - from the list containing the allele frequencies
  data[["freqs"]] <- lapply(1:nContigs, function(contig)
    data[["freqs"]][[contig]][tokeep[[contig]], , drop = FALSE])
  # 3- from the list containing the number of reads for the major allele
  data[["rMajor"]] <- lapply(1:nContigs, function(contig)
    data[["rMajor"]][[contig]][tokeep[[contig]], , drop = FALSE])
  # 4- from the list containing the number of reads for the minor allele
  data[["rMinor"]] <- lapply(1:nContigs, function(contig)
    data[["rMinor"]][[contig]][tokeep[[contig]], , drop = FALSE])
  # 5- from the vector containing the position of the SNPs
  data[["positions"]] <- lapply(1:nContigs, function(contig)
    data[["positions"]][[contig]][tokeep[[contig]]])

  # we need to remove empty list elements i.e. contigs that have no sites left after the filtering
  # first, get the list elements to remove
  toremove <- sapply(data[["coverage"]], nrow) == 0
  # if there are any contigs to remove
  if(sum(toremove) != 0) {

    # remove those elements from the lists
    # 1 - from the list containing the information about the depth of coverage
    data[["coverage"]] <- data[["coverage"]][!toremove]
    # 2 - from the list containing the allele frequencies
    data[["freqs"]] <- data[["freqs"]][!toremove]
    # 3- from the list containing the number of reads for the major allele
    data[["rMajor"]] <- data[["rMajor"]][!toremove]
    # 4- from the list containing the number of reads for the minor allele
    data[["rMinor"]] <- data[["rMinor"]][!toremove]
    # 5- from the list containing the range of the contigs
    data[["range"]] <- data[["range"]][!toremove]
    # 6- and from the vector containing the position of the SNPs
    data[["positions"]] <- data[["positions"]][!toremove]
  }

  # then, we should also remove these contigs from the vector containing the names of the contigs
  contigs <- contigs[!toremove]
  # we can check to see if the names of the contigs kept in the range list are the same as the names in this vector
  if(identical(names(data[["range"]]), contigs) == FALSE)
    stop("There is something wrong with the names of the contigs")

  # then, we just need to add the names of the contigs to the various list entries
  names(data[["rMajor"]]) <- contigs; names(data[["coverage"]]) <- contigs; names(data[["freqs"]]) <- contigs;
  names(data[["rMinor"]]) <- contigs; names(data[["positions"]]) <- contigs

  # output the dataset
  data
}


#' Obtain the index of SNPs inside a block with defined size
#'
#' Selects a random block of a smaller size from a larger contig and obtain the
#' index of the SNPs that are contained within that block.
#'
#' This function starts by removing the edges of the contig. The size of the
#' removed portion is equal to the size of the block to keep. Then, a SNP is
#' randomly pick from the vector of all possible SNP positions. An initial block
#' is constructed by selecting all SNPs contained in a window of \code{window}
#' size, both upstream and downstream from that SNP. Finally, SNPs are removed
#' from both ends of that initial block until all remaining SNPs are contained
#' within a block of \code{window} size.
#'
#' @param positions is a numeric vector where each entry corresponds to the
#'   position of a SNP present in the contig.
#' @param range is a numeric vector with two entries: the first is the minimum
#'   position of any SNP of the contig and the second is the maximum position of
#'   any SNP in that same contig.
#' @param window is a non-negative integer indicating the size, in base pairs,
#'   of the block of the contig to keep.
#'
#' @return a numeric vector containing the index of the SNPs present within a
#'   randomly selected window of a given contig.
#'
#' @keywords internal
#'
#' @export
indexSNPs <- function(positions, range, window) {

  # set the default output of the function:
  # if there are NO windows with SNPs sufficiently far from the edges of the contig
  # the output of this function will be a 0
  final_index <- 0

  # remove contig edges - remove a block of "window" size from the start and the end of the contig
  # create vector of true or false with the SNPs to keep
  keep <- positions >= min(range) + window & positions <= max(range) - window
  # use this vector to filter out the start and the end of the contig
  pos <- positions[keep]

  # if after removing the edges of the contig there are no SNPs left in the contig:
  # stop the function and output the default result
  if(length(pos) == 0)
    stop(return(final_index))

  # randomly select one SNP from the vector of possible SNPs
  selectSNP <- sample(x = pos, size = 1)

  # which SNPs have a higher bp position than the selected SNP but are still within the distance allowed by the window
  above <- pos >= selectSNP & pos <= selectSNP + window
  # which SNPs have a lower bp position than the selected SNP but are still within the distance allowed by the window
  below <- pos <= selectSNP & pos >= selectSNP - window
  # combine both the below and above SNPs to create a vector of all the SNPs that are within a window block of the selected SNP
  # note that this includes SNPs that are both above and below the selected SNP:
  # this vector contains a block of roughly twice the size of the window we wish to keep
  SNPs <- unique(c(pos[below], pos[above]))

  # if, after removing the SNPs that are too far from the selected SNP, no SNPs are left:
  # stop the function and output the default result
  if(length(SNPs) == 0)
    stop(return(final_index))

  # if, after removing the SNPs that are too far from the selected SNP, only the selected SNP is present in the window
  # stop the function and output the index of that SNP
  if(length(SNPs) == 1)
    stop(return(which(positions %in% SNPs)))


  # so we need to remove SNPs from the vector until the minimum and maximum position of the kept SNPs are "window" bp apart
  # the following steps will be repeated until the condition is met:
  while (max(SNPs) - min(SNPs) > window) { # the condition is that the SNPs are contained within a block of "window" size

    # while the kept SNPs are too far apart, remove the last entry of the vector
    SNPs <- SNPs[-length(SNPs)]

    # if after removing the last entry of the vector, the SNPs are still too far apart:
    # remove the first entry of the vector
    if (max(SNPs) - min(SNPs) > window) {

      # remove first entry
      SNPs <- SNPs[-1]
    }
  }

  # output the index of the SNPs that are contained inside the randomly selected window (if one exists)
  final_index <- which(positions %in% SNPs)
  # output the result of the function
  final_index
}


#' Randomly select blocks of a given size from several contigs
#'
#' Selects one random block of a smaller size from multiple larger contigs and
#' obtain the index of the SNPs that are contained within that block.
#'
#' This function starts by removing the edges of the contigs. The size of the
#' removed portion is equal to the size of the block to keep. Then, a SNP is
#' randomly pick from the vector of all possible SNP positions. An initial block
#' is constructed by selecting all SNPs contained in a window of \code{window}
#' size, both upstream and downstream from that SNP. Finally, SNPs are removed
#' from both ends of that initial block until all remaining SNPs are contained
#' within a block of \code{window} size. All of these steps are performed for
#' each of the contigs present in the dataset, obtaining one window per contig.
#' Note that, in the end, only \code{nLoci} windows are kept.
#'
#' @param freqs is a list containing the allelic frequencies. Each entry of that
#'   list should represent a different contig and be a matrix where each row
#'   corresponds to a different site and each column to a different population.
#' @param positions is a list containing the position of the SNPs. Each entry
#'   should represent a different contig and be a vector containing the position
#'   of each SNP present in the contig.
#' @param range is a list containing the range of the contig. Each entry should
#'   represent a different contig and be a vector with two entries: the first
#'   detailing the minimum position of the contig and the second the maximum
#'   position of the contig.
#' @param rMajor is a list containing the number of major allele reads. Each
#'   entry of that list should represent a different contig and be a matrix
#'   where each row corresponds to a different site and each column to a
#'   different population.
#' @param rMinor is a list containing the number of minor allele reads. Each
#'   entry of that list should represent a different contig and be a matrix
#'   where each row corresponds to a different site and each column to a
#'   different population.
#' @param coverage is a list containing the depth of coverage. Each entry should
#'   represent a different contig and be a matrix with the sites as rows and the
#'   different populations as columns.
#' @param window is a non-negative integer indicating the size, in base pairs,
#'   of the block of the contig to keep.
#' @param nLoci is a non-negative integer indicating how many different contigs
#'   should be kept in the output. If each randomly selected \code{window} is a
#'   different loci, then how many different \code{window} should be selected?
#'
#' @return a list with the following elements:
#'
#'   \item{freqs}{a list with the allele frequencies, computed by dividing the
#'   number of minor-allele reads by the total coverage. Each entry of this list
#'   corresponds to a different contig. Each entry is a matrix where each row is
#'   a different site and each column is a different population.}
#'
#'   \item{positions}{a list with the positions of each SNP. Each entry of this
#'   list is a vector corresponding to a different contig.}
#'
#'   \item{rMajor}{a list with the number of major-allele reads. Each entry of
#'   this list corresponds to a different contig. Each entry is a matrix where
#'   each row is a different site and each column is a different population.}
#'
#'   \item{rMinor}{a list with the number of minor-allele reads. Each entry of
#'   this list corresponds to a different contig. Each entry is a matrix where
#'   each row is a different site and each column is a different population.}
#'
#'   \item{coverage}{a list with the total coverage. Each entry of this list
#'   corresponds to a different contig. Each entry is a matrix where each row is
#'   a different site and each column is a different population.}
#'
#' @keywords internal
#'
#' @export
pickWindows <- function(freqs, positions, range, rMajor, rMinor, coverage, window, nLoci) {

  # check the size of each contig - by subtracting the minimum of the range from the maximum
  # check which contigs are larger than 3 times the size of the desired window
  contigSize <- unname(sapply(range, FUN = function(contig) contig[2] - contig[1] >= window*3))

  # keep only those contigs that are large enough - i.e. larger than three times the size of the desired window
  freqs <- freqs[contigSize]; positions <- positions[contigSize]; rMajor <- rMajor[contigSize]; rMinor <- rMinor[contigSize]
  range <- range[contigSize]; coverage <- coverage[contigSize]

  # get the name of all the contigs still present in the dataset
  contigNames <- names(freqs)
  # randomly select nLoci*2 contigs to keep
  keep <- sort(sample(x = 1:length(contigNames), size = nLoci*2))

  # keep only those contigs that were randomly selected
  freqs <- freqs[keep]; positions <- positions[keep]; rMajor <- rMajor[keep]; rMinor <- rMinor[keep]
  range <- range[keep]; coverage <- coverage[keep]

  # this will create a list, where each entry is a different contig. Each entry contains the index of the SNPs that
  # are contained inside the randomly selected window - of size "window" bp
  indexWindows <- mapply(FUN = function(position, range, window) indexSNPs(position, range, window),
                         positions, range, window)

  # use the previous index to filter the allelic frequencies - keeping only the frequencies of SNPs inside the window
  freqs <- mapply(function(freq, index)
    freq[index, , drop = FALSE], freqs, indexWindows, SIMPLIFY = FALSE)

  # use the previous index to filter the number of major allele reads - keeping only the reads for SNPs inside the window
  rMajor <- mapply(function(reads, index)
    reads[index, , drop = FALSE], rMajor, indexWindows, SIMPLIFY = FALSE)

  # use the previous index to filter the number of minor allele reads - keeping only the reads for SNPs inside the window
  rMinor <- mapply(function(reads, index)
    reads[index, , drop = FALSE], rMinor, indexWindows, SIMPLIFY = FALSE)

  # use the previous index to filter the coverage - keeping only the depth of coverage for the SNPs inside the window
  coverage <- mapply(function(coverage, index)
    coverage[index, , drop = FALSE], coverage, indexWindows, SIMPLIFY = FALSE)

  # use the previous index to filter the positions - keeping only the positions of the SNPs inside the window
  positions <- mapply(function(pos, index)
    pos[index, drop = FALSE], positions, indexWindows, SIMPLIFY = FALSE)

  # some contigs might not have selectable windows - these contigs have zero rows
  # so, we can remove entries with nrow = 0 - get the index of the list entries with nrow = 0
  toremove <- unlist(sapply(freqs, nrow)) == 0
  # if there are entries to remove
  if(sum(toremove) != 0) {

    # remove these entries from the list with the allele frequencies
    freqs <- freqs[!toremove]
    # from the list with the SNP positions
    positions <- positions[!toremove]
    # from the list with the major allele reads
    rMajor <- rMajor[!toremove]
    # from the list with the minor allele reads
    rMinor <- rMinor[!toremove]
    # and from the coverage list
    coverage <- coverage[!toremove]
  }

  # finally, if after removing the contigs with no information, we still have more than nLoci contigs
  # we need to randomly select nLoci contigs as the final pick
  if(length(freqs) > nLoci) {

    # randomly select indexes of the contigs to keep
    indexFinal <- sort(sample(length(freqs), size = nLoci))
    # using this index to filter the allelic frequencies - keeping only the frequencies for the windows of the selected contigs
    freqs <- freqs[indexFinal]
    # keep only the SNP positions for the windows of the selected contigs
    positions <- positions[indexFinal]
    # keep only the SNP positions for the major allele reads of the selected contigs
    rMajor <- rMajor[indexFinal]
    # keep only the SNP positions for the minor allele reads of the selected contigs
    rMinor <- rMinor[indexFinal]
    # and keep only the depth of coverage for the windows of the selected contigs
    coverage <- coverage[indexFinal]
  }

  # create the output with the various entries for the windows of the nLoci contigs
  output <- list(freqs = freqs, positions = positions, rMajor = rMajor, rMinor = rMinor, coverage = coverage)
  # output the information for those windows
  output
}
