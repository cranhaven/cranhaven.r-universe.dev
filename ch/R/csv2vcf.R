#' @title about csv2vcf
#' @description A simple method to  generate vcf file.
#' @param csv_file The csv file contains
#' names and  phone numbers.
#' The style is like this:
#' Joey  18100
#' Hans  12788
#' Tim   34689
#' The first column is the name,
#' the second column is the phone number corresponding to each person.
#' The above is an example, and it is not true personal information summary.
#' @param vcf_file The vcf file to create.
#' @param header For more see \code{\link[utils]{read.csv}},
#' the default is FALSE.
#' @return  NULL. t will be saved in a file with the suffix vcf.
#' @export
csv2vcf <- function(csv_file, vcf_file, header = FALSE) {
  csvvcf(csv_file, vcf_file, header = header)
}
#' @author Chai
#' @importFrom utils read.csv write.table

csvvcf <- function(csv_file, vcf_file, header = FALSE) {
  a <- read.csv(csv_file, header = header)
  b <- list(
    "BEGIN:VCARD", "VERSION:3.0",
    NA, NA, "END:VCARD"
  )
  if (file.exists(vcf_file)) {
    stop("Warining, vcf existed!\nYou can set a new file.")
  } else {
    for (i in 1:nrow(a)) {
      b[3] <- paste("FN:", a[i, 1])
      b[4] <- paste("TEL;TYPE=CELL:", a[i, 2])
      write.table(b, vcf_file,
        append = TRUE, sep = "\n",
        row.names = FALSE, col.names = FALSE,
        quote = FALSE,
        fileEncoding = "UTF-8"
      )
    }
    message(paste("vcf:", vcf_file))
  }
}
