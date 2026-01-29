\docType{methods}
\name{downloadFromMirror}
\alias{downloadFromMirror}
\title{Download file from a mirror}
\description{
Downloads a file from a given mirror and checks its md5 sum. 
The file is stored in a given path
}
\usage{
downloadFromMirror(mirror, file_path, md5_sum)
}
\arguments{
\item{mirror}{URL from which to download the file.}
\item{file_path}{Path or folder where the downloaded file will be stored.}
\item{md5_sum}{md5 checksum string corresponding to the file to download. The method will check that the downloaded file checksum and the md5_sum parameter match.}
}
\value{
Returns 1 if the download was successful and -1 otherwise.
}
\examples{
\donttest{
# Download RKEELjars file
dCode = RKEEL::downloadFromMirror("https://personal.us.es/jmoyano1/RKEELjars_1.1.zip", 
  downloadedJarFile, md5_sum)

# Check if the download was successful
if(dCode<0){
  print('There was an error during the download.')
}
}
}
\keyword{utils}

