# Functions to wrap C code that uses libmseed.
#
# See https://web.archive.org/web/20121117072251/http://mazamascience.com/WorkingWithData/?p=1099
#

# See seismic/src/parseMiniSEED.c for code comments
parseMiniSEED <- function(buffer) {
  result <- .Call("parseMiniSEED",buffer)
  return(result)
}


