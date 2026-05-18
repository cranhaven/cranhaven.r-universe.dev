# Part of the SMITIDstruct R package.
# Copyright (C) 2018 Jean-François Rey <jean-francois.rey@inra.fr>
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation, Inc.,i
# 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
#


#' @title demo.SMITIDstruct.run
#' @description run a demo to load HostSet, ViralPopSet and index
#' @export
demo.SMITIDstruct.run <- function() {

  path = system.file("extdata", "data-simul/", package="SMITIDstruct")

  # load inference tree information (mainly host id and time)
  lhost <- list()
  class(lhost) <- "hostSet"
  lhost <- loadTree(lhost,paste(path,"/tree.txt",sep=''))

  #load Viral population from fasta files
  files <- list.files(path, pattern = ".*.fasta" ,full.names=FALSE)
  # remove ".fasta"
  lfileinfo <- sapply(files,function(x){return(substr(x,1,nchar(x)-6))})
  # split ID_TIME
  splitFiles <- strsplit(lfileinfo, "_");
  # data frame
  listF <- cbind(data.frame(matrix(unlist(splitFiles),nrow=length(splitFiles), byrow=T), stringsAsFactors = FALSE), names(splitFiles))
  colnames(listF) <- c("id", "time", "filename")
  lvpop <- loadViralPop(path,listF)
  lhost <- addViralObs(lhost,lvpop)

  # load Observation data
  obs <- read.table(paste(path,"/obs.txt",sep=''),header=TRUE, check.names=FALSE)
  obs.states <- c(colnames(obs[-grep("ID|Tobs.*",colnames(obs))]))
  lhost <- loadStates(lhost, obs, colStates=obs.states)

  # load host coordinates
  coords <- read.table(file=paste(path,"/hosts_coords.txt",sep=''), header=TRUE, check.names=FALSE)
  lhost <- loadCoords(lhost,coords)


  # create index to retreive all informations
  index <- createIndex(lhost)
  
  return(list(lhost=lhost, lviralpop=lvpop, index=index))
}
