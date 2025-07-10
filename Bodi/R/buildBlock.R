#' buildBlock
#'
#' Compute blocks of consecutive data for blockwise CV or sampling.
#'
#' @param Nblock number of blocks
#' @param data0 the learning set
#' @return A list of vectors containing the indices of each block. 
#' @examples
#' buildBlock(4, data.frame(id = 1:15))
#' @author Yannig Goude <yannig.goude@edf.fr>
#' @export

buildBlock <- function(Nblock, data0)
{
  borne_block<-floor(seq(1, nrow(data0), length=Nblock+1))
  block_list<-list()
  l<-length(borne_block)
  for(i in c(2:(l-1)))
  {
    block_list[[i-1]] <- c(borne_block[i-1]:(borne_block[i]-1))
  }
  block_list[[l-1]]<-c(borne_block[l-1]:(borne_block[l]))

  return(block_list)
}

## Faster version 
#Nblock <- 4
#nx <- 15 #nrow(data0)
#fuzz <- min((nx - 1L)/1000, 0.4 * nx/Nblock)
#breaks <- seq(1 - fuzz, nx + fuzz, length.out = Nblock + 1L)
#structure(split(seq(nx), cut(seq(nx), breaks)), names = NULL)
