###################################################################
### Convert cross object from R\qtl package to netgwas object #####
###################################################################

cross2netgwas <- function(cross.obj)
{
  nchr <- length(cross.obj$geno)
  netgwas.obj <- cross.obj$geno[[1]]$data
  
  if(nchr == 1) return(netgwas.obj) else{ 
      for(i in 2: nchr ) {
      netgwas.obj <- cbind(netgwas.obj, cross.obj$geno[[i]]$data)
      }
  return(netgwas.obj)
  }
  
  #pos.map$geno[[i]]$map
}