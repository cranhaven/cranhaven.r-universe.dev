##
## Function to group means
##
make.TukeyC.groups <- function(x)
{
  ##++++++++ 
  # O procedimento abaixo é para eliminar colunas iguais, pois não há sentido termos:
  # t1  a  b
  # t2  a  b
  # é tudo a
  x[upper.tri(x)] <- FALSE
  auxx <- t(x)
  auxy <- unique(auxx)
  newmat <- t(auxy)
  ##++++++++

  ncolnew <- ncol(newmat)
  nrownew <- nrow(newmat)

  ##+++++++++
  # Este procedimento é para marcar a partir de qual linha deve ser colocado as letras
  mat24 <- sapply(colnames(newmat),function(x)x==rownames(newmat))
  mat25 <- which(mat24 == TRUE, arr.ind = TRUE)
  ##+++++++++

  ##+++++++++
  # Criando a matrix indicadora das letras
  matzero <- matrix(NA,nrow=nrownew,ncol=ncolnew)
  for(i in 1:dim(mat25)[1]) {
    matzero[seq(mat25[i,1],nrownew),i] <- newmat[seq(mat25[i,1],nrownew),i]  
  }
  ##+++++++++

  ##+++++++++
  # Este procedimento é para obter as colunas que de fato serão colocado as letras
  aux <- apply(matzero,2,function(x)all(x==FALSE,na.rm=TRUE))
  aux3 <- matzero[,1:(length(aux[aux==FALSE])+1)]
  ##+++++++++

  ##+++++++++
  # Quando o pesquisador usa diretamento o teste de Tukey (sem ANOVA prévia), pode acontecer dos tratamentos serem iguais. O procedimento abaixo é uma proteção da função caso isso ocorra.
  ifelse(!is.vector(aux3),
         matreal <- apply(aux3,2,function(x)gsub(TRUE,'',x)),
         matreal <- matzero)

  matreal[is.na(matreal)] <- ''

  ##+++++++++
  # Criando um vetor de letras. Com as letras atuais do R, só é possível termos 52 letras entre minúsculas e maiúsculas. Ou seja, se todos os tratamentos fossem diferentes entre si, só seria possível  diferenciamos 52 tratamentos. Colocando caracteres como acentos entre outros, conseguiremos expandir o número de comparações.
  letras <- c(letters, paste(letters,rep(0:9,rep(26,10)),sep=''))
  ##+++++++++

  ##++++++++++
  # Detecando o número de núcleos da máquina
  #ifelse(Sys.info()['sysname']=='Windows', ncore <- 1,
  #ncore <- parallel::detectCores())
  #Devido a complicações na compilação do pacote devido as políticas do CRAN, resolvi retirar esta parte!
  #++++++++++

  ##+++++++++
  # Começando a brincadeira de fato
  #matnew <- parallel::mclapply(1:dim(matreal)[2],
  #                 function(i)gsub(FALSE,letras[i],matreal[,i]),mc.cores=ncore)

  matnew <- lapply(1:dim(matreal)[2],
                   function(i)gsub(FALSE,letras[i],matreal[,i]))
  matnew1 <- do.call('cbind',matnew)
}
