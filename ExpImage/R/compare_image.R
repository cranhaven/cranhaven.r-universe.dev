#'Function to compare image sizes  (Funcao para comparar os tamanhos das imagens).
#'
#' @description This function compares the size of images  (Esta funcao compara o tamanho das imagens).
#' @usage compare_image(imgs=NULL,filesnames=NULL,path = NULL)

#' @param imgs    :List object containing the images  (Objeto do tipo lista contendo as imagens).
#' @param filesnames    :Images names (Nomes das imagens).
#' @param path    :Path files  (Endereco das pastas).

#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)

#' @return Return images size (Retorna o tamanho das imagens).
#' @seealso  \code{\link{info_image}} , \code{\link{read_image}}

#' @examples
#' #Carregando imagens de exemplo
#' im1=read_image(example_image(1))
#' im2=read_image(example_image(2))
#' im3=read_image(example_image(3))
#'
#' #Comparando o tamanho das imagens
#' compare_image(list(im1,im2,im3))
#
#'@export
#'
compare_image=function(imgs=NULL,filesnames=NULL,path = NULL){

  if(!is.null(path)){ filesnames=paste(path,filesnames,sep="/")}





  if(is.null(imgs)&!is.null(filesnames)){
  imgs=list()

  pb <- progress::progress_bar$new(total = length(filesnames))
    for(i in 1:length(filesnames)){
      pb$tick()
      imgs[[i]]=read_image(filesnames[i])
    }

  }


RES=NULL
#pb <- progress::progress_bar$new(total = length(filesnames))
for(i in 1:length(imgs)){
  #pb$tick()
  a=imgs[[i]]@.Data

  if(is.matrix(a)){
   res= c(dim(a),1)
  }

  if(!is.matrix(a)){
    res=dim(a)
  }

  RES=rbind(RES,res)
  }

colnames(RES)=c("rows","cols","bands")
rownames(RES)=1:nrow(RES)
list(Images_size=RES,Same_size=mean(apply(RES,1,function(x) mean(x==colMeans(RES))))==1)

}
