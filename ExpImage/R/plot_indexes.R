#' Function to plot multiple indices (funcao para plotar varios indices)
#'
#' @description Function to plot multiple indices (funcao para plotar varios
#'  indices).
#' @usage plot_indexes(im,indexes=c("r","g","b","rg","rb","gb","rgb",
#' "r/rgb","g/rgb","b/rgb",
#'"BI","BIM","SCI","GLI","HI",
#'"NGRDI","SI","VARI","HUE","MGVRI","GLI",
#'"MPRI","RGVBI","ExG","VEG"),
#'NumberCores="all")
#' @param indexes    :indices que se deseja testar (indexes you want to test):\cr
#'    "r" = extrair a banda de vermelho\cr
#'    "g" = extrair a banda de verde\cr
#'     "b" = extrair a banda de azul\cr
#'    "rg" = considera a media da banda de vermelho e verde: (r+g)/2\cr
#'    "rb" = considera a media da banda de vermelho e azul: (r+b)/2\cr
#'    "gb" = considera a media da banda de verde e azul: (g+b)/2\cr
#'   "rgb" = considera a media das 3 bandas: (r+g+b)/3\cr
#'   "r/g"=r/g\cr
#'   "r/b"=r/b\cr
#'   "g/r"=g/r\cr
#'   "g/b"=g/b\cr
#'   "b/r"=b/r\cr
#'   "b/g"=b/g\cr
#'   "S"=((R+G+B)-3*B)/(R+G+B)
#'    "BI"=sqrt((r^2+g^2+b^2)/3)\cr
#'    "BIM"=sqrt((2r+2g+2b)/3)\cr
#'    "SCI"=(r-g)/(r+g)\cr
#'    "GLI"=(2g-r-b)/(2g+r+b)\cr
#'    "HI"=(2r-g-b)/(g-b)\cr
#'    "NGRDI"=(g-r)/(g+r)\cr
#'    "SI"=(r-b)/(r+b)\cr
#'    "VARI"=(g-r)/(g+r-b)\cr
#'    "HUE"=atan(2(b-g-r)/30.5(g-r))\cr
#'    "MGVRI"=(g^2-r^2)/(g^2+r^2)\cr
#'    "GLI"=(2g-r-b)/(2g+r+b)\cr
#'    "MPRI"=(g-r)/(g+r)\cr
#'    "RGVBI"=(g-(br))/(g^2(br))\cr
#'    "ExG"=(2*g-r-b)\cr
#'    "VEG"=(g/(g^0.66667*b^0.66667))\cr
#' @param im    :
#' @param NumberCores Indica o numero de cores a serem utilizados no processamento.
#'   Pode ser um valor numerico. Se for 'ALL' sera considerado o numero maximo de
#'    cores do PC. (Indicates the number of colors to be used in processing.
#'    It can be a numerical value. If it is 'ALL' it will be considered the
#'    maximum number of PC cores).
#' @seealso  \code{\link{gray_scale}}
#' @importFrom graphics par
#' @importFrom parallel clusterExport stopCluster parLapply

#' @examples
#' \donttest{
#'#Carregar imagem de exemplo
#'im=read_image(example_image(2),plot=TRUE)
#'plot_indexes(im,NumberCores=2)
#'}

#'@export


plot_indexes=function(im,indexes=c("r","g","b","rg","rb","gb","rgb","r/rgb","g/rgb","b/rgb",
                                  "BI","BIM","SCI","GLI","HI",
                                  "NGRDI","SI","VARI","HUE","MGVRI","GLI","MPRI","RGVBI","ExG","VEG"),NumberCores="all"){
  method=indexes

   #Separar a imagem em bandas
im=EBImage::resize(im,w =200 )

lin=nrow(im@.Data[,,1])
col=ncol(im@.Data[,,1])
lay=length(im@.Data[1,1,])




if(NumberCores=="all"){NumberCores=parallel::detectCores()}

if (NumberCores > parallel::detectCores()) {
  message(paste0(" O numero de cores maximo (Maximum number of cores): ", parallel::detectCores()))
  NumberCores = parallel::detectCores()
}

cl <- parallel::makeCluster(NumberCores)
parallel::clusterExport(cl,
              varlist = c("gray_scale","im","method"),
              envir=environment())
on.exit(parallel::stopCluster(cl))



r <- parallel::parLapply(cl = cl,1:25, function(x){gray_scale(im,method =method[x],plot = F)})

n=length(indexes)
linha=ceiling(sqrt(n))
coluna=ceiling(n/linha)

op <- par(mfrow = c(linha,coluna))
#on.exit(par(op))
for(i in 1:n){
  plot_image((r[i][[1]]))
  text(lin/2,col/2,method[i],col="red",cex=2)
  #print(method[i])
}
par(mfrow = c(1,1))
#dev.off()
}










