#' Divide a imagem em objetos/parcelas de acordo com o shapefile
#'
#' @description Esta funcao divide a imagem em objetos/parcelas a partir do shapefile criado pela funcao
#'  shape_file.create. Dessa forma e criada uma imagem para cada objeto.
#' @usage shape_file.split(im,shapefile,namesFile="test",path=getwd(),type="jpg")
#' @param im    :Este objeto deve conter uma imagem no formato do EBImage/ExpImage.
#' @param shapefile Objeto criado pela funcao shape_file.create
#' @param namesFile Nome do arquivo a ser salvo.
#' @param path Endereco da pasta onde as imagens seram salvas.
#' @param type Extensao da imagem a ser salva.EX: jpg, png, tiff.
#' @seealso  \code{\link{shape_file.create}}, \code{\link{shape_file.split}}
#' @importFrom graphics par
#' @examples
#'\dontrun{
#' end=example_image(13)
#' im=read_image(end,plot=TRUE)
#' A=shape_file.create(im,rows=5,cols=5,rectangular=F,Matrix=NULL,SelectSeveral = F)
#' B=shape_file.BorderExtract(im,A,p.rows = .9,p.cols = .9)
#' shape_file.plot(im,ShapeFile = B)
#' shape_file.split(im =im,shapefile = B,path = getwd(),namesFile = "TEST",type = ".jpg")
#'}
#'@export


shape_file.split= function(im,shapefile,namesFile="test",path=getwd(),type="jpg"){

  #  shapefile=sh;namesFile="test";path=getwd();type="jpg"
  # info=shapefile$info
  #shapefile=shapefile$Mat
  pbbb = txtProgressBar(min = 0, max = length(unique(shapefile[,1])), initial = 0)
Nomes=NULL
print("Progress:")
for(i in unique(shapefile[,1])){
  setTxtProgressBar(pbbb,i)
  id=shapefile[,1]==i
  pa=shapefile[id,3:4][1,]
  pb=shapefile[id,3:4][2,]
  pc=shapefile[id,3:4][3,]
  pd=shapefile[id,3:4][4,]
  sh1=round(rbind(pa,pb,pc,pd),0)

  ii=im
  ii@.Data[,,]=im@.Data[,,]*0+1
  ii[sh1[1,1],sh1[1,2],]=0
  ii[sh1[2,1],sh1[2,2],]=0
  ii[sh1[3,1],sh1[3,2],]=0
  ii[sh1[4,1],sh1[4,2],]=0

    pa0=pa;pb0=pb;pc0=pc;pd0=pd
    pb[2]=pa[2]=min(pb0[2],pa0[2])
    pc[1]=pb[1]=max(pc0[1],pb0[1])
    pd[2]=pc[2]=max(pd0[2],pc0[2])
    pa[1]=pd[1]=min(pa0[1],pd0[1])

    sh2=rbind(pa,pb,pc,pd)

 im2= crop_image(im,w=round(min(sh2[,1]),0):round(max(sh2[,1]),0),
             h=round(min(sh2[,2]),0):round(max(sh2[,2]),0),plot = F)
 nome=paste0(path,"/",namesFile,"_",i,".",type)
write_image(im2,files = nome)

Nomes=c(Nomes,nome)
 }

print("Arquivos criados (files created):")
print(Nomes)

return(Nomes)

}
