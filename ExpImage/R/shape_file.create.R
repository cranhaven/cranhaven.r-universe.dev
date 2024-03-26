#' Funcao para criar as coordenadas dos objetos/parcelas na imagem
#'
#' @description Esta funcao cria um arquivo com as coordenadas dos vertices de cada objeto/parcela
#' de interesse na imagem.
#' @usage shape_file.create(im,
#'                          rows=10,
#'                          cols=6,
#'                          rectangular=TRUE,
#'                          colLines="white",
#'                          ColPlot="red",
#'                          ColNumber="red",
#'                          SelectSeveral=FALSE,
#'                          Matrix=NULL)
#' @param im    :Este objeto deve conter uma imagem no formato do EBImage.
#' @param rows Numbers of rows (Numero de linhas).
#' @param cols Numbers of columns (Numero de colunas).
#' @param rectangular Valor logico. Se for TRUE sera feita uma correcao para as parcelas ficarem retangulares
#' @param colLines Nome da cor que as linhas tracadas serao apresentadas. Default e "white".
#' @param ColPlot Nome da cor que as linhas do grid a serem apresentadas. Default e "red".
#' @param ColNumber Nome da cor que de cada retangulo a serem apresentadas. Default e "red".
#' @param SelectSeveral Valor logico indicando se deseja-se desenhar mais de um grid. Default e FALSE.
#' @param Matrix Objeto com os grids criados anteriormente, caso exista. Default e NULL.
#' @seealso  \code{\link{shape_file.BorderExtract}}, \code{\link{shape_file.split}}
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

shape_file.create=function(im,rows=10,
                     cols=6,
                     rectangular=TRUE,
                     colLines="white",
                     ColPlot="red",
                     ColNumber="red",
                     SelectSeveral=FALSE,

                     Matrix=NULL){
  # rows=6;cols=10;rectangular=TRUE;colLines="white";ColPlot="red";ColNumber="red";SelectSeveral=FALSE;Matrix=NULL
  col1=colLines
  col2=ColPlot


  stop=FALSE
 im0=im
 im=resize_image(im,w=400)

 p= plot_image(im)


  if(isFALSE(is.null(Matrix))){
  #  Matrix[,3]=nrow(im@.Data[,,1])*Matrix[,3]/nrow(im0@.Data[,,1])
  #  Matrix[,4]=ncol(im@.Data[,,1])*Matrix[,4]/ncol(im0@.Data[,,1])

    points(Matrix[,3],Matrix[,4],col=col2)

  }


  colu1=cols+1
  row1=rows+1

  while(stop==FALSE){

#Colunas
    n=colu1
    message("Clique sobre os quatro pontos delimitadores (Click on the four bounding points)")
    pa <- unlist(locator(type="p",n = 1, col="red",pch=19))
    pb <- unlist(locator(type="p",n = 1, col="red",pch=19))
    lines(rbind(pa,pb), col=col1)
    pc <- unlist(locator(type="p",n = 1, col="red",pch=19))
    lines(rbind(pb,pc), col=col1)
    pd <- unlist(locator(type="p",n = 1, col="red",pch=19))
    lines(rbind(pc,pd), col=col1)
    lines(rbind(pd,pa), col=col1)

      if(isTRUE(rectangular)){
        pa0=pa;pb0=pb;pc0=pc;pd0=pd
        pb[2]=pa[2]=min(pb0[2],pa0[2])
        pc[1]=pb[1]=max(pc0[1],pb0[1])
        pd[2]=pc[2]=max(pd0[2],pc0[2])
        pa[1]=pd[1]=min(pa0[1],pd0[1])
        }
    V1=rbind(pa,pb);V2=rbind(pd,pc)


    lines(rbind(pc,pd), col=col2)
    lines(rbind(pd,pa), col=col2)
    lines(rbind(pa,pb), col=col2)
    lines(rbind(pb,pc), col=col2)


    a=V1[1,]
    b=V1[2,]
    d=seq(0,sqrt(sum((a-b)^2)),l=n)

    B=(b[2]-a[2])/(b[1]-a[1])

    if(is.infinite(B)){B=0}

    mat=cbind(seq(a[1],b[1],l=n),a[2]+B*(seq(a[1],b[1],l=n)-a[1]))
    mat=cbind(1,1:nrow(mat),mat,sqrt((a[1]-mat[,1])^2+(a[2]-mat[,2])^2))

    coord=t(sapply(1:n,function(i) mat[abs(mat[,5]-d[i])==min(abs(mat[,5]-d[i])),3:4]))

    CC1=cbind(coord[,1],coord[,2])

    #############################################

    a=V2[1,]
    b=V2[2,]
    d=seq(0,sqrt(sum((a-b)^2)),l=n)

    B=(b[2]-a[2])/(b[1]-a[1])

    if(is.infinite(B)){B=0}

    mat=cbind(seq(a[1],b[1],l=n),a[2]+B*(seq(a[1],b[1],l=n)-a[1]))
    mat=cbind(1,1:nrow(mat),mat,sqrt((a[1]-mat[,1])^2+(a[2]-mat[,2])^2))

    coord=t(sapply(1:n,function(i) mat[abs(mat[,5]-d[i])==min(abs(mat[,5]-d[i])),3:4]))

    CC2=cbind(coord[,1],coord[,2])

    sapply(1:n, function(i) lines(rbind(CC1[i,],CC2[i,]),col=col2))

CC1v=CC1;CC2v=CC2

    colnames(mat)=c("Line","Point","x","y","Distance")
    #print(n)

    # if(isFALSE(is.null(Matrix))){
    #   mat[,1]=mat[,1]+max(Matrix[,1])
    #   mat[,2]=mat[,2]+max(Matrix[,2])
    #
    #   mat=rbind(Matrix,mat)
    # }
    # Matrix=mat

  #############################################################
    ###################################################
    ############################################
    #linhas
    n=row1
  V1=rbind(pc,pb);V2=rbind(pd,pa)


    a=V1[1,]
    b=V1[2,]+.01
    d=seq(0,sqrt(sum((a-b)^2)),l=n)

    B=(b[2]-a[2])/(b[1]-a[1])
    if(is.infinite(B)){B=0}

    mat=cbind(seq(a[1],b[1],l=n),a[2]+B*(seq(a[1],b[1],l=n)-a[1]))
    mat=cbind(1,1:nrow(mat),mat,sqrt((a[1]-mat[,1])^2+(a[2]-mat[,2])^2))

    coord=t(sapply(1:n,function(i) mat[abs(mat[,5]-d[i])==min(abs(mat[,5]-d[i])),3:4]))

    CC1=cbind(coord[,1],coord[,2])
    #############################################

    a=V2[1,]
    b=V2[2,]+0.01
    d=seq(0,sqrt(sum((a-b)^2)),l=n)

    B=(b[2]-a[2])/(b[1]-a[1])
    if(is.infinite(B)){B=0}


    mat=cbind(seq(a[1],b[1],l=n),a[2]+B*(seq(a[1],b[1],l=n)-a[1]))
    mat=cbind(1,1:nrow(mat),mat,sqrt((a[1]-mat[,1])^2+(a[2]-mat[,2])^2))

    coord=t(sapply(1:n,function(i) mat[abs(mat[,5]-d[i])==min(abs(mat[,5]-d[i])),3:4]))

    CC2=cbind(coord[,1],coord[,2])

    sapply(1:n, function(i) lines(rbind(CC1[i,],CC2[i,]),col=col2))


    CC1h=CC1;CC2h=CC2




MAT=NULL
for(i in 1:colu1){
a=CC1v[i,]
b=CC2v[i,]+0.001
d=seq(0,sqrt(sum((a-b)^2)),l=n)

B=(b[2]-a[2])/(b[1]-a[1])

if(is.infinite(B)){B=0}

mat=cbind(seq(a[1],b[1],l=n),a[2]+B*(seq(a[1],b[1],l=n)-a[1]))
mat=cbind(i,1:nrow(mat),mat,sqrt((a[1]-mat[,1])^2+(a[2]-mat[,2])^2))

MAT=rbind(MAT,mat)
}



MAT2=Matrix
parcela=0
if(!is.null(Matrix)){parcela=max(Matrix[,1])}

    for(j in 1:(row1-1)){
      jj=j+1
      for(i in 1:(colu1-1)){
        ii=i+1

        id_i=MAT[,1]==i
        id_ii=MAT[,1]==ii

        id_j=MAT[,2]==j
        id_jj=MAT[,2]==jj

        parcela=parcela+1

        MAT2=rbind(MAT2,
                   cbind(Plot=parcela,intercept=1:4,
        rbind(MAT[id_i&id_j,3:4],
              MAT[id_i&id_jj,3:4],
              MAT[id_ii&id_jj,3:4],
              MAT[id_ii&id_j,3:4]))
        )




  }
}
Matrix=MAT2
colnames(MAT2)[3:4]=c("x","y")

  if(SelectSeveral==FALSE){stop=TRUE}

    if(SelectSeveral==TRUE){

    bk=readline(prompt = " Deseja selecionar mais um shape_plot (y/n)? \n Do you want to select one more plot (y/n)? ")
    if(sum((bk!="y"),(bk!="n"))==2){
      while(sum((bk!="y"),(bk!="n"))==2){
        message(" A resposta deve ser 'y' ou 'n'. \n The answer must be 'y' or 'n'.")
        bk=readline(prompt = " Deseja selecionar mais um shape_plot (y/n)?  \n Do you want to select one more plot (y/n)? ")
      }

    }

    if(bk=="n"){stop=TRUE}



    }
  }




  MAT3=aggregate(MAT2,list(as.factor(MAT2[,1])),mean)
text(MAT3[,4],MAT3[,5],MAT3[,1],col=ColNumber)

MAT2[,3]=nrow(im0@.Data[,,1])*MAT2[,3]/nrow(im@.Data[,,1])
MAT2[,4]=ncol(im0@.Data[,,1])*MAT2[,4]/ncol(im@.Data[,,1])
info=info_image(im)

#res=list(length=info$Length,Mat=MAT2)


  return(MAT2)
}










