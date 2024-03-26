#'Function to create a color palette for image color calibration (Funcao para criar uma paleta de cores para calibracao das cores de imagens).
#'
#' @description This function creates a palette for color calibration of images.(Esta funcao cria uma paleta para calibracao das cores de imagens).
#' @usage calibration_palette(n=4,px=500,file=NULL,plot=TRUE)

#' @param n    :Number of color tones to be created (n.n.n). Numero de tons de cores que se deseja criar (n.n.n).
#' @param px    :Number of rows and columns in the image (Numero de linhas e de colunas na imagem).
#' @param file    :Address of the file to be created (Endereco do arquivo a ser criado).
#' @param plot    :Plot the image  (Plota a imagem).
#'
#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)

#' @return Returns an image with a color palette (Retorna um imagem com uma paleta de cores).
#' @seealso  \code{\link{info_image}} , \code{\link{read_image}}

#' @examples
#' im=calibration_palette(n=4,px = 500)
#' im=calibration_palette(n=3,px = 100)

#
#'@export
#'


calibration_palette=function(n=4,px=500,file=NULL,plot=TRUE){
  if((n<2)|(n>7))stop("The n value must be between 2 and 7 (O valor de n deve ser entre 2 e 7)")
niveis=seq(0,1,l=n)

grid=expand.grid(R=niveis,G=niveis,B=niveis)

if(n==2){
a=0
MAT1=NULL
for(i in 1:2){
  MAT0=NULL
  for(j in 1:4){
    a=a+1
    mat=matrix(rgb(grid[a,]),round(px/2,0),round(px/4,0))
    MAT0=cbind(MAT0,mat)
  }
  MAT1=rbind(MAT1,MAT0)
}
}

if(n==3){
  a=0
  MAT1=NULL
  for(i in 1:3){
    MAT0=NULL
    for(j in 1:9){
      a=a+1
      mat=matrix(rgb(grid[a,]),round(px/3,0),round(px/9,0))
      MAT0=cbind(MAT0,mat)
    }
    MAT1=rbind(MAT1,MAT0)
  }
}

if(n==4){
  a=0
  MAT1=NULL
  for(i in 1:8){
    MAT0=NULL
    for(j in 1:8){
      a=a+1
      mat=matrix(rgb(grid[a,]),round(px/8,0),round(px/8,0))
      MAT0=cbind(MAT0,mat)
    }
    MAT1=rbind(MAT1,MAT0)
  }
}


if(n==5){
  a=0
  MAT1=NULL
  for(i in 1:8){
    MAT0=NULL
    for(j in 1:8){
      a=a+1
      mat=matrix(rgb(grid[a,]),round(px/8,0),round(px/8,0))
      MAT0=cbind(MAT0,mat)
    }
    MAT1=rbind(MAT1,MAT0)
  }
}

if(n==6){
  a=0
  MAT1=NULL
  for(i in 1:9){
    MAT0=NULL
    for(j in 1:24){
      a=a+1
      mat=matrix(rgb(grid[a,]),round(px/9,0),round(px/24,0))
      MAT0=cbind(MAT0,mat)
    }
    MAT1=rbind(MAT1,MAT0)
  }
}

if(n==7){
  a=0
  MAT1=NULL
  for(i in 1:7){
    MAT0=NULL
    for(j in 1:49){
      a=a+1
      mat=matrix(rgb(grid[a,]),round(px/7,0),round(px/49,0))
      MAT0=cbind(MAT0,mat)
    }
    MAT1=rbind(MAT1,MAT0)
  }
}


if(isTRUE(plot) ){plot_image(MAT1)}

if(!is.null(file)){
  write_image(MAT1,files = file)
}
return(MAT1)

}







