#' Images used as an example (Banco de imagens utilizadas como exemplo).
#'
#' @description Show the path of images used in the example file (Apresenta
#'   endereco de imagens utilizadas no arquivo de exemplo).
#' @usage example_image(ex)

#' @param ex    :example number (numero do exemplo).
#' The numbers corresponding to each example are(Os numeros correspondentes a
#' cada exemplo sao):
#' \itemize{
#'  \item 1 = Imagem com sementes de feijao fava ("Feijao.jpg")
#'  \item 2 = Imagem com ovos sobre folha de fumo ("Ovos1.jpg")
#'  \item 3 = Imagem com folhas de acereola ("imagem.jpg")
#'  \item 4 = Imagem com a paleta de cores do background da imagem com folhas de
#'  acerola ("fundo.jpg")
#'  \item 5 = Imagem com a paleta de cores das folhas de acerola ("folhas.jpg")
#'  \item 6 = Imagem com a paleta de cores do obejeto de referencia da imagem
#'  com folhas de  acerola ("Referencia.jpg")
#'  \item 7 = Imagem com folha de tomate com doenca ("FolhaTomate.jpg")
#'  \item 8 = Imagem com paleta de cores das partes sadias da folha de tomateiro
#'  ("TomateFolha.jpg")
#'  \item 9 = Imagem com a paleta de cores das partes doentes da folha de
#'  tomateiro ("TomateDoenca.jpg")
#'  \item 10 = Imagem de uma plantula ("Plantula.jpg")
#'  \item 11 = Imagem aerea de um rebanho (https://www.istockphoto.com/) ("gado.jpg")
#'  \item 12 = Imagem aerea com plantio de alface ("alface.jpg")
#'  \item 13 = Imagem de sementes de Amburana tratadas com tetrazolio ("Tetrazolio.jpg")
#'  \item 14 = Imagem de satelite - Banda de azul ("satelite1_Blue.tif")
#'  \item 15 = Imagem de satelite - Banda de verde ("satelite2_Green.tif")
#'  \item 16 = Imagem de satelite - Banda de vermelho ("satelite3_Red.tif")
#'  \item 17 = Imagem de satelite - Banda de IR("satelite4_NIR.tif")
#'  \item 18 = Imagem de satelite - Banda de SWIR("satelite5_SWIR.tif")
#'  \item 19 = Imagem de uma placa de petri("PlacaPetri.jpg")

#'          }

#' @return Returns the address of the example images (Retorna o endereco das
#'   imagens de exemplo).

#' @examples
# \donttest{
#' example_image(1)
#' example_image(2)
#}
#'@export
example_image=function(ex) {
  if((ex<1)|(ex>18)){stop("Image not available for this number", call. = FALSE)}
  Imagens=c("Feijao.jpg", #1
            "Ovos1.jpg",  #2
            "imagem.jpg",#3
            "fundo.jpg",#4
            "folhas.jpg",#5
            "Referencia.jpg",#6
            "FolhaTomate.jpg",#7
            "TomateFolha.jpg",#8
            "TomateDoenca.jpg",#9
            "Plantula.jpg",#10
            "gado.jpg",#11
            "alface.jpg",#12
            "Tetrazolio.jpg" ) #13
#            "satelite1_Blue.tif", #14
 #           "satelite2_Green.tif",#15
  #          "satelite3_Red.tif", #16
   #         "satelite4_NIR.tif", #17
    #        "satelite5_SWIR.tif"    #18
     #       )

  #system.file(paste0("images/", Imagens[ex]), package = "ExpImage")
 if(ex<14){
   return(system.file("images",Imagens[ex],package="ExpImage"))
   }

  github=c(
    "https://github.com/AlcineiAzevedo/Files_ExpImage/raw/main/satelite1_Blue.tif",
    "https://github.com/AlcineiAzevedo/Files_ExpImage/raw/main/satelite2_Green.tif",
    "https://github.com/AlcineiAzevedo/Files_ExpImage/raw/main/satelite3_Red.tif",
    "https://github.com/AlcineiAzevedo/Files_ExpImage/raw/main/satelite4_NIR.tif",
    "https://github.com/AlcineiAzevedo/Files_ExpImage/raw/main/satelite5_SWIR.tif"
  )

  if(ex>13){
    Imagens=c(Imagens,github)
    return(Imagens[ex])
  }


}





