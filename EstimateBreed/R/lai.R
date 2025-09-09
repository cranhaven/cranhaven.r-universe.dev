#'Leaf Area Index (LAI)
#'@description
#'Utility function for estimating crop LAI
#'@param GEN The column with the genotype name
#'@param W The column with the width of the leaf (in meters).
#'@param L The column with the length of the leaf (in meters).
#'@param TNL The column with the total number of leaves.
#'@param TDL The column with the total number of dry leaves.
#'@param crop Crop sampled. Use 'soy' for soybean and 'maize' for maize, 'trit'
#'for wheat, 'rice' for rice, 'bean' for bean, 'sunflower' for sunflower,
#''cotton' for cotton, 'sugarcane' for sugarcane, 'potato' for potato and
#''tomato' for tomato.
#'@param sp Row spacing (Standard sp=0.45).
#'@param sden Sowing density, in plants per linear meter (standard sden=14).
#'@param verbose Logical argument. Runs the code silently if FALSE.
#'@return Returns the accumulated leaf area, the potential leaf area index
#'(considering the total number of leaves) and the actual leaf area index
#'(making the adjustment considering the number of dry leaves) for each genotype
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@references
#'Meira, D., Queiroz de Souza, V., Carvalho, I. R., Nardino, M., Follmann,
#'D. N., Meier, C., Brezolin, P., Ferrari, M., & Pelegrin, A. J. (2015).
#'Plastocrono e caracteres morfologicos da soja com habito de crescimento
#'indeterminado. Revista Cultivando o Saber, 8(2), 184-200.
#'@examples
#'library(EstimateBreed)
#'
#'data("leafarea")
#'#Crop selection
#'soy_lai<-with(leafarea,lai(GEN,C,L,TNL,TDL,crop="soy"))
#'
#'#Changing row spacing and sowing density
#'maize_lai<-with(leafarea,lai(GEN,C,L,TNL,TDL,crop="maize",sp=0.45,sden=4))
#'@export

lai <- function(GEN, W, L, TNL, TDL, crop = "soy", sp = 0.45, sden = 14,
                verbose=TRUE) {

  k_correction <- c(
    soy = 0.7,
    maize = 0.75,
    trit = 0.88,
    rice = 0.85,
    bean = 0.72,
    sunflower = 0.80,
    cotton = 0.85,
    sugarcane = 0.75,
    potato = 0.90,
    tomato = 0.92
  )

  if(!(crop %in% names(k_correction))){
    stop("Unknown culture. Please enter a valid culture: \n",
         paste(names(k_correction),collapse = ", "))
  }
  k <- k_correction[crop]
  if (missing(W)) {
    stop("Please enter the width of the leaf", call. = FALSE)
  }
  if (missing(L)) {
    stop("Please enter the length of the leaf", call. = FALSE)
  }
  if (missing(GEN)) {
    stop("Please enter the genotype", call. = FALSE)
  }
  if(!is.numeric(W) || !is.numeric(L)){
    stop("The width and length of the leaf must be numerical",call. = FALSE)
  }
    a1 <- data.frame(GEN, W, L, TNL, TDL)
    a1 <- a1 %>%
      mutate(AF = (W * L) * k)
    resultado <- a1 %>%
      group_by(GEN) %>%
      summarise(
        ALA = sum(AF)/10000,
        SD = (10000 / sp) * sden,
        AS = 10000 / ((10000 / sp) * sden),
        RED = TDL/TNL,
        PotLAI = (ALA / AS),
        RealLAI = PotLAI-(PotLAI*RED)
      )
    resultado_f <- resultado %>%
      select(GEN,ALA,PotLAI,RealLAI)
    return(resultado_f)
    if(verbose==TRUE){
      cat("Selected crop:",paste(crop),"\n")
      cat("Row spacing used: ",paste(sp),"\n")
      cat("Sowing density used: ",paste(sden),"\n")
    }
  }
