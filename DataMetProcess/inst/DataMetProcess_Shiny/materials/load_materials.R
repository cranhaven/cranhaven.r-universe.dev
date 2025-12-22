load_materials <- function(path="./inst/DataMetProcess_Shiny/"){
  names <- c("css","logo","ufrpe","uast","pgea","capes","cnpq","facepe","gas")
  folders <- c(
    "materials/styles.css",
    "materials/Logo.png",
    "materials/UFRPE2.png",
    "materials/UAST2.png",
    "materials/PGEA.png",
    "materials/CAPES.png",
    "materials/CNPQ.png",
    "materials/FACEPE.png",
    "materials/gas.png"
  )
  materials <- lapply(folders,function(x) paste0(path,x))
  names(materials) <- names
  return(materials)
}


