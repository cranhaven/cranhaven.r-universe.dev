#' build_3d_mesh
#'
#' This function reads a XYZRGB text file, requires a search radius in meters
#' and an output file name to save the resulting mesh.
#' for data format see file in package extdata folder
#'
#' @param path2myXYZRGBtxt Path to the XYZRGB.txt input file
#' @param search_radius Path to the XYZRGB.txt input file
#' @param file_name name of the output .ply mesh file
#' @return A 3D triangular mesh
#' @export
#' @importFrom Rvcg vcgBallPivoting vcgPlyWrite vcgClean vcgPlyRead
#' @importFrom utils read.table write.table
#' @importFrom rgl open3d wire3d axes3d shade3d tmesh3d
#' @examples 
#' \dontrun{path2myXYZRGBtxt<-system.file("extdata", "test.txt", package = "JFM")
#' 
#' file_name<- "test"
#' 
#'mesh3d<-build_3d_mesh(path2myXYZRGBtxt,0.5,file_name)}

build_3d_mesh<-function(path2myXYZRGBtxt, search_radius, file_name){
  
  mydata = read.table(path2myXYZRGBtxt, header=T )
  
  #head(mydata)
  
  tab_xyz<-data.frame(mydata$X,mydata$Y,mydata$Z)
  
  X_c <- as.matrix(as.numeric(as.character(tab_xyz[1:(dim(tab_xyz)[1]),1])))
  
  Y_c <- as.matrix(as.numeric(as.character(tab_xyz[1:(dim(tab_xyz)[1]),2])))
  
  Z_c <- as.matrix(as.numeric(as.character(tab_xyz[1:(dim(tab_xyz)[1]),3])))
  
  matrix_coord <- cbind(X_c,Y_c,Z_c)
  
  local_coord<-matrix(0,dim(X_c)[1],3)
  
  val<-matrix(0,dim(X_c)[1],1)
  
  #matrix_lc <- cbind(local_coord,val, val)
  
  #colnames(matrix_lc)<- c("X_loc","Y_loc","Z_loc","cluster","seed")
  
  min_X=min(X_c)
  
  min_Y=min(Y_c)
  
  min_Z=min(Z_c)
  
  matrix_lc_r<-apply(matrix_coord,1,function(x) c(x[1]-min_X,x[2]-min_Y,x[3]-min_Z))
  
  matrix_lc<-t(matrix_lc_r)
  
  mesh_tr<-vcgBallPivoting(matrix_lc,radius = search_radius)
  
  mesh_tr_clean<-vcgClean(mesh_tr, sel = c(0,1), tol = 0, silent = FALSE, iterate = FALSE)
  
  vcgPlyWrite(mesh_tr_clean,paste0(file_name,".ply"),binary = TRUE,writeCol = TRUE )
  
  #normali<-t(mesh_tr$normals)
  
  bound<-10
  
  open3d()
  
  wire3d(mesh_tr_clean) 
  
  #bbox3d()
  
  axes3d(c('x','y','z'))
  
  return(mesh_tr_clean)
  
}
