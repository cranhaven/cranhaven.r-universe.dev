#' plot_joint_poles
#'
#' This function plots on schmidt stereogram selected joints poles, draws density contour
#' lines and retrieves poles maxima.
#' Selected joints and poles maxima are saved in working folder.
#' 
#'
#' @param normal_from_wild the output matrix resulting from wildfire search
#' @param planes_mtx the list of joints output from calculate_plane function
#' @param area_ls the list of joints area output from calculate_planes_area function
#' @param file_name Name of the output data file containing joint poles
#' @param min_dens the minimum density pole value to be plotted
#' @param plane_area minimum value of joint area to be considered in plot
#' @return A Schmidt density plot with maxima values of joints
#' @importFrom MASS kde2d
#' @importFrom RockFab StereoPoint StereoPlot StereoWeb StereoCirc StereoPlane
#' @importFrom grDevices graphics.off rainbow
#' @importFrom graphics contour lines
#' @examples 
#' \dontrun{
#' 
#' normali_recalc<-Rcpp_wildfire_search(7,normals[,1:3],neighbours)
#' 
#' joint_list_Cpp<-calculate_joints(mesh3d,normali_recalc)
#' 
#' val_area<-calculate_joints_area(normali_recalc)
#' 
#' file_name<-"my_out_file"
#' 
#' max_pole<-0.3
#' 
#' min_area<-1
#' 
#' poles_maxima<-plot_joint_poles(normali_recalc,joint_list_Cpp,val_area,file_name,max_pole,min_area) }
#' 
#' @export

plot_joint_poles<-function(normal_from_wild,planes_mtx,area_ls,file_name,min_dens,plane_area ) {
  
  id_fam<-normal_from_wild[!duplicated(normal_from_wild[,5]),5]
  
  id_fam_no_zero<-id_fam[which(id_fam!=0)]
  
  dip_plunge_all<-apply(planes_mtx,1,function(x)dip_dir(c(x[[1]],x[[2]],x[[3]])))
  
  dip_plunge_all<-t(dip_plunge_all)
  
  dip_plunge_all<-cbind(dip_plunge_all,area_ls,id_fam_no_zero)
  
  dip_plunge<-dip_plunge_all[which(dip_plunge_all[,3]>plane_area),]  
  
  colnames(dip_plunge)<-c("dip","plunge","area","id_fam")
  
  write.table(dip_plunge, file = paste0(file_name,"_dp_",plane_area,"m2.txt"), quote=FALSE, sep = " ", col.names = T, row.names = F,qmethod = "double")
  
  poles<-t(apply(dip_plunge[,1:2], 1,function(x) planes2poles(x)))
  
  poles<-cbind(poles[,"plunge"],poles[,"dip"],dip_plunge[,"id_fam"])
  
  colnames(poles)<-c("plunge","dip","id_fam")
  
  poles_xy<-t(apply(poles,1,function(x) dip_plunge2xy(x[2],x[1])))
  
  matrix_smo<-kde2d(poles_xy[,1],poles_xy[,2],n=100,h=0.15)
  
  graphics.off()
  
  StereoPlot(my.title = file_name)
  
  StereoWeb()
  
  StereoCirc()
  
  lapply(1:dim(dip_plunge)[1], function(x) plot_giac(c(dip_plunge[x,1],dip_plunge[x,2]),"gray",0.25))
  
  contour(x = matrix_smo$x,y = matrix_smo$y, matrix_smo$z,xlim = range(matrix_smo$x, finite = TRUE),
          ylim = range(matrix_smo$y, finite = TRUE), zlim = range(matrix_smo$z, finite = TRUE), col="gray",add=T) 
  
  
  mpd_max<-c(0,0,0)
  
  for (r in 1:(dim(matrix_smo$z)[1]-2)){#//r<ROWS-2
    for (s in 1:(dim(matrix_smo$z)[2]-2)){  # //c<COLS-2
      c_center<- matrix_smo$z[r+1,s+1]
      c_E <- matrix_smo$z[r+1,s+2]
      c_N <- matrix_smo$z[r,s+1]
      c_S <- matrix_smo$z[r+2,s+1]
      c_W <- matrix_smo$z[r+1,s]
      c_NW<- matrix_smo$z[r,s]
      c_NE<- matrix_smo$z[r,s+2]
      c_SW<- matrix_smo$z[r,s+2]
      c_SE<- matrix_smo$z[r+2,s+2]
      if(!is.na(c_center) && !is.na(c_E) && !is.na(c_N)&& !is.na(c_S)&& !is.na(c_W)&& !is.na(c_NE)&& !is.na(c_NW)&& !is.na(c_SE)&& !is.na(c_SW)){
        
        if (c_center > c_E && c_center > c_W && c_center > c_N && c_center > c_S && c_center > c_NW && c_center > c_SE && c_center > c_NE && c_center > c_SW){
          
          mpd_max<-rbind(mpd_max,c((r+1),s+1,matrix_smo$z[r+1,s+1]))
        }
      }
    }
  }
  
  mpd_max<-mpd_max[-1,]
  
  mpd_max<-mpd_max[which(mpd_max[,3]>min_dens),]
  
  n_colors<-dim(mpd_max)[1]
  
  list_col_fam<-rainbow(n_colors)
  
  
  
  coord_max_x<-matrix_smo$x[mpd_max[,1]]
  coord_max_y<-matrix_smo$y[mpd_max[,2]]
  
  giac_max<-t(sapply(1:length(coord_max_x), function(x) xy2dip_plunge(coord_max_x[x],coord_max_y[x])))
  
  lapply(1:dim(giac_max)[1], function(x) StereoPoint(giac_max[x,1],giac_max[x,2],my.color = list_col_fam[x],my.size=1,my.label = paste(round(giac_max[x,1],0),round(giac_max[x,2],0),sep="/")))
  
  write.table(giac_max, file = paste0(file_name,"_max_",plane_area,"m2.txt"), quote=FALSE, sep = " ", col.names = T, row.names = F,qmethod = "double")
  
  return(giac_max)
  
  
}

