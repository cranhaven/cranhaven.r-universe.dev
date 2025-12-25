#' plot_maxima2mesh
#'
#' This funtion plots a coloured mesh facets as great circles plot colours
#'
#' @param mesh_tr an object of type mesh3d
#' @param planes_max the output of plot_joint_great_circles function
#' @param normal_from_wild the output matrix iresulting from wildfire search
#' @param tol_ang_fam a tolerance angle to include joints in the same joint set color
#' @return A plot with great circles of joint maxima
#' @examples 
#' 
#' \dontrun{
#' 
#' azi_dip_maxima<-plot_joint_great_circles(poles_maxima, file_name)
#' 
#' plot_maxima2mesh(mesh3d,azi_dip_maxima,normali_recalc,10)
#' }
#' 
#' @export
#'

plot_maxima2mesh<-function(mesh_tr,planes_max,normal_from_wild, tol_ang_fam){
  
  n_colors<-dim(planes_max)[1]
  
  list_col_fam<-rainbow(n_colors)
  
  #write.table(list_col_fam, file = "list_col_fam_mesh.txt", quote=FALSE, sep = " ", col.names = T, row.names = F,qmethod = "double")
  
  normali_coord_max_planes<-t(apply(planes_max,1,function(x) fnormals(x[1],x[2])))
  
  id_fam_max<-matrix(0,dim(normal_from_wild)[1],1)
  
  fam_palette<-cbind (normal_from_wild, id_fam_max) 
  
  colnames(fam_palette)[7]<-"famiglia"
  
  
  fam_palette<-as.matrix(fam_palette) 
  
  
  indice_perc<-0
  
  tot<-dim(normali_coord_max_planes)[1]
  
  for (j in 1:dim(normali_coord_max_planes)[1]){  
    
    for (i in 1:dim(fam_palette)[1]){
      
      if (fam_palette[i,7]==0 ){
        
        cos_alfa=dotProduct(normali_coord_max_planes[j,],fam_palette[i,2:4])  
        
        if (is.nan(cos_alfa)) {cos_alfa<-0}
        
        if (cos_alfa>1) { cos_alfa<- 1} 
        
        if (cos_alfa < (-1)) {cos_alfa<- -1} 
        
        if (((acos(cos_alfa)*180/3.1415 <=tol_ang_fam) | (acos(cos_alfa)*180/3.1415>=180-tol_ang_fam))){   #
          
          acos(cos_alfa)*180/3.1415
          
          fam_palette[i,7]<-j
          
          #print(j)
        }
      }
    }
    perc = j/tot * 100;
    
    if (perc >= indice_perc){
      
      cat(indice_perc , "%.. ")
      
      indice_perc = indice_perc + 10;}
    
    
  }
  
  fam_palette[which(fam_palette[,7]==6),]
  
  legend_colors<-cbind(seq(1:dim(planes_max)[1]),list_col_fam)
  
  legend_colors<-rbind(c(0,"#D3D3D3"),legend_colors)
  
  colnames(legend_colors)[1]<-"num_fam"
  
  
  fam_col_palette<-merge(fam_palette,legend_colors, by.x="famiglia",by.y="num_fam",all.x=T)
  
  fam_col_palette_sort<-fam_col_palette[ order(fam_col_palette[,2]), ]
  
  #mesh_tr$material <- list(color=rep(tr_col_sort[,'list_col'], each = 3, alpha = 0.5))
  
  mesh_tr$material <-list(color=fam_col_palette_sort[,'list_col_fam'])
  
  #wire3d(mesh_tr)
  
  shade3d(mesh_tr, meshColor="faces")
  
  axes3d(c('x','y','z'))
  
  return(mesh_tr)
  
}
