plot_giac<-function(dip_and_dir, v_color, pnt_dim){
  
  
  if (dip_and_dir[1] <= 180) {
    
    
    StereoPoint(my.az = dip_and_dir[1]+180, my.inc = 90-dip_and_dir[2],my.color = v_color, my.size = pnt_dim)
    
  } else
    
    StereoPoint(my.az = dip_and_dir[1]-180, my.inc = 90-dip_and_dir[2],my.color = v_color, my.size = pnt_dim)
  
  
  
}


dip_dir<-function(nxnynz){
  
  if (nxnynz[3] < 0){
    
    nxnynz[3]<-nxnynz[3]*-1
    nxnynz[2]<-nxnynz[2]*-1
    nxnynz[1]<-nxnynz[1]*-1
    
  }
  dip_v<-90+180/pi*asin(-nxnynz[3])
  
  val_atan<-atan(nxnynz[1]/nxnynz[2])
  
  if(nxnynz[2]<=0) {
    
    dip_dir_v<-180+180/pi*val_atan }
  
  else if((nxnynz[1]>=0) && (nxnynz[2]>=0)){
    
    dip_dir_v<-180/pi*val_atan
    
  }
  else {dip_dir_v<-360+180/pi*val_atan}
  
  return(c(dip_dir_v,dip_v)) 
}




dotProduct <- function(ab,ac){
  
  dot = (ab[1] * ac[1] + ab[2] * ac[2] + ab[3] * ac[3])/(sqrt(ab[1]^2+ab[2]^2+ab[3]^2)*sqrt(ac[1]^2+ac[2]^2+ac[3]^2));
  
  if (is.nan(dot)) {dot<- 0}
  
  if (dot > 1) {dot<- 1}
  
  if (dot< -1) {dot<- -1}
  
  
  return (dot)
}

crossProd <- function(ab,ac){
  abci = ab[2] * ac[3] - ac[2] * ab[3];
  abcj = ac[1] * ab[3] - ab[1] * ac[3];
  abck = ab[1] * ac[2] - ac[1] * ab[2];
  return (c(abci, abcj, abck))
}


plot_triangoli<-function(trian,vert){
  
  matrice_mesh<-matrix(0,nrow=1, ncol=dim(trian)[1]*3*4,byrow = T)
  
  u=1
  
  for (m in 1:dim(trian)[1]){
    
    matrice_mesh[u]<-vert[trian[m,1],1]
    
    matrice_mesh[u+1]<-vert[trian[m,1],2]
    
    matrice_mesh[u+2]<-vert[trian[m,1],3]
    
    matrice_mesh[u+3]<-1 #trian[m,1]
    
    
    matrice_mesh[u+4]<-vert[trian[m,2],1]
    
    matrice_mesh[u+5]<-vert[trian[m,2],2]
    
    matrice_mesh[u+6]<-vert[trian[m,2],3]
    
    matrice_mesh[u+7]<-1 #trian[m,2]
    
    
    matrice_mesh[u+8]<-vert[trian[m,3],1]
    
    matrice_mesh[u+9]<-vert[trian[m,3],2]
    
    matrice_mesh[u+10]<-vert[trian[m,3],3]
    
    matrice_mesh[u+11]<-1 #trian[m,3]
    
    u=u+12
    
  }
  indici<-seq(1,dim(trian)[1]*3,1)
  
  open3d()
  
  wire3d(tmesh3d(matrice_mesh,indici))
  
  
  
}

plot_triangoli_legenda<-function(trian,vert,list_col){
  
  matrice_mesh<-matrix(0,nrow=1, ncol=dim(trian)[1]*3*4,byrow = T)
  
  u=1
  
  for (m in 1:dim(trian)[1]){
    
    matrice_mesh[u]<-vert[trian[m,1],1]
    
    matrice_mesh[u+1]<-vert[trian[m,1],2]
    
    matrice_mesh[u+2]<-vert[trian[m,1],3]
    
    matrice_mesh[u+3]<-1 #trian[m,1]
    
    
    matrice_mesh[u+4]<-vert[trian[m,2],1]
    
    matrice_mesh[u+5]<-vert[trian[m,2],2]
    
    matrice_mesh[u+6]<-vert[trian[m,2],3]
    
    matrice_mesh[u+7]<-1 #trian[m,2]
    
    
    matrice_mesh[u+8]<-vert[trian[m,3],1]
    
    matrice_mesh[u+9]<-vert[trian[m,3],2]
    
    matrice_mesh[u+10]<-vert[trian[m,3],3]
    
    matrice_mesh[u+11]<-1 #trian[m,3]
    
    u=u+12
    
  }
  indici<-seq(1,dim(trian)[1]*3,1)
  
  open3d()
  
  mesh_blank<-tmesh3d(matrice_mesh,indici)
  
  mesh_blank$material= list_col
  
  wire3d(mesh_blank)
  
  
  
}



# for (j in 1:dim(vertici)[1]){
#
compute_normals<-function(vertici,indici_tr,j){
  
  t1<-c(vertici[indici_tr[j,1],1],vertici[indici_tr[j,1],2],vertici[indici_tr[j,1],3])
  
  t2<-c(vertici[indici_tr[j,2],1],vertici[indici_tr[j,2],2],vertici[indici_tr[j,2],3])
  
  t3<-c(vertici[indici_tr[j,3],1],vertici[indici_tr[j,3],2],vertici[indici_tr[j,3],3])
  
  v1<-t1-t2
  
  v2<-t3-t2
  
  v_n<-crossProd(v1,v2)
  
  facet_area<-0.5*sqrt(v_n[1]^2+v_n[2]^2+v_n[3]^2)
  
  facet_char<-c(v_n,facet_area)
  
 
  
  return(facet_char)
}

compute_normals_test<-function(vertici,indici_tr,j){
  
  t1<-vertici[indici_tr[j,1],]
  
  t2<-vertici[indici_tr[j,2],]
  
  t3<-vertici[indici_tr[j,3],]
  
  v1<-t1-t2
  
  v2<-t3-t2
  
  v_n<-crossProd(v1,v2)
  
  facet_area<-0.5*sqrt(v_n[1]^2+v_n[2]^2+v_n[3]^2)
  
  facet_char<-c(v_n,facet_area)
  
  return(facet_char)
}

get_faces_ind<-function(indici_tr,r){
  #   
  tr1<-indici_tr[r,]
  #   
  l1<-which(indici_tr[,1]==tr1[1] | indici_tr[,2]==tr1[1]|indici_tr[,3]==tr1[1])
  #   
  l2<-which(indici_tr[,1]==tr1[2] | indici_tr[,2]==tr1[2]|indici_tr[,3]==tr1[2])
  #   
  l3<-which(indici_tr[,1]==tr1[3] | indici_tr[,2]==tr1[3]|indici_tr[,3]==tr1[3])
  #   
  vrt_all<-c(l1,l2,l3)
  #   
  vrt_all<-vrt_all[which(vrt_all!=r)]
  #   
  vrt_all<-vrt_all[!duplicated(vrt_all)]
  #   
  return(vrt_all)
  
}
list_to_array<-function(lista,r,max_val){
  
  facce<-lista[[r]]
  
  facce<-facce[which(facce!=r)]
  
  n_fac<-length(facce)
  
  n_zeri<-matrix(0,1,max_val-n_fac)
  
  riga<-c(facce,n_zeri)
  
  return(riga)
  
}

combine<-function(id,list_id,r){
  
  colonne<-cbind(matrix(id[r,1],length(list_id[[r]])-1,1),list_id[[r]][which(list_id[[r]]!=id[r,1])])
  
  return(colonne)
  
}

planes2poles<-function(dip_and_dir){
  
  
  if (dip_and_dir[1] <= 180) {
    
    
    az <- dip_and_dir[1]+180
    inc = 90-dip_and_dir[2]
    
  } else
    
   az <- dip_and_dir[1]-180
   inc <- 90-dip_and_dir[2]
  
  
   return(c(az,inc))
}


poles2planes<-function(dip_and_dir){
  
  
  if (dip_and_dir[1] <= 180) {
    
    
    az <- dip_and_dir[1]+180
    inc = 90-dip_and_dir[2]
    
  } else
    
    az <- dip_and_dir[1]-180
  inc <- 90-dip_and_dir[2]
  
  
  return(c(az,inc))
}

fnormals<-function(strike_,dip_){
  


deg_to_rad = pi/180;

strike <- strike_ * deg_to_rad;
dip    <- dip_    * deg_to_rad;

n1 = -sin(dip)*sin(strike); # north component
n2 = -sin(dip)*cos(strike); #  east component
n3 = -cos(dip);             # vertical component



return (c(round(n1,3),round(n2,3),round(n3,3)))

}

f_labels<-function(lat_long){
  
  if (lat_long[2] < 0) {strike<-360+lat_long[2]} else {strike<-lat_long[2]}
  
  return( paste(round(strike,0),round(lat_long[1],0),sep = '-'))
  
}



xy2dip_plunge<-function(x,y){
  
  if (x>1){x<- 1}
  if (x< -1){x<- -1}
  if (y>1){y<- 1}
  if (y< -1){y<- -1
  } else if (x==0 && y==0) {
    az<-90
  } else if (x==0 && y==1) {
    az<-0
  } else if (x==0 && y==-1) { 
    az<-180
  } else{az<-atan(y/x)*180/pi
  if (y>=0 & x<=0){
    az<-360-(90+az)
  } else if (y<=0 & x<=0){
    az<-270-az
  } else if (y<=0 & x>=0){
    az<-90-az
  }else if (y>=0 & x>=0){
    az<-90-az
  }}
  raggio_c<- sqrt(x^2+y^2)
  ang<-acos(raggio_c/sqrt(2))
  incl<-2*ang*180/pi-90
  return (c(az,incl))
}

dip_plunge2xy<-function (my.az , my.inc ) 
{
  my.az <- my.az * (pi/180) + pi
  my.inc <- my.inc * (pi/180) - pi/2
  my.tq <- sqrt(2) * sin(my.inc/2)
  my.x <- my.tq * sin(my.az)
  my.y <- my.tq * cos(my.az)

  return (c(my.x,my.y))
}



#' findNeighbourFacets
#'
#' This function finds the IDs of each mesh facet
#' It requires number of cores of your pc to use and list of facets indexes
#' corresponding to the "it" property of mesh3d object.
#'
#' @param no_cores number of core to use in search computation
#' @param indici_tri list of facets indexes ("it property of mesh3d object")
#' @return a matrix of indexes of facets neighbours of target face saved on working dir
#' @export
#' @importFrom parallel makeCluster clusterEvalQ clusterExport parLapply stopCluster
#' @importFrom utils read.table write.table
#' @importFrom rgl open3d wire3d axes3d
#' @examples
#' 
#' \dontrun{indici_tri<-t(mesh3d[['it']])
#'  
#' require("parallel")
#' 
#' detectCores()
#' 
#' no_cores <- detectCores() - 4 ### keep free some cores
#' 
#' neighbours<-findNeighbourFacets(no_cores,indici_tri)}

findNeighbourFacets<-function(no_cores,indici_tri){
  
  # Initiate cluster
  cl <- makeCluster(no_cores)
  
  clusterEvalQ(cl, library(JFM))
  
  clusterExport(cl, "indici_tri")
  
  n_facce1<-parLapply(cl,1:dim(indici_tri)[1],function(i) find_triangles_rcpp(indici_tri,i))
  
  stopCluster(cl)
  
  max_n_triang<-max(sapply(1:dim(indici_tri)[1],function(j) length( n_facce1[[j]])))
  
  n_facce<-matrix(0,1:dim(indici_tri)[1],max_n_triang)
  
  neighb<-sapply(1:dim(indici_tri)[1],function(h) list_to_array(n_facce1,h,max_n_triang),simplify = TRUE)
  
  neighb<-t(neighb)
  
  return(neighb)
}


#' compute_facets_normal
#'
#' This function returns a matrix of the three component vector of the normal of each facet.
#' 

#'
#' @param vertici_tr list of facets vertexes coordinates ("vb property of mesh3d object")
#' @param indici_tri list of facets indexes ("it property of mesh3d object")
#' @return matrix of the three component  of the normal vector and area of each face 
#' @export
#' @examples 
#' \dontrun{indici_tri<-t(mesh3d[['it']])
#' 
#' vertici_tr<-t(mesh3d[["vb"]])
#' 
#' normals<-compute_facets_normal(vertici_tr,indici_tri)}
#' 
compute_facets_normal<-function(vertici_tr,indici_tri){
  
 
  normali_recalc<-sapply(1:dim(indici_tri)[1],function(h) compute_normals(vertici_tr,indici_tri,h),simplify = TRUE)
  
  normali_recalc<-t(normali_recalc)
  
  for (n in 1:dim(normali_recalc)[1]) {
    
    if (normali_recalc[n,3] < 0){
      
      normali_recalc[n,3]<-normali_recalc[n,3]*-1
      normali_recalc[n,2]<-normali_recalc[n,2]*-1
      normali_recalc[n,1]<-normali_recalc[n,1]*-1
      
    }
    
  }
  

  
 
  
  return(normali_recalc)
  
}

#' plotrand_col_planes
#'
#' This function returns a 3d plot of mesh where facets of the same plane
#' are of same color.

#'
#' @param mesh_tr an object of type mesh3d
#' @param normal_from_wild the output matrix resulting from wildfire search
#' @return a 3d plot of mesh with facets of the same plane
#' @importFrom randomcoloR randomColor
#' @examples
#' \dontrun{
#' 
#' mesh3d<-build_3d_mesh(path2myXYZRGBtxt,0.5,file_name)
#' 
#' normali_recalc<-Rcpp_wildfire_search(7,normals[,1:3],neighbours)
#' 
#' plotrand_col_planes(mesh3d,normali_recalc)}
#' 
#' @export
#'

plotrand_col_planes<-function(mesh_tr, normal_from_wild){
  
  colnames(normal_from_wild)<- c("Id","Nx","Ny","Nz","cluster","seed")
  
  id_fam<-normal_from_wild[!duplicated(normal_from_wild[,5]),5]
  
  n_colors<-length(id_fam)
  
  list_col<-randomColor(count = n_colors)
  
  legend_colors<-cbind(id_fam,list_col)
  
  legend_colors[which(legend_colors[,1]==0),2]<-"#000000"
  
  tr_col<-merge(normal_from_wild,legend_colors, by.x="cluster",by.y="id_fam",all.x=T)
  
  tr_col_sort<-tr_col[ order(tr_col[,2]), ]
  
  #mesh_tr$material <- list(color=rep(tr_col_sort[,'list_col'], each = 3, alpha = 0.5))
  
  mesh_tr$material <-list(color=tr_col_sort[,'list_col'])
  
  #wire3d(mesh_tr)
  
  shade3d(mesh_tr, meshColor="faces")
  
  axes3d(c('x','y','z'))
  
  return(mesh_tr)
}



#' calculate_joints
#'
#' This function calculates joint orientation with the least square method
#' selecting vertexes of each facet plane
#'
#' @param vertici_tr list of facets vertexes coordinates ("vb property of mesh3d object")
#' @param indici_tri list of facets indexes ("it property of mesh3d object")
#' @param normal_from_wild matrix of data resulting from wildfire search
#' @return a matrix of least square plane for each joint
#' @examples 
#' \dontrun{
#' 
#' mesh3d<-build_3d_mesh(path2myXYZRGBtxt,0.5,file_name)
#' 
#' normali_recalc<-Rcpp_wildfire_search(7,normals[,1:3],neighbours)
#' 
#' joint_list_Cpp<-calculate_joints(mesh3d,normali_recalc)}
#' 
#' @export
#' 
calculate_joints<-function(vertici_tr,indici_tri,normal_from_wild){
  
  #vertici_tr<-t(d_mesh[["vb"]])
  
  #indici_tri<-t(d_mesh[['it']]) 
 
  id_fam<-normal_from_wild[!duplicated(normal_from_wild[,5]),5]
  
  id_fam_no_zero<-id_fam[which(id_fam!=0)]

  it_id_fam<-cbind(indici_tri,normal_from_wild[,5])
  
  joint_list_Cpp<-compute_plane_normal(it_id_fam,vertici_tr,id_fam_no_zero) 

  return(joint_list_Cpp)
  
}

#' calculate_joints_area
#'
#' This function calculates the area of each cluster of facets 
#' belonging to the same plane
#'
#' 
#' @param normal_from_wild matrix of data resulting from wildfire search
#' @return a list of the area of each plane
#' @examples 
#' 
#' \dontrun{
#' 
#' normali_recalc<-Rcpp_wildfire_search(7,normals[,1:3],neighbours)
#' 
#' calculate_joints(mesh3d,normali_recalc)
#' 
#' }
#' 
#' 
#' @export

calculate_joints_area<-function(normal_from_wild){
  
  id_fam<-normal_from_wild[!duplicated(normal_from_wild[,5]),5]
  
  id_fam_no_zero<-id_fam[which(id_fam!=0)]
  
  tr_area<-normal_from_wild[,4:5]
  
  val_area<-compute_plane_area_rcpp(tr_area,id_fam_no_zero)
  
  return(val_area)
  
}





