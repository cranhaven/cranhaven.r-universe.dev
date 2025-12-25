#' plot_joint_great_circles
#'
#' This function loads joint maxima poles, convert them to great circles and 
#' plot them on Schmidt stereogram.
#' Data are also saved in working folder.
#' 
#'
#' @param giac_max Joint maxima poles returned from plot_joint_poles function
#' @param file_name Name of the output data file
#' @return A plot with great circles of joint maxima saved in working dir
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' poles_maxima<-plot_joint_poles(normali_recalc,joint_list_Cpp,val_area,file_name,max_pole,min_area)
#' 
#' azi_dip_maxima<-plot_joint_great_circles(poles_maxima, file_name)}
#' 
#' @export
#' 
plot_joint_great_circles<-function(giac_max, file_name){
  
  graphics.off()
  
  StereoPlot(my.title = file_name)
  
  StereoWeb()
  
  StereoCirc()
  
  n_colors<-dim(giac_max)[1]
  
  list_col_fam<-rainbow(n_colors)
  
  planes_max<-t(sapply(1:dim(giac_max)[1], function(x) poles2planes(c(giac_max[x,1],giac_max[x,2]))))
  
  write.table(cbind(planes_max[,1],planes_max[,2]), file = paste0(file_name,"_max_gc","m2.txt"), quote=FALSE, sep = " ", col.names = T, row.names = F,qmethod = "double")
  
  
  lapply(1:dim(giac_max)[1], function(x) StereoPlane(planes_max[x,1]-90,planes_max[x,2],my.color = list_col_fam[x]))
  
  lapply(1:dim(giac_max)[1], function(x) StereoPoint(planes_max[x,1],planes_max[x,2],my.color = list_col_fam[x],my.size=.5,my.label = paste(round(planes_max[x,1],0),round(planes_max[x,2],0),sep="/")))
  
  return(planes_max)
  
}
