

 #This is an example of workflow script in test folder
 
path2myXYZRGBtxt<-system.file("test", "test.txt", package = "JFM")
 
file_name<- "test"
 
mesh3d<-build_3d_mesh(path2myXYZRGBtxt,0.5,paste0(tempdir(),"/",file_name))
 
vertici_tr<-t(mesh3d[["vb"]])
 
indici_tri<-t(mesh3d[['it']])
 
neighbours<-find_neighbours_rcpp(indici_tri)
 
### find neighbours of each triangle facet using a Rcpp function 

neighbours<-find_neighbours_rcpp(indici_tri)

### or a hybrid R-Rcpp function

#### core number to dedicate to computational processes; check with
#### detectCores() function how many cores your pc owns

require("parallel")

detectCores()

### use only 2 cores

no_cores <- 2

neighbours<-findNeighbourFacets(no_cores,indici_tri)

### compute normal of each triangle facet

normals<-compute_facets_normal(vertici_tr,indici_tri)

### apply wildfire search 

normali_recalc<-Rcpp_wildfire_search(7,normals[,1:3],neighbours)

### plot search result and if not satisfied repeat search increasing/decreasing tolerance angle

plotrand_col_planes(mesh3d,normali_recalc)

### calculate least square plane for each group of facets

joint_list_Cpp<-calculate_joints(vertici_tr,indici_tri,normali_recalc)

### calculate area for each group of facets

val_area<-calculate_joints_area(normali_recalc)

### extract pole maxima setting your minimum contour density
### and area to filter data, plot and save them

poles_maxima<-plot_joint_poles(normali_recalc,joint_list_Cpp,
                  val_area,paste0(tempdir(),"/",file_name),0.3,1)

##### plot and save great circle of pole maxima

azi_dip_maxima<-plot_joint_great_circles(poles_maxima, paste0(tempdir(),"/",file_name))

### plot colors of pole maxima onto mesh facets

plot_maxima2mesh(mesh3d,azi_dip_maxima,normali_recalc,10)

remove()
