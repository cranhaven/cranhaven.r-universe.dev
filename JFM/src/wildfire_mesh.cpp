// [[Rcpp::depends(RcppArmadillo)]]

//#include <RInside.h>
#include <RcppArmadillo.h>
#include <math.h> 
//#include <RcppEigen.h>
using namespace Rcpp;
using namespace arma;


//' @name rcpparma_dotproduct
//' @title returns the inner product of ab and ac 
//'
//' @param ab        a 3D numeric vector  
//' @param ac        a 3D numeric vector 
//' @return  the dot product of \code{ab} and \code{ac}
//' @examples
//' a1<-c(1,2,3)
//' a2<-c(3,4,5)
//' rcpparma_dotproduct(a1,a2)
//' @export


// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]

double rcpparma_dotproduct (std::vector<double> ab, std::vector<double> ac ) {
  
  double dot_num = (ab[0] * ac[0] + ab[1] * ac[1] + ab[2] * ac[2]);
  
  double dot_den =(sqrt(pow(ab[0],2.0) + pow(ab[1],2.0) + pow(ab[2],2.0)) * sqrt(pow(ac[0],2.0) + pow(ac[1],2.0) + pow(ac[2],2.0)));
  
  double dot =dot_num/dot_den;
  
  if (dot > 1) dot= 1;
  
  if (dot< -1) dot= -1;
  
  return dot;
}

//' @name rcpp_crossProd
//' @title returns the outer product of ab and ac 
//'
//' @param ab        a 3D numeric vector  
//' @param ac        a 3D numeric vector
//' @return  the outer product of \code{ab} and \code{ac}
//' @examples
//' a1<-c(1,2,3)
//' a2<-c(3,4,5)
//' rcpp_crossProd(a1,a2)
//' @export
// [[Rcpp::export]]


std::vector<double> rcpp_crossProd (std::vector<double> ab ,  std::vector<double> ac ){
  double abci = ab[1] * ac[2] - ac[1] * ab[2];
  double abcj = ac[0] * ab[2] - ab[0] * ac[2];
  double abck = ab[0] * ac[1] - ac[0] * ab[1];
  
  std::vector<double>cr_prod(3);
  
  cr_prod[0] = abci;
  cr_prod[1] = abcj;
  cr_prod[2] = abck;
  
  return cr_prod;
}

//' @name Rcpp_wildfire_search
//' @title returns a matrix with the 3 components of each face normal vector; the 4th column is the ID of the plane each facet belongs to
//' the 5th colum the area of each facet
//' @param tol_ang        the maximum angle between facets normal belonging to the same plane
//' @param list_of_normals        the matrix of the components of each facet normal vector
//' @param list_neighbours        the matrix of facets ID neighbours of each target facet
//' @return the IDs of same joint facets given a \code{tol_angle} between facets normal and 3Dmesh \code{list_of_normals} and \code{list_neighbours} 
//' @examples \dontrun{neighbours<-find_neighbours_rcpp(indici_tri)
//' normals<-compute_facets_normal(vertici_tr,indici_tri)
//' tol_ang<-7
//' normali_recalc<-Rcpp_wildfire_search(tol_ang,normals[,1:3],neighbours)}
//' @export
// [[Rcpp::export]]

arma::mat Rcpp_wildfire_search(double tol_ang, arma::mat list_of_normals, arma::mat list_neighbours){
  
  int nrow_norm = list_of_normals.n_rows;
  int ncol_norm = list_of_normals.n_cols;
  
  int nrow_neigh = list_neighbours.n_rows;
  int ncol_neigh = list_neighbours.n_cols;
  
  arma::mat new_norm(nrow_norm,ncol_norm+3);
  
  for (int i=0; i < nrow_norm; i++) {
    
    new_norm(i,0)=i;
    new_norm(i,1)=list_of_normals(i,0);
    new_norm(i,2)=list_of_normals(i,1);
    new_norm(i,3)=list_of_normals(i,2);
    new_norm(i,4)=0;
    new_norm(i,5)=0;
    
  }
 
  
  arma::mat new_neigh(nrow_neigh,ncol_neigh);
  
  for (int i=0; i < nrow_neigh; i++) {
    
    for (int j=0; j < ncol_neigh; j++){
      
      new_neigh(i,j)=list_neighbours(i,j)-1;         

    }
  }

  
  
  
  int k=0;
  
  unsigned int r=0;
  
  double indice_perc = 0;
  
  double perc = 0;
  
  
  
  //nrow_norm
  
  while (r < nrow_norm) {

    
    std::vector<int>neigh_row;
    
    for (int i=0; i < ncol_neigh; i++) {  
      
      if (new_neigh(r,i)>-1) neigh_row.push_back(new_neigh(r,i));
      
    }
    
    if ((neigh_row.size()>1) && (new_norm(r,4)==0))  { 
      

      k=k+1;

      std::vector<int>seeds;
      
      std::vector<double>norm_centrale(3);
      
      for (int i=1;i<4; i++) norm_centrale[i-1]=new_norm(r,i);
      

      for ( unsigned int s=0; s<neigh_row.size(); s++){
        
        std::vector<double>norm_confronto(3);
        
        for (int j=1;j<4; j++) norm_confronto[j-1]=new_norm(neigh_row[s],j);

        
        double cos_alfa = rcpparma_dotproduct(norm_centrale,norm_confronto);
        

        if (R_IsNA(cos_alfa)) cos_alfa=0;
        
        if (cos_alfa > 1) cos_alfa= 1; 
        
        if (cos_alfa < -1) cos_alfa=-1;
        
        double angle = acos(cos_alfa)*180/3.1415;
        

        if((( angle <= tol_ang) || (angle >= 180-tol_ang)) && (R_IsNA(cos_alfa)==0)) {
          
          
          if (new_norm(neigh_row[s],5)==0) {
            
            seeds.push_back(neigh_row[s]);
            
            new_norm(neigh_row[s],5)=1;  
            
          }
          
          if ((new_norm(r,4)==0) && (new_norm(neigh_row[s],4)==0)) {
            
            new_norm(r,4)=k;
            
            new_norm(neigh_row[s],4)=k;
            
            
          } else if ((new_norm(r,4)!=0) && (new_norm(neigh_row[s],4)==0)) {
            
            new_norm(neigh_row[s],4)=new_norm(r,4);
            
          }  
          
        }
        
      }
      
      while (seeds.size()>0){
        
        std::vector<int>neigh_row_seeds;
        
        for (int i=0; i < ncol_neigh; i++) {
          
          if (new_neigh(seeds[0],i)>-1) neigh_row_seeds.push_back(new_neigh(seeds[0],i));
          
        }
        

        int seed_centrale=seeds[0]; 
        
        std::vector<int>seeds_confronto=neigh_row_seeds;
        
        std::vector<double>norm_seed_centrale(3);
        
        for (int i=1;i<4; i++) norm_seed_centrale[i-1] = new_norm(seed_centrale,i);
        

        
        for(unsigned int n=0; n < seeds_confronto.size(); n++) { 
          
          std::vector<double>norm_seeds_confronto(3);
          
          for (int j=1;j<4; j++) norm_seeds_confronto[j-1] = new_norm(seeds_confronto[n],j);
          
          
          double cos_alfa = rcpparma_dotproduct(norm_seed_centrale,norm_seeds_confronto);
          
          if (R_IsNA(cos_alfa)) cos_alfa=0;
          
          if (cos_alfa > 1) cos_alfa= 1; 
          
          if (cos_alfa < -1) cos_alfa=-1;
          
          double angle1=acos(cos_alfa)*180/3.1415;
          
          if(((angle1 <= tol_ang) || (angle1 >= 180-tol_ang)) && (R_IsNA(cos_alfa)==0)) {
            
            if ((new_norm(seeds_confronto[n],4)==0) && (new_norm(seed_centrale,4)!=0)) {
              
              new_norm(seeds_confronto[n],4)= new_norm(seed_centrale,4);}
            
            if (new_norm(seeds_confronto[n],5)==0) {
              
              new_norm(seeds_confronto[n],5)=1;
              
              seeds.push_back(seeds_confronto[n]);
                 
              }
            
          }
          
        }
        
        seeds.erase(seeds.begin());

        
      }
    }
    r=r+1;

    
    perc = double(r)/double(nrow_norm) * 100;
    

    if (perc >= indice_perc){

      Rcout << indice_perc << "%.. " <<std::endl ;

      indice_perc = indice_perc + 10;

      
    }

  }
  for (unsigned int i=0; i< nrow_norm;i++){
    
    float area=0.5*sqrt(pow(new_norm(i,1),2.0) + pow(new_norm(i,2),2.0) + pow(new_norm(i,3),2.0));
    
    new_norm(i,5)=area;
  }
 
 
  
  return new_norm;
}



//' @name compute_triangle_area_rcpp
//' @title returns the area of a mesh facet
//'
//' @param tr_vertex_coords        A 3x3 matrix of the coordinates of facet vertexes

// [[Rcpp::export]]

std::vector<double> compute_triangle_area_rcpp( arma::mat tr_vertex_coords){
  
  int nrow_coord = tr_vertex_coords.n_rows;

  double indice_perc = 0;
  
  double perc = 0;

  std::vector<double> v1(3);
    
  std::vector<double> v2(3);
    
  std::vector<double> area_list(nrow_coord);
    
  for (unsigned int i=0; i < nrow_coord; i++) {
    
    v1[0]= tr_vertex_coords(i,3)-tr_vertex_coords(i,0);
    v1[1]= tr_vertex_coords(i,4)-tr_vertex_coords(i,1);
    v1[2]= tr_vertex_coords(i,5)-tr_vertex_coords(i,2);
    
    v2[0]= tr_vertex_coords(i,6)-tr_vertex_coords(i,3);
    v2[1]= tr_vertex_coords(i,7)-tr_vertex_coords(i,4);
    v2[2]= tr_vertex_coords(i,8)-tr_vertex_coords(i,5);
    
    std::vector<double> v_area = rcpp_crossProd(v1,v2);
    
    double area=0.5*sqrt(pow(v_area[0],2.0) + pow(v_area[1],2.0) + pow(v_area[2],2.0));
    
    area_list[i]=area;
    
    perc = double(i)/double(nrow_coord) * 100;
    
    if (perc >= indice_perc){

      Rcout << indice_perc << "%.. " <<std::endl ;
 
      indice_perc = indice_perc + 10;
    }
    
  } 
  return area_list;
}

//' @name compute_plane_area_rcpp
//' @title returns the sum of the area of facets belonging to the same plane
//'
//' @param tr_area        a matrix with the first column facet area and second column the ID of plane it belows
//' @param id_fam_no_zero        the list of planes ID
//' @return the sum of the area of facets belonging to the same plane given \code{tr_area} and \code{id_fam_no_zero}

//'
//' @export
// [[Rcpp::export]]

std::vector<double> compute_plane_area_rcpp( arma::mat tr_area, std::vector<int> id_fam_no_zero){
  
  int nrow_coord = tr_area.n_rows;
  
  double indice_perc = 0;
  
  double perc = 0;
  
  
  
  std::vector<double>plane_area_list;
    
  for (unsigned int k=0; k<id_fam_no_zero.size(); k++){
    
    double plane_area=0;
    
    for (unsigned int i=0; i < nrow_coord; i++) {
    
      if (tr_area(i,1)==id_fam_no_zero[k]){
      
        plane_area=plane_area+tr_area(i,0);
      
        }
    
    }
    plane_area_list.push_back(plane_area);
    
    perc = double(k)/double(id_fam_no_zero.size()) * 100;
    
    if (perc >= indice_perc){

      Rcout << indice_perc << "%.. " <<std::endl ;
      
      indice_perc = indice_perc + 10;
    }
  }
 return plane_area_list;
}


//' @name least_square_plane_rcpp
//' @title returns the coefficients of the least square plane and the relative mean square error
//'
//' @param PointsXYZ        matrix of coordinates of point
//' @return returns the coefficients of the least square plane and the relative mean square error of a set of 3d points \code{PointsXYZ} 
//' @examples
//' list_xyz<-matrix(data = c(-10.0, -10.0, -15.0 ,10.0, -10.0,
//' -5.0, -10.0, 10.0, 5.0, 10.0, 10.0 ,15.0),
//' nrow = 4,ncol = 3, byrow = TRUE)
//' least_square_plane_rcpp(list_xyz)
//' @export
// [[Rcpp::export]]

NumericVector least_square_plane_rcpp (NumericMatrix PointsXYZ) {
  
  int nrow_Points = PointsXYZ.rows()  ;//n_rows
  //int ncol_Points = PointsXYZ.n_cols;
  double x_centroid=0.0;
  double y_centroid=0.0;
  double z_centroid=0.0;
  //double x_centroidd;
  //double y_centroidd;
  //double z_centroidd;
  //double sum_x_diff;
  //double sum_y_diff;
  //double sum_z_diff;
  NumericMatrix differences(nrow_Points,3);
  std::vector<double> cov_component(6);
  arma::mat covariance_matrix(3,3);
  arma::vec eigen_values(3);
  arma::mat eigen_vectors(3,3);
  arma::vec plane_normals(3);
  double d=0.0;
  double distances_quad=0.0;
  
  for ( int i = 0; i < nrow_Points; i++) {
    
  
    
    
    x_centroid += PointsXYZ(i,0);
    y_centroid += PointsXYZ(i,1);
    z_centroid += PointsXYZ(i,2);
  }
  
  x_centroid=x_centroid/double(nrow_Points);
  y_centroid=y_centroid/double(nrow_Points);
  z_centroid=z_centroid/double(nrow_Points);
  
  for ( int i = 0; i < nrow_Points; i++) {
    differences(i,0) = x_centroid-PointsXYZ(i,0);
    differences(i,1) = y_centroid-PointsXYZ(i,1);
    differences(i,2) = z_centroid-PointsXYZ(i,2);
  }
  
  for ( int i = 0; i < nrow_Points; i++) {
    
    cov_component[0]+=differences(i,0)*differences(i,0);
    cov_component[1]+=differences(i,0)*differences(i,1); 
    cov_component[2]+=differences(i,0)*differences(i,2); 
    cov_component[3]+=differences(i,1)*differences(i,1);   
    cov_component[4]+=differences(i,1)*differences(i,2);
    cov_component[5]+=differences(i,2)*differences(i,2);
    
  }
  
  
  
  covariance_matrix(0,0)=cov_component[0];
  covariance_matrix(0,1)=cov_component[1];
  covariance_matrix(0,2)=cov_component[2];
  covariance_matrix(1,0)=cov_component[1];
  covariance_matrix(1,1)=cov_component[3];
  covariance_matrix(1,2)=cov_component[4];
  covariance_matrix(2,0)=cov_component[2];
  covariance_matrix(2,1)=cov_component[4];
  covariance_matrix(2,2)=cov_component[5];
  
  //for ( int i = 0; i < 3; i++) {
    
  //    for ( int j = 0; j < 3; j++) {
      
  //       Rcout<<covariance_matrix(i,j)<<std::endl;
      
  //      }
  //     }

  eig_sym(eigen_values,eigen_vectors,covariance_matrix);
  
  NumericVector::iterator it = std::min_element(eigen_values.begin(), eigen_values.end());
  
  int min_eigen=it - eigen_values.begin();//Rcpp::which_min(eigen_values);
  
  plane_normals[0]=eigen_vectors(0,min_eigen);
  plane_normals[1]=eigen_vectors(1,min_eigen);
  plane_normals[2]=eigen_vectors(2,min_eigen);
  
  d=-(x_centroid*plane_normals[0]+y_centroid*plane_normals[1]+z_centroid*plane_normals[2]);
  
  
  for (unsigned int i = 0; i < nrow_Points; i++) {
    
   distances_quad +=  pow(((differences(i,0)*plane_normals[0]+differences(i,1)*plane_normals[1]+differences(i,2)*plane_normals[2])/sqrt(pow(plane_normals[0],2.0)+pow(plane_normals[1],2.0)+pow(plane_normals[2],2.0))),2.0);
  }
  
  
  double err=sqrt(distances_quad/double(nrow_Points));
  
  NumericVector plane(5);
  
  plane[0]=  plane_normals[0];
  plane[1]=  plane_normals[1];
  plane[2]=  plane_normals[2];
  plane[3]=  d;
  plane[4]=  err;
  
  return plane;
  
}


//' @name find_triangles_rcpp
//' @title returns the row indexes of the neighbour facets of a target facet (nested in findNeighbourFacets R function)
//'
//' @param indici_tr matrix of facets ID the "it" property of a mesh3D
//' @param r index of the row of the target facet
//' @return returns the row indexes of the neighbour facets of the facet at \code{r} row of \code{indici_tr} facet indexes matrix
//' @examples 
//' indici_tri<-matrix(data = c(1, 2, 3 ,5, 6,
//' 3, 2, 3, 5,7, 8 ,1),
//' nrow = 4,ncol = 3, byrow = TRUE)
//' row_index<-1
//' find_triangles_rcpp (indici_tri,row_index)
//' @export
// [[Rcpp::export]]

std::vector<int> find_triangles_rcpp (NumericMatrix indici_tr, int r) {
  
  int num_rows = indici_tr.nrow();
  std::vector<int>l1;
  
  int tr1=indici_tr(r-1,0);
  int tr2=indici_tr(r-1,1); 
  int tr3=indici_tr(r-1,2);
  
  for (int i = 0; i < num_rows; i++) {
    if ((indici_tr(i,0)==tr1) || (indici_tr(i,1)==tr1)||(indici_tr(i,2)==tr1)) l1.push_back(i+1);
  } 
  for (int i = 0; i < num_rows; i++) {
    if ((indici_tr(i,0)==tr2) || (indici_tr(i,1)==tr2)||(indici_tr(i,2)==tr2)) l1.push_back(i+1);
  }  
  for (int i = 0; i < num_rows; i++) {
    if ((indici_tr(i,0)==tr3) || (indici_tr(i,1)==tr3)||(indici_tr(i,2)==tr3)) l1.push_back(i+1);
  } 
  sort(l1.begin(), l1.end());
  
  std::vector<int> temp(l1.size()); 
  
  // Start traversing elements 
  int j = 0; 
  for (unsigned int i=0; i<l1.size()-1; i++) 
    
    // If current element is not equal 
    // to next element then store that 
    // current element 
    if (l1[i] != l1[i+1]) 
      temp[j++] = l1[i]; 
    
    // Store the last element as whether 
    // it is unique or repeated, it hasn't 
    // stored previously 
    temp[j++] = l1[l1.size()-1]; 
    
    // Modify original array 
    l1.clear();
    
    for (int i=0; i<j; i++) 
      if (temp[i]!=r) l1.push_back(temp[i]); 
      
      return l1;
}

//' @name find_neighbours_rcpp
//' @title This function finds the  rows IDs of neighbours of each mesh facet. It requires a list of facets indexes corresponding to the "it" property of mesh3d object
//'
//' @param indici_tr        matrix of facets ID the "it" property of a mesh3D
//' @return  this function returns the  rows IDs of neighbours of each mesh facet given a list of facets indexes \code{indici_tri}
//' @examples
//' indici_tri<-matrix(data = c(1, 2, 3 ,5, 6,
//' 3, 2, 3, 5,7, 8 ,1),
//' nrow = 4,ncol = 3, byrow = TRUE)  
//' find_neighbours_rcpp(indici_tri)
//' @export
// [[Rcpp::export]]

NumericMatrix find_neighbours_rcpp (NumericMatrix indici_tr) {
  
  int num_rows = indici_tr.nrow();
  std::vector<int>neighbours_v;
  double indice_perc = 0;
  double perc = 0;
  for (unsigned int r = 0; r< num_rows; r++) {
  
    std::vector<int>l1;
    
    int tr1=indici_tr(r-1,0);
    int tr2=indici_tr(r-1,1); 
    int tr3=indici_tr(r-1,2);
    
    for (int i = 0; i < num_rows; i++) {
      if ((indici_tr(i,0)==tr1) || (indici_tr(i,1)==tr1)||(indici_tr(i,2)==tr1)) l1.push_back(i+1);
    } 
    for (int i = 0; i < num_rows; i++) {
      if ((indici_tr(i,0)==tr2) || (indici_tr(i,1)==tr2)||(indici_tr(i,2)==tr2)) l1.push_back(i+1);
    }  
    for (int i = 0; i < num_rows; i++) {
      if ((indici_tr(i,0)==tr3) || (indici_tr(i,1)==tr3)||(indici_tr(i,2)==tr3)) l1.push_back(i+1);
    } 
    sort(l1.begin(), l1.end());
    
    std::vector<int> temp(l1.size()); 
    
    // Start traversing elements 
    int j = 0; 
    for (unsigned int i=0; i<l1.size()-1; i++) 
      
      // If current element is not equal 
      // to next element then store that 
      // current element 
      if (l1[i] != l1[i+1]) 
        temp[j++] = l1[i]; 
      
      // Store the last element as whether 
      // it is unique or repeated, it hasn't 
      // stored previously 
      temp[j++] = l1[l1.size()-1]; 
      
      // Modify original array 
      l1.clear();
      
      for (int i=0; i<j; i++) 
        if (temp[i]!=r) neighbours_v.push_back(temp[i]);
        
      neighbours_v.push_back(-1);
        //return l1;
        
      perc = double(r)/double(num_rows) * 100;
      
      if (perc >= indice_perc){

        Rcout << indice_perc << "%.. " <<std::endl ;
          
        indice_perc = indice_perc + 10;
      }      
        
  }
  
  int max_ind=0;
  int max_temp_ind=0;
  
  for (unsigned int i=0; i<neighbours_v.size(); i++){
    
    if (neighbours_v[i]!=-1) {max_temp_ind=max_temp_ind+1;
      
      } else { if (max_temp_ind>max_ind){
      
      max_ind=max_temp_ind;
        
      }
      
      max_temp_ind=0;
      
    }
      
  }
  
   
    
  NumericMatrix neighbours(num_rows,max_ind);
  
  int neigh_row=0;
  int neigh_col=0;
   
   for (unsigned int i=0; i<neighbours_v.size(); i++){
     
     if (neighbours_v[i]!=-1) {
       
       neighbours(neigh_row,neigh_col)=neighbours_v[i];
       
       neigh_col++;
       
     }else {neigh_row++;
       
       neigh_col=0;
       
       }

    }
  return neighbours;
}
//' @name compute_plane_normal
//' @title returns the least square plane from the vertexes of facets of the same plane (nested in calculate_joints function)
//'
//' @param it_id_plane_points        the "it"property of mesh object binded with ID column of widfire search 
//' @param id_fam_no_zero            the list of planes ID
//' @param vb_facets                 the vb property of mesh object (vertexes coordinates)
//' @return returns the least square plane from the vertexes of facets of the same plane given \code{it_id_plane_points}
//'  the list of planes ID \code{id_fam_no_zero}, the matrix of vertexes coordinates \code{vb_facets}
//' @example .\inst\test\example.R
//' @export
// [[Rcpp::export]]

arma::mat compute_plane_normal( arma::mat it_id_plane_points,arma::mat vb_facets, std::vector<int> id_fam_no_zero){
  
  int nrow_it = it_id_plane_points.n_rows;
  int ncol_it = it_id_plane_points.n_cols;
  
  arma::mat new_it(nrow_it,ncol_it);
  NumericVector plane_normal(5);
  


  
  for (int i=0; i < nrow_it; i++) {
    
    new_it(i,0)=it_id_plane_points(i,0)-1;
    new_it(i,1)=it_id_plane_points(i,1)-1;
    new_it(i,2)=it_id_plane_points(i,2)-1;
    new_it(i,3)=it_id_plane_points(i,3);
      
    }
  
  double indice_perc = 0;
  
  double perc = 0;
 
  arma::mat plane_norm_list(id_fam_no_zero.size(),5);
  
  
  
  for (unsigned int k=0; k<id_fam_no_zero.size(); k++){
    
    std::vector<int>it;
    

    for (unsigned int i=0; i < nrow_it; i++) {
      
      if (new_it(i,3)==id_fam_no_zero[k]){
        
        it.push_back(new_it(i,0));
        it.push_back(new_it(i,1));
        it.push_back(new_it(i,2));
        
      }
    }
    

 
      std::vector<int> bookmark(it.size(), 0);
      
      std::vector<int> it_uniq;
      
      int m=0;
     
      for (unsigned int l=0; l < it.size()-1; l++) {
      
        if (bookmark[l]!=1) {
          
          it_uniq.push_back(it[l]);
          
          bookmark[l]=1;
          
          m=l+1;
          
        }
        
        for (unsigned int n=m;n< it.size(); n++){
          
          if (it[n]==it[l]){
            
            bookmark[n]=1;
            
            }
          
          }
          
      }
      
      if(bookmark[it.size()-1]!=1) {
        
        it_uniq.push_back(it[it.size()-1]);}
    
    //it.erase();
    
    if(it_uniq.size()>2){
    
      NumericMatrix xyz(it_uniq.size(),3);
      
      for (unsigned int j=0; j < it_uniq.size(); j++) {
          
        xyz(j,0)=vb_facets(it_uniq[j],0);
        xyz(j,1)=vb_facets(it_uniq[j],1);
        xyz(j,2)=vb_facets(it_uniq[j],2);
        
       }
      
      
      
      plane_normal=least_square_plane_rcpp(xyz);
      
      //std::cout << k <<std::endl ;
      
      //Rcout << "ok" <<std::endl ;
      
      plane_norm_list(k,0)=plane_normal[0];
      plane_norm_list(k,1)=plane_normal[1];
      plane_norm_list(k,2)=plane_normal[2];
      plane_norm_list(k,3)=plane_normal[3];
      plane_norm_list(k,4)=plane_normal[4];
      
      perc = double(k)/double(id_fam_no_zero.size()) * 100;
      
      if (perc >= indice_perc){

        Rcout << indice_perc << "%.. " <<std::endl ;
        
        indice_perc = indice_perc + 10;
      }
      }else{
        warning("no punti");
    }
  }
  
  return plane_norm_list;
}

// [[Rcpp::export]]

NumericMatrix test_rcpp( arma::mat it_id_plane_points,arma::mat vb_facets, std::vector<int> id_fam_no_zero){
  
  int nrow_it = it_id_plane_points.n_rows;
  int ncol_it = it_id_plane_points.n_cols;
  
  arma::mat new_it(nrow_it,ncol_it);
  NumericMatrix xyz_1;
  NumericVector plane_normal(5);
  std::vector<double> plane (5);
  
  for (int i=0; i < nrow_it; i++) {
    
    new_it(i,0)=it_id_plane_points(i,0)-1;
    new_it(i,1)=it_id_plane_points(i,1)-1;
    new_it(i,2)=it_id_plane_points(i,2)-1;
    new_it(i,3)=it_id_plane_points(i,3);
    
  }
  
  double indice_perc = 0;
  
  double perc = 0;
  
  arma::mat plane_norm_list(id_fam_no_zero.size(),5);
  
  
  
  for (unsigned int k=0; k<id_fam_no_zero.size(); k++){
    
    std::vector<int>it;
    
    
    for (unsigned int i=0; i < nrow_it; i++) {
      
      if (new_it(i,3)==id_fam_no_zero[k]){
        
        it.push_back(new_it(i,0));
        it.push_back(new_it(i,1));
        it.push_back(new_it(i,2));
        
      }
    }
    
    
    
    std::vector<int> bookmark(it.size(), 0);
    
    std::vector<int> it_uniq;
    
    int m=0;
    
    for (unsigned int l=0; l < it.size()-1; l++) {
      
      if (bookmark[l]!=1) {
        
        it_uniq.push_back(it[l]);
        
        bookmark[l]=1;
        
        m=l+1;
        
      }
      
      for (unsigned int n=m;n< it.size(); n++){
        
        if (it[n]==it[l]){
          
          bookmark[n]=1;
          
        }
        
      }
      
    }
    
    if(bookmark[it.size()-1]!=1) {
      
      it_uniq.push_back(it[it.size()-1]);}
    
    //it.erase();
    
    if(it_uniq.size()>2){
      
      NumericMatrix xyz(it_uniq.size(),3);
      
      for (unsigned int j=0; j < it_uniq.size(); j++) {
        
        xyz(j,0)=vb_facets(it_uniq[j],0);
        xyz(j,1)=vb_facets(it_uniq[j],1);
        xyz(j,2)=vb_facets(it_uniq[j],2);
        
      }
      xyz_1= xyz;
      
      plane_normal=least_square_plane_rcpp(xyz);
      
      perc = double(k)/double(id_fam_no_zero.size()) * 100;
      
      if (perc >= indice_perc){
        
        Rcout << indice_perc << "%.. " <<std::endl ;
        
        indice_perc = indice_perc + 10;
      }
    else{
      warning("no punti");
    }
  }
  }
  return xyz_1;
}

/*** R


*/
