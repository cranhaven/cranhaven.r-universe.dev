#' sphere.integration
#'
#' Calculates the integral of a function f on the unit sphere
#'
#' @param f Function to be integrated
#' @param mesh Triangular mesh to be used. More details can be found in the description of sphere.hdr function
#' @param deg Degree of the quadrature rules for triangles. More details can be found in the description of sphere.hdr function
sphere.integration<-function(f,mesh=40,deg=3){
	if(!is.function(f)){
		stop("argument 'f' must be a function")
	}else{
		polar2rect<-function (r, phi){
    			m <- length(r)
    			if (!is.matrix(phi)) {phi <- as.matrix(phi, ncol = 1)}
   		      stopifnot(m == ncol(phi))
                  n <- nrow(phi) + 1
                  x <- matrix(0, nrow = n, ncol = m)
   			for (j in 1:m) {
        			col.cos <- cos(phi[, j])
        			col.sin <- sin(phi[, j])
        			s <- c(col.cos[1], rep(col.sin[1], n - 1))
       	            if (n > 2) {
            			for (k in 2:(n - 1)) {
                				s[k] <- s[k] * col.cos[k]
               			      s[(k + 1):n] <- s[(k + 1):n] * col.sin[k]
            			}
        			}
                        x[, j] <- r[j] * s
                   }
                  return(x)
		   }
  		g<-function(x){return(f(t(polar2rect(r=1,phi=x))))}
	}
	if(!is.numeric(mesh)){
		stop("argument 'mesh' must be numeric")
	}else if (!any(mesh==c(10,20,40))){
		stop("argument 'mesh' is not valid")
	}else if(mesh==40){
 		points<-points40
            cells<-cells40
 	}else if(mesh==20){
 		points<-points20
            cells<-cells20
	}else{
		points<-points10
            cells<-cells10
	}

 # Quadrature rules for triangles
 # degree : degree of the quadrature rules
 # p: points of quadrature in the triangle with respect its barycentric coordinates
 # w: weigths of the quadrature formula

  quadrature<-function(degree){
    if((degree == 0)|(degree == 1)){
        # Scheme from Zienkiewicz and Taylor, 1 point, degree of precision 1
        x = c(1.0/3.0,1.0/3.0)
        w = c(0.5)
    }else if(degree == 2){
        # Scheme from Strang and Fix, 3 points, degree of precision 2
        aux = c(1.0/6.0, 1.0/6.0,1.0/6.0, 2.0/3.0,2.0/3.0, 1.0/6.0)
        x = matrix(aux,nrow=3,ncol=2,byrow=TRUE)
        w = rep(1.0/6.0,times=3)
    }else if(degree == 3){
        # Scheme from Strang and Fix, 6 points, degree of precision 3
        aux = c(0.659027622374092, 0.231933368553031,
                   0.659027622374092, 0.109039009072877,
                   0.231933368553031, 0.659027622374092,
                   0.231933368553031, 0.109039009072877,
                   0.109039009072877, 0.659027622374092,
                   0.109039009072877, 0.231933368553031)
        x = matrix(aux,nrow=6,ncol=2,byrow=TRUE)
        w = rep(1.0/12.0,times=6)
     }else if(degree == 4){
        # Scheme from Strang and Fix, 6 points, degree of precision 4
        aux = c(0.816847572980459, 0.091576213509771,
                   0.091576213509771, 0.816847572980459,
                   0.091576213509771, 0.091576213509771,
                   0.108103018168070, 0.445948490915965,
                   0.445948490915965, 0.108103018168070,
                   0.445948490915965, 0.445948490915965)
        x = matrix(aux,nrow=6,ncol=2,byrow=TRUE)
        w = numeric(6)
        w[1:3] = 0.109951743655322
        w[4:6] = 0.223381589678011
        w = w/2.0
      }else if(degree == 5){
        # Scheme from Strang and Fix, 7 points, degree of precision 5
        aux = c(0.33333333333333333, 0.33333333333333333,
                   0.79742698535308720, 0.10128650732345633,
                   0.10128650732345633, 0.79742698535308720,
                   0.10128650732345633, 0.10128650732345633,
                   0.05971587178976981, 0.47014206410511505,
                   0.47014206410511505, 0.05971587178976981,
                   0.47014206410511505, 0.47014206410511505)
        x = matrix(aux,ncol=2,nrow=7,byrow=TRUE)
        w = numeric(7)
        w[1] = 0.22500000000000000
        w[2:4] = 0.12593918054482717
        w[5:7] = 0.13239415278850616
        w = w/2.0
     }else if(degree == 6){
        # Scheme from Strang and Fix, 12 points, degree of precision 6
        aux = c(0.873821971016996, 0.063089014491502,
                   0.063089014491502, 0.873821971016996,
                   0.063089014491502, 0.063089014491502,
                   0.501426509658179, 0.249286745170910,
                   0.249286745170910, 0.501426509658179,
                   0.249286745170910, 0.249286745170910,
                   0.636502499121399, 0.310352451033785,
                   0.636502499121399, 0.053145049844816,
                   0.310352451033785, 0.636502499121399,
                   0.310352451033785, 0.053145049844816,
                   0.053145049844816, 0.636502499121399,
                   0.053145049844816, 0.310352451033785)
        x = matrix(aux,nrow=12,ncol=2,byrow=TRUE)
        w = numeric(12)
        w[1:3] = 0.050844906370207
        w[4:6] = 0.116786275726379
        w[7:12] = 0.082851075618374
        w = w/2.0
    }
    # Add the third barycentric coordinate
    qnodes = matrix(0,nrow=length(w),ncol=3)
    for(j in 1:length(w)) qnodes[j,]<-c(x[j,1],x[j,2],1-x[j,1]-x[j,2])
    return(list("nodes"=qnodes, "weights"=w))
 }
   # Computing quadrature node and weights
   quad = quadrature(deg)

# Transform Cartesian coordinates to spherical coordinates (only angles are computed)

  cart2sph<-function(x){
    r = x[1]^2 + x[2]^2 + x[3]^2
    elev = acos(x[3]/r)     # theta (elevation angle) in [0,pi]
    az = atan2(x[2], x[1])   # phi (azimuthal angle) in [-pi,pi]
    res = c(elev,az)
    return(res)
  }

# Transform the 3D Cartesian points to 2D angle spherical coordinates (theta and phi)
   coor = matrix(0,nrow=dim(points)[1],2)
   for(i in 1:dim(points)[1]) coor[i,]<-cart2sph(as.matrix(points[i,]))

# Loop on the rows of matrix <<cells>> to compute the integral
  integral = 0.
  for(j in 1:dim(cells)[1]){
    index = as.numeric(cells[j,])
    # Compute the affine transformation from the barycentric coordinates to the cell <<index>>
    aux = c(coor[index[1],1],coor[index[2],1],coor[index[3],1],
            coor[index[1],2],coor[index[2],2],coor[index[3],2],
            1,1,1)
    BT = matrix(aux,nrow=3,ncol=3,byrow=TRUE)
    detBT = det(BT)

# Transform the barycentric coordinates to Cartesian coordinates (where function f is defined)
    T<-function(x){
        aux<-BT%*%x
        return(aux[1:2])
    }    # Compute the Jacobian associated to the barycentric coordinates (0.5*detBt=area of the triangle)

    # Compute the integrand: f composed with T multiplied by the spherical Jacobian sin(theta)
    integrand<-function(x) g(T(x))*sin(T(x)[1])*0.5*abs(detBT)

    # Compute the quadrture value of the integral in the cell <<index>>
    for(i in 1:dim(quad$nodes)[1]){
        integral = integral + quad$weights[i]*integrand(quad$nodes[i,])
    }#loop in i
}#loop in j
return(integral)
}
#' @noRd
#' @keywords internal
