library(testthat)
library(dmri.tracking)


test_that('Test output of v.track with example dataset',{
  n=2
  r=0.6
  r_grid = seq(from = 1/(2*n), to = 1-1/(2*n), length.out = n)

  r_x=matrix(r_grid, n, n)
  r_y=matrix(r_grid, n, n, byrow=T)

  fib_indi1 = ((1-(r_x+0.05))**2+(r_y-0.05)**2 < 1-1e-6) & ((1-(r_x-0.05))**2+(r_y+0.05)**2 > r**2+1e-6)
  fib_indi2 = ((r_x-0.05)**2+(r_y-0.05)**2 < 1-1e-6) & ((r_x+0.05)**2+(r_y+0.05)**2 > r**2+1e-6)
  fib_indi = fib_indi1 + 2*fib_indi2

  theta_fib_1 = atan2(r_x, 1-r_y)
  theta_fib_2 = (atan2(r_x, r_y)+pi/2 )* fib_indi2

  dir1_x=cos(theta_fib_1)*sin(pi/2) * fib_indi1
  dir1_y=sin(theta_fib_1)*sin(pi/2) * fib_indi1
  dir1_z=cos(matrix(pi/2,n,n))* fib_indi1

  dir2_x=cos(theta_fib_2)*sin(pi/2) * fib_indi2
  dir2_y=sin(theta_fib_2)*sin(pi/2) * fib_indi2
  dir2_z=cos(matrix(pi/2,n,n))* fib_indi2


  xgrid.sp = 1
  ygrid.sp = 1
  zgrid.sp = 1
  braindim = c(n,n,1)
  braingrid = array(dim=c(3,braindim))
  for (i in (1:braindim[1])){
    for (j in (1:braindim[2])){
      for (k in (1:braindim[3])){
        braingrid[,i,j,k] <- c((i-braindim[1]/2-.5)*xgrid.sp, (j-braindim[2]/2-.5)*ygrid.sp, (k-braindim[3]/2-.5)*zgrid.sp)
      }
    }
  }

  vec=NULL
  loc=NULL
  n.fiber = rep(NA, prod(braindim))
  map = c()

  for (k in (1:braindim[3])){
    for (j in (1:braindim[2])){
      for (i in (1:braindim[1])){
        ind = ((k-1)*n*n + (j-1)*n +i)
        if(fib_indi[i,j]==3){
          n.fiber[ind] = 2
          vec = rbind(vec,c(dir1_x[i,j],dir1_y[i,j],dir1_z[i,j]))
          loc = rbind(loc,braingrid[,i,j,k])
          map = c(map,ind)
          vec = rbind(vec,c(dir2_x[i,j],dir2_y[i,j],dir2_z[i,j]))
          loc = rbind(loc,braingrid[,i,j,k])
          map = c(map,ind)
        }else if(fib_indi[i,j]==2){
          n.fiber[ind] = 1
          vec = rbind(vec,c(dir2_x[i,j],dir2_y[i,j],dir2_z[i,j]))
          loc = rbind(loc,braingrid[,i,j,k])
          map = c(map,ind)
        }else if(fib_indi[i,j]==1){
          n.fiber[ind] = 1
          vec = rbind(vec,c(dir1_x[i,j],dir1_y[i,j],dir1_z[i,j]))
          loc = rbind(loc,braingrid[,i,j,k])
          map = c(map,ind)
        }else if(fib_indi[i,j]==0){
          n.fiber[ind] = 0
          vec = rbind(vec,c(NA,NA,NA))
          loc = rbind(loc,braingrid[,i,j,k])
          map = c(map,ind)
        }
      }
    }
  }

  rmap = c()
  n.fiber2 = c()
  for(i in 1:prod(braindim)){
    rmap = c(rmap, which(map == i)[1])
    n.fiber2 = c(n.fiber2, rep(n.fiber[i],max(n.fiber[i],1)))
  }

  v.obj = NULL
  v.obj$vec = vec
  v.obj$loc = loc
  v.obj$map = map
  v.obj$rmap = rmap
  v.obj$n.fiber = n.fiber
  v.obj$n.fiber2 = n.fiber2
  v.obj$braingrid = braingrid
  v.obj$xgrid.sp = xgrid.sp
  v.obj$ygrid.sp = ygrid.sp
  v.obj$zgrid.sp = zgrid.sp

  expect_equal(length(v.track(v.obj)$lens), 6)
})
