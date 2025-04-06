#' Optimal design for 3-dimensional with visulization
#' @description This function uses GPU parallel computing to calculate the high dimensional integral and apply the smoothing method(thin plate splines) to get the optimum of power values given the prior information: the harzard reduction distribution. This function guides to choose the size of nested populations, i.e. find optimal r-values. The function visualizes and optimizes r-values, but only supports 3-dimension. The optimization of r-values in more than 3-dimension is trivial, but visualization can be too hard.
#' @param m integer, the number of grid points in each dimension for r, and we suggest m around 20 is enough for 3 dimension
#' @param n_dim integer, the number of dimension
#' @param r_set the matrix of proportion for each sub-population, r_1 is 1, r_i>r_{i+1}
#' @param N1 integer, which is fixed as 10240 in our package
#' @param N2 integer, which is fixed as 20480 in our package
#' @param N3 integer, the number of grid point for the sig.lv, which should be the multiples of 5, because we apply 5 stream parallel
#' @param E integer, the total number of events for the Phase 3 clinical trail, if not specified by user, then an estimation will apply
#' @param SIGMA the matrix of standard deviation of each sub-population, which should coincide with r_set or the default setting of each sub-population(i.e each entry of each row coincides to the corresponding entry in r_set)
#' @param sd_full a numeric number, which denotes the prior information of standard deviation for the harzard reduction if sig is not specified by user, then sd_full must has an input value to define the standard deviation of the full population
#' @param DELTA matrix, each row is an vector stands for the point estimation of harzard reduction in prior information corresponds to the r setting, if not specified we apply a linear scheme by giving bound to the linear harzard reduction 
#' @param delta_linear_bd vector of length 2, specifying the upper bound and lower bound for the harzard reduction; if user don't specify the delta for each sub-population, then the linear scheme will apply and the input is a must. 
#' @param seed integer,  seed for random number generation
#' @return list of 5 parts: plot_power: 3-d plot of the optimal power values versus r2 and r3; plot_alpha: 3-d plot of the optimal alpha-split values versus r2 and r3; opt_r_split: the optimal choice of proportion for each sub-population; opt_power: the optimal power values with the optimal r choice; opt_alpha_split: the optimal alpha split with the optimal r choice
#' @details the standard deviation of each population can be specified by giving SIGMA as input, and specify the harzard reduction rate DELTA for each population. Just enter values to SIGMA and DELTA, but note that the entered matrix should coincides with the matrix of r-split setting.
#' @seealso Grid setting of proportions for each sub-population proportion() and alpha.split()
#' @examples
#' \dontrun{
#' # the default setting of our paper's strong biomarker effect 
#' res <- designCTPB()
#' res$plot_power # to see 3-d plot for the optimal power versus r2 and r3
#' res$plot_alpha # to see 3-d plot for the optimal alpha versus r2 and r3
#' res$opt_r_split #  to see the optimal cutoff of the sub-population, 
#' #and here suggesting not cutoff at the 2-nd sub-population
#' res$opt_power 
#' res$opt_alpha_split
#'}
#' @export
designCTPB <- function(m=24, r_set = NULL, n_dim=3, N1=20480, N2=10240, N3=2000, E=NULL, SIGMA=NULL, sd_full=1/base::sqrt(20), DELTA=NULL, delta_linear_bd=c(0.2,0.8), seed=NULL){

  opt_res <- Optim_Res(m, r_set, n_dim, N1, N2, N3, E, SIGMA, sd_full, DELTA, delta_linear_bd, seed)
  r_setting <- as.matrix(opt_res[,1:n_dim]); opt_alpha <- as.matrix(opt_res[,(n_dim+1):(2*n_dim)]); opt_power <- opt_res[,NCOL(opt_res)]
  # we only develop for 3-dim right now, but we can easily extend it into higher dimensional case
  if(n_dim == 3){
    r2 <- r_setting[,2];r3 <- r_setting[,3]; opt_alpha1 <- opt_alpha[,1]; opt_alpha2 <- opt_alpha[,2];opt_alpha3 <- opt_alpha[,3]
    data1<- data.frame(alpha1 = opt_alpha1, r2 =r2, r3=r3 ); data2 <- data.frame(alpha2 = opt_alpha2, r2 =r2, r3=r3 ); data3 <- data.frame(alpha3 = opt_alpha3, r2 =r2, r3=r3 )
    model.p <- suppressWarnings(fields::Tps(cbind(r2,r3), opt_power, m=4))
    model.a1 <- suppressWarnings(fields::Tps(cbind(r2,r3), opt_alpha1, m=4))
    model.a2 <- suppressWarnings(fields::Tps(cbind(r2,r3), opt_alpha2, m=4))
    model.a3 <- suppressWarnings(fields::Tps(cbind(r2,r3), opt_alpha3, m=4))
    #power
    f.p = function(x,y){
      new=data.frame(r2=x,r3=y)
      p = stats::predict(model.p,new)
      return(p)
    }
    r2 <- seq(0,1,0.01); r3 <- seq(0,1,0.01)
    Power<- outer(r2,r3,f.p)
    for(jj in 1:101){
      for(kk in 1:101){
        if(kk<jj){ Power[jj,kk]=Power[jj,kk]}
        else{Power[jj,kk]=NA}
      }
    }
    # 3d-plot of optimal power versus r2 & r3
    if (requireNamespace("dplyr", quitely=TRUE)&&requireNamespace("plotly", quietly=TRUE)){
      fig_ <- plotly::plot_ly(x = r2, y = r3, z = t(Power))
      fig_ <- plotly::add_surface(fig_)
      fig_ <- plotly::layout(fig_,
                             scene = list(camera = list(eye = list(x = 2, y = -1, z = 0.34)),
                                          xaxis = list(title = "r2"),
                                          yaxis = list(title = "r3"),
                                          zaxis = list(title = "Optimal Power ")))
      fig.optim.power <- fig_
    }
    
     #alpha
    f1 = function(x,y){
      new=data.frame(r2=x,r3=y)
      p = stats::predict(model.a1,new)
      return(p)
    }
    
    f2 = function(x,y){
      new=data.frame(r2=x,r3=y)
      p = stats::predict(model.a2,new)
      return(p)
    }
    
    f3 = function(x,y){
      new=data.frame(r2=x,r3=y)
      p = stats::predict(model.a3,new)
      return(p)
    }
    
    r2 <- seq(0,1,0.01); r3 <- seq(0,1,0.01)
    pre_alpha1 <- outer(r2,r3,f1); pre_alpha2 <- outer(r2,r3,f2); pre_alpha3 <- outer(r2,r3,f3)
    for(jj in 1:101){
      for(kk in 1:101){
        if(kk<jj){ pre_alpha1[jj,kk]=pre_alpha1[jj,kk];pre_alpha2[jj,kk]=pre_alpha2[jj,kk];pre_alpha3[jj,kk]=pre_alpha3[jj,kk]}
        else{pre_alpha1[jj,kk]=NA; pre_alpha2[jj,kk]=NA; pre_alpha3[jj,kk]=NA}
      }
    }
    # 3d-plot of optimal alpha versus r2 & r3
    if (requireNamespace("dplyr", quitely=TRUE)&&requireNamespace("plotly", quietly=TRUE)){
      
      fig.alpha <- plotly::plot_ly() #showscale = FALSE)
      fig.alpha <- plotly::add_surface(fig.alpha, x = r2, y = r3, z = t(pre_alpha1))
      fig.alpha <- plotly::add_data(fig.alpha, data1)
      fig.alpha <- plotly::add_markers(fig.alpha, x = ~r2, y = ~r3, z = ~alpha1, size = 2, symbol = 0, name = "alpha1")
      
      fig.alpha <- plotly::add_surface(fig.alpha, x = r2, y = r3, z = t(pre_alpha2), opacity = 0.98)
      fig.alpha <- plotly::add_data(fig.alpha, data2)
      fig.alpha <- plotly::add_markers(fig.alpha, x=~r2, y=~r3, z=~alpha2, size=2,symbol= 100,name = "alpha2")
      
      fig.alpha <- plotly::add_surface(fig.alpha, x = r2, y = r3, z = t(pre_alpha3), opacity = 0.98)
      fig.alpha <- plotly::add_data(fig.alpha, data3)
      fig.alpha <- plotly::add_markers(fig.alpha, x=~r2, y=~r3, z=~alpha3, size=2,symbol= 200,name = "alpha3")
      fig.alpha <- plotly::layout(fig.alpha, scene=list(camera=list(eye=list(x=2, y=-1, z=0.34)),
                                                        xaxis = list(title = "r2"),
                                                        yaxis = list(title = "r3"),
                                                        zaxis = list(title = "Optimal alpha")))
    }
    # obtain the optimal power at cutoff of r2 and r3 to decide whether cut or not
    y <- function(x){
      new=data.frame(r2=x[1],r3=x[2])
      p = -stats::predict(model.p,new)
      return(p)
    }
    r2.max <- r2[which.max(opt_power)]; r3.max <- r3[which.max(opt_power)]
    opt <- stats::optim(c(r2.max, r3.max), y, upper = c(1,1),lower = c(0,0), method = "L-BFGS-B")
  }
  opt_r <- opt$par
  names(opt_r) <- c("r2",'r3')
  ct_opt_alpha1 <- f1(opt_r[1],opt_r[2])
  ct_opt_alpha2 <- f2(opt_r[1],opt_r[2])
  ct_opt_alpha3 <- f3(opt_r[1],opt_r[2])
  ct_opt_alpha <- c(ct_opt_alpha1, ct_opt_alpha2, ct_opt_alpha3)
  names(  ct_opt_alpha) <- paste("alpha",1:3, sep='')
  
  return(list(plot_power=fig.optim.power, plot_alpha = fig.alpha, opt_r_split = opt_r, opt_power = -opt$value, opt_alpha_split= ct_opt_alpha))
}






