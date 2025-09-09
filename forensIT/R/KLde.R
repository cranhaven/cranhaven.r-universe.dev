#' @title KL divergence
#' @description KL divergence
#' @param px probability distribution
#' @param py probability distribution
#' @param epsilon small number to avoid log(0)
#' @return KL divergence
#' @export
KLde <- function(px,py,epsilon=1e-20){
  i0 <- py==0
  if(sum(i0)>0){
    a1 <- KLd(px[!i0],py[!i0],epsilon=epsilon)
    a0 <- KLd(px[i0] ,py[i0],epsilon=epsilon) #nolint
    b  <- H(px[i0])
    c  <- sum(px[i0])
    return(c(KLd(px,py,epsilon=epsilon),kld_g=a1,h_gg=b,p1=c,epsilon=epsilon))
  }else{
    return(c(KLd(px,py,epsilon=epsilon),kld_g=0,h_gg=0,p1=0,epsilon=epsilon))
  }
}