reScale <-
function(i, j, est, vc){
  return(est[j] ^ 2 * vc[i, i] + est[i] ^ 2 * vc[j, j] + 2 * est[j] * est[i] * vc[i, j] + vc[j, j] * vc[i, i] + vc[i, j] ^ 2)
}
