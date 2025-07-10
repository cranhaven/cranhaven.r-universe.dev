### data generator ###
pro_trt_cal = function(or, pro_ctr){
  # or:
  # pro_ctr:
  pro_trt_sum = c()
  for (i in 1:length(or)){
    #pro_trt_sum[i] = 1/((1-sum(pro_ctr[1:i]))/(sum(pro_ctr[1:i])*or[i])+1)
    pro_trt_sum[i] = or[i]*sum(pro_ctr[1:i])/(1-sum(pro_ctr[1:i])+or[i]*sum(pro_ctr[1:i]))
  }
  pro_trt_sum_new = c(pro_trt_sum,1)
  pro_trt = c(pro_trt_sum[1], diff(pro_trt_sum_new))
  return(pro_trt)
}

data_gene_npo = function(or, sd, pro_ctr, N){
  # or:
  # pro_ctr:
  # N:
  L = length(or)+1
  #x = 0

  while (TRUE){
    log_or = unlist(lapply(or, function(x) rnorm(1, mean = log(x), sd = sd)))
    OR = exp(log_or)
    pro_trt = pro_trt_cal(OR, pro_ctr)
    s = sum(schoolmath::is.negative(pro_trt))
    #x = sum(x,1)
    if (s <= 0){break}
  }

  table = rbind(pro_ctr, pro_trt)
  rownames(table) = c("control", "treatment")
  count = round(table*N)
  treatment = c(rep("0", sum(count[1,])), rep("1", sum(count[2,])))
  response = c(rep(1:L, count[1,]), rep(1:L, count[2,]))
  data = data.frame(response, treatment)
  data$treatment = as.factor(data$treatment)
  data$response = as.factor(data$response)
  #output = list(pro_trt, OR,x)
  #names(output) = c('proportion_treatment', 'data',"count")
  #return(output)
  return(data)
}


