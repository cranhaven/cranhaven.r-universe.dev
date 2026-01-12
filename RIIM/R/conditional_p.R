
# calculate the conditional probability
conditional_p <- function(treated.subject.index,matched.control.subject.index,p,alpha=0.1){
  
  prob = rep(0,length(p))
  for (i in 1:length(treated.subject.index)) {
    index = c(treated.subject.index[[i]],matched.control.subject.index[[i]])
    n = length(index)
    
    # when set length = 2
    if (n == 2) {
      g_i_all = rep(0,n)
      # calculate the numerator
      for(j in 1:n){
        d_i1 = p[index[j]]
        d_i2 = p[index[-j]]
        g_i = d_i1*prod((1-d_i2))
        g_i_all[j] = g_i
      }
      # calculate pij
      for(m in 1:n){
        prob[index[m]] = g_i_all[m]/sum(g_i_all)
      }
      # regularized estimated treatment assignment probability
      if(min(prob[index]) < alpha | max(prob[index]) > 1-alpha){
        prob[index][which.min(prob[index])] <- alpha
        prob[index][which.max(prob[index])] <- 1-alpha
      }
    }
    
    # when set length > 2
    else if (n > 2) {
      # when mi = 1
      if(length(treated.subject.index[[i]]) == 1) {
        g_i_all = rep(0,n)
        # calculate the numerator
        for(j in 1:n){
          d_i1 = p[index[j]]
          d_i2 = p[index[-j]]
          g_i = d_i1*prod((1-d_i2))
          g_i_all[j] = g_i
        }
        # calculate pij
        for(m in 1:n){
          prob[index[m]] = g_i_all[m]/sum(g_i_all)
        }
        # regularized estimated treatment assignment probability
        if(min(prob[index]) < alpha | max(prob[index]) > 1-alpha){
          prob[index] = 1/n
        }
        # when ni-mi =1 & mi >1
      } else if(length(matched.control.subject.index[[i]]) == 1 & length(treated.subject.index) > 1) {
        h_i_all = rep(0,n)
        # calculate the numerator
        for(j in 1:n){
          d_i1 = p[index[j]]
          d_i2 = p[index[-j]]
          h_i = (1-d_i1)*prod(d_i2)
          h_i_all[j] = h_i
        }
        # calculate pij
        for(m in 1:n){
          prob[index[m]] = 1-(h_i_all[m]/sum(h_i_all))
        }
        # regularized estimated treatment assignment probability
        if(min(prob[index]) < alpha | max(prob[index]) > 1-alpha){
          prob[index] = 1-(1/n)
        }
      }
    }
    
  }
  return(prob)
}
