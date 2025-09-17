createData <-
function(n.case, n.control, mean.upper=13, mean.lower=5, sd.upper=1, sd.lower=1, 
                        n.features = 10000, 
                        subtype1.feats = 1:5,
                        subtype2.feats = 6:10,
                        subtype3.feats = 11:15){
  # features 1 through 15 are used to simulate class
  # subtypes is a vector containing any combination of the values 1 through 4
  subtype <- sample(c(1, 2, 3, 4, 3, 4), n.case, replace=T) # 1 and 2 are same subtype but with an "or" 
  cs <- apply(as.matrix(subtype), 1, function(x){createCase(x, mean.upper, mean.lower, sd.upper, 
                                                             sd.lower, n.features, subtype1.feats,
                                                             subtype2.feats, subtype3.feats)})
  ctrl <- replicate(n.control, createControl(mean.upper, mean.lower, sd.upper, sd.lower, n.features, 
                                              subtype1.feats, subtype2.feats, subtype3.feats))
  y <- c(rep(1, n.case), rep(0, n.control))
  
  graph <- createGraph(subtype1.feats, subtype2.feats, subtype3.feats, 
                      n.interactions = 1e6, n.features)
  out <- list(x=rbind(t(cs), t(ctrl)), y=y, subtype = subtype, graph = graph)
  return(out)
}
