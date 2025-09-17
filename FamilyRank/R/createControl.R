createControl <-
function(upper.mean, lower.mean, upper.sd, lower.sd, n.features, 
         subtype1.feats = 1:5,
         subtype2.feats = 6:10,
         subtype3.feats = 11:15){
  p <- rep(NA, n.features)
  
  # not subtype 1
  loc1 <- sample(subtype1.feats[1:4], 1)
  p[loc1] <- rnorm(1, lower.mean, lower.sd)
  
  if(loc1==subtype1.feats[4]){
    # not subtype 2
    loc2 <- subtype1.feats[5]
    p[loc2] <- rnorm(1, lower.mean, lower.sd)
  }
  
  # not subtype 3
  loc3 <- sample(subtype2.feats, 1)
  p[loc3] <- rnorm(1, lower.mean, lower.sd)
  
  # not subtype 4
  loc4 <- sample(subtype3.feats, 1)
  p[loc4] <- ifelse(loc4==subtype3.feats[5], rnorm(1, upper.mean, upper.sd), rnorm(1, lower.mean, lower.sd))
  
  p[which(is.na(p))] <- rbinorm((n.features-length(p[which(!is.na(p))])), lower.mean, upper.mean, lower.sd, upper.sd, .5)
  return(p)
}
