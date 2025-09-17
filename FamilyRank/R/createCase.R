createCase <-
function(subtype, upper.mean, lower.mean, upper.sd, lower.sd, n.features, 
                        subtype1.feats = 1:5,
                        subtype2.feats = 6:10,
                        subtype3.feats = 11:15){
  p <- rep(NA, n.features)
  if(subtype==1){
    p[subtype1.feats[1:4]] <- rnorm(4, upper.mean, upper.sd)
    p[which(is.na(p))] <- rbinorm((n.features-length(p[which(!is.na(p))])), 
                                lower.mean, upper.mean, lower.sd, upper.sd, .5)
  }
  if(subtype==2){
    p[c(subtype1.feats[1:3],subtype1.feats[5])] <- rnorm(4, upper.mean, upper.sd)
    p[which(is.na(p))] <- rbinorm((n.features-length(p[which(!is.na(p))])), 
                                lower.mean, upper.mean, lower.sd, upper.sd, .5)
  }
  if(subtype==3){
    p[subtype2.feats] <- rnorm(5, upper.mean, upper.sd)
    p[which(is.na(p))] <- rbinorm((n.features-length(p[which(!is.na(p))])),  lower.mean, upper.mean, lower.sd, 
                                upper.sd, .5)
  }
  if(subtype==4){
    p[subtype3.feats[1:4]] <- rnorm(4, upper.mean, upper.sd)
    p[subtype3.feats[5]] <- rnorm(1, lower.mean, lower.sd)
    p[which(is.na(p))] <- rbinorm((n.features-length(p[which(!is.na(p))])),  lower.mean, upper.mean, 
                                lower.sd, upper.sd, .5)
  }
  return(p)
}
