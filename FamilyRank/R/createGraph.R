createGraph <-
function(subtype1.feats = 1:5, subtype2.feats = 6:10, subtype3.feats = 11:15, 
                        n.interactions = 1e6, n.features = 10000){
  f1 <- sample(1:n.features, size = n.interactions, replace = T)
  f2 <- sample(1:n.features, size = n.interactions, replace = T)
  feats <- unique(data.frame(f1 = f1, f2 = f2))
  
  # get subtype feats
  df.subtype1 <- expand.grid(f1 = subtype1.feats, f2 = subtype1.feats)
  df.subtype2 <- expand.grid(f1 = subtype2.feats, f2 = subtype2.feats)
  df.subtype3 <- expand.grid(f1 = subtype3.feats, f2 = subtype3.feats)
  subtype.feats0 <- rbind(df.subtype1, df.subtype2, df.subtype3)
  # remove self interationcs:
  subtype.feats1 <- subtype.feats0[-which(subtype.feats0$f1 == subtype.feats0$f2),]
  # remove inverted dups
  f1.sub <- apply(subtype.feats1, 1, min)
  f2.sub <- apply(subtype.feats1, 1, max)
  subtype.feats <- unique(data.frame(f1 = f1.sub, f2 = f2.sub))
  
  # get expanded feats
  feats.expanded0 <- rbind(subtype.feats, feats)
  # remove self interactions
  feats.expanded1 <- feats.expanded0[-which(feats.expanded0$f1 == feats.expanded0$f2),]
  # remove inverted duplicates
  f1.exp <- apply(feats.expanded1, 1, min)
  f2.exp <- apply(feats.expanded1, 1, max)
  feats.expanded <- unique(data.frame(f1 = f1.exp, f2 = f2.exp))
  
  if(!identical(feats.expanded[1:nrow(subtype.feats),],
                subtype.feats)){stop("Subtype feats not at head of expanded feats")}
  
  cs <- runif(nrow(feats.expanded))
  cs[1:nrow(subtype.feats)] <- 1
  graph <- data.frame(feats.expanded, cs = cs)
  return(graph)
}
