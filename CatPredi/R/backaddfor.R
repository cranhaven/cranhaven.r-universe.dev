backaddfor <-
function(formula, cat.var, data, range = NULL, k = 1, l.s.points = 100, eps = 0.001, repmax = 50, min.p.cat = 1, randnodes = FALSE){
  
  Y <- data[,all.vars(formula)[1]]
  if(is.null(range)) {
    X <- data[[cat.var]]
  } else{
    X <- data[[cat.var]][data[[cat.var]] >= range[1] & data[[cat.var]] <= range[2]]
  }
  # range is a vector such as: range=c(a,b)

  # Initial nodes
  cuts <- seq(quantile(X, 0.05), quantile(X, 0.95), length = k+2)[-c(1,k+2)]
  cuts0 <- c(min(X)-0.001, cuts, max(X)+0.001)
  data$x.cut <- cut(X, breaks = cuts0, include.lowest=TRUE, right=TRUE)
  formula.n <- update(formula, as.formula(". ~ . + x.cut"))
  fit <- gam(formula.n, family = "binomial", data = data)
  auc0 <- compute.empirical.AUC(predict(fit)[Y==1], predict(fit)[Y==0])

  # Random nodes
  if(randnodes == TRUE){
  for(rep in 1:5) {
    auc1 = 0
    cuts1 = cuts0
    cuts1[2:(k+1)] <- sample(X,k)
    cuts1 <- sort(cuts1)
    if(length(unique(cuts1)) == length(cuts1)) {
      data$x.cut=cut(X, breaks = cuts1, include.lowest=TRUE, right=TRUE)
      if (min(table(data$x.cut)) >= min.p.cat) {
        fit <- gam(formula.n, family="binomial", data = data)
        auc1 <- compute.empirical.AUC(predict(fit)[Y==1], predict(fit)[Y==0])}
      if (auc1 > auc0) {
        auc0=auc1; cuts0=cuts1; cuts0=sort(cuts0)}}}
  }

  # Sequential search setup
  caux <- seq(min(X), max(X), length.out = l.s.points + 2)[-c(1, l.s.points + 2)]
  nrep <- 0
  auc_old <- 0
  continue <- TRUE

  # Sequential search
  while (continue) {
    nrep = nrep+1
    for (j in 2:(k+1)) {
      for (ifino in 1:l.s.points) {
        auc1 = 0
        cuts1 = cuts0
        cuts1[j] = caux[ifino]
        cuts1 = sort(cuts1)
        if(length(cuts1) > length(unique(cuts1))) {auc1=0}
        else {
          data$x.cut <- cut(X, breaks = cuts1, include.lowest=TRUE, right=TRUE)
          fit <- gam(formula.n, family = "binomial", data = data)
          if (min(table(data$x.cut)) >= min.p.cat) {
            auc1 <- compute.empirical.AUC(predict(fit)[Y==1], predict(fit)[Y==0])}
          }
        if (auc1 > auc0) {auc0=auc1; cuts0=cuts1; cuts0=sort(cuts0)}
        }}

      if (nrep > 1 & abs((auc0-auc_old)/auc_old) < eps) {continue=FALSE}
      if (nrep > repmax) {continue = FALSE}
      auc_old = auc0}

    data$x.cut <- cut(X, breaks = cuts0, include.lowest=TRUE, right=TRUE)
    fit <- mgcv::gam(formula.n, family = "binomial", data = data)
    list(cuts = cuts0[-c(1,k+2)], auc = auc0, pred = predict(fit, type = "response"))
  }
