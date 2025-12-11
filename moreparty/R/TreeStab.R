#' @importFrom partykit nodeapply as.simpleparty nodeids info_node
#' @importFrom stats getCall terms xtabs
#' @importFrom utils combn

#' @export

TreeStab <- function(ct, B = 20) {

  call <- stats::getCall(ct)
  tr <- stats::terms(ct)
  env <- try(environment(tr))
  if (inherits(env, "try-error")) env <- NULL
  data <- eval(call$data, envir = env, enclos = parent.frame())

  get_var_freq <- function(object) {
    id_term <- names(partykit::nodeapply(partykit::as.simpleparty(object), ids = partykit::nodeids(object, terminal = TRUE), partykit::info_node))
    vars <- unlist(partykit::nodeapply(partykit::as.simpleparty(object), ids = partykit::nodeids(object, terminal = FALSE), function(x) {names(partykit::info_node(x)$p.value)}))
    vars <- vars[!(names(vars) %in% id_term)]
    res <- as.data.frame(table(vars))
    return(res)
  }
  
  jaccard <- function(p,q) {
    combi <- utils::combn(1:length(p),2)
    i <- combi[1,]
    j <- combi[2,]
    p1 <- p[i]==p[j]
    q1 <- q[i]==q[j]
    n11 <- sum(p1 & q1)
    n00 <- sum(!p1 & !q1)
    n10 <- sum(p1 & !q1)
    n01 <- sum(!p1 & q1)
    return(n11 / (n11+n10+n01))  
  }
  
  jacc <- numeric()
  varfreq <- list()
  for(i in 1:B) {
    # bootstrap tree
    call$data <- data[sample(1:nrow(data), replace = TRUE),]
    ct_b <- eval(call)
    # cramer V for table partition x bootstrap partition
    nodes <- predict(ct, type = "node")
    nodes_b <- predict(ct_b, type = "node")
    jacc[i] <- jaccard(nodes, nodes_b)
    # variables used in bootstrap tree
    temp <- get_var_freq(ct_b)
    temp$id <- rep(i, nrow(temp))
    varfreq[[i]] <- temp
  }
  
  varfreq <- do.call("rbind.data.frame", varfreq)
  df <- stats::xtabs(Freq ~ id + vars, data = varfreq)
  class(df) <- NULL
  df <- as.data.frame(df)
  temp <- colMeans(df)
  df2 <- data.frame(vars = names(temp), Freq_boot = temp)
  temp <- colMeans(df>0)
  df3 <- data.frame(vars = names(temp), Prob_boot = temp)
  df1 <- get_var_freq(ct)
  df1$Prob <- as.numeric(df1$Freq>0)
  res <- data.frame(vars = colnames(ct[[1]]$node$info$criterion))
  res <- merge(res, df1)
  res <- merge(res, df2)
  res <- merge(res, df3)
  res <- res[,c("vars","Freq","Freq_boot","Prob","Prob_boot")]

  return(list(partition = mean(jacc),
              variables = res))
}
