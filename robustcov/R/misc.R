perform_metrics <- function(graph_hat,graph){
    normOmega <- norm(graph_hat-graph, type = "I")
    normSigma <- norm(solve(graph_hat)-solve(graph), type = "I")
    graph <- graph != 0
    graph_hat <- graph_hat != 0
    FP <- sum(graph_hat[upper.tri(graph_hat)] & !graph[upper.tri(!graph)])
    FN <- sum(!graph_hat[upper.tri(!graph_hat)] & graph[upper.tri(graph)])
    return(c(normOmega,normSigma,FP,FN))
}
