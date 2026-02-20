#' consensusNet
#'
#' @description This function computes the INet Algorithm for the construction of a
#' **Consensus Network**.
#'
#' @param adjL list of weighted adjacency matrix with the same name in rows
#' and columns for all the matrices.
#' @param threshold threshold for the construction of the Consensus
#' (default 0.5). Used in the last step on the similar graphs.
#' @param tolerance the tolerance of differences between similar graphs for the
#' construction of the Consensus (default 0.1).
#' @param theta importance to give to the neighbourhood part of the weight
#' (default 0.04).
#' @param nitermax maximum number of iteration before stopping the algorithm
#' (default 50).
#' @param ncores number of CPU cores to use (default is 2). We suggest to use
#' ncores equal to the number of graphs to integrate.
#' @param verbose flag for verbose output (default as TRUE).
#'
#' @return a list of 3 types:
#' $graphConsensus the Consensus Network,
#' $Comparison the Jaccard weighted distances between the graphs
#' calculated in each iteration,
#' $similarGraphs the similar graphs before the Thresholding
#'
#' @export
#' @import igraph parallel r2r
#'
#' @examples
#' data("adjL_data")
#' consensusNet(adjL_data)


consensusNet <- function (adjL, threshold=0.5,
                          tolerance=0.1, theta=0.04,
                           nitermax=50, ncores=2, verbose=TRUE)


{


  ##### Convert adjacency Matrix in graph as it need it
  graph <- vector(mode = "list", length = length(adjL))
  for (t in 1:length(adjL))
  {
    if(length(rownames(adjL[[1]]))>0)
    {
      graph[[t]] <- igraph::graph_from_adjacency_matrix(adjL[[t]],
                                                mode = "upper",
                                                diag = FALSE,
                                                add.colnames = "NA",
                                                weighted = TRUE)

    }else{
      graph[[t]] <- igraph::graph_from_adjacency_matrix(adjL[[t]],
                                                mode = "upper",
                                                diag = FALSE,
                                                weighted = TRUE)
    }
  }

  ############### Function Weight
  weightMat <- function(Weight, grafo, nodes)
  {
    edgID <- igraph::get.edge.ids(grafo, nodes)#riesce a leggere gli edge in
    #entrambe le direzioni
    if (edgID==0){
      grafo <- igraph::add_edges(grafo, nodes)
      edgID <- igraph::get.edge.ids(grafo, nodes)
    }

    igraph::E(grafo)$weight[edgID] <- Weight

    return(grafo)
  }

  ######### lower triangolar no Diagonal
  get_lower_tri_noDiag <- function(cormat){
    cormat[upper.tri(cormat)] <- NA
    diag(cormat) <- NA
    return(cormat)
  }



  ############ Jaccard Weighted Function
  JaccardAll <- function(grafi)
  {

      #To check if the names of the nodes are the same
      comp <- utils::combn(1:length(grafi), 2)
      for (j in 1:(dim(comp)[2]))
      {
          if(names(table(igraph::V(grafi[[comp[1,j]]])==igraph::V(grafi[[comp[2,j]]])))=="TRUE")
          {}else{stop("Check: Not same nodes in all the graphs. Add them as
                      isolated nodes")}
      }

    A <- NULL
    for (l in 1:length(grafi))
    {
      AdjW <- igraph::as_adjacency_matrix(grafi[[l]], attr="weight")
      triA <- get_lower_tri_noDiag(AdjW)
      vettriA <- as.vector(triA)
      vetI <- vettriA[!is.na(vettriA)]
      A <- rbind(A,vetI)
    }


    #weighted jaccard similarity matrix setup
    sim.jac <- matrix(0, nrow=nrow(A), ncol=nrow(A))
    #weighted jaccard function
    pairs <- t(utils::combn(1:nrow(A), 2))
    for (i in 1:nrow(pairs)){
      num <- sum(sapply(1:ncol(A), function(x)(min(A[pairs[i,1],x],
                                                   A[pairs[i,2],x]))))
      den <- sum(sapply(1:ncol(A), function(x)(max(A[pairs[i,1],x],
                                                   A[pairs[i,2],x]))))
      sim.jac[pairs[i,1],pairs[i,2]] <- num/den
      sim.jac[pairs[i,2],pairs[i,1]] <- num/den
    }
    sim.jac[which(is.na(sim.jac))] <- 0
    diag(sim.jac) <- NA

    #weighted jaccard distance
    dist.jac <- 1-sim.jac
    #print(dist.jac)
    return(mean(dist.jac, na.rm=TRUE))

  }






  ####### Compare
  Comparison <- NULL
  count <- 0
  graphChange <- vector(mode = "list", length = length(graph))

  ######################################## Algorithm  #########################

  repeat
  {
     ##Jaccard
    Comp <- JaccardAll(grafi=graph)

    # Per il primo giro:
    if(count==0)
    {

      if(Comp < tolerance )
      {
        if (verbose) cat("Multilayer network distance: less than tolerance.\n")
        #print("less than tolerance")
        break
      }
    }
    #print(Comp)



    graphBeginning <- graph


    ##Constraction of the Union graph:
    unionGraph <- graph[[1]]
    for(j in seq(1,length(graph))[-1])
    {
      unionGraph <- igraph::union(unionGraph,graph[[j]])
      #print(j)
    }
    #Create Edgelist:
    Edgelist <- igraph::as_edgelist(unionGraph,names=FALSE)# FALSE perche altrimenti
    #li prende come character e non li legge nei vicini
    # PesoOutput <- matrix(data=NA,nrow=dim(Edgelist)[1],ncol=length(graph)+2)
    # PesoOutput[,1] <- Edgelist[,1]
    # PesoOutput[,2] <- Edgelist[,2]
    # PesiEgoOutput <- matrix(data=NA,nrow=dim(Edgelist)[1],ncol=length(graph)+2)
    # PesiEgoOutput[,1] <- Edgelist[,1]
    # PesiEgoOutput[,2] <- Edgelist[,2]










    ######################## Parallelization ##############

    ### number of cores:
    #ncores <- (parallel::detectCores(logical = FALSE)-1)
    #ncores <- length(graph)
    vet <- seq(1,length(graph))
    #vet <- 1
    cl <- parallel::makeCluster(ncores)
    parallel::clusterExport(cl,varlist =c("graph","Edgelist","theta",
                                          "weightMat"),
                            envir=environment())

    Graphs <- parallel::clusterApply(cl, vet, function(i)
    {


      ########### Functions for the hashmap

      #neigbhors
      funNeig <- function(x) {
        r2r::insert(m, node_id, as.vector(x))
        node_id <<- node_id + 1
      }

      #weights
      code <- function(a,b) {x <- min(a,b); y <- max(a,b); (x+y)*(x+y+1)/2 + y}

      funWeights <- function(x) {
        r2r::insert(s, code(E[edge_id,][1],E[edge_id,][2]),
                               igraph::E(graph[[h]])$weight[edge_id])
        edge_id <<- edge_id + 1
      }

      #egoweights
      funegoWeights <- function(x) {
        r2r::insert(t, code(E[edge_id,][1],E[edge_id,][2]),0)
        edge_id <<- edge_id + 1
      }



      ##### Creating structures
      Neig_list <- vector(mode = "list", length = length(graph))
      Weights_list <- vector(mode = "list", length = length(graph))
      Intersect_list <- vector(mode = "list", length = length(graph))
      EgoWeights_list <- vector(mode = "list", length = length(graph))

      #utils::globalVariables(c("Neig_list","Weights_list","Intersect_list","EgoWeights_list"))



      ##We constract a hashmap for all the graphs
      for (h in 1:length(graph))
      {
        #print(h)
        #Hashmap neighbors
        m <- r2r::hashmap()
        l <- igraph::as_adj_list(graph[[h]])
        node_id <- 1
        lapply(l,funNeig)
        Neig_list[[h]] <- m
        #print("m")

        #Hashmap weights
        s <- r2r::hashmap()
        E <- igraph::as_edgelist(graph[[h]])
        edge_id <- 1
        apply(E,1,funWeights)
        Weights_list[[h]] <- s
        #print("s")

        #Hashmap egoWeights
        t <- r2r::hashmap()
        E <- Edgelist # Edgelist of the union graph
        edge_id <- 1
        apply(E,1,funegoWeights)
        EgoWeights_list[[h]] <- t
        #print("t")
      }







      #Take the weight of the edge between the 2 nodes in all network and
      #make (wI+1/2*(wII+wIII))/2
      for (z in 1:dim(Edgelist)[1])
      {
        #print(z)
        nodes <- c(Edgelist[z,1],Edgelist[z,2])
        Inei <- Neig_list [[i]][nodes[1]][[1]]
        Jnei <- Neig_list [[i]][nodes[2]][[1]]
        Intersect_list[[i]] <- intersect(Inei,Jnei)


        ####Peso dell'edge nella network di interesse:
        wUso <- Weights_list[[i]][code(nodes[1],nodes[2])][[1]]
        if(is.null(wUso)){
          wUso <- 0
          }
        # se non esiste l'edge mette zero



        wOthers <- NULL
        for(j in seq(1,length(graph))[-i])
        {


          Intersect_list[[j]] <- intersect(Neig_list [[j]][nodes[1]][[1]],
                                           Neig_list [[j]][nodes[2]][[1]])
          ###Peso dell' edge nelle altre network:
          wAltri <- Weights_list[[j]][code(nodes[1],nodes[2])][[1]]
          if(is.null(wAltri)){
            wAltri <- 0
          }
          wOthers <- c(wOthers,wAltri)
          #print(j)
        }


        ##### Modifica i pesi :
        peso <- (wUso+mean(wOthers))/2

        ###Ego Network pesi:  ##########

        #Per la network d'interesse NUS:
        if(length(Intersect_list[[i]])==0)
        {
          pesiEgoNUS <- 0

        }else{
          pesiint1_2 <- NULL

          for(k in 1:length(Intersect_list[[i]]))
          {
            pesiint1 <- Weights_list[[i]][code(nodes[1],Intersect_list[[i]][k])][[1]]
            if(is.null(pesiint1)){
              pesiint1 <- 0
            }
            pesiint2 <- Weights_list[[i]][code(nodes[2],Intersect_list[[i]][k])][[1]]
            if(is.null(pesiint2)){
              pesiint2 <- 0
            }



            #Devi moltiplicarlo per quante volte quel vicino e presente nelle altre network
            numberCom <- table(unlist(Intersect_list))[names(table(unlist(Intersect_list)))==Intersect_list[[i]][k]]

            pint1_2 <- (numberCom/length(graph))*(pesiint1+pesiint2)

            pesiint1_2 <- c(pesiint1_2,pint1_2)
          }
          Spesiint1_2 <- sum(pesiint1_2)


          if(wUso==0)
          {
            denomin <- length(Inei)+length(Jnei)
          }else{
            denomin <- (length(Inei)+length(Jnei))-2
          }

          #Pesi ego network
          pesiEgoNUS <- (Spesiint1_2/denomin)^(1/length(Intersect_list[[i]]))

        }


        #Salva nella hashmap
        #EgoWeights_list[[i]][code(nodes[1],nodes[2])] <- pesiEgoNUS



        ###Per le altre network :
        pesiEgoOthers <- NULL
        for(j in seq(1,length(graph))[-i])
        {

          #Per la network d'interesse NUS:
          if((length(Intersect_list[[j]]))==0)

          {
            pesiEgoAltri <- 0

          }else{


            pesiint1_2 <- NULL
            for(k in 1:(length(Intersect_list[[j]])))

            {
              pesiint1 <- Weights_list[[j]][code(nodes[1],Intersect_list[[j]][k])][[1]]

              if(is.null(pesiint1)){
                pesiint1 <- 0
              }
              pesiint2 <- Weights_list[[j]][code(nodes[2],Intersect_list[[j]][k])][[1]]
              if(is.null(pesiint2)){
                pesiint2 <- 0
              }



              #Devi moltiplicarlo per quante volte quel vicino e' presente nelle altre network
              numberCom <- table(unlist(Intersect_list))[names(table(unlist(Intersect_list)))==Intersect_list[[j]][k]]

              pint1_2 <- (numberCom/length(graph))*(pesiint1+pesiint2)

              pesiint1_2 <- c(pesiint1_2,pint1_2)
            }
            Spesiint1_2 <- sum(pesiint1_2)


            #If the nodes of the edge taken into account are neigbhours we deleate them from the count of the denominator
            graphsOtherVectors <- seq(1,length(graph))[-i]
            positionVector <- which(graphsOtherVectors==j)
             if((wOthers[positionVector])==0)
             {
               denomin <- length(Neig_list [[j]][nodes[1]][[1]])+length(Neig_list [[j]][nodes[2]][[1]])
             }else{
               denomin <- length(Neig_list [[j]][nodes[1]][[1]])+length(Neig_list [[j]][nodes[2]][[1]])-2
             }

            #Pesi ego network
            pesiEgoAltri <- (Spesiint1_2/denomin)^(1/length(Intersect_list[[j]]))

          }




          # Salva nella Hashmap
          #EgoWeights_list[[j]][code(nodes[1],nodes[2])] <- pesiEgoAltri

          pesiEgoOthers <- c(pesiEgoOthers,pesiEgoAltri)
        }

        #print(c(pesiEgoNUS,pesiEgoNUS))
        pesiEgo <- (pesiEgoNUS+mean(pesiEgoOthers))/2



        #print(pesiEgo)
        ##### Modifica i pesi :
        WeightI1 <- peso + (theta*pesiEgo)
        #PesoOutput[z,i+2] <- peso
        #PesiEgoOutput[z,i+2] <- pesiEgo
        # print(WeightI1)
        if(WeightI1 > 1)
        {
          WeightI1 <- 1
        }

        graph[[i]] <- weightMat(Weight=WeightI1,
                              grafo=graph[[i]], nodes=nodes)

          }###Finisce z


      graph[[i]]


    })
  parallel::stopCluster(cl)



    graphChange <- Graphs
    graph <- graphBeginning


    #ricalcola Jaccard:
    CompPost <- JaccardAll(grafi=graphChange)
    if (verbose) cat("Multilayer network distance:", CompPost, "\n")
    #print(CompPost)

    if(count==0)
    {
      Comparison <- Comp
    }


    Comparison <- rbind(Comparison,CompPost)


    # if((ecount(difference(graph[[1]],graphBeginning[[1]]))+
    #    ecount(difference(graph[[2]],graphBeginning[[2]]))+
    #    ecount(difference(graph[[3]],graphBeginning[[3]])))==0
    #    )
    # { print("TRUE")
    #   }



    ######Ferma il repeat:
    #1) Distanza non decresce:
    if(Comp > CompPost)
    {
      graph <- graphChange

    }else{

      if (verbose) cat("Distance doesn't decrease.\n")
      #print("distance doesn't decrease")
      break
    }

    count <- count+1
    #Save different weights
    #write.csv2(PesoOutput,paste0("PesoOutput",count,".csv"))
    #write.csv2(PesiEgoOutput,paste0("PesiEgoOutput",count,".csv"))

    #2) massimo numero di itarazioni
    if( count > nitermax )
    {
      if (verbose) cat("Maximum iteration.\n")
      #print("maximum iteration")
      break
    }


    #3) arriva alla tolerance
    if( CompPost < tolerance )
    {
      break
    }







  }#closes the repeat function



  ###### Function Consensus
  Mat <- vector(mode = "list", length = length(graph))
  for (z in 1:length(graph))
  {
    Mat[[z]] <- as.matrix(igraph::as_adjacency_matrix(graph[[z]], attr="weight"))
  }

  matrixMean <- matrix(0, nrow=dim(Mat[[1]])[1], ncol=dim(Mat[[1]])[1])

  for (i in 1:dim(Mat[[1]])[1])
  {
    for(j in 1:dim(Mat[[1]])[2]){

      vect <- NULL
      for(k in 1:length(Mat))
      {
        Weig <- c(Mat[[k]][i,j])
        vect <- c(vect,Weig)
      }

      matrixMean[i,j] <- mean(vect)


    }
  }
  matrix <- as.matrix(matrixMean)
  matrix[matrix < threshold] <- 0

  ###Reassign names to the nodes
  if(length(rownames(adjL[[1]]))>0)
  {
    rownames(matrix) <- rownames(adjL[[1]])
    colnames(matrix) <- colnames(adjL[[1]])
  }
  graphConsensus <- igraph::graph_from_adjacency_matrix(matrix, mode = "upper",
                                                diag = FALSE, weighted = TRUE)




  ##Reassign names to the nodes in similarity graphs
  if(length(rownames(adjL[[1]]))>0)
  {
  for (x in 1:length(graph))
  {
    V(graph[[x]])$name <- rownames(adjL[[1]])
  }
  }


  output <- list( graphConsensus=graphConsensus,
                  Comparison=Comparison,
                  similarGraphs=graph)
  #class(output) <- "INet"
  return(output)
}



