evalFunc<-function(Y,Indices,TAll, measure, NTrees){


  N<-NROW(Y)
  alpha<-0.25 #1 is another option

  criterion<-c()

  if(measure==0){ #Y continuous


    for(i in 1:NTrees){
      Tree<-TAll[[i]]
      TerminalNodes<-which(Tree[,1]==0)

      if(NCOL(Y)==1 || NROW(Y)==1){ #Y is 1-dimensional
        ordY<-c()
        predY<-c()
        for(j in 1:length(TerminalNodes)){
          ordY<-c(ordY,Y[Indices[[i]][[j]]])
          TermNode<-TerminalNodes[j]
          predY<-c(predY,rep(Tree[TermNode,7],Tree[TermNode,6]))
        }

        SSq<-sum((ordY-predY)^2)
        loss<-N*log(SSq)
        complexity<-alpha*4*(length(TerminalNodes)+1)*log(N)

        criterion[i]<-loss+complexity


      }else{ #Y is multidimensional

        criterion<-NULL
        warning("Evaluation function is not implemented for multiple Y variables yet")
        #MISSING

      }




    }



  }else{ #Y discrete



    for(i in 1:NTrees){
      Tree<-TAll[[i]]
      TerminalNodes<-which(Tree[,1]==0)

      if(NCOL(Y)==1 || NROW(Y)==1){ #Y is 1-dimensional

        ordY<-c()
        predY<-c()
        for(j in 1:length(TerminalNodes)){
          ordY<-c(ordY,Y[Indices[[i]][[j]]])
          TermNode<-TerminalNodes[j]
          predY<-c(predY,rep(Tree[TermNode,7],Tree[TermNode,6]))
        }

        Indicator<-ordY==predY

        loss<-2*sum(Indicator)
        complexity<-alpha*(length(TerminalNodes))*log(N)

        criterion[i]<-loss+complexity

      }else{ #Y is multidimensional


        criterion<-NULL
        warning("Evaluation function is not implemented for multiple Y variables yet")
        #Missing


      }

    }




  }


  return(criterion)


}
