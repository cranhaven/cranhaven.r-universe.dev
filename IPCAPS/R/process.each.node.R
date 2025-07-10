#' (Internal function) Perform the iterative process for each node
#'
#' @param node An integer representing the current node number which is being
#' analyzed.
#' @param work.dir A working directory.
#'
#' @return \code{NULL}
#'
#' @include clustering.R
#'
process.each.node <- function(node,work.dir){
  start.time <- Sys.time()

  label <- NULL
  raw.data <- NULL
  threshold <- NULL
  min.fst <- NULL
  method <- NULL
  min.in.group <- NULL
  datatype <- NULL
  nonlinear <- NULL
  result.dir <- NULL

  cat(paste0("Node ",node,": Start the process\n"))

  #load tree list, no need to lock file, just reading info from the file
  file.name = file.path(work.dir,"RData","condition.RData")
  load(file=file.name)
  file.name = file.path(work.dir,"RData","tree.RData")
  cat(paste0("Node ",node,": Loading ",file.name,"\n"))
  load(file=file.name)

  which.row = which(tree$node==node)
  #Status -1 = deleted node due to error
  if (tree$status[which.row[1]] != -1){

    #load experiment condition
    file.name = file.path(work.dir,"RData","condition.RData")
    cat(paste0("Node ",node,": Loading ",file.name,"\n"))
    load(file=file.name)


    #load raw data
    file.name = file.path(work.dir,"RData","rawdata.RData")
    cat(paste0("Node ",node,": Loading ",file.name,"\n"))
    load(file=file.name)
    ref.label = label

    #load node data
    file.name = file.path(work.dir,"RData",paste0("node",node,".RData"))
    cat(paste0("Node ",node,": Loading ",file.name,"\n"))
    load(file=file.name)
    ref.index = index

    dataframe = list("raw.data"=raw.data[ref.index,],"label"=ref.label[ref.index],"index"=ref.index)

    cat(paste0("Node ",node,": Check for splitting\n"))
    res = clustering(dataframe,node,work.dir,threshold,min.fst,method,min.in.group,datatype,nonlinear)
    #list.new.node = data.frame()
    cat(paste0("Node ",node,": Return status ",res$status,"\n"))
    if (res$status == 0){
      cat(paste0("Node ",node,": Split to sub-nodes\n"))

      for (i in 1:length(res$new.index)){
        file.name = file.path(work.dir,"RData","tree.RData")
        load(file=file.name)
        new.node = max(tree$node) + 1
        #list.new.node = rbind(list.new.node,c('node'=new.node,'parent.node'=node,"status"=0))
        tree = rbind(tree, c('node'=new.node,'parent.node'=node,"status"=0))
        save(tree,file=file.name, compress = 'bzip2')

        index = res$new.index[[i]]
        file.name = file.path(result.dir,"RData",paste0("node",new.node,".RData"))
        cat(paste0("Node ",node,": Saving ",file.name,"\n"))
        save(index,file=file.name, compress = 'bzip2')
      }


      #load tree list
      file.name = file.path(work.dir,"RData","tree.RData")
      cat(paste0("Node ",node,": Loading ",file.name,"\n"))
      load(file=file.name)

      which.row = which(tree$node==node)
      #Status -1 = deleted node due to error
      if (tree$status[which.row[1]] != -1){
        tree$status[which.row[1]] = 2

        #colnames(list.new.node) = c('node','parent.node','status')
        #tree = rbind(tree,list.new.node)

        file.name = file.path(result.dir,"RData","tree.RData")
        cat(paste0("Node ",node,": Updating ",file.name,"\n"))
        save(tree,file=file.name, compress = 'bzip2')
      }


    }else if (res$status == 1){
      #case of status = 5, no split, stopping criteria are met
      cat(paste0("Node ",node,": No split was performed, Status=1\n"))

      file.name = file.path(result.dir,"RData","leafnode.RData")
      load(file=file.name)
      leaf.node = c(leaf.node,node)
      cat(paste0("Node ",node,": Updating ",file.name,"\n"))
      save(leaf.node,file=file.name, compress = 'bzip2')


      #load tree
      file.name = file.path(work.dir,"RData","tree.RData")
      cat(paste0("Node ",node,": Loading ",file.name,"\n"))
      load(file=file.name)

      which.row = which(tree$node==node)
      tree$status[which.row[1]] = 2

      file.name = file.path(result.dir,"RData","tree.RData")
      cat(paste0("Node ",node,": Updating ",file.name,"\n"))
      save(tree,file=file.name, compress = 'bzip2')


    }
  }

  end.time = Sys.time()
  cat(paste0("Node ",node,": Done!\n"))
  print(end.time - start.time)

  invisible(NULL)
}



