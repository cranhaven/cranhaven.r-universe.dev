#' For each row of the data.frame main.data, find the index of the matching element in vector ids
#'
#' @param mtch.data A data.frame to be linked with the ids
#' @param ids A vector of ids to be linked in mtch.data
#' @param warn A logical value whether to include warnings with results
#'
#' @returns A data.frame with matching id index
#' @export
#'
#' @examples
#' data(clinf)
#' data(omicdat)
#' mtx.clms <- colnames(omicdat[[1]])
#' id_index <- get_id_index(clinf,mtx.clms)
get_id_index=function(mtch.data,
                      ids,       #
                      warn=TRUE)

{
  if (any(duplicated(ids)))
    stop("Each id must be unique.")

  if (any(is.na(ids))&&warn)
  {
    ids=ids[!is.na(ids)]
    warning("NAs removed from ids.")
  }

  if (any(ids=="")&&warn)
  {
    ids=ids[ids!=""]
    warning("Blanks removed from ids.")
  }

  mtch.data=cbind.data.frame(row.names=rownames(mtch.data),
                             mtch.data)

  # reduce length of ids to compare if very long
  id.mtch.index=(1:length(ids))
  if (length(id.mtch.index)>1000)
    id.mtch.index=sample(length(ids),1000,replace=FALSE)

  #mtch.mtx=as.data.frame(t(apply(mtch.data,2,
  #                               is.element,ids[id.mtch.index]))) # check each column of mtch.data for matches
  mtch.mtx <- apply(mtch.data, 2, is.element, ids[id.mtch.index])
  if(is.matrix(mtch.mtx)){
    n.mtch=colSums(mtch.mtx)# compute number of matches
  }
  else{
    n.mtch=colSums(matrix(mtch.mtx)) # compute number of matches
  }
  mtch.clm=which.max(n.mtch)                    # find column with most matches

  ids=as.character(ids)
  mtch.data[,mtch.clm]=as.character(mtch.data[,mtch.clm])

  mtch.id=cbind.data.frame(id=mtch.data[,mtch.clm],
                           mtch.row=1:nrow(mtch.data))

  id.index=cbind.data.frame(id=ids,
                            id.indx=1:length(ids))

  id.mtch=base::merge(mtch.id,id.index,
                      by="id",
                      all.x=TRUE,all.y=FALSE)

  ord=order(id.mtch$mtch.row)
  id.mtch=id.mtch[ord,]

  if (nrow(id.mtch)!=nrow(mtch.id))
    stop("Error in merging data.")

  ok=(id.mtch$id==mtch.id$id)
  na.ok=is.na(ok)
  ok[na.ok]=(is.na(id.mtch$id[na.ok])&is.na(mtch.id$id[na.ok]))
  if (any(!ok))
    stop("Error in merging data.")

  res=id.mtch$id.indx

  if (all(is.na(res))&&warn)
    warning("No ids matched; returning all NAs.")

  if (any(is.na(res))&&warn)
    warning("Some ids not matched; returning NAs for those.")

  return(res)
}

