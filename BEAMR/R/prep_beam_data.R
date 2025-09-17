#' Prepare data for BEAM analysis
#'
#' @param main.data A data.frame
#' @param mtx.data A list, each element is a matrix
#' @param mtx.anns A list, each element is a data.frame
#' @param set.data A data.frame with columns set.id, mtx.id, row.id
#' @param set.anns A data frame with set.id and other columns
#' @param n.boot Number of bootstraps
#' @param seed Initial seed for random number generation
#'
#' @returns A beam.data object, which is a list with main.data, mtx.data, mtx.anns, anns.mtch, set.data, set.anns, and boot.index
#' @export
#'
#' @examples
#' data(clinf)
#' data(omicdat)
#' data(omicann)
#' data(setdat)
#' test.beam.data <- prep_beam_data(main.data=clinf, mtx.data=omicdat,
#'                                  mtx.anns=omicann, set.data=setdat,
#'                                  set.anns=NULL, n.boot=10, seed=123)
prep_beam_data=function(main.data,
                        mtx.data,
                        mtx.anns=NULL,
                        set.data=NULL,
                        set.anns=NULL,
                        n.boot=1000,
                        seed=NULL)

{
  #############################################
  # Some data checking
  message(paste0("Checking inputs: ",date()))

  if(!inherits(main.data, "data.frame"))
    stop("main.data must be a data.frame")

  # create names for mtx.data if none were provided
  n.mtx=length(mtx.data)
  if (is.null(names(mtx.data)))
    names(mtx.data)=paste0("mtx",1:n.mtx)
  mtx.names=names(mtx.data)

  message(paste0("  Checking that each element of mtx.data is a matrix: ",date()))
  ok.mtx=check_list_class(mtx.data,"matrix")
  if (!ok.mtx)
    stop("mtx.data must be a list with a matrix for each element.")

  if (!is.null(mtx.anns))
  {
    message(paste0("  Checking that each element of mtx.anns is a data.frame: ",date()))
    ok.mtx.anns=check_list_class(mtx.anns,"data.frame")
    if (!ok.mtx.anns)
      stop("mtx.anns must be a list with a data.frame for each element.")
  }


  #################################################
  # Match mtx.data with main.data
  message(paste0("Aligning main.data with each mtx.data: ",date()))

  for (i in 1:n.mtx)
  {
    message(paste0("  Working on mtx.data ",mtx.names[i]," (",i," of ",n.mtx,"): ",date()))
    mtx.clms=colnames(mtx.data[[i]])
    main.data[,paste0(mtx.names[i],".clm")]=get_id_index(main.data,mtx.clms)
  }

  some.omic=rowSums(!is.na(main.data[,paste0(mtx.names,".clm")]))>0
  if (any(!some.omic))
  {
    warning("Some rows of main.data not linked to any data.mtx and will be dropped.")
    main.data=main.data[some.omic,]
  }

  #########################################
  # Match mtx.data with mtx.anns

  anns.mtch=NULL
  if (!is.null(mtx.anns))
  {
    message(paste0("  Working on mtx.anns: ",date()))
    anns.mtch=cbind.data.frame(mtx.data=mtx.names,
                               mtx.anns="",
                               id.clm=NA,
                               nrow.mtx=NA,
                               nrow.ann=NA,
                               nrow.map=NA)

    n.anns=length(mtx.anns)
    if (is.null(names(mtx.anns)))
      names(mtx.anns)=paste0("mtx.ann",1:n.anns)

    anns.names=names(mtx.anns)
    for (i in 1:n.mtx)
    {
      message(paste0("  Matching matrix ",i," with annotations: ",date()))
      mtx.rows=rownames(mtx.data[[i]])
      anns.mtch[i,"nrow.mtx"]=length(mtx.rows)
      n.mtch=rep(NA,n.anns)
      for (j in 1:n.anns)
      {
        best.mtch=get_id_index(mtx.anns[[j]],mtx.rows,warn=FALSE)
        n.mtch[j]=sum(!is.na(best.mtch))
      }
      if (any(n.mtch)>0)
      {
        best.ann=which.max(n.mtch)
        anns.mtch[i,"mtx.anns"]=anns.names[best.ann]
        anns.mtch[i,"id.clm"]=find_id_clm(mtx.anns[[best.ann]],mtx.rows)
        anns.mtch[i,"nrow.ann"]=nrow(mtx.anns[[best.ann]])
        anns.mtch[i,"nrow.map"]=max(n.mtch)
      }
    }
  }

  ######################################
  # Check set data and set annotations

  if (!is.null(set.anns))
  {
    message(paste0("  Checking set annotations: ",date()))
    if(!inherits(set.anns,"data.frame"))
      stop("set.anns must be a data.frame.")

    set.id.clm=any(is.element(colnames(set.anns),"set.id"))

    if (!set.id.clm)
      stop("set.anns should have a column named 'set.id'.")


    dup.set=duplicated(set.anns$set.id)
    if (any(dup.set))
      stop("No duplicate set.id allowed in set.anns.")
  }

  ##########################################
  # Check the set data

  if (!is.null(set.data))
  {
    message(paste0("  Checking set.data: ",date()))
    if(!inherits(set.data, "data.frame"))
      stop("set.data must be a data.frame.")

    if (any(!is.element(c("set.id","mtx.id","row.id"),
                        colnames(set.data))))
      stop("set.data must have columns named 'set.id', 'mtx.id', and 'row.id'.")


    message(paste0("    Ordering and indexing set.data: ",date()))
    ord=order(set.data$set.id,
              set.data$mtx.id)

    set.data=set.data[ord,]
    a=nrow(set.data)

    new.sect=which((set.data$set.id[-1]!=set.data$set.id[-a])|
                     (set.data$mtx.id[-1]!=set.data$mtx.id[-a]))
    row.start=c(1,new.sect+1)

    set.index=cbind.data.frame(row.start=row.start,
                               row.end=c(new.sect,a),
                               mtx.id=set.data$mtx.id[row.start])
    nsect=nrow(set.index)
    ok.set=rep(NA,a)
    for (i in 1:nsect)
    {
      if (((i-1)%%1000)==0)
        message(paste0("    Checking section ",i," of ",nsect," of set.data: ",date()))
      row.start=set.index$row.start[i]
      row.end=set.index$row.end[i]
      row.index=row.start:row.end
      mtx.id=set.index$mtx.id[i]
      row.ids=rownames(mtx.data[[mtx.id]])
      ok.set[row.index]=is.element(set.index$mtx.id[i],mtx.names)&
        is.element(set.data$row.id[row.index],row.ids)
    }

    if (any(!ok.set))
      warning("Some set assignments are invalid and will be removed.")

    set.data=set.data[ok.set,]

  }

  ###############################################
  # Generate bootstrap index matrix

  message(paste0("Generating bootstrap index matrix: ",date()))
  n=nrow(main.data)
  set.seed(seed)
  n.subj=nrow(main.data)
  boot.index=replicate(n.boot,sample(n,replace=TRUE))
  boot.index=t(boot.index)
  boot.index=rbind(1:n,boot.index)

  ##############################################
  # Package and return results

  message(paste0("Packaging and returning result: ",date()))
  res=list(main.data=main.data,
           mtx.data=mtx.data,
           mtx.anns=mtx.anns,
           anns.mtch=anns.mtch,
           set.data=set.data,
           set.anns=set.anns,
           boot.index=boot.index)

  class(res)="beam.data"

  return(res)
}
