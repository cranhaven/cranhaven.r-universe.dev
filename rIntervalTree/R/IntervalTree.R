#' @import methods
NULL

## data documentation
#' Compounds with mass ranges
#'
#' A dataset containing the the mass ranges of chemical compounds with a 10 ppm tolerance.
#' The variables are as follows.
#'
#' \itemize{
#'   \item name. name of the compound
#'   \item low. low bound of the mass
#'   \item high. high bound of the mass
#' }
#'
#' @docType data
#' @keywords datasets
#' @name compounds
#' @usage data(compounds)
#' @format A data frame with 23 rows and 3 variables
NULL


#' IntervalTree
#'
#' A S4 class to represent interval tree.
#' @slot data a dataframe providing the intervals to be stored in the interval tree. The columns are key, start and end of intervals
#' @slot root the root list of the interval tree built upon the data
#' @include Interval.R
#' @export IntervalTree
#' @exportClass IntervalTree


IntervalTree <- setClass(
  "IntervalTree",
  slots = c(
    data = "data.frame",
    root = "list"
  ),
  prototype=list(
    data = data.frame(),
    root = list()

  ),
  validity=function(object)
  {
    if(dim(object@data)[1] < 1 | dim(object@data)[2] != 3) {
      stop("Invalid data input. Require a data frame with 3 columns")
    }
    return(TRUE)
  }
)


setMethod("initialize", "IntervalTree",
          function(.Object, ...) {
            .Object <- callNextMethod()

            if(dim(.Object@data)[1] >= 1 & dim(.Object@data)[2] == 3){
              colnames(.Object@data) <- c("key", "start", "end")
              .Object <- buildTree(.Object)
            }
            .Object
          })

#' treeFromInterval
#'
#' Method for constructing interval tree for a list of Interval objects. A node in the tree is a list object.
#' As the leftChild and rightChild of each node are nodes themselves, a binary interval tree stored in a
#' recursive list will be produced if this function is executed successfully.
#'
#' @param interval_list a list of Interval objects
#' @return a list object representing a binary interval tree
#'
#' @examples
#' i1 <- new("Interval", start=1.1,end=1.2, key="dummy1")
#' i2 <- new("Interval", start=-1.1,end=1.2, key="dummy2")
#' i3 <- new("Interval", start=-10.1,end=-1.2, key="dummy3")
#' i4 <- new("Interval", start=-1.1,end=1.2, key="dummy4")
#' i5 <- new("Interval", start=-10,end=2, key="dummy5")
#' i6 <- new("Interval", start=-8,end=-5, key="dummy6")
#' myList <- list(i1, i2, i3, i4, i5, i6)
#' atree <- treeFromInterval(myList)
#' @export treeFromInterval

treeFromInterval <- function(interval_list){
  #print("making a tree")
  first_interval <- interval_list[[1]]
  min_start <- first_interval@start
  max_end <- first_interval@end
  for(i in 1:length(interval_list)){
    anInterval <- interval_list[[i]]
    if(anInterval@start < min_start){
      min_start <- anInterval@start
    }
    if(anInterval@end > max_end){
      max_end <- anInterval@end
    }
  }
  center <- (max_end + min_start)/2


  center_list <- list()
  left_list <- list()
  right_list <- list()

  for(i in 1:length(interval_list)){
    anInterval <- interval_list[[i]]
    if(anInterval@start > center){
      right_list <- c(right_list, anInterval)
    }else if(anInterval@end < center){
      left_list <- c(left_list, anInterval)
    }else{
      center_list <- c(center_list, anInterval)
    }
  }

  tree <- list()
  tree$center <- center
  tree$left_bound <- min_start
  tree$right_bound <- max_end
  tree$intervals <- center_list

  if(length(left_list) > 0){
    tree$leftChild <- treeFromInterval(left_list)
  }else{
    tree$leftChild <- list()
  }
  if(length(right_list) > 0){
    tree$rightChild <- treeFromInterval(right_list)
  }else{
    tree$rightChild <- list()
  }
  return(tree)
}

setGeneric("treeFromInterval")


#' intersectInterval
#'
#' Method for searching the interval tree. Given a single number or an ordered pair of numbers denoting the start and end of an interval,
#' all intervals that overlapping the query interval in the interval tree will be retrieved.
#'
#' @param aTree a list object representing an interval tree
#' @param someNumbers a vector of one or two numbers to test for overlap. If two numbers are provided, they are treated as
#' an interval (start, end).
#' @return a list of vectors. Each vector contains (name, start, end) of an interval
#'
#' @examples
#' i1 <- new("Interval", start=1.1,end=1.2, key="dummy1")
#' i2 <- new("Interval", start=-1.1,end=1.2, key="dummy2")
#' i3 <- new("Interval", start=-10.1,end=-1.2, key="dummy3")
#' i4 <- new("Interval", start=-1.1,end=1.2, key="dummy4")
#' i5 <- new("Interval", start=-10,end=2, key="dummy5")
#' i6 <- new("Interval", start=-8,end=-5, key="dummy6")
#'
#' myList <- list(i1, i2, i3, i4, i5, i6)
#' atree <- treeFromInterval(myList)
#' \dontrun{
#' intersectInterval(atree, c(-16, -26)) # generate an error
#' }
#' intersectInterval(atree, c(1, 5))
#' intersectInterval(atree, c(-12, 15))
#' intersectInterval(atree, 0)
#' @export intersectInterval


intersectInterval <- function(aTree, someNumbers){
  if(length(aTree) == 0){
    return(NULL)
  }
  if(length(someNumbers) == 1){
    low <- someNumbers
    high <- someNumbers
  }else if(length(someNumbers) == 2){
    low <- someNumbers[1]
    high <- someNumbers[2]
    if(high < low){
      stop("invalid input. The first number must be smaller than the second number.")

    }
  }else{
    stop("invalid input. Requires 1 or 2 numbers.")
  }

  if(low > aTree$right_bound | high < aTree$left_bound){
    return(NULL)
  }

  #print(paste("searching", aTree$left_bound, aTree$right_bound))
  results <- list()
  centerIntervals <- aTree$intervals
  if(length(centerIntervals) > 0){
    for(i in 1:length(centerIntervals)){
      anInterval <- centerIntervals[[i]]
      if(isOverlap(anInterval, someNumbers)){
        res <- c(anInterval@key, anInterval@start, anInterval@end)
        results[[length(results)+1]] <- res
      }
    }
  }
  if(low < aTree$center){
    left_results <- intersectInterval(aTree$leftChild, someNumbers)
    results <- c(results, left_results)
  }
  if(high > aTree$center){
    right_results <- intersectInterval(aTree$rightChild, someNumbers)
    results <- c(results, right_results)
  }
  return(results)
}


setGeneric("intersectInterval")

#' collectIntervals
#'
#' Method for enumerating all intervals in a interval tree (a list object).
#'
#' @param aTree a recursive list storing Interval object
#' @return a flattened list of Interval object
#'
#' @examples
#' i1 <- new("Interval", start=1.1, end=1.2, key="dummy1")
#' i2 <- new("Interval", start=-1.1, end=1.2, key="dummy2")
#' i3 <- new("Interval", start=-10.1, end=-1.2, key="dummy3")
#' i4 <- new("Interval", start=-1.1, end=1.2, key="dummy4")
#' i5 <- new("Interval", start=-10, end=2, key="dummy5")
#' i6 <- new("Interval", start=-8, end=-5, key="dummy6")
#' myList <- list(i1, i2, i3, i4, i5, i6)
#' atree <- treeFromInterval(myList)
#' collectIntervals(atree)
#' collectIntervals(list())
#' @export collectIntervals

collectIntervals <- function(aTree){
  #if(length(aTree == 0)){
  #  return(NULL)
  #}
  #aTree <- atree
  collection <- list()
  if(length(aTree$intervals) > 0){
    for(i in 1:length(aTree$intervals)){
      collection[[length(collection)+1]] <- aTree$intervals[[i]]
    }
  }

  if(length(aTree$leftChild) > 0){
    leftCollect <- collectIntervals(aTree$leftChild)
    if(length(leftCollect) > 0){
      for(i in 1:length(leftCollect)){
        collection[[length(collection)+1]] <- leftCollect[[i]]
      }
    }
  }
  if(length(aTree$rightChild) > 0){
    rightCollect <- collectIntervals(aTree$rightChild)
    if(length(rightCollect) > 0){
      for(i in 1:length(rightCollect)){
        collection[[length(collection)+1]] <- rightCollect[[i]]
      }
    }
  }
  return(collection)
}
setGeneric("collectIntervals")

#' buildTree
#'
#' Method for building a binary interval tree given an IntervalTree object with a defined data slot but an undefined root slot.
#' This method is a wrapper function of the treeFromInterval function. In the first step, the dataframe in the data slot is
#' converted into a list of Interval objects. Then, the treeFromInterval function is called to construct an interval tree using
#' the list as an input, and the root of the resulting interval tree is assigned to the root slot of the IntervalTree object.
#' This method is called implicitly when an IntervalTree object is initialized with a non-empty dataframe.
#'
#' @param theObject an IntervalTree object containing a non-empty dataframe.
#' @return an IntervalTree object, with the root being an recursive list of Intervals.
#' @examples
#' m_ranges <- data.frame(c("A", "B", "C", "D"), c(1,2,3,4), c(5,4,6,10))
#' I <- new("IntervalTree")
#' I@data <- m_ranges
#' m_interval_tree <- buildTree(I)
#' ## buildTree is called implicitly
#' II <- IntervalTree(data=m_ranges, root=list())
#' ## buildTree is called implicitly
#' m_interval_tree <- new("IntervalTree", data=m_ranges, root=list())
#' @export buildTree
#' @exportMethod buildTree
#'
setGeneric(name="buildTree",
           def=function(theObject)
           {
             standardGeneric("buildTree")
           }
)


#' buildTree
#'
#' Method for building a binary interval tree given an IntervalTree object with a defined data slot but an undefined root slot.
#' This method is a wrapper function of the treeFromInterval function. In the first step, the dataframe in the data slot is
#' converted into a list of Interval objects. Then, the treeFromInterval function is called to construct an interval tree using
#' the list as an input, and the root of the resulting interval tree is assigned to the root slot of the IntervalTree object.
#' This method is called implicitly when an IntervalTree object is initialized with a nonempty dataframe.
#'
#' @param theObject an IntervalTree object containing a non-empty dataframe.
#' @return an IntervalTree object, with the root being an recursive list of Intervals.
#' @examples
#' m_ranges <- data.frame(c("A", "B", "C", "D"), c(1,2,3,4), c(5,4,6,10))
#' I <- new("IntervalTree")
#' I@data <- m_ranges
#' m_interval_tree <- buildTree(I)
#' ## buildTree is called implicitly
#' II <- IntervalTree(data=m_ranges, root=list())
#' ## buildTree is called implicitly
#' m_interval_tree <- new("IntervalTree", data=m_ranges, root=list())
#' @export buildTree
#' @exportMethod buildTree

setMethod(f="buildTree",
          signature="IntervalTree",
          definition=function(theObject)
          {
            #print("Building interval tree")
            if(dim(theObject@data)[1] < 1 | dim(theObject@data)[2] != 3){
              stop("the input dataframe is not valid")
            }
            colnames(theObject@data) <- c("key", "start", "end")
            input <- theObject@data

            intervals_from_input <- list()
            for(i in 1:dim(input)[1]){
              intervals_from_input[[i]] <- new("Interval", start=input[i,2], end=input[i,3], key=as.character(input[i,1]))
            }

            theObject@root <- treeFromInterval(intervals_from_input)
            return(theObject)
          }
)

#' overlapQuery
#'
#' Method for searching an IntervalTree object. Given a number or an ordered pair of numbers denoting the
#' start and end of an interval, all intervals that overlapping the query interval in the IntervalTree
#' object will be retrieved.
#' @param theObject an IntervalTree object
#' @param anInterval a vector of one or two numbers to check overlap, if two numbers are provided,
#' they are treated as an interval (start, end).
#' @return a list of vectors. Each vector contains information about an interval (name, start, end).
#' @examples
#' m_ranges <- data.frame(c("A", "B", "C", "D"), c(-1.1,2,3,4), c(5,4,6,10))
#'
#' m_interval_tree <- new("IntervalTree", data=m_ranges, root=list())
#' overlapQuery(m_interval_tree, 4)
#' res <- overlapQuery(m_interval_tree, c(2.5,7))
#' res
#' @export overlapQuery
#' @exportMethod overlapQuery

setGeneric(name="overlapQuery",
           def=function(theObject, anInterval)
           {
             standardGeneric("overlapQuery")
           }
)

#' overlapQuery
#'
#' Method for searching an IntervalTree object. Given a number or an ordered pair of numbers denoting the
#' start and end of an interval, all intervals that overlapping the query interval in the IntervalTree
#' object will be retrieved.
#'
#' @param theObject an IntervalTree object
#' @param anInterval a vector of one or two numbers to check overlap, if two numbers are provided,
#' they are treated as an interval (start, end).
#' @return a list of vectors. Each vector contains information about an interval (name, start, end).
#' @examples
#' m_ranges <- data.frame(c("A","B","C","D","E","F"), c(-1.1,2,3,4,20,200), c(5,4,6,10,21.2,400))
#'
#' m_interval_tree <- new("IntervalTree", data=m_ranges, root=list())
#' overlapQuery(m_interval_tree, 4)
#' res <- overlapQuery(m_interval_tree, c(2.5,7))
#' res
#' @export overlapQuery
#' @exportMethod overlapQuery

setMethod(f="overlapQuery",
          signature=c("IntervalTree", "numeric"),
          definition=function(theObject, anInterval)
          {
            #print("Searching interval tree")
            rootTree <- theObject@root


            if(length(anInterval) == 1){
              sta <- anInterval
              end <- anInterval
            }else if(length(anInterval) == 2){
              sta <- anInterval[1]
              end <- anInterval[2]
              if(end < sta){
                warning("invalid input. Number 1 must be smaller than number 2.")
                return(NULL)
              }
            }else{
              warning("invalid input. Requires 1 or 2 numbers.")
              return(NULL)
            }

            overlaping_results <- intersectInterval(rootTree, anInterval)
            return(overlaping_results)
          }
)

#' insert
#'
#' Method for inserting an Interval into an interval tree (a recursive list). The structure of the final
#' tree is invariant of the order of insertion. Tree balancing is not implemented, therefore, the
#' resulting tree may not be balanced.
#'
#' @param aTree an interval tree (a list object)
#' @param anInterval an Interval object
#' @return a list object, representing a binary interval tree
#'
#' @examples
#' i1 <- new("Interval", start=1.1,end=1.2, key="dummy1")
#' i2 <- new("Interval", start=-1.1,end=1.2, key="dummy2")
#' i3 <- new("Interval", start=-10.1,end=-1.2, key="dummy3")
#' i4 <- new("Interval", start=-1.1,end=1.2, key="dummy4")
#' i5 <- new("Interval", start=-10,end=2, key="dummy5")
#' i6 <- new("Interval", start=-8,end=-5, key="dummy6")
#' i7 <- new("Interval", start=80,end=100, key="dummy7")
#' i8 <- new("Interval", start=-80,end=-15, key="dummy8")
#'
#' atree <- list()
#' atree <- insert(atree, i1)
#' atree <- insert(atree, i2)
#' atree <- insert(atree, i3)
#' atree <- insert(atree, i4)
#' atree <- insert(atree, i5)
#' atree <- insert(atree, i6)
#' atree <- insert(atree, i7)
#' atree <- insert(atree, i8)
#'
#' intersectInterval(atree, 85)
#' intersectInterval(atree, 0)
#' intersectInterval(atree, c(-70, -9))
#' \dontrun{
#' intersectInterval(atree, c(80,0)) ## generate an error
#' }
#' @export insert

insert <- function(aTree, anInterval){
  #print("inserting an interval into a tree")

  i_start <- anInterval@start
  i_end <- anInterval@end
  if(i_end < i_start){
    warning("invalid input. The start must be smaller than the end. The interval is not inserted!")
    return(NULL)
  }

  if(length(aTree) == 0){
    center_list <- list()
    center_list[[1]] <- anInterval
    aTree$center <- (i_end + i_start)/2
    aTree$left_bound <- i_start
    aTree$right_bound <- i_end
    aTree$intervals <- center_list
    aTree$leftChild <- list()
    aTree$rightChild <- list()


  }else{
    if(aTree$left_bound > i_start){
      aTree$left_bound <- i_start

    }
    if(aTree$right_bound < i_end){
      aTree$right_bound <- i_end
    }

    new_center <- (aTree$left_bound + aTree$right_bound)/2

    if(new_center == aTree$center){
      if(isOverlap(anInterval, aTree$center)){
        aTree$intervals[[length(aTree$intervals)+1]] <- anInterval
      }else if(i_end < aTree$center){
        aTree$leftChild <- insert(aTree$leftChild, anInterval)
      }else{
        aTree$rightChild <- insert(aTree$rightChild, anInterval)
      }
    }else{

      ## as the center is shifted, both existing and the new intervals have to be re-partitioned
      ## the entire tree has to be rebuilt
      ## print("rebuilding the tree")
      aTree$intervals[[length(aTree$intervals)+1]] <- anInterval ## add the new interval to existing ones
      intervalAll <- collectIntervals(aTree)
      aTree <- treeFromInterval(intervalAll)
    }
  }

  return(aTree)
}

setGeneric("insert")

#' insertInterval
#'
#' Method for inserting an interval into an IntervalTree object. Given an ordered pair of numbers denoting the
#' start and end of an interval, the interval is first converted into an Interval object, then the
#' Interval object is inserted by calling the insert() function.
#' @param theObject an IntervalTree object
#' @param anInterval an interval in the form of (name, start, end).
#' @return an IntervalTree object.
#' @examples
#' m_ranges <- data.frame(c("A", "B", "C", "D"), c(-1.1,2,3,4), c(5,4,6,10))
#' m_interval_tree <- new("IntervalTree", data=m_ranges, root=list())
#' m_interval_tree <- insertInterval(m_interval_tree, c("testInterval1", 2, 5))
#' res <- insertInterval(m_interval_tree, c("a",2.5,7))
#' @export insertInterval
#' @exportMethod insertInterval

setGeneric(name="insertInterval",
           def=function(theObject, anInterval)
           {
             standardGeneric("insertInterval")
           }
)

#' insertInterval
#'
#' Method for inserting an interval into an IntervalTree object. Given an ordered pair of numbers denoting the
#' start and end of an interval, the interval is first converted into an Interval object, then the
#' Interval object is inserted by calling the insert() function.
#' @param theObject an IntervalTree object
#' @param anInterval an interval in the form of (name, start, end).
#' @return an IntervalTree object.
#' @examples
#' m_ranges <- data.frame(c("A", "B", "C", "D"), c(-1.1,2,3,4), c(5,4,6,10))
#' m_interval_tree <- new("IntervalTree", data=m_ranges, root=list())
#' m_interval_tree <- insertInterval(m_interval_tree, c("testInterval2", 200, 500))
#' res <- insertInterval(m_interval_tree, c("a",-25,7))
#' @export insertInterval
#' @exportMethod insertInterval
#'
setMethod(f="insertInterval",
  signature=c("IntervalTree", "character"),
  definition=function(theObject, anInterval)
  {
    #print("Inserting into interval tree")
    if(length(anInterval) != 3){
      stop("Invalid input, the name, start and end of the interval must be provided. The interval tree is not modified")

    }else if(as.numeric(anInterval[2]) > as.numeric(anInterval[3])){
      stop("invalid input. The start must be smaller than the end. The interval tree is not modified")
    }else{
      names(anInterval) <- colnames(theObject@data)
      theObject@data <- rbind(theObject@data, t(anInterval))

      anInt <- new("Interval", key=anInterval[1], start=as.numeric(anInterval[2]), end=as.numeric(anInterval[3]))
      theObject@root <- insert(theObject@root, anInt)
    }
    return(theObject)
  }
)



