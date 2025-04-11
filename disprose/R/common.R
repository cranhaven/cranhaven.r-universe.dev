#'
#'Store data in SQLite database
#'
#'Write, read and delete tables from SQLite database.
#'
#'@param database character; SQLite database name and path.
#'@param data data frame that should be stored as database table.
#'@param table character; table name.
#'@param overwrite logical; use \code{overwrite = TRUE} if you want to overwrite a table that already exists in database
#'@param append logical; append rows to table
#'@param index.unique logical; vector of indicators to create unique or not unique indexes
#'@param index.column.name vector of indexed columns' names
#'@param verbose logical; show messages
#'@param choose.columns logical; return chosen columns only
#'@param column.names character; vector of name of columns that are chosen to be returned
#'@param select logical; return only rows that contain selected values in one column
#'@param select.column.name character; name of column that contains selected values
#'@param select.val vector of values that define rows that should be returned
#'@param unique logical; delete duplicated rows
#'
#'@details
#'This functions help to store big data frames in SQLite database which makes it faster to save and read the data.
#'
#'This function creates SQLlite connection to database, fulfills the task and then disconnects.
#'If no database has been created yet, creates one.
#'
#'Do not use \code{overwrite = TRUE} if table does not exists.
#'Do not use \code{append = TRUE} and \code{overwrite = TRUE} at the same time, no append is possible while overwriting.
#'
#'If multiple indexes are created in one table, they are unrelated.
#'
#'Do not use dots in data frame character variables, use underscore.
#'
#'Parameters \code{choose.columns=FALSE, column.names, select, select.column.name, select.val, unique} are only used with
#'link{read_from_DB} function. Those parameters define rows and columns that will be returned.
#'
#'@return
#' \code{list_DB} returns character vector of names of database tables.
#'
#'\code{read_from_DB} returns a data frame with the content of SQLite table.
#'
#'@examples
#'mydata <- as.data.frame (matrix(1:10, 2, 5))
#'database <- tempfile()
#'write_to_DB (database, data = mydata, table = "table1", overwrite = FALSE)
#'list_DB (database)
#'mydata2 <- as.data.frame (matrix(11:20, 2, 5))
#'write_to_DB (database, data = mydata2, table = "table1", overwrite = TRUE)
#'mydata3 <- read_from_DB (database, table = "table1")
#'delete_from_DB (database, table = "table1")
#'file.remove (database)
#'
#'# example with reading table with restricted columns and rows.
#'mydata <- data.frame(ids = c(1:6), titles = c("A", "B", "C", "D", "E", "E"),
#'                     other = rep("other", 6))
#'database <- tempfile()
#'write_to_DB (database, data = mydata, table = "table1", overwrite = FALSE)
#'read_from_DB(database, "table1", choose.columns = TRUE, column.names = c("ids", "titles", "other"),
#'             select = TRUE, select.column.name = "ids",  select.val = 3:6, unique = TRUE)
#'read_from_DB(database, "table1", choose.columns = TRUE, column.names = c("titles", "other"),
#'             select = TRUE, select.column.name = "ids",  select.val = 3:6, unique = TRUE)
#'file.remove (database)
#'
#' @author Elena N. Filatova
#' @name store_in_DB
NULL

#' @describeIn store_in_DB Lists all tables from SQLite database
#' @export
list_DB <- function (database) {
  # test package dependencies
  if (!requireNamespace("DBI", quietly = TRUE)) { stop("Package \"DBI\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("RSQLite", quietly = TRUE)) { stop("Package \"RSQLite\" needed for this function to work. Please install it.", call. = FALSE)}
  # run
  conn <- DBI::dbConnect (RSQLite::SQLite (), database)
  on.exit(DBI::dbDisconnect (conn), add=T)
  res <- DBI::dbListTables (conn)
  return (res)
}

#' @describeIn store_in_DB Writes data frame into SQLite database table
#' @export
write_to_DB <- function (database, data, table, overwrite=FALSE, append=FALSE, verbose = TRUE) {
  # test package dependencies
  if (!requireNamespace("DBI", quietly = TRUE)) { stop("Package \"DBI\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("RSQLite", quietly = TRUE)) { stop("Package \"RSQLite\" needed for this function to work. Please install it.", call. = FALSE)}
  # run
  conn <- DBI::dbConnect (RSQLite::SQLite (), database)
  on.exit(DBI::dbDisconnect (conn), add=T)
  names <- DBI::dbListTables (conn) # test if table is already in database
  test <- table %in% names
  if (test == TRUE & append==FALSE & overwrite == FALSE){stop ("Such table already exists in database. Use overwrite = TRUE")}
  if (test == FALSE & overwrite == TRUE){stop ("There is no such table in database. Use overwrite = FALSE")}
  if (overwrite == FALSE) { DBI::dbWriteTable (conn, table, data, append=append)} # simple writing
  if (overwrite == TRUE) { DBI::dbRemoveTable (conn, table); DBI::dbWriteTable (conn, table, data)}
  if (verbose){
    names <- DBI::dbListTables (conn) # test if table is in database
    test <- table %in% names
    if(test==TRUE){message ("table ", table, " is in database ", database)
    } else  {message ("table ", table, " is not in database ", database)} } }
#' @describeIn store_in_DB Creates SQLite indexes in database table
#' @export
index_DB<-function(database, table, index.unique, index.column.name, verbose = TRUE){
  # test package dependencies
  if (!requireNamespace("DBI", quietly = TRUE)) { stop("Package \"DBI\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("RSQLite", quietly = TRUE)) { stop("Package \"RSQLite\" needed for this function to work. Please install it.", call. = FALSE)}
  #check index conditions
  if(length(index.unique)!=length(index.column.name)){stop("index.unique and index.column.name must be same length")}
  #connect
  conn <- DBI::dbConnect (RSQLite::SQLite (), database)
  on.exit(DBI::dbDisconnect (conn), add=T)
  #query
  for (i in 1:length(index.unique)){
    if (index.unique[i]==TRUE){
      query<-c()
      query<-paste("CREATE UNIQUE INDEX ", table, "_", index.column.name[i], "_index ON ", table, "(", index.column.name[i], ")", sep="")} else{
        query<-paste("CREATE INDEX ", table, "_", index.column.name[i], "_index ON ", table, "(", index.column.name[i], ")", sep="")}
    DBI::dbExecute(conn, query)}
  if (verbose) message ("Index ", table, "_", index.column.name, "_index", " is created")
}

#' @describeIn  store_in_DB Reads table from SQLite database and writes it into data frame.
#' @export
read_from_DB <- function (database, table, choose.columns=FALSE, column.names,
                          select=FALSE, select.column.name, select.val, unique = FALSE) {
  # test package dependencies
  if (!requireNamespace("DBI", quietly = TRUE)) { stop("Package \"DBI\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("RSQLite", quietly = TRUE)) { stop("Package \"RSQLite\" needed for this function to work. Please install it.", call. = FALSE)}
  # run
  conn <- DBI::dbConnect (RSQLite::SQLite (), database)
  on.exit(DBI::dbDisconnect (conn), add=T)
  # test if table is in database
  names <- DBI::dbListTables (conn)
  test <- table %in% names
  if (test==FALSE){DBI::dbDisconnect (conn); stop("There is no such table in database")}
  # make query
  #columns
  if (choose.columns == FALSE){columns.note<-"*"} else { columns.note<-paste(column.names, collapse=", ") }
  #select
  if (select == TRUE){
    if (inherits(select.val, "character")){select.val<-paste("\"", select.val, "\"", sep="")} # add quotes for characters
    select.val<-paste(select.val, collapse=", ")
    select.note<-paste("WHERE", select.column.name, "IN (", select.val, ")", sep=" ")} else {select.note<-""}
  #unique
  if (unique==TRUE){distinct.note<-"DISTINCT"} else{distinct.note=""}
  # query and result
  query <- paste ("SELECT", distinct.note, columns.note, "FROM", table, select.note, sep=" ")
  res <- DBI::dbGetQuery (conn, query)
  return (res)
}

#' @describeIn  store_in_DB Deletes table from SQLite database.
#' @export
delete_from_DB <- function (database, table, verbose = TRUE) {
  # test package dependencies
  if (!requireNamespace("DBI", quietly = TRUE)) { stop("Package \"DBI\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("RSQLite", quietly = TRUE)) { stop("Package \"RSQLite\" needed for this function to work. Please install it.", call. = FALSE)}
  # run
  conn <- DBI::dbConnect (RSQLite::SQLite (), database)
  on.exit(DBI::dbDisconnect (conn), add=T)
  names <- DBI::dbListTables (conn) # test if table is in database
  test <- table %in% names
  if (test==FALSE){DBI::dbDisconnect (conn); stop("There is no such table in database")}
  DBI::dbRemoveTable (conn, table)
  if (verbose){
  names <- DBI::dbListTables (conn) # test if table is in database
  test <- table %in% names
  if (test==TRUE) { message ("table ", table, " is in database ", database)
    } else {message ("table ", table, " is not in database ", database)}}
}

#' Delete rows with duplicated values
#'
#' Delete data frame rows if they contain duplicated values.
#'
#'@param data data frame;
#'@param duplicated.var variable that contains duplicated values
#'@param exact logical; values are to be matched as is
#'@param stay character; which row with duplicated values will stay; possible values are \code{"first"} (first of rows),
#'\code{"choose"} (depending of the value of other variable) and \code{"none"} (rows with values that contain pattern will be removed)
#'@param choose.var,choose.stay.val vector of additional variable to choose the preferred row and it's preferred value
#'(used if \code{stay = "choose"})
#'@param pattern deleted pattern (used if \code{stay = "none"})
#'@param mc.cores integer; number of processors for parallel computation (not supported on Windows)
#'@param verbose logical; show messages
#'
#'@details
#'This function checks if there are repeated values in the data frame (in the \code{duplicated.var}).
#'If repeated values are found, the first row with duplicated value stays, others are deleted (if \code{stay = "first"}).
#'If \code{stay = "choose"} the first row with duplicated values and \code{choose.var = choose.stay.val} will stay.
#'If there are no rows with \code{choose.var = choose.stay.val}, the first row will stay.
#'
#'If \code{stay = "none"} all rows with values that contain pattern will be removed.
#'
#'@return Data frame without rows that contain duplicates in \code{duplicated.var}
#'
#'@examples
#'data <- data.frame (N = c(1:5, 11:15), name = c(rep( "A",4), "AA", rep( "B",3), "BB", "C"),
#'                 choose = c(rep(c("yes", "no"), 3), "yes", "yes", "no", "no"))
#'delete_duplicates_DF (data = data, duplicated.var = data$N, exact = TRUE, stay = "first")
#'delete_duplicates_DF (data = data, duplicated.var = data$N, exact = FALSE, stay = "first")
#'delete_duplicates_DF (data = data, duplicated.var = data$name, exact = TRUE, stay = "first")
#'delete_duplicates_DF (data = data, duplicated.var = data$name, exact = TRUE,
#'                     stay = "choose", choose.var = data$choose, choose.stay.val = "yes")
#'delete_duplicates_DF (data = data, duplicated.var = data$name, exact = FALSE, stay = "first")
#'delete_duplicates_DF (data = data, duplicated.var = data$name, exact = FALSE,
#'                     stay = "choose", choose.var = data$choose, choose.stay.val = "yes")
#'delete_duplicates_DF (data =data, duplicated.var = data$name, stay = "none",
#'                     pattern = c("A", "B"), exact = TRUE)
#'delete_duplicates_DF (data =data, duplicated.var = data$name, stay = "none",
#'                     pattern = c("A", "B"), exact = FALSE)
#'
#' @author Elena N. Filatova
#' @name delete_duplicates_DF
#' @export
delete_duplicates_DF <- function (data, duplicated.var, exact = FALSE, stay = "first",
                                  choose.var, choose.stay.val, pattern, mc.cores = 1, verbose = TRUE){
  # test package dependencies
  if (!requireNamespace("parallel", quietly = TRUE)) { stop("Package \"parallel\" needed for this function to work. Please install it.", call. = FALSE)}
  #check stay
  if(stay!="first" & stay!="choose" & stay!="none") {stop("Choose stay method")}
  # stay and choose
  if (stay=="first" | stay=="choose"){
    #get duplicated nums
    get.num.tab<-function(i){
      var<-duplicated.var[i]
      if (exact==FALSE){nums<-grep(var, duplicated.var, fixed=TRUE)}
      if (exact==TRUE){nums<-c(1:length(duplicated.var))[duplicated.var %in% var]}
      num.table<-data.frame(ind=i, num=nums)
      return(num.table)}
    num.tabs<-parallel::mclapply(X=1:length(duplicated.var), FUN=function(x) get.num.tab(i=x), mc.cores = mc.cores)
    num.data<-data.frame(); for (j in 1:length(num.tabs)){num.data<-rbind.data.frame(num.data, num.tabs[[j]])}
    # stay = first
    if (stay=="first"){
      dup.n<-duplicated(num.data$num); num.data<-num.data[!dup.n,] # by num
      dup.n<-duplicated(num.data$ind); num.data<-num.data[!dup.n,]#by indexes
      NUM<-num.data$ind}
    # stay = choose
    if (stay=="choose"){
      choose<-c()
      for (i in 1:nrow(num.data)){choose[i]<-choose.var[num.data$num[i]]}
      num.data<-cbind.data.frame(num.data, choose)
      data.res<-data.frame()
      for (i in 1:length(duplicated.var)){ # take row for each
        data.small<-data.frame();data.small.choose<-data.frame()
        data.small<-num.data[num.data$num==i,]
        if (nrow(data.small)<=1){data.res<-rbind.data.frame(data.res, data.small) # 1 row or no rows at all
        }else{data.small.choose<-data.small[data.small$choose==choose.stay.val,]}
        if (nrow(data.small)>1 & nrow(data.small.choose)==0){data.res<-rbind.data.frame(data.res, data.small[1,])} # all rows not to choose
        if (nrow(data.small.choose)>0) {data.res<-rbind.data.frame(data.res, data.small.choose[1,])} # row to choose
        # deleting rest
        if (nrow(data.small)!=0){num.del<-unique(data.small$ind); num.data<-num.data[!num.data$ind %in% num.del,]}
      }
      NUM<-data.res$num}
    #check unique
    if (length(NUM)==length(duplicated.var)){
      if (verbose) message ("All variables are unique, return ", length(NUM), " rows")
      return<-data}
    if (length(NUM)!=length(duplicated.var)){
      if (verbose) message (length(duplicated.var)-length(NUM), " variables are not unique, return ", length(NUM), " rows")
      return<-data[NUM,]}
  }
  # stay NONE
  if (stay=="none"){ # delete rows with pattern= duplicated.var
    nums.del<-c()
    for (i in 1:length(pattern)){
      if (exact==FALSE){nums<-grep(pattern[i], duplicated.var, fixed=TRUE)}
      if (exact==TRUE){nums<-c(1:length(duplicated.var))[duplicated.var %in% pattern[i]]}
      nums.del<-c(nums.del, nums)}
    nums.del<-unique(nums.del)
    if (length(nums)>0){return<-data[-nums.del,]} else{return<-data}  }
  return(return)
}


#' Trim data frame
#'
#' If the numeric value of the data frame variable does not meet the specified conditions, the function deletes the entire row.
#'
#' @param data data frame
#' @param trim.var.name character; vector of data frame column names with numeric variables that should be tested for conditions
#' @param trim.action character; vector of test conditions; possible values are:
#' \code{"more", "eqmore"} (more or equal), \code{"less", "eqless"} (less or equal).
#' @param trim.thresh numeric; vector of condition threshold values
#'
#' @details
#' This function takes the vector of data frame variables and for each of them test if they satisfy the specified conditions.
#' Not satisfying values are deleted with the entire data frame row. You may set as many conditions for as many variables as you like.
#'
#' \code{trim.values} must be exact column names as in data frame.
#'
#' @return data frame without rows with values that do not satisfy the specified conditions.
#'
#' @examples
#'data <- data.frame ("a" = 1:10, "b" = 101:110)
#'trim_DF (data, trim.var.name = c("a", "b"), trim.action = c("less", "eqmore"),
#'        trim.thresh = c(6, 104))
#'
#' @author Elena N. Filatova
#' @name trim_DF
#' @export
trim_DF <- function (data, trim.var.name, trim.action, trim.thresh){
  # check length values=action=number
  if(length(trim.var.name)!=length(trim.action) | length(trim.var.name)!=length(trim.thresh)){
    stop("trim.var.name, trim.action and trim.thresh must be of same length")}
  #get column for trim numbers
  cols<-colnames(data)
  trim.n<-c(); for (i in 1:length(trim.var.name)){trim.n[i]<-which(cols==trim.var.name[i])}
  #trimming
  for (i in 1:length(trim.var.name)){
    if(trim.action[i]=="more"){data<-data[which(data[, trim.n[i]]>trim.thresh[i]),]}
    if(trim.action[i]=="eqmore"){data<-data[which(data[, trim.n[i]]>=trim.thresh[i]),]}
    if(trim.action[i]=="less"){data<-data[which(data[, trim.n[i]]<trim.thresh[i]),]}
    if(trim.action[i]=="eqless"){data<-data[which(data[, trim.n[i]]<=trim.thresh[i]),]} }
  return(data)}

#' Combine two data frames
#'
#'Combine two data frames according to shared variable
#'
#'@param data1,data2 data frames
#'@param data1.shared.var,data2.shared.var same variables in data frames
#'@param data1.shared.column.num,data2.shared.column.num integer; column numbers of same variables in data frames
#'@param delete.not.shared logical; delete rows that present in one data frame but do not present in other data frame
#'@param not.shared character; which rows to delete; possible values are
#'\code{"data1"} (delete rows that present in \code{data1} but do not present in \code{data2}),
#'\code{"data2"} (delete rows that present in \code{data2} but do not present in \code{data1}),
#'\code{"all"} (both variants)
#'@param verbose logical; show messages
#'
#'@details
#'This function combines columns of two data frames according to \code{shared.var} which acts like rows' identification number.
#'If \code{shared.var} value from one data frame do not present in other data frame, NAs are produced.
#'Those absent rows are deleted when \code{delete.not.shared = TRUE}.
#'
#'\code{data1.shared.var} and {data2.shared.var} must contain unique values within its own data frame.
#'
#'Order of rows in resulting data frame is according to \code{data1}.
#'\code{data2.shared.var} is removed from resulting data frame.
#'
#'@return
#'Combined data frame.
#'
#'@examples
#'#same values in shared variables
#'data1 <- data.frame (N = 1:5, letter = rep("A", 5))
#'data2 <- data.frame (N = 1:5, letter = rep("B", 5), cs = rep("cs",5))
#'unite_two_DF (data1 = data1, data1.shared.var = data1$N, data2 = data2, data2.shared.var = data2$N,
#'             delete.not.shared = TRUE, not.shared = "all")
#'#different values in shared variables
#'data1 <- data.frame (N = 1:5, letter = rep("A", 5))
#'data2 <- data.frame (N = 3:8, letter = rep("B", 6), cs = rep("cs",6))
#'unite_two_DF (data1 = data1, data1.shared.var = data1$N, data2 = data2, data2.shared.var = data2$N)
#'unite_two_DF (data1 = data1, data1.shared.var = data1$N, data2 = data2, data2.shared.var = data2$N,
#'             delete.not.shared = TRUE, not.shared = "data1")
#'unite_two_DF (data1 = data1, data1.shared.var = data1$N, data2 = data2, data2.shared.var = data2$N,
#'             delete.not.shared = TRUE, not.shared = "data2")
#'unite_two_DF (data1 = data1, data1.shared.var = data1$N, data2 = data2, data2.shared.var = data2$N,
#'             delete.not.shared = TRUE, not.shared = "all")
#'
#' @author Elena N. Filatova
#' @name unite_two_DF
#' @export
# data1.shared.var, and data2.shared.var must not repeat
#not shared data1 - delete rows that are in data1 but not in data2
# columns in data2 renamed
unite_two_DF <- function (data1, data1.shared.var, data1.shared.column.num = 1,
                          data2, data2.shared.var, data2.shared.column.num = 1,
                          delete.not.shared = FALSE, not.shared = "all", verbose = TRUE){
  # check not shared
  if (not.shared!="data1" & not.shared!="data2" & not.shared!="all") {stop("Choose not.shared type")}
  data1.new<-data1; data2.new<-data.frame()
  lines1<-c(); lines2<-c() # numbers of lines that will be added as NAs
  for (i in 1:length(data1.shared.var)){
    data2.small<-data.frame(); data2.small<-data2[data2.shared.var==data1.shared.var[i],]
    if (nrow(data2.small)==0){data2.small[1,]<-matrix(rep(NA, ncol(data2.small)), nrow=1); lines2<-c(lines2, i)}
    data2.new<-rbind.data.frame(data2.new, data2.small)
  }
  # HAVE: full data1 and full/partionall data2
  #check if all data2 is there
  add<-data2.shared.var %in% data2.new[, data2.shared.column.num]
  if (sum(!add)>0){
    data2.new<-rbind.data.frame(data2.new, data2[!add,]) # add to data2 rest of data2
    add.matr<-matrix(rep(NA, (ncol(data1.new)*sum(!add))), nrow=sum(!add)) # add NAs to data1
    colnames(add.matr)<-colnames(data1.new)
    add.matr[,data1.shared.column.num]<-data2.shared.var[!add]
    lines1<-seq(from=(nrow(data1.new)+1), to=(nrow(data1.new)+sum(!add)))
    data1.new<-rbind.data.frame(data1.new, add.matr)}
  #deleting NAs
  if (delete.not.shared==TRUE){
    if (not.shared=="data1"){
      if (length(lines2)>0){data1.new<-data1.new[-lines2,]; data2.new<-data2.new[-lines2,]
      if (verbose) message ("Absent in data2 ", length(lines2), " rows deleted")
      } else {if (verbose) message ("All rows of data1 present in data2")}}
    if (not.shared=="data2"){
      if (length(lines1)>0){data1.new<-data1.new[-lines1,]; data2.new<-data2.new[-lines1,]
      if (verbose) message("Absent in data1 ", length(lines1), " rows deleted")
      } else {if (verbose) message ("All rows of data2 present in data1")}}
    if (not.shared == "all"){
      lines<-c(lines1, lines2)
      if (length(lines)>0){data1.new<-data1.new[-lines,]; data2.new<-data2.new[-lines,]
      if (verbose) message ("Absent in data1 ", length(lines1), " rows deleted, absent in data2 ", length(lines2), " rows deleted")
      } else {if (verbose) message ("All rows of data1 and data2 combined")}}}
  #return
  # test colnames
  same<-colnames(data2.new) %in% colnames(data1.new)
  if (sum(same)>0){colnames(data2.new)[same]<-paste("data2",colnames(data2.new)[same], sep=".")}
  #combine result
  data.res<-cbind.data.frame(data1.new, data2.new[,-data2.shared.column.num])
  rownames(data.res)=NULL
  return(data.res)
}

#' Normalize variable
#'
#' Normalize variable in a data frame
#'
#'@param data data frame with numeric variable that should be normalized
#'@param var.name character; data frame column name with numeric variable that should be normalized
#'@param method character; normalization method; possible values are: \code{"mean"} (normalize to mean),
#'\code{"median"} (normalize to median), \code{"number"} (normalize to given number)
#'@param norm.number numeric; a value to normalize to (if \code{method = "number"})
#'@param return character; return object; possible values are: \code{"vector"} (return a vector of normalized values),
#'\code{"replace"} (replace \code{var.name} values with normalized values in data frame),
#'\code{"add.near"} (add normalized values next to \code{var.name} values in data frame),
#'\code{"add.end"} (add normalized values as the latter column in data frame)
#'@param digits integer; number of decimal places to round the normalized value
#'
#'@details
#'This function scales variable to a range of (0-1), where 1 get values that are the most close to mean, median or given number.
#'See \link[BBmisc]{normalize} for details.
#'
#'\code{var.name} must be exact column name as in data frame.
#'
#'@return
#'Vector or data frame with normalized values.
#'
#'@examples
#'data <- data.frame (N = 1:5, temperature = c(37.5, 36.6, 41.2, 38.8, 36.7),
#'                 name = c("Bob", "Kate", "Steve", "Sonya", "Mary"))
#'normalize_DF (data = data, var.name = "temperature", method = "mean", return = "vector")
#'normalize_DF (data = data, var.name = "temperature", method = "mean", return = "replace")
#'normalize_DF (data = data, var.name = "temperature", method = "mean", return = "add.near")
#'normalize_DF (data = data, var.name = "temperature", method = "number",
#'             norm.number = 36.6, return = "add.end")
#'
#' @author Elena N. Filatova
#' @name normalize_DF
#' @export
normalize_DF <- function (data, var.name,  method = "mean", norm.number, return = "add.end", digits=2){
  # test package dependencies
  if (!requireNamespace("stats", quietly = TRUE)) { stop("Package \"stats\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("BBmisc", quietly = TRUE)) { stop("Package \"BBmisc\" needed for this function to work. Please install it.", call. = FALSE)}
  #check method and return
  if (method!="mean" & method!="median" & method!="number"){stop("Choose normalization method")}
  if (return!="vector" & return!="replace" & return!="add.near" & return!="add.end" ){stop("Choose return object")}
  #get column number
  cols<-colnames(data); num<-which(cols==var.name)
  #get normalize point
  if (method=="mean") {diff<-abs(data[, num]-mean(data[, num]))}
  if (method=="median") {diff<-abs(data[, num]-stats::median(data[, num]))}
  if (method=="number") {diff<-abs(data[, num]-norm.number)}
  # normalize
  var.norm<-BBmisc::normalize(diff, method = "range", range=c(1,0))
  var.norm<-round(var.norm, digits=digits)
  #return
  if(return=="vector"){return(var.norm)}
  if(return=="replace"){data[, num]<-var.norm;
  colnames(data)<-c(cols[1:(num-1)], paste(var.name, ".norm", sep=""), cols[(num+1):length(cols)])
  return(data)}
  if (return=="add.near"){data<-cbind.data.frame(data[, 1:num], var.norm, data[,(num+1):ncol(data)])
  colnames(data)<-c(cols[1:(num)], paste(var.name, ".norm", sep=""), cols[(num+1):length(cols)])
  return(data)}
  if(return=="add.end"){data<-cbind.data.frame(data, var.norm)
  colnames(data)<-c(cols, paste(var.name, ".norm", sep="")); return(data)}
}

#'Rate variables
#'
#'Count data frame's row rate according to several variables
#'
#'@param data data frame with rated variables
#'@param rate.var character; vector of data frame column names with numeric variables of range (0-1) that should be used for rating
#'@param weights numeric; vector of variables' weights (their sum must be 1)
#'@param return character; return object; possible values are: \code{"vector"} (return a vector of rate values),
#'\code{"add"} (add rated values as the latter column in data frame)
#'@param as.percent logical; if some rated variables are percentages
#'@param percent.var character;  vector of data frame column names with rated variables that are percentages
#'@param digits integer; number of decimal places to round the rate value
#'
#'@details
#'This function counts rate as \code{rate = var1*weight1 + var2*weight2 + var3*weight3 +...} etc.
#'All variables must be in range (0-1) and sum of weights must be 1. If you use percentages as rating variable, use \code{as.percent = TRUE}.
#'Those variables would be divided by 100 before rating and then would be multiplicated by 100 after rating.
#'
#' \code{rate.var} and  \code{percent.var} must be exact column names as in data frame.
#'
#' @return
#' Vector or data frame with rate values.
#'
#' @examples
#'data <- data.frame (N = 1:5, percent = c(12, 15, 18, 20, 94), number = c(0.1, 0.5, 0.6, 0.8 ,0.9))
#'rate_DF (data = data, rate.var = c("percent", "number"), weights = c(0.4, 0.6), return = "add",
#'                              as.percent = TRUE, percent.var = "percent")
#'
#' @author Elena N. Filatova
#' @name rate_DF
#' @export
rate_DF <- function (data, rate.var, weights, return = "add", as.percent = FALSE, percent.var, digits = 2){
  # check rate
  if (return!="vector" & return!="add" ){stop("Choose return object")}
  # check length values=weight
  if(length(rate.var)!=length(weights) | sum(weights)!=1){
    stop("rate.var and weights must be of same length and the sum of weights must be 1.")}
  # get column numbers
  cols<-colnames(data)
  rate.n<-c(); for (i in 1:length(rate.var)){rate.n[i]<-which(cols==rate.var[i])}
  if(as.percent==TRUE){ # if percents - *100
    perc.n<-c(); for (i in 1:length(percent.var)){perc.n[i]<-which(cols==percent.var[i])}
    data[, perc.n]<-data[, perc.n]/100}
  #rating
  rate<-0; for (i in 1:length(rate.n)){rate=rate+(data[,rate.n[i]]*weights[i])}
  rate=round(rate, digits=digits)
  if(return=="vector"){return(rate)}
  if(return=="add"){
    if (as.percent==TRUE){data[, perc.n]<-data[, perc.n]*100}
    data<-cbind.data.frame(data, rate); return(data)}
}

#' Read and unite files
#'
#' Read a bunch of table files and unite them in one data frame
#'
#' @param path character; directory path
#' @param pattern character; file names pattern
#' @param sep character; the field separator character
#' @param header logical; files contain the names of the variables as its first line
#' @param add.file.id logical; add file identification columns
#' @param file.id data frame with file identification values
#' @param unique logical; delete repeated rows
#'
#' @details
#' All files must be tables of same type. All files must be in one directory.
#'
#' File identification columns might be added. There might be any number of such columns.
#' They are added at the beginning of result data frame.
#' File identification values are set as \code{file.id} data frame,
#' where each column contains possible identification values and column names are names of identificator.
#' If no \code{file.id} provided file names are set by default.
#'
#'@return data frame with united files' content.
#'
#' @examples
#' path <- tempdir()
#' dir.create(path)
#' t1<-paste0(path, "/table1")
#' t2<-paste0(path, "/table2")
#' table1 <- data.frame (Num = 1:10, Letter = rep("A", 10))
#' write.table (table1, t1, sep = ";")
#' table2 <- data.frame (Num = 1:10, Letter = rep("B", 10))
#' write.table (table2, t2, sep = ";")
#' read_and_unite_files (path = path, pattern = "table", header = TRUE, sep = ";",
#'                       add.file.id = TRUE)
#' read_and_unite_files (path = path, pattern = "table", header = TRUE, sep = ";",
#'                       add.file.id = TRUE,
#'                       file.id = data.frame (id1 = c(1,2), id2 = c("one", "two")))
#' file.remove (t1); file.remove (t2)
#'
#' @author Elena N. Filatova
#' @name read_and_unite_files
#' @export
read_and_unite_files <- function (path, pattern, sep = ";", header = TRUE,
                                  add.file.id = FALSE, file.id = NULL, unique = FALSE){
  # test package dependencies
  if (!requireNamespace("utils", quietly = TRUE)) { stop("Package \"utils\" needed for this function to work. Please install it.", call. = FALSE)}
  # run
  files<-list.files(path=path, pattern=pattern) # list of files in directory
  # define files id
  if (add.file.id==TRUE){
    #no file ids
    if(is.null(file.id)==TRUE){ID.data<-data.frame(col1=files); colnames(ID.data)<-"file"
    } else {
      #file ids in dataframe
      #test length
      if (length(files)!=nrow(file.id)){stop("Number of file ids are not equal to number of files")}
      ID.data<-file.id}
    if (ncol(ID.data)>1){#reverse dataframe - for good looking
      Ncol<- seq(from=ncol(ID.data), to=1)
      ID.data<-ID.data[,  Ncol]}}
  data.res<-data.frame()
  for (i in 1:length(files)){
    tab<-data.frame()
    tab<-utils::read.table(paste(path, "/", files[i], sep=""), sep=sep, header=header)
    #add file ids
    if(add.file.id==TRUE){
      for (j in 1:ncol(ID.data)){tab<-cbind(rep(ID.data[i, j], nrow(tab)), tab)
      colnames(tab)<-c(colnames(ID.data)[j], colnames(tab)[2:ncol(tab)])}}
    data.res<-rbind.data.frame(data.res, tab) # combine results
  }
  if (unique==TRUE){del.n<-duplicated(data.res); data.res<-data.res[!del.n,]}
  return(data.res)
}

#'Read table file
#'
#'Read table file and selects the required rows and columns.
#'
#'@param file character; file name and path
#'@param choose.columns logical; return chosen columns only
#'@param column.names character; vector of name of columns that are chosen to be returned
#'@param select logical; return only rows that contain selected values in one column
#'@param select.column.name character; name of column that contains selected values
#'@param select.val vector of values that define rows that should be returned
#'@param unique logical; delete duplicated rows
#'@param sep character; the field separator character
#'@param header logical; files contain the names of the variables as its first line
#'
#' @details This function reads table files and returns data frame with selected
#' rows (only rows with specified values) and columns.
#' Also duplicated rows may be deleted.
#'
#' \code{column.names} and  \code{select.column.name} must be exact column names as in data frame.
#'
#' @return Data frame with file content, optionally trimmed.
#'
#'@examples
#'mydata <- data.frame (N = 1:10, letter = c(rep ("A", 5), rep ("B", 4), "C"),
#'                    num = c(1, rep(1:4, 2), 5))
#'t1<-tempfile()
#'write.table (mydata, t1, sep = ";")
#'read_from_table_file (file = t1)
#'read_from_table_file (file = t1, select = TRUE, select.column.name = "letter",
#'                      select.val = c("A", "C"))
#'read_from_table_file (file = t1, select = TRUE, select.column.name = "letter",
#'                      select.val = c("A", "C"), unique=TRUE, choose.columns = TRUE,
#'                      column.names = c("letter", "num"))
#'read_from_table_file (file = t1, select = TRUE, select.column.name = "letter",
#'                      select.val = c("A", "C"), unique = TRUE, choose.columns = TRUE,
#'                      column.names = c("N", "num"))
#'read_from_table_file (file = t1, select = TRUE, select.column.name = "letter",
#'                      select.val = c("A", "C"), unique = TRUE, choose.columns = TRUE,
#'                      column.names = c("letter", "N"))
#'file.remove (t1)
#'
#' @author Elena N. Filatova
#' @name read_from_table_file
#' @export
read_from_table_file<-function(file, choose.columns=FALSE, column.names,
                               select=FALSE, select.column.name, select.val, unique = FALSE,
                               sep=";", header = TRUE){
  # test package dependencies
  if (!requireNamespace("utils", quietly = TRUE)) { stop("Package \"utils\" needed for this function to work. Please install it.", call. = FALSE)}
  # run
  if(file.exists(file)==FALSE){stop("No such file")} # file exists
  data<-utils::read.table(file, header=header, sep=sep)
  # select rows
  if (select == TRUE){
    cols<-colnames(data)
    select.col.n<-which(cols==select.column.name) # number of selection column
    nums= data[, select.col.n] %in% select.val
    data<-data[nums,] }
  #leave only needed columns
  if (choose.columns==TRUE){
    cols<-colnames(data)
    need.col.n<-which((cols %in% column.names) == TRUE)# number of needed column
    data<-data[, need.col.n]}
  #unique
  if(unique==TRUE){
    num.del<-duplicated(data)
    if (sum(num.del)>0){data<-data[!num.del,]}}
  return(data)
}

#' Cut string into segments
#'
#' Cuts character string into segments of given size.
#'
#' @param string character string; vector of length 1
#' @param size integral; vector of length of segments
#'
#' @details
#' This function works with one string only.
#' Segments are cut from start to end of a string.
#' \code{size} might be a vector of any length, all possible variants will be cut.
#'
#' @return Data frame with segment size, start and end point, segment string.
#'
#' @examples
#' cut_string (string = "aaatttttttccgc", size = 12:14)
#'
#' @author Elena N. Filatova
#' @name cut_string
#' @export
cut_string <- function (string, size){
  if (length(string)>1){stop("string must be a vector of length 1")}
  data.nums<-data.frame()
  for (i in 1:length(size)){
    data.one<-data.frame(); start<-c(); stop<-c()
    start<-1:(nchar(string)-size[i]+1)
    stop<-size[i]:nchar(string)
    data.one<-data.frame(rep(size[i], length(start)), start, stop)
    data.nums<-rbind.data.frame(data.nums, data.one)
  }
  colnames(data.nums)<-c("size", "start", "stop")
  zond<-substring(string, data.nums$start, data.nums$stop)
  data.res<-cbind.data.frame(data.nums, "segment"=zond)
  return(data.res)
}

#' Create unique identification values
#'
#' Creates unique identification values by adding numbers to identical values.
#'
#' @param var vector of values
#' @param sep character; string to separate the terms
#'
#'@details
#'This function takes vector with same values and adds numbers to create unique values.
#'
#'@return
#'Character vector of \code{var} with attached numbers.
#'
#'@examples
#'var<-c("one", "two", "three", "one", "two", "three", "one")
#'make_ids(var)
#'
#' @author Elena N. Filatova
#' @name make_ids
#' @export
make_ids<-function(var, sep = "_"){
  uids.unique<-unique(var)
  zond.id<-c();  zond.id.internal<-c()
  for (i in 1: length(uids.unique)){
    uid.n<-which(var==uids.unique[i]) # numbers of this uid
    zond.id.internal<-paste(uids.unique[i], c(1:length(uid.n)), sep=sep)
    zond.id[uid.n]<-zond.id.internal
  }
  return(zond.id)
}

