#' Formula Interface: Left Hand Side
#'
#' Parses Left hand side of formula
#'
#' @param f formula
#' @param grp character, name of variable which groups rows together
#' @param lvl character, name of variable which labels each row
#' @param rhs.lpad boolean, right pad 
#' @author Rocco Napoli
#' @export
fmla_lhs <-
function(f, grp=NULL, lvl=NULL, rhs.lpad=FALSE) {

  fl = as.list(f)
  l=length(fl)
  rl = list()
  i=0
  colon.op  = FALSE
  Rn.op     = FALSE
  lvl.found = FALSE

  if (l == 1) return(f)
  for (tm in fl) {
    i=i+1
    if (i==1) {
      rl[[i]] = tm
      if (tm == ":")  colon.op = TRUE
      if (tm == "Rn") Rn.op = TRUE
      if (tm == "|")  rl[[i]] = as.symbol("+")
    }
    if (i > 1) {
      if (typeof(tm) == "language")
           {rl[[i]] = fmla_lhs(tm)
           }
      else #group or level
           if (tm == lvl) lvl.found = TRUE
             rl[[i]] = tm
    }
  }
  if (colon.op == TRUE)  return(rl[[3]]) #third element is the variables that are spanned
  if (Rn.op == TRUE)     return(rl[[2]]) #second element is variables being renamed
  if (rhs.lpad == TRUE && lvl.found == TRUE) return(rl[[2]]) #second element is level
  return(as.call(rl))
}

#' Formula Interface: Right Hand Side
#'
#' Parses Right hand side of formula
#'
#' @param f formula
#' @param span There are two types of calls, when TRUE returns the spanning text, does not when FALSE
#' @param Rnl Rename
#' @param byvars Condition on these variables
#' @author Rocco Napoli
#' @export
fmla_rhs <-
function(f,
                  span, # There are two types of calls, when TRUE returns the spanning text, does not when FALSE
                  Rnl=list(Rn.o=list(), Rn.n=NULL, rn.i=0),
                  byvars=list(byvars1=NULL, byvars2=NULL, byvars.i=0)
                  )
 {
  fl = as.list(f)
  l=length(fl)
  rl = list()
  i=0;
  colon.op  = FALSE  # uses colon operator
  Rn.op     = FALSE  # uses Rn function (rename)
  cond.op   = FALSE  # uses | operator
  period.op = FALSE  # uses . operator to indicate all
  I.op      = FALSE  # Some type of other function should be kept as expression
  for (tm in fl)
    {
      i=i+1
      if (i==1)
        {
          rl[[i]] = tm
          if (tm == ".")  {period.op = TRUE;}
          if (tm == ":")  {colon.op = TRUE}
          if (tm == "Rn")
            {Rn.op = TRUE;
             # Store old name(expression) with new name and update rename index
             Rnl$rn.i<-Rnl$rn.i+1
             Rnl$Rn.o[[Rnl$rn.i]] <- fl[[2]]
             Rnl$Rn.n <- c(Rnl$Rn.n, fl[[3]])
             }
          if (tm == "|")
          {
            cond.op = TRUE
            rl[[i]] = as.symbol("+"); # Replace symbol so model.frame will keep column in data.frame
            byvars$byvars.i=byvars$byvars.i+1;
            if (byvars$byvars.i==1) {byvars$byvars1 <-  all.vars(fl[[3]])}
            if (byvars$byvars.i==2)
            {
              byvars$byvars2 <- byvars$byvars1
              byvars$byvars1 <- all.vars(fl[[3]])
              if (byvars$byvars1==".") {byvars$byvars1<-NULL}
            }
          }
          if (sum(colon.op, Rn.op, cond.op, period.op)==0) {I.op<-TRUE}
        }
      if (i > 1)
        { # If elements of formula are still functinos call again
          if (typeof(tm) == "language")
           {
             rhs.obj <-  fmla_rhs(tm, span=span, Rnl=Rnl, byvars=byvars)
             rl <- c(rl, rhs.obj$rl)
             Rnl = rhs.obj$Rnl
             byvars = rhs.obj$byvars
           }
          else rl[[i]] = tm
        }
   }
  # When span = FALSE, remove spanning text from formula
  if (!span) {if (colon.op) {return(list(rl=rl[[3]], Rnl=Rnl, byvars=byvars))}}
  # Rename, second element is variables being renamed
  if (Rn.op)    {return(list(rl=rl[[2]], Rnl=Rnl, byvars=byvars))}
  if (cond.op)  {return(list(rl=rl[[2]], Rnl=Rnl, byvars=byvars))}
  # Any other function/operator not defined here, treat as is
  if (I.op)     {return(list(rl=as.call(rl), Rnl=Rnl, byvars=byvars))}

  # Last Call of Recursion
  if (period.op)
  {
    return(list(rl=as.symbol(rl[[1]]), Rnl=Rnl, byvars=byvars))
  }
  return(list(rl=as.call(rl), Rnl=Rnl, byvars=byvars))
}

#' Formula Interface Describing the Structure of Tabular Data
#'
#' Parse the left and right hand side of the formula.
#'
#' @param f formula
#' @param data data.frmae
#' @param regx remove regular expression from column name
#' @author Rocco Napoli
#' @export
fmla_inter <-
function(f, data=NULL, regx=NA) {
  # Removing ... from parameter, doesn't look like this is used at all 10/14/2017 cb
  level       <- NULL  # Character, name of column containing row labels
  group       <- NULL  # first level grouping
  byvars      <- NULL
  rhs.lpad    <- FALSE
  grp         <- NULL
  lvl         <- NULL
  lvl.v <- NULL
  byvars1.v <- NULL
  byvars2.v <- NULL
  fl = as.list(f)
  rl = list()
  l = length(fl)
  rhs <- if(l > 2) fl[[3]] else fl[[2]]
  lhs <- if(l > 2) fl[[2]] else NULL

  #  indicate if there is a group and level variable on the lhs
  ## rhs.lpad indicates if level variable is present - that must be moved to the rhs of the formula for model.frame to work
  #just group
  if (length(all.vars(lhs)) == 1 )
  {
    rhs.lpad = FALSE
    lvl = all.vars(lhs)[1]
  }
  #both group and level, need to move level to rhs
  if (length(all.vars(lhs)) > 1 )
  {
    rhs.lpad = TRUE
    grp = all.vars(lhs)[1]
    lvl = all.vars(lhs)[2]
  }

  rhs.obj      <- fmla_rhs(rhs, span=FALSE) # Removes spanning from formula
  rhs.obj.span <- fmla_rhs(rhs, span=TRUE)  # Keeps spanning in
  # Exception for when all variables are used
  rhs      = rhs.obj$rl
  rhs.span = rhs.obj.span$rl
  Rn.o <- as.character(unlist(rhs.obj$Rnl$Rn.o))
  Rn.n <- rhs.obj$Rnl$Rn.n

  if (!is.null(lhs))
  {
    lhs = fmla_lhs(lhs, grp, lvl, rhs.lpad)
    fmla      = as.call(list(as.symbol("~"), lhs, rhs))
    fmla.span = as.call(list(as.symbol("~"), lhs, rhs.span))
  }
  else
  {
    fmla      = as.call(list(as.symbol("~"), rhs))
    fmla.span = as.call(list(as.symbol("~"), rhs.span))
  }
  fmla.span     <- as.formula(fmla.span)
  fmla          <- as.formula(fmla)
  ### Column Hiearchy ###
  trms          <- terms(fmla.span, data=data, keep.order=T)    # returns terms obect
  t.trmlbls     <- attr(trms, "term.labels")                    # returns labels of terms on rhs
  # Exception to have group and no level, "." is place holder here for level
  if (!is.null(lvl))
    {if (lvl=="."){lvl<- NULL}
     else {lvl.v <- data[,lvl]} # save vector to add to data frame since model.frame can not handle custom lhs with two variables
     }
  if (!is.null(rhs.obj$byvars$byvars1)) {byvars1.v <- data[,rhs.obj$byvars$byvars1]}
  if (!is.null(rhs.obj$byvars$byvars2)) {byvars2.v <- data[,rhs.obj$byvars$byvars2]}
  #  Need to subtract byvars from terms
  #  Exception for formula of the type y~.|group2.
  if (sum(!is.null(rhs.obj$byvars$byvars1), !is.null(rhs.obj$byvars$byvars2), !is.null(lvl)))
  {
    byvartrms <- which(t.trmlbls %in% c(rhs.obj$byvars$byvars1, rhs.obj$byvars$byvars2, lvl))
    if (length(byvartrms)>0) {t.trmlbls <- t.trmlbls[-byvartrms]}
  }

  colnames.obj  <- colnames.struct(t.trmlbls, FALSE)
  if (length(Rn.o) > 0)
   {for (rn.i in 1:length(Rn.o)) {colnames.obj$cname[colnames.obj$cname == Rn.o[rn.i]] <- Rn.n[rn.i]}}

  # Line Break in Rename Text
  colnames.obj <- colnames.linebreak(colnames.obj)
  colnames.obj <- colnames.row(colnames.obj)       #  Adjust index (reference # of rows above table for colnames) that accounts for line breaks
  if (!is.na(regx)) {colnames.obj$cname <- kill.multiregx(colnames.obj$cname, regx=regx)}
  # Use model.frame to apply embedded functions
  trms.cn <- attr(terms(fmla, data=data), "term.labels")
  trms.cn <- kill.multiregx(trms.cn, "`") #Kill Squote added by terms.  01/16/2011~CB
  data=model.frame(fmla, data)
  # Drops variables from data frame when "-" was used
  cn.dx <- which(colnames(data) %in% c(grp, trms.cn))
  data <-data[,cn.dx]
  if (!is.data.frame(data))
    {data <- as.data.frame(data)}
  if(!is.null(lvl)) {data[,lvl] <- lvl.v}
  if(!is.null(byvars1.v)) {data[,rhs.obj$byvars$byvars1] <- byvars1.v}
  if(!is.null(byvars2.v)) {data[,rhs.obj$byvars$byvars2] <- byvars2.v}
  return(list(tbl=data, group=grp, label=lvl, byvars1=rhs.obj$byvars$byvars1, byvars2=rhs.obj$byvars$byvars2, fmla=fmla, colnames.obj=colnames.obj))
}

