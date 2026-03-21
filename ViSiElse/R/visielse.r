#' \code{visielse} plots the graphic from time data and build an object class
#' \code{ViSigrid} with, at least, the time data of each punctual action defined
#'  in the \code{ViSibook} for all subjects.
#' @name visielse
#' @title Function \code{visielse}
#' @rdname visielse
#' @aliases visielse
#' @export visielse
#' @param X  A \code{data.frame} or \code{matrix}. \code{X} stores punctual action timestamps.
#' The actions are defined in \code{book}, and X columns names should correspond to the slot "vars" of \code{book}.
#' X must also have a column to identify individuals (id column).
#' @param book  A \code{data.frame} or a \code{ViSibook} or NULL.  \code{book} stores the process structure. \itemize{
#'  \item{ If it is a \code{data.frame} it should contains at least the columns \strong{vars, label, typeA, showorder, deb, fin }.
#' Optionally, other characteristics can be filled : \strong{GZDebn,  GZFin, Repetition, BZBeforeDeb, BZBeforeFin, BZAfterDeb, BZAfterFin, BZLong , BZLtype }.}
#' \item{ If it is a \code{ViSibook} it should correspond to the columns names of X.}
#' \item{ If it is \code{NULL} the process is the ordered list of punctual actions given by the columns names of X.}
#' }
#' @param doplot A logical If \code{FALSE}, the graphic is not plotted.
#' @param is.ViSibook A logical \itemize{ \item{ \code{FALSE} if book is a \code{data.frame} or \code{NULL}}. \item{ \code{TRUE} is book is a ViSibook.}}
#' @param group A \code{factor} with two \code{levels}.
#' \code{group} indicates the group attributed to the individuals,
#' it has same the length as the number of rows of \code{X}.
#' @param Xsup  A \code{data.frame} or \code{matrix} storing supplementary time data,  \strong{all individuals in}  \code{Xsup}  \strong{must be in } \code{X}.
#' @param method   In \{ \code{"global"} ,  \code{"cut"} ,  \code{"join"} , \code{"within"} \}.
#' \code{method} specifies the plotting method, see \code{details}. If \code{group} is \code{NULL},
#'  \code{method} is set to \code{"global"}.
#' @param grwithin  A level of \code{group}.
#' If \code{method} is set to \code{within}, \code{grwithin} specifies the group to consider.
#' @param quantity
#' In \{ "\code{N}" , "\code{dens}" \}. \code{quantity} allows choosing the quantity represented for punctual action
#' When \code{quantity} is set to "N" the number of individuals is considered. Otherwise when it
#' is set to "dens" proportion of individuals is considered instead. If \code{group} is defined and
#' \code{method} set to "cut" or "within", this proportion is calculated regarding each represented group.
#' @param informer  In \{ "\code{NULL}" , "\code{median}" , "\code{mean}" \}.
#' If \code{informer} is set
#' to "median" the median and quartiles are computed,
#' if it is set to "mean" the  mean and standard deviation are. If \code{informer} is \code{NULL} no indicators are computed.
#' @param tests  A boolean.
#' When \code{informer} is not NULL and \code{group} is defined, if \code{tests} is \code{TRUE}, tests are computed to compare groups.
#' If the parameter \code{informer} is set to "\code{mean}",
#' the function \code{wilcox.test()} is used, if \code{informer} is set to "\code{median}" the function \code{mood.test()} is used.
#' @param threshold.test  A numeric between 0 and  1.
#' \code{threshold.test} is the value of the p-value under which the H0 hypothesis
#'  of the test is rejected when \code{tests} is \code{TRUE}.
#' @param max_tps  A numeric, \code{>0}. \code{max_tps} is the maximum time used to build the grid in the plot.
#' \code{max_tps} is useful when \code{Xsup} is given. If \code{max_tps} is \code{NULL} it is automatically computed.
#' @param pixel An integer.
#' It is the number of unit of time under which individuals are aggregated in the plot.
#' @param t_0  either 0, either a value of the slot "\code{vars}" in \code{book},
#' \code{t_0} indicates the starting time to plot.
#' @param decrgr2  A boolean. When sorted.line is TRUE and decrgr2 is TRUE, long actions of the second group are plotted in decreasing order by starting times.
#' @param sorted.line  A boolean.
#' When \code{sorted.line} is \code{TRUE}, it allows long actions to be sorted by starting time.
#' @param times  A boolean. If \code{times} is \code{TRUE}, it indicates that \code{X} contains data in a time format.
#' @param timeformat  time format.  If \code{times} is \code{TRUE}.
#' @param idsubject  An integer between 1 and \code{dim(X)[2]}.  \code{idsubject} indicates the
#' number of the column of X that contains individuals id numbers.
#' @param colvect   A \code{matrix} containing colors.
#'  Colors are automatically computed if \code{colvect} is \code{NULL}.
#'  If \code{group} is not \code{NULL} colvect should have two rows otherwise one.
#' @param ncolvect  A \code{numeric}.
#'  \code{ncolvect} indicates the number of columns of \code{colvect}. Its default setting is \code{dim(X)[1]}.
#'  \code{ncolvect} is considered only if \code{colvect} is \code{NULL}.
#' @return a ViSigrid object.
#' @details
#' \itemize{
#' \item{ \code{method} }{   \itemize{
#'  \item{ \code{global} }{:  The plot of the ViSigrid object will not consider the parameter \code{group} and plot indistinctly all individuals. }
#'  \item{ \code{cut} }{: In the plot of the ViSigrid object, each group will be plotted separately, one under the other with different colors. }
#'  \item{  \code{join} }{: In the plot of the ViSigrid object, groups are spatially mixed but they are represented by different colors. }
#'  \item{\code{within}}{ : In the plot of the ViSigrid object, all individuals are plotted together then the group specified in \code{grwithin}
#'  is plotted another time underneath. }
#'  } }
#' \item{ \code{informer}}{
#'
#' The parameter \code{informer} allows users to choose the statistics to be plotted. \code{informer} can take three values:
#' \itemize{
#' \item{ \code{median}:}{ Median and quartiles are calculated for each action, using the function
#' quantile from the package stats. This is the default value.}
#' \item{\code{mean}:}{ Mean and standard deviation are calculated for each action, using the
#' functions mean and var from the package stats.}
#' \item{\code{NULL}:}{ no indicators are computed.}
#' }
#' When a group is defined, statistics are calculated per group if the method cut or
#' within is chosen.
#'
#' When plotting the \code{\linkS4class{ViSigrid}} object, statistics for punctual actions are represented by
#' white circles linked by a line. For long action, only a black line is plotted starting at 
#' the median (or mean) value of the punctual action staring times. The line length represents
#' the median (or mean) of the long action duration.
#' Informers are computed directly on the given matrix for punctual action.
#' And for long actions, it is based on the difference between the punctual action defining its beginning and the one defining its ending.
#' }
#' \item{ \code{tests} and \code{threshold.test}}{
#'
#' As for the parameter informer, tests are computed on the given
#' matrix or data.frame X for a punctual action. And for a long action, it is calculated on its difference between its beginning and its ending punctual actions.
#' In  \code{\link{plot-ViSigrid-method}}, results of the tests are represented by a star only when the
#'  resulted p-value is bellow or equal to value defined by the parameter threshold.test.
#' }
#' \item{ \code{pixel}}{
#'
#' The parameter pixel represents the number of unit of time under which individuals
#' are aggregated for punctual action in the plot. When the parameter pixel is too
#' small the information represented will be too much aggregated to allow interpretation.
#'
#' For punctual actions data are aggregated in a matrix \eqn{M} . The number of row of \eqn{M} is the
#' number of action and its number of columns is  \eqn{[ ( max(X)-t_{0} )/pixel]}.
#'
#' \eqn{M_{i,j}} contains the number of observations of the \eqn{i}-th punctual action (by the order
#' of the ViSibook object) between \eqn{t_0 + (j-1)pixel} included and
#' \eqn{t_0 + j*pixel} excluded.
#' }
#' \item{ \code{t_0}}{
#'
#' The origin of the graphic can be set using the parameter t_0. There is two ways to define it:
#' \itemize{
#' \item{A number:}{  set to 0__. It can be change at convenience, but for long actions black zones will not
#' be drawn, and for punctual actions black and green zones will not be translated.}
#' \item{The name of a punctual action:}{ To set the origin of the graphic to the moment
#'  when the action was done for each individual. Black and green zones will not be translated as well.}
#' }
#' }
#'
#' }
#'
#' @details x can also has the columns : GZDebn,  GZFin, Repetition, BZBeforeDeb, BZBeforeFin, BZAfterDeb, BZAfterFin, BZLong , BZLtype
#' @seealso Classes \code{\linkS4class{ViSigrid}} and \code{\linkS4class{ViSibook}}.
#'  The method plot for ViSigrid object \code{\link{plot-ViSigrid-method}} for examples.
#'
#' @examples
#' coffee <- c(  58, 11,  5, 53, 53, 59, 24, 59, 46, 20)
#' fill_coffee <- c(162,  57,103,154,165,132,  74, 107, 104,  93)
#' fill_water <- c(  66,  92,54, 78, 74, 114, 91, 129, 71, 56)
#' push_B <- c( 74, 99, 62, 84, 83, 120, 95, 129, 80, 63 )
#' drink <- c( 472, 176, 475, 283, 265, 207, 234, 184, 490, 520)
#' X <- data.frame(id = seq(1,10), coffee, fill_coffee,fill_water,push_B,drink)
#' library(ViSiElse)
#' visi1 <- visielse(X)
#'
#' #### Changing the pixel of time
#'
#' visi1 <- visielse(X, pixel = 10)
#' # Plot the mean and standart deviation
#'
#' visi1 <- visielse(X,informer = "mean")
#'
#' #### Do not plot indicators
#' visi1 <- visielse(X,informer = NULL)
#'
#'
#' # Extraction of the visibook from the data
#'
#' visi1 <- visielse(X,informer = NULL, doplot = FALSE)
#' book <- visi1@book
#' plot(book)
#'
#' #### Changing labels
#'
#' book[,2]<- c("Taking the coffee",
#'              "Fill the machine with coffee",
#'              "Fill the tank with water",
#'              "Push the Button",
#'              "Drink the coffee")
#' plot(book)
#' visi1 <- visielse(X, book=book, is.ViSibook = TRUE,informer = NULL)
#'
#'
#' #### Change the order of Actions in the process
#'
#' book[,4]<- c(5,1,2,4,3)
#' plot(book)
#' visi1 <- visielse(X, book=book, is.ViSibook = TRUE)
#'
#'
#'
#' #### Adding a long Actions
#'
#' visi1 <- visielse( X )
#' book <- ConvertFromViSibook( visi1@book ) # Convert book into data.frame
#' add_delay <- c( "delay_coffee_push","Preparation","l","6","coffee","push_B")
#' book[6,] <- add_delay
#' book
#'
#'
#' ### ViSiElse representation of long actions
#'
#' visi2 <- visielse( X=X , book=book,informer=NULL)
#'
#' ## Green & Black zones
#'
#' book$GZDeb <- c(NA,60,NA,NA,NA,NA)
#' book$GZFin <- c(NA,120,NA,NA,NA,NA)
#' book$BZBeforeDeb <- c(NA,0,NA,NA,NA,NA)
#' book$BZBeforeFin <- c(NA,30,NA,NA,NA,NA)
#' book$BZAfterDeb <- c(NA,180,NA,NA,NA,NA)
#' book$BZAfterFin <- c(NA,Inf,NA,NA,NA,NA)
#' book$BZLong <- c(rep(NA,5),150)
#' book$BZLtype <- c(rep(NA,5),"time")
#' visi1 <- visielse( X, book=book , informer = NULL)
#'
#' book$BZLtype <- c(rep(NA,5),"span")
#' visi1 <- visielse( X, book=book ,informer = NULL)
#'
#'
#' ## Group
#'
#' ### Method : Cut
#' group <- c( "group2","group1","group2","group1","group1",
#'              "group2","group1","group1","group1","group2")
#' visi1 <- visielse( X,group=group, book=book ,informer = NULL, method = "cut")
#'
#' visi1 <- visielse( X,group=group, book=book ,informer = NULL, method = "join")
#'
#' visi1 <- visielse( X,group=group, book=book ,informer = NULL, method = "within",grwithin = "group1")

visielse <- function(X , book=NULL, is.ViSibook=FALSE, doplot =TRUE , Xsup = NULL,
                       method = "global" , group = NULL  , grwithin = NULL ,
                       informer = "median" , tests = TRUE , threshold.test = 0.01 ,
                       quantity = "N" , pixel = 20 , t_0 = 0, sorted.line = TRUE ,
                       decrgr2 = FALSE, max_tps = NULL, colvect = NULL , ncolvect = NULL ,
                       times = FALSE , timeformat = c('hh:mm:ss') , idsubject = 1 ) {
  # Buildbind the ViSibook object
          if( length(book) == 0){
           book <- ViSibookfromDATA( X , idsubject =  idsubject)
          }else{
                    if( is.ViSibook==FALSE){
           book <- ConvertoViSibook(book)
           # Modification
           X <- X[ , match(append(methods::slot( book , "vars" )[ which( methods::slot( book , "typeA" ) == "p" ) ], colnames(X)[1], after = 0), colnames(X))]
                    }
          }
  # A data.frame containing the times performances for punctual actions, colnames( X ) must corresponds to names given in the book in the slot vars
  ### Verification group if method !="global"
  if (method != "global" ) {
    if (is.null( group ) ) {
      warning( " Group is NULL, method set to \"global\" \n " )
      method <- "global"
    }else{
      X <- X[order(group),]
      group <- group[order(group)]
      group <- factor(group)
      if (nlevels( factor( group ) ) > 2) {
        warning( " Incorrect number of groups (2 max), method set to \"global\" \n " )
        method <- "global"
      }
    }
  }
  #...................................................................................................................................................................................................................
  ### contruction of the grid matrix
  if (method == "global" ) {
    nind <- dim(X)[1]
    temp <- MATgrid( X , book , pixel = pixel , times = times , timeformat = timeformat , idsubject = idsubject , retX = TRUE , t_0 = t_0 , max_tps = max_tps )
    MATp <- temp$MATGrid
    X <- temp$X
    vect_tps <- temp$vect_tps
    t_0_action <- temp$t_0_action
    t_0 <- temp$t_0
  }else{
    group <- as.factor(group)
    if (method == "join" || method == "cut" || method == "within" ) {
      ####For within method coying of the data
      if (method == "within" ) {
        # if the level to considered is not given the function arbitraly select the first level
        grwithin <- switch( as.character( is.null( grwithin ) ) , "TRUE" = levels( group )[ 1 ], "FALSE" = grwithin )
        temp  <-  dim( X )[ 1 ]
        # Coping time data
        # X <- Matrix::rBind( X , X[ which( group == grwithin ), ] ) rBind deprecated
        # Modification
        X <- rbind( X , X[ which( group == grwithin ), ] )
        # Adapting the group vector to the new time matrix
        group <- gl( 2 , temp , labels = c( "All" , as.character( grwithin ) ) ) [ seq( 1 , temp + sum( group == grwithin ) , 1 ) ]
      }
      nind1 <- sum( group == levels( group )[ 1 ] )
      nind2 <- sum( group == levels( group )[ 2 ] )
      tempvect_tps <- MATgrid( X , book , pixel = pixel , times = times , timeformat = timeformat , idsubject = idsubject , onlyvect_tps = TRUE , t_0 = t_0 , max_tps = max_tps)
      vect_tps <- tempvect_tps$vect_tps
      t_0_action <- tempvect_tps$t_0_action
      MATp <- Matrix::Matrix( rep( 0 , sum( methods::slot( book , "typeA") == "p" ) * 2 *  length( vect_tps ) ) , nrow =  sum( methods::slot( book , "typeA") == "p" ) * 2 , length( vect_tps ) , sparse = TRUE )
      temp <- MATgrid( X[ which( group == levels( group )[ 1 ] ) , ] , book , pixel = pixel , times = times , timeformat = timeformat , idsubject = idsubject ,  vect_tps = vect_tps , t_0 = t_0, t_0_action=t_0_action, retX = TRUE)
      MATp[ seq( 1 , sum( methods::slot( book , "typeA") == "p" ) , 1 ), ] <- temp$MATGrid
      tempX <- temp$X
      temp <- MATgrid( X=X[ which( group == levels( group )[ 2 ] ) , ] ,
                       book , pixel = pixel , times = times , timeformat = timeformat ,
                       vect_tps = vect_tps , t_0 = t_0 , t_0_action=t_0_action, idsubject = idsubject , retX = TRUE)
      MATp[ seq( sum( methods::slot( book , "typeA") == "p" ) + 1 , sum( methods::slot( book , "typeA") == "p" ) *2 , 1 ), ] <-  temp$MATGrid
      t_0 <- temp$t_0
      # Modification
      # X <- Matrix::rBind( tempX , temp$X ) rBind deprecated
      X <- rbind( tempX , temp$X )
      rm( tempX )
    }
  }
#...................................................................................................................................................................................................................
  ### informers Punctual action
  if (is.null( informer ) == FALSE ) {
    if (method == "join" || method == "global" ) {
      if (informer == "mean") {
        informers <- apply( X[ , -idsubject ] , MARGIN = 2 , FUN = function(x )(c( mean( x , na.rm = TRUE ) - sqrt( stats::var( x , na.rm = TRUE ) ) , mean( x , na.rm = TRUE ) , mean( x , na.rm = TRUE ) + sqrt( var( x , na.rm = TRUE ) ) ) ) )
      }else{
        if (informer == "median") {
          informers <- apply( X[ , -idsubject ] , MARGIN = 2 , FUN = function(x )(c( stats::quantile( x , na.rm = TRUE )[ 2 ]  , stats::median( x , na.rm = TRUE )  , stats::quantile( x , na.rm = TRUE )[ 4 ] ) ) )
        }else{
          warning( " informer value not recognized, it must be \"mean\", \" median\" or NULL - informer set to NULL \n " )
          informer <- NULL
        }
      }
    }else{
      if ( method == "cut" || method == "within" ) {
        if (informer == "mean") {
                  # Modification : Matrix::rBind --> rbind
          informers <- rbind(
            apply( X[ which( group == levels( group ) [ 1 ] ) , -idsubject ] , MARGIN = 2 , FUN = function(x )(c( mean( x , na.rm = TRUE ) - sqrt( stats::var( x , na.rm = TRUE ) ) , mean( x , na.rm = TRUE ) , mean( x , na.rm = TRUE ) + sqrt( stats::var( x , na.rm = TRUE ) ) ) ) ),
            apply( X[ which( group == levels( group ) [ 2 ] ) , -idsubject ] , MARGIN = 2 , FUN = function(x )(c( mean( x , na.rm = TRUE ) - sqrt( stats::var( x , na.rm = TRUE ) ) , mean( x , na.rm = TRUE ) , mean( x , na.rm = TRUE ) + sqrt( stats::var( x , na.rm = TRUE ) ) ) ) )
          )
        }else{
          if (informer == "median") {
                    # Modification : Matrix::rBind --> rbind
            informers <- rbind( apply( X[ which( group == levels( group )[ 1 ] ) , -idsubject ] , MARGIN = 2 , FUN = function(x )(c( stats::quantile( x , na.rm = TRUE )[ 2 ]  , stats::median( x , na.rm = TRUE )  , stats::quantile( x , na.rm = TRUE )[ 4 ] ) ) ) ,
                                apply( X[ which( group == levels( group )[ 2 ] ) , -idsubject ] , MARGIN = 2 , FUN = function(x )(c( stats::quantile( x , na.rm = TRUE )[ 2 ]  , stats::median( x , na.rm = TRUE )  , stats::quantile( x , na.rm = TRUE )[ 4 ] ) ) ) )
          }else{
            warning( " informer value not recognized, it must be \"mean\", \" median\" or NULL - informer set to NULL \n " )
            informer <- NULL
          }
        }
      }
    }
    # Modification ordre des informers en fonction de l'ordre des noms des variables du book qui est classe en fonction de showorder
    #informers <- informers[ , match(methods::slot( book , "vars"), colnames(informers))]
    #informers <- informers[ , match(methods::slot( book , "vars" )[ which( methods::slot( book , "typeA" ) == "p" ) ], colnames(informers))]
  }else(
    informers <- new( "matrix" )
  )
  #...................................................................................................................................................................................................................
  ################# Tests Punctual Actions
  if (tests == TRUE & method != "global" & is.null( informer ) == FALSE  ) {
    if (informer == "mean") {
      testsP <- apply( X[ , -idsubject ] , MARGIN = 2 , FUN = function(x ) { stats::wilcox.test( x[ which( group == levels( group )[ 1 ] ) ] , x[ which( group == levels( group )[ 2 ] ) ] )$p.value } ) < threshold.test
    }else{
      if (informer == "median") {
        testsP <- apply( X[ , -idsubject ] , MARGIN = 2 , function(x ) { stats::mood.test( x[ which( group == levels( group )[ 1 ] ) ] , x[ which( group == levels( group )[ 2 ] ) ] )$p.value } ) < threshold.test
      }
    }
  }else{
    if (tests == TRUE & method == "global" ) {
      tests <- FALSE
      testsP <- NULL
    }
    if (tests == TRUE & is.null( informer ) ) {
      warning("if test=TRUE, informer should not be NULL, no tests are computed")
    }
    tests <- FALSE
    testsP <- NULL
  }
  #...................................................................................................................................................................................................................
  ################### Long Actions
  if (any(methods::slot( book , "typeA") == "l" &  is.na( book[ , 4] ) == FALSE ) ) {
  sia <- which( methods::slot( book , "typeA") == "l" &  is.na( book[ , 4] ) == FALSE )
  ia <- sia[ which.min(book[ , 4 ][ sia ]) ]
  temp <- buildL( X , book , ia , group , decrgr2 , sorted.line , method , vect_tps)
  idsort <- temp$idsort
  L <- temp$L
  BZL <- new( "dgCMatrix" )
  BZL <- Matrix::Matrix( rep( 0 , dim( X )[ 1 ] * sum( methods::slot( book , "typeA") == "l" &  is.na( book[ , 4] ) == FALSE ) ) ,
                 nrow =  dim( X )[ 1 ] ,
                 ncol = sum( methods::slot( book , "typeA") == "l" &  is.na( book[ , 4] ) == FALSE ) , sparse = TRUE , doDiag = FALSE )
  BZL <- temp$BZL
  for (ia in sia[ order(book[ , 4][ sia ]) ][ -1 ] ) {
    temp <- buildL( X , book , ia , group , decrgr2 , sorted.line , method , vect_tps)
    idsort <- cbind( idsort , temp$idsort )
    L <- cbind( L , temp$L )
    BZL <- cbind( BZL , temp$BZL )
  }
  #...................................................................................................................................................................................................................
  ################### Long Actions Informers
  if (is.null( informer ) == FALSE ) {
    if (method == "join" || method == "global" ) {

      if (informer == "mean") {
        for (ia in seq( 1 , dim( L )[ 2 ] , 2 ) ) {  # ia = 1
          informers <- cbind( informers ,
                              c( mean( L[ , ia + 1 ] - L[ , ia ] , na.rm = TRUE ) - sqrt( stats::var( L[ , ia + 1 ] - L[ , ia ] , na.rm = TRUE ) ),
                                 mean( L[ , ia + 1 ] - L[ , ia ] , na.rm = TRUE ) ,
                                 mean( L[ , ia + 1 ] - L[ , ia ] , na.rm = TRUE ) + sqrt( stats::var( L[ , ia + 1 ] - L[ , ia ] , na.rm = TRUE ) ) ) )
        }
      }else{
        for (ia in seq( 1 , dim( L )[ 2 ] , 2 ) ) {
          if (informer == "median" ) {
            informers <- cbind( informers ,
                                c( stats::quantile( L[ , ia + 1 ] - L[ , ia ] , na.rm = TRUE )[ 2 ] ,
                                   stats::quantile( L[ , ia + 1 ] - L[ , ia ] , na.rm = TRUE )[ 3 ],
                                   stats::quantile( L[ , ia + 1 ] - L[ , ia ] , na.rm = TRUE )[ 4 ] ) )
          }
        }
      }
    }else{
      if (informer == "mean" ) {
        for (ia in seq( 1 , dim( L )[ 2 ] , 2 ) ) {
          informers <- cbind( informers ,
                              c( mean( L[ which( group == levels( group ) [ 1 ] ), ia + 1 ] - L[ which( group == levels( group ) [ 1 ] ) , ia ] , na.rm = TRUE ) - sqrt( stats::var( L[ which( group == levels( group ) [ 1 ] ) , ia + 1 ] - L[ which( group == levels( group ) [ 1 ] ) , ia ] , na.rm = TRUE ) ),
                                 mean( L[ which( group == levels( group ) [ 1 ] ) , ia + 1 ] - L[ which( group == levels( group ) [ 1 ] ) , ia ] , na.rm = TRUE ),
                                 mean( L[ which( group == levels( group ) [ 1 ] ), ia + 1 ] - L[ which( group == levels( group ) [ 1 ] ) , ia ] , na.rm = TRUE ) + sqrt( stats::var( L[ which( group == levels( group ) [ 1 ] ) , ia + 1 ] - L[ which( group == levels( group ) [ 1 ] ) , ia ] , na.rm = TRUE ) )  ,
                                 mean( L[ which( group == levels( group ) [ 2 ] ), ia + 1 ] - L[ which( group == levels( group ) [ 2 ] ) , ia ] , na.rm = TRUE ) - sqrt( stats::var( L[ which( group == levels( group ) [ 2 ] ) , ia + 1 ] - L[ which( group == levels( group ) [ 2 ] ) , ia ] , na.rm = TRUE ) ),
                                 mean( L[ which( group == levels( group ) [ 2 ] ) , ia + 1 ] - L[ which( group == levels( group ) [ 2 ] ) , ia ] , na.rm = TRUE ),
                                 mean( L[ which( group == levels( group ) [ 2 ] ), ia + 1 ] - L[ which( group == levels( group ) [ 2 ] ) , ia ] , na.rm = TRUE ) + sqrt( stats::var( L[ which( group == levels( group ) [ 2 ] ) , ia + 1 ] - L[ which( group == levels( group ) [ 2 ] ) , ia ] , na.rm = TRUE ) ) ) )
        }
      }else{
        if (informer == "median" ) {
          for (ia in seq( 1 , dim( L )[ 2 ] , 2 ) ) {
            informers <- cbind( informers ,
                                c( stats::quantile( L[ which( group == levels( group ) [ 1 ] ) , ia + 1 ] - L[ which( group == levels( group ) [ 1 ] ) , ia ] , na.rm = TRUE )[ 2 ] ,
                                   stats::quantile( L[ which( group == levels( group ) [ 1 ] ), ia + 1 ] - L[ which( group == levels( group ) [ 1 ] ) , ia ] , na.rm = TRUE )[ 3 ],
                                   stats::quantile( L[ which( group == levels( group ) [ 1 ] ), ia + 1 ] - L[ which( group == levels( group ) [ 1 ] ) , ia ] , na.rm = TRUE )[ 4 ],
                                   stats::quantile( L[ which( group == levels( group ) [ 2 ] ) , ia + 1 ] - L[ which( group == levels( group ) [ 2 ] ) , ia ] , na.rm = TRUE )[ 2 ] ,
                                   stats::quantile( L[ which( group == levels( group ) [ 2 ] ), ia + 1 ] - L[ which( group == levels( group ) [ 2 ] ) , ia ] , na.rm = TRUE )[ 3 ],
                                   stats::quantile( L[ which( group == levels( group ) [ 2 ] ), ia + 1 ] - L[ which( group == levels( group ) [ 2 ] ) , ia ] , na.rm = TRUE )[ 4 ]
                                ))
          }
        }
      }
    }
  index <- unlist( lapply(methods::slot(book ,"deb")[ sia[ sort(book[ , 4][ sia ], index.return = TRUE)$ix ]],
                          function(x )(which( colnames(informers)[ seq(1 , dim(informers)[ 2 ] - dim(L)[ 2 ] / 2 , 1)] == x  )))
  )
  informers <- cbind( informers , informers[ ,seq(dim(informers)[ 2 ] - dim(L)[ 2 ] / 2 + 1 , dim(informers)[ 2 ] ,1) ]  + informers[ , index ] )
    colnames(informers)[seq( sum( methods::slot( book , "typeA") == "p") + 1 ,sum( methods::slot( book , "typeA") == "p" ) + dim( L )[ 2 ]  , 1 )] = c(
      unlist( lapply( methods::slot( book , "vars")[ which( methods::slot( book , "typeA") == "l" &  is.na( book[ , 4] ) == FALSE ) ], function(x)(paste0("span_" , x)))) ,
      unlist( lapply( methods::slot( book , "vars")[ which( methods::slot( book , "typeA") == "l" &  is.na( book[ , 4] ) == FALSE ) ], function(x)(paste0("plot_" , x)))) )
  }
  if (tests == TRUE & method != "global" & is.null( informer ) == FALSE  ) {
    temp <- matrix( unlist( lapply( seq( 1 , dim( L )[ 2 ] , 2 ) ,  function(x ) (L[ , x + 1 ] - L[ , x ] ) )  ) , nrow = dim( L )[ 1 ] , byrow = FALSE )
    if (informer == "mean") {
      testsP <- c( testsP , apply( temp , MARGIN = 2 , FUN = function(x ) { stats::wilcox.test( x[ which( group == levels( group )[ 1 ] ) ] , x[ which( group == levels( group )[ 2 ] ) ] )$p.value } ) < threshold.test )
    # Modification testP
      # print(testsP)
      }else{
      if (informer == "median") {
        testsP <- c( testsP , apply( temp , MARGIN = 2 , function(x ) { stats::mood.test( x[ which( group == levels( group )[ 1 ] ) ] , x[ which( group == levels( group )[ 2 ] ) ] )$p.value } ) < threshold.test )
      }
    }
  }
  #...................................................................................................................................................................................................................
  }else{
  L <- NULL
  BZL <- NULL
  idsort <- NULL
  }
  if ( is.matrix(idsort) == FALSE & is.null(idsort) == FALSE ) {
    idsort <- matrix(idsort)
  }
   ################### Supplementary individuals
  if (is.null( Xsup ) == FALSE ) {
    if ( method == "global" || is.null( group ) || nlevels( factor( group ) ) > 2 ) {
      temp 	<- MATgrid( Xsup , book , pixel = pixel , times = times , timeformat = timeformat , idsubject = idsubject ,  vect_tps = vect_tps , retX = TRUE )
      MATpsup <- temp$MATGrid
      Xsup 	<- temp$X
    }else{
      if (method == "join" || method == "cut" || method == "within" ) {
        idsup 		<- Xsup[ , idsubject ]
        groupsup	<- group[ idsup ]
        if (method == "within" ) {
          temp 	<- dim( Xsup )[ 1 ]
          temp1 	<- which( groupsup == rep( grwithin , length( groupsup ) ) )
          if (any( groupsup == rep( grwithin , length( groupsup ) ) ) ) {
            # Modification
            # Xsup 		<- Matrix::rBind( Xsup , Xsup[ temp1 , ] ) rBind deprecated
            Xsup 		<- rbind( Xsup , Xsup[ temp1 , ] )
            groupsup	<- gl( n = 2 , k = temp , labels = c( "All" , grwithin ) )[ seq( 1 , temp + length( temp1 ) , 1) ]
            idsup 		<- c( idsup , idsup[ temp1 ] )
          }
        }
        groupsup 	<- factor(groupsup)
        MATpsup 	<- Matrix::Matrix( rep( 0 , sum( methods::slot( book , "typeA") == "p" ) * 2 *  length( vect_tps ) ) , nrow =  sum( methods::slot( book , "typeA") == "p" ) * 2 , length( vect_tps ) , sparse = TRUE )
        temp <- MATgrid( Xsup[ which( groupsup == levels( group )[ 1 ] ) , ] , book , pixel = pixel , times = times , timeformat = timeformat , idsubject = idsubject ,  vect_tps = vect_tps , retX = TRUE )
        MATpsup[ seq( 1 , sum( methods::slot( book , "typeA") == "p" ) , 1), ] <- temp$MATGrid
        tempX <- temp$X
        temp <- MATgrid( Xsup[ which( groupsup == levels( group )[ 2 ] ) , ] , book , pixel = pixel , times = times , timeformat = timeformat , idsubject = idsubject , vect_tps = vect_tps , retX = TRUE )
        MATpsup[ seq( 1 + sum( methods::slot( book , "typeA") == "p" ), 2 * sum( methods::slot( book , "typeA") == "p" ) , 1 ) , ] <- temp$MATGrid
        # Modification
        # Xsup <- Matrix::rBind( tempX , temp$X ) rBind deprecated
        Xsup <- rbind( tempX , temp$X )
      }
    }
    if (quantity == "dens" ) {
      if (method != "global" & method != "join" ) {
          # Modification Matrix::rBind-->rbind
        MATpsup <- rbind( round( MATpsup[ seq( 1 , dim( MATpsup )[ 1 ] / 2 , 1 ), ] * 100 / nind1 , 0 ) ,
                          round( MATpsup[ seq( dim( MATpsup )[ 1 ] / 2 + 1 , dim( MATpsup )[ 1 ] , 1 ),] * 100 / nind2 , 0 ) )
      }else{
        MATpsup <- round( MATpsup * 100 / nind , 0 )
      }
    }
    if (any(methods::slot( book , "typeA") == "l") ) {
      ia <- which( methods::slot( book , "typeA") == "l" &  is.na( book[ , 4] ) == FALSE )[ 1 ]
      temp <- buildL( Xsup , book , ia , group = groupsup , decrgr2 , sorted.line = FALSE , method , BZL = FALSE , vect_tps)
      idsortsup <- temp$idsort
      Lsup <- temp$L
      for (ia in which( methods::slot( book , "typeA") == "l" &  is.na( book[ , 4] ) == FALSE )[ -1 ] ) {
        temp <- buildL( Xsup , book , ia , group , decrgr2 , sorted.line = FALSE , method , BZL = FALSE , vect_tps)
        idsortsup <- cbind( idsortsup , temp$idsort )
        Lsup <- cbind( Lsup , temp$L )
      }
    }else{
      Lsup <- NULL
    }
  }
    #...................................................................................................................................................................................................................
    #### Density or count  & colors grid matrice
    if( quantity == "dens" ){
              if (method != "global" & method != "join" ) {
                        # Modification Matrix::rBind-->rbind
                        MATp <- rbind( round( MATp[ seq( 1 , dim( MATp )[ 1 ] / 2 , 1 ), ] * 100 / (nind1),0),
                                               round( MATp[ seq( dim( MATp )[ 1 ] / 2 + 1 , dim( MATp )[ 1 ] , 1 ) , ] * 100 / (nind2),0))
              }else{
                        MATp <- round( MATp * 100 / (nind),0)
              }
    }
    ncolvect <- switch( as.character( is.null( Xsup ) ) , "TRUE" = max(MATp)+1 , "FALSE" = max(max(MATp),max(MATpsup))+1 )
    if (method == "global" ) {
              if ( (is.null( colvect ) ) | ( length( colvect ) < ncolvect )   ) {
                        colvect <- matrix( colorspace::sequential_hcl( n = ncolvect ,
                                                                       h = 264 ,
                                                                       c. = c( 80, 85 ),
                                                                       l = c( 30, 95 ),
                                                                       power = 0.7
                        )[ seq( ncolvect , 1 , -1 )[-1] ] , nrow = 1 )
              }
    }else{
              if ( (is.null( colvect ) ) ) {
                        # Modification Matrix::rBind-->rbind
                        colvect <- as.matrix( rbind( colorspace::sequential_hcl( n = ncolvect ,
                                                                                         h = 264,
                                                                                         c. = c( 80 , 85 ) ,
                                                                                         l = c( 30 , 95 ) ,
                                                                                         power = 0.7
                        )[ seq( ncolvect , 1 , -1 )[-1] ] ,
                        colorspace::sequential_hcl( n = ncolvect ,
                                                    h = -32 ,
                                                    c. = c( 80 , 85 ) ,
                                                    l = c( 30 , 95 ) ,
                                                    power = 0.7
                        )[ seq( ncolvect , 1 , -1 )[-1] ] ) )
              }else{
              if ( ( dim( colvect )[ 1 ] < ncolvect )   ) {
                        # Modification Matrix::rBind-->rbind
                        colvect <- as.matrix( rbind( colorspace::sequential_hcl( n = ncolvect ,
                                                                                         h = 264,
                                                                                         c. = c( 80 , 85 ) ,
                                                                                         l = c( 30 , 95 ) ,
                                                                                         power = 0.7
                        )[ seq( ncolvect , 1 , -1 )[-1] ] ,
                        colorspace::sequential_hcl( n = ncolvect ,
                                                    h = -32 ,
                                                    c. = c( 80 , 85 ) ,
                                                    l = c( 30 , 95 ) ,
                                                    power = 0.7
                        )[ seq( ncolvect , 1 , -1 )[-1] ] ) )
              }
              }
    }

  ret <- ViSigrid( 	MATp = MATp ,
                    MATpsup = switch( as.character( is.null( Xsup ) ) , "TRUE" = new( "dgCMatrix" ) , "FALSE" = MATpsup ) ,
                    idsup = switch( as.character( is.null( Xsup ) ) , "TRUE" = vector() , "FALSE" = Xsup[ , idsubject] ) ,
                    colvect = colvect ,
                    L = switch( as.character( is.null( L ) ) , "TRUE" = new( "data.frame" ) , "FALSE" = L )  ,
                    idsort = switch( as.character( is.null(idsort ) ) , "TRUE" = new( "matrix" ) , "FALSE" = idsort )   ,
                    BZL = switch( as.character( is.null( BZL ) ) , "TRUE" = new( "dgCMatrix" ) , "FALSE" = BZL ) ,
                    Lsup = switch( as.character( is.null( Xsup ) ) , "TRUE" = new( "data.frame" ) , "FALSE" = Lsup ) ,
                    book = book ,
                    group = switch( as.character( is.null( group ) ) , "TRUE" = factor() , "FALSE" = group ) ,
                    vect_tps = vect_tps ,
                    informers = informers ,
                    testsP = switch( as.character( is.null( testsP ) ) , "TRUE" = vector(), "FALSE" = testsP ),
                    parameters = list(
                      method = method ,
                      grwithin = switch( as.character( is.null( grwithin ) ) ,"TRUE" = NULL, "FALSE" = grwithin ) ,
                      quantity = quantity ,
                      informer = informer ,
                      tests = tests ,
                      threshold.test = threshold.test ,
                      pixel = pixel ,
                      t_0 = t_0_action
                    )
  )
  if( doplot == TRUE){
    plot(ret, scal.unit.tps = pixel)
  }
  return(ret)
}

