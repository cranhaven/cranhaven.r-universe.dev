#' @name taxMachine
#' @title Classifying 16S sequences
#' 
#' @description Optimized classification of 16S sequence data.
#' 
#' @param sequence Character vector with DNA sequences.
#' @param model.in.memory Logical indicating if model should be cached in memory (default=TRUE).
#' @param model.on.disk Logical or text, for reading/saving models, see Deatils below (default=FALSE).
#' @param verbose Logical, if \code{TRUE} progress is reported during computations (default=TRUE).
#' @param chunk.size The number of sequence to classify in each iteration of the loop (default=10000).

#' 
#' @details This function provides optimized taxonomy classifications from 16S sequence data.
#' 
#' All sequences are classified to the genus level based on a Multinomial model (see \code{\link{multinomTrain}})
#' trained on the designed consensus taxonomy data set \code{\link[microcontax]{contax.trim}} found in
#' the R-package \code{microcontax}. The word length K=8 has been used in the model.
#' 
#' To avoid saving fitted models in the package, a model is trained the first time you run \code{taxMachine} in an R session.
#' This takes only a few seconds, and the result is cached for latter use if \code{model.in.memory} is \code{TRUE}.
#' 
#' If a path to an existing file with a trained model is supplied in \code{model.on.disk}, this Multinomial model is read
#' from the file and used. If a path to a new file is supplied, the trained Multinomial model will be saved to that file.
#' The default (\code{model.on.disk=FALSE}), means no files are read/saved, while \code{model.on.disk=TRUE} will attempt to load/save models from the
#' \code{microclass/extdata} directory.
#' 
#' Both \code{verbose} and \code{chunk.size} are used to monitor the progress, which is nice when classifying huge data sets,
#' since this will take some time.
#' 
#' @return A \code{data.frame} with one row for each sequence. The columns are Genus, D.score, R.score and P.recognize.
#' 
#' Genus is the predicted genus for each sequence. Note that all sequences get a prediction, but may still be
#' more or less reliable.
#' 
#' The D.score is a measure of how the predicted genus wins over all other genera in the race for being the chosen one.
#' A large D.score means the winner stands out clearly, and we can be confident it is the correct genus. A D.score close to 0
#' means we have an uncertain classification. Only D.scores below 1.0, should be of any concern, see Liland et al
#' (2016) for details.
#' 
#' The R.score is a measure of the models ability to recognize the sequence. The more negative the R.score gets, the more
#' unusual the sequence is compared to the training set (the \code{\link[microcontax]{contax.trim}} data set). The P.recognize
#' is a rough probability of seing an R.score this small, or smaller, given the training data. Thus, a very small P.recognize means
#' the sequence is not really recognized, and the classification is worthless. A very negative R.score indicates either not 16S at all,
#' many sequencing errors that has destroyed the read, or a completely new taxon never seen before. See Liland et al (2016) for
#' details. 
#' 
#' @author Lars Snipen and Kristian Hovde Liland
#' 
#' @references Liland, KH, Vinje, H, Snipen, L (2016). \code{microclass} - An 
#' R-package for 16S taxonomy classification. BMC Bioinformatics, xx:yy.
#' 
#' @seealso \code{\link{KmerCount}}, \code{\link{multinomClassify}}.
#' 
#' @examples 
#' \dontrun{
#' data(small.16S)
#' tax.tab <- taxMachine(small.16S$Sequence)
#' }
#' 
#' @importFrom utils data
#' @importFrom microcontax getGenus
#' @importFrom stats approx median
#' @export taxMachine
#' 
taxMachine <- function(sequence, model.in.memory = TRUE, model.on.disk = FALSE,
                        verbose = TRUE, chunk.size = 10000){
  if(verbose) cat("Welcome to the taxMachine!\n")
  N.seq <- length(sequence)
  if(!(is.numeric(chunk.size) & chunk.size>0)) stop("chunk.size must be a positive integer")
  chunks <- unique(c(seq(0, N.seq, chunk.size), N.seq))
  nc <- length(chunks)
  sl <- nchar(sequence)
  if(verbose) cat("   classifying", N.seq, "sequences with median length", median(sl), "...\n")
  ml <- c(150, 300, 500, 1400)
  dl <- abs(median(sl) - ml)
  ml <- ml[which(dl == min(dl))[1]]
  n.pseudo <- 100
  
  # Check if model is saved or should be saved
  do.load <- do.save <- FALSE
  if(is.character(model.on.disk) | (is.logical(model.on.disk) && model.on.disk)){
    if(is.logical(model.on.disk)){
        model.on.disk <- file.path(path.package("microclass"), "extdata/model100.rda") 
    }
    if(file.exists(model.on.disk)){
      load(model.on.disk)
      if(!exists("fitted.model")) stop('fitted.model was not found in saved file.')
      do.load <- TRUE
      if(verbose) cat("   model loaded...\n")
    } else {
      do.save <- TRUE
    }
  }
  
  # Check if counts and/or correct model is cached
  do.cache <- was.cached <- FALSE
  if(!do.load && model.in.memory){
    fitted.model <- getmicroEnv("fitted.model")
    if(is.null(fitted.model)){
      do.cache <- TRUE
    } else {
      was.cached <- TRUE
        if(verbose) cat("   reusing model...\n")
    }
  }
  
  # Create multinomial model if not available
  if(!do.load && !was.cached){
    if(verbose) cat("   creating model...\n")
    contax.trim <- NULL
    load(file.path(path.package("microcontax"), "data/contax.trim.rda"))
    fitted.model <- multinomTrain(contax.trim$Sequence, getGenus(contax.trim$Header), n.pseudo = n.pseudo)
  }
  
  coef.bias <- coef.sd <- std.frame <- rprob.mat <- NULL
  load(file.path(path.package("microclass"), "extdata/norm.models.rda"))
  load(file.path(path.package("microclass"), "extdata/std.frame.rda" ) )
  load(file.path(path.package("microclass"), paste0("extdata/rprob.mat.", ml, ".rda")))
  
  
  # Save multinomial model for later use
  if(do.save){
    if(verbose) cat("   saving model...\n")
    save(fitted.model, file = model.on.disk, compress = "xz")
  }
  if(do.cache){
    if(verbose) cat("   caching model...\n")
    putmicroEnv("fitted.model", fitted.model)
  }
  
  restab <- data.frame(Genus = rep("unclassified", N.seq),
                        D.score = rep(0, N.seq),
                        R.score = rep(0, N.seq),
                        P.recognized = rep(1, N.seq),
                        stringsAsFactors = F)
  for(i in 1:(nc-1)){
    idx <- (chunks[i]+1):chunks[i+1]
    if(verbose) cat("   classifying sequence", min(idx), "to", max(idx), "\n")
    dfr <- multinomClassify(sequence[idx], fitted.model, post.prob = T)
    l <- sl[idx]
    P1 <- (dfr$Post.prob.1 - (coef.bias[1]+coef.bias[2]*l))/(coef.sd[1]+coef.sd[2]*l)
    P2 <- (dfr$Post.prob.2 - (coef.bias[1]+coef.bias[2]*l))/(coef.sd[1]+coef.sd[2]*l)
    Ds <- P1 - P2
    idd <- as.integer(factor(dfr$Taxon.1, levels = std.frame[,1]))
    Rs <- (P1 - std.frame[idd,2])/std.frame[idd,3]
    lst <- approx(rprob.mat[,1], rprob.mat[,2], xout = Rs, yleft = 0, yright = 1)
    
    restab$Genus[idx] <- dfr$Taxon.1
    restab$D.score[idx] <- Ds
    restab$R.score[idx] <- Rs
    restab$P.recognized[idx] <- lst$y
  }
  
  if(verbose) cat("done!\n")
  return(restab)
}

## Backup: No space saving
# taxMachine <- function( sequence, n.pseudo=1, chunk.size=1000, verbose=TRUE ){
#   if( verbose ) cat( "Welcome to the taxMachine!\n")
#   N.seq <- length( sequence )
#   if( verbose ) cat( "   classifying", N.seq, "sequences...\n" )
#   
#   if( chunk.size < 1 ) stop( "chunk.size must be positive integer" )
#   chunks <- unique( c( seq( 0, N.seq, chunk.size ), N.seq ) )
#   nc <- length( chunks )
#   
#   if( n.pseudo!=1 & n.pseudo!=100 ){
#     n.pseudo <- 1
#     warning( "n.pseudo must be either 1 or 100, using 1" )
#   }
#   
#   if( verbose ) cat( "   loading model...\n" )
#   if( n.pseudo == 1 ){
#     load( file.path( path.package( "microclass" ), "extdata/mltn.ctx.K8.np1.rda" ) )
#     load( file.path( path.package( "microclass" ), "extdata/norm.mat.np1.rda" ) )
#     load( file.path( path.package( "microclass" ), "extdata/std.lookup.np1.rda" ) )
#   } else {
#     load( file.path( path.package( "microclass" ), "extdata/mltn.ctx.K8.np100.rda" ) )
#     load( file.path( path.package( "microclass" ), "extdata/norm.mat.np100.rda" ) )
#     load( file.path( path.package( "microclass" ), "extdata/std.lookup.np100.rda" ) )
#   }
#   
#   restab <- data.frame( Genus=rep( "unclassified", N.seq ),
#                         D.score=rep( 0, N.seq ),
#                         R.score=rep( 0, N.seq ),
#                         Flag=rep( "", N.seq ),
#                         stringsAsFactors=F )
#   for( i in 1:(nc-1) ){
#     if( verbose ) cat( "   chunk", i, "out of", (nc-1), "\n" )
#     idx <- (chunks[i]+1):chunks[i+1]
#     dfr <- multinomClassify( sequence[idx], fitted.model, post.prob=T )
#     l <- nchar( sequence[idx] )
#     idd <- match( pmin( l, 2500 ), norm.mat[,1] )
#     P1 <- ( dfr$Post.prob.1 - norm.mat[idd,2] )/norm.mat[idd,3]
#     P2 <- ( dfr$Post.prob.2 - norm.mat[idd,2] )/norm.mat[idd,3]
#     Ds <- P1 - P2
#     idd <- as.integer( factor( dfr$Taxon, levels=std.lookup[,1] ) )
#     Rs <- (P1 - std.lookup[idd,2])/std.lookup[idd,3]
#     
#     restab$Genus[idx] <- dfr$Taxon
#     restab$D.score[idx] <- Ds
#     restab$R.score[idx] <- Rs
#     idd <- which( Rs < -9 )
#     if( length( idd ) > 0 ) restab$Flag[idx[idd]] <- "UNRECOGNIZED"
#     idd <- which( (Ds < 0.5) | (Rs >= -9 & Rs < -4 ) )
#     if( length( idd ) > 0 ) restab$Flag[idx[idd]] <- "Uncertain"
#   }
#   
#   if( verbose ) cat( "done!\n" )
#   return( restab )
# }