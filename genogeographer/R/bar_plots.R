#' bar_colour
#' 
#' Creates the colour scale for the accepted and rejected populations based on z-score and the log likelihood (log P).
#' @param df A data.frame with at least three coloums. The first column is the logP, the second logical (z_score accept/reject), the third a unique naming column.
#' @param alpha Should the alpha opacity be applied? And what value, 1 = solid, 0 = transparent.
#' @export
bar_colour <- function(df, alpha = 1){
  if(alpha>1) alpha <- 1
  if(alpha<0) alpha <- 0
  df <- as.data.frame(df)
  x <- df[,1];   y <- df[,2];  z <- df[,3]
  if(any(y)) xTRUE <- leaflet::colorNumeric("Reds", c(x[y],min(x[y])*1.05)) ## [y]
  if(any(!y)) xFALSE <- leaflet::colorNumeric("GnBu", c(x[!y], min(x[!y])*1.5)) ## [!y] ## Blues
  bar_cols <- rep("#000000", length(y))
  if(any(y)){ bar_cols[y] <- xTRUE(x[y]) }
  if(any(!y)){ bar_cols[!y] <- xFALSE(x[!y]) }
  bar_cols <- paste0(bar_cols, sprintf("%02X",as.hexmode(round(alpha*255))))
  setNames(bar_cols, paste(z))
}

rgba2rgb <- function(hex_rgba){
  rgba <- col2rgb(hex_rgba, alpha = TRUE)
  rgb_ <- rgba[4,]*rgba[1:3,] + (255-rgba[4,])*col2rgb("#FFFFFF")[,rep(1,length(hex_rgba))]
  apply(rgb_, 2, function(RGB) rgb(RGB[1], RGB[2], RGB[3], maxColorValue = 255^2))
}

#' Plot log likelihoods of profiles with approximate confidence intervals
#' 
#' Plots the estimated profile probabilities in each population.
#' The colour depends on the profiles likelihood and rejection/acceptance (blue/red) based on z-score
#' 
#' @name error_bar_plot
#' @author Torben Tvedebrink, \email{tvede@@math.aau.dk}
#' @param data The output from the \code{genogeo} function
#' @return A barplot of the log likelihoods for each population with confidence limits
#' @export
#' @examples
#' df_ <- simulate_pops(pop_n = 20, aims_n = 50)
#' df_db <- pops_to_DB(df_)
#' profile <- random_AIMs_profile(df_db, keep_pop = TRUE)
#' profile$pop[1] # The true population
#' result <- genogeo(profile[,c("locus","x0")], df = df_db)
#' error_bar_plot(result)

error_bar_plot <- function(data){
  ## build fixes : start ##
  logP <- NULL
  logP_lwr <- NULL
  logP_upr <- NULL
  ## build fixes : end ##
  p1 <- ggplot(data, aes(y=labs,x=logP,xmin=logP_lwr,xmax=logP_upr, colour=labs)) +
    geom_point() + geom_errorbarh() +
    labs(y="",x=expression(log[10]~P(Genotype~"|"~Population))) +
    guides(colour=FALSE, lwd=FALSE) + 
    scale_colour_manual(values = bar_colour(data[,c("logP","accept","labs")])) +
    scale_x_reverse()
  p1
}

