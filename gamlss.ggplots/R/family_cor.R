################################################################################
################################################################################
################################################################################
################################################################################
#   family orthogonality of parameters  
#   Mikis Stasinopoulos Bob Rigby Fernanda de Bastiani
#   February, 2022 
################################################################################
################################################################################
################################################################################
################################################################################
family_cor <- function(family = NO(),
                      mu = NULL,
                   sigma = NULL,
                      nu = NULL,
                     tau = NULL,
                      bd = 10, 
                    data = NULL,
                  no.sim = 1000, 
                  digits = 3,
                    plot = TRUE,
                diag.off = TRUE,
           lower.tri.off = FALSE,  
                  method = c("square", "circle"),
           outline.color = "gray",
                  colors = c("blue", "white", "red"),
            legend.title = "Corr",
                   title,
                 ggtheme = ggplot2::theme_minimal(),
                  tl.cex = 12,
                  tl.col = "black", 
                  tl.srt = 45,
                    lab = TRUE, 
                lab_col = "black", 
               lab_size = 3) 
{
####################################################################
####################################################################
####################################################################
  # local function 
  meltit <- function(mat)
  {
    rna <- rownames(mat)
    lrna <- length(rna)
    value <- as.vector(mat)
    Var1 <- gl(length(rna), 1, length = lrna*lrna, labels=rna)
    Var2 <- gl(length(rna), lrna, length = lrna*lrna, labels=rna)
    daf <-  na.omit(data.frame(Var1, Var2, value=value)) 
    daf
  }
#################################################################### 
####################################################################  
gamlss.bi.list <- .binom
        fname <- if (is.name(family)) as.character(family)
                 else if (is.character(family)) family
                 else if (is.call(family)) as.character(family[[1]])
                 else if (is.function(family)) deparse(substitute(family))
                 else if (is(family, "gamlss.family"))  family$family[1]
                 else stop("the family must be a character or a gamlss.family name")
 #       fam1 <- eval(parse(text=fname))   # the family to output
         fam <- as.gamlss.family(family)  # this is created so I can get things
   #   dorfun <- paste("d",fname,sep="")   # say dNO
      rorfun <- paste("r",fname,sep="") 
    #   nopar <- fam$nopar                 # or fam1$nopar
   #     type <- fam$type
   par.names <- names(fam$parameters)
#################################################################
   txt.title <- if (missing(title))  paste("information matrix from ",fname)
   else title   
   if (fname%in%gamlss.bi.list) bd=bd
##    whether discrete distribution or not
      # y.var <- if(type=="Discrete")  seq(from, to, by=1)
      #         else seq(from, to, length=no.points)
## whether binomial type
  if(any(fname%in%.gamlss.bi.list)) bd <- bd   
## the number of plots  
    lobs <- max(c(length(mu),length(sigma),length(nu),length(tau)))
#################################################################  
# get the parameters
if ("mu"%in%par.names)
   { if (is.null(mu)) stop("At least one value of mu has to be set")
      mu.var <- rep(mu, length = lobs) 
      if (!fam$mu.valid(mu.var))  stop( "`mu' parameter out of range")
      samp <- eval(call(rorfun, no.sim, mu=mu.var))
   }
if ("sigma"%in%par.names)
   { if (is.null(sigma)) stop("At least one value of sigma has to be set") 
      sigma.var <- rep(sigma, length = lobs)
      if (!fam$sigma.valid(sigma.var))  stop( "`sigma' parameter out of range")
      samp <- eval(call(rorfun, no.sim , mu=mu.var, sigma=sigma.var))
    }
if ("nu"%in%par.names)
   { 
      if (is.null(nu)) stop("At least one value of nu has to be set")
      nu.var <- rep(nu, length = lobs)
      if (!fam$nu.valid(nu.var))  stop( "`nu' parameter out of range")
      samp <- eval(call(rorfun, no.sim , mu=mu.var, sigma=sigma.var, nu=nu.var))
   }
if ("tau"%in%par.names)
   { if (is.null(tau)) stop("At least one value of tau has to be set") 
      tau.var <- rep(tau, length = lobs)
      if (!fam$tau.valid(tau.var))  stop( "`tau' parameter out of range")
      samp <- eval(call(rorfun, no.sim, mu=mu.var, sigma=sigma.var, nu=nu.var, tau=tau.var))
}
     model0 <- gamlssML(samp, family=fname) 
         CC <- vcov(model0, type="cor")
         CC <- base::round(x = CC, digits = digits)
if ( diag.off) diag(CC) <- NA
if (lower.tri.off)  CC[lower.tri(CC)] <- NA            
txt.title <- if (missing(title))  paste("Correlations from family",fname)
             else title  
if (plot==FALSE) return(CC)
# end loop
#y.title <- if(type=="Discrete")  "P(Y=y)" else  "f(y)"
############################################################
        method <- match.arg(method)
          corr <- meltit(CC)
colnames(corr) <- c("var_1", "var_2", "value")

corr$abs_corr <- abs(corr$value) * 10
            p <- ggplot2::ggplot(data = corr, 
                 mapping = ggplot2::aes_string(x = "var_1", y = "var_2", fill = "value"))
if (method == "square") {
            p <- p + ggplot2::geom_tile(color = outline.color)
}
else if (method == "circle") {
            p <- p + ggplot2::geom_point(color = outline.color, shape = 21, 
                  ggplot2::aes_string(size = "abs_corr")) +
              ggplot2::scale_size(range = c(4, 10)) +
              ggplot2::guides(size = "none")
}
            p <- p + ggplot2::scale_fill_gradient2(low = colors[1], high = colors[3], 
                    mid = colors[2], midpoint = 0, limit = c(-1, 1), space = "Lab",
                      name = legend.title)+
              ggplot2::ggtitle(txt.title)
if (class(ggtheme)[[1]] == "function") {
           p <- p + ggtheme
}
else if (class(ggtheme)[[1]] == "theme") {
           p <- p + ggtheme
}
           p <- p + theme(axis.text.x = element_text(angle = tl.srt, 
                                          vjust = 1, size = tl.cex, hjust = 1), 
               axis.text.y = element_text(size = tl.cex)) + 
  coord_fixed()
label <- round(x = corr[, "value"], digits = digits)  
if (lab) {
           p <- p + ggplot2::geom_text(mapping = aes_string(x = "var_1", 
                                                   y = "var_2"), 
                              label = label, color = lab_col, size = lab_size)
}
p
}
