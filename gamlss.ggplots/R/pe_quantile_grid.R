################################################################################
################################################################################
################################################################################
pe_quantile_grid <- function(model, terms, maxcol=2, maxrow=3, ...)
{  
################################################################# 
define_region <- function(row, col){
    viewport(layout.pos.row=row, layout.pos.col=col) }
#################################################################  
# function starts
   lterms <- length(terms)
if (lterms  >   maxcol*maxrow) stop("increase the maxcol or maxrow")   
#if (lterms  <= 1) stop("only one term use pe_quantile()")
     norow <- ceiling(lterms/maxcol)
nocol <- if (norow == 1)  lterms  else  maxcol    
       IJ <- expand.grid(j=1:nocol, i=1:norow)
grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow=norow,ncol=nocol)))   
     GG <- list()
# for (i in xs ) gg[[i]] <-pe_pdf(linear_3, term=i, title=i)
for (p  in 1:lterms) 
    {
   if (length(terms[[p]])==1)
   {
     GG[[terms[[p]]]] <- pe_1_quantile(model, term=terms[[p]], 
                                       title=terms[[p]],...)
   } else 
   {
     GG[[terms[[p]]]] <- pe_2_quantile(model, terms=terms[[p]], 
                        title= paste(terms[[p]][1],terms[[p]][2],sep=":"),...)  
   }   

      print(GG[[terms[[p]]]], vp=define_region(IJ$i[p], IJ$j[p]))
    }
}
################################################################################
################################################################################
################################################################################
################################################################################