pe_pdf_grid <- function(model, terms, maxcol=2, maxrow=3, ...)
{  
################################################################# 
define_region <- function(row, col){
    viewport(layout.pos.row=row, layout.pos.col=col) }
#################################################################  
# function starts
     lterms <- length(terms)
if (lterms  >   maxcol*maxrow) stop("increase the maxcol or maxrow")   
if (lterms  <= 1) stop("only one term use pe_pdf()")
     norow <- ceiling(lterms/maxcol)
nocol <- if (norow == 1)  lterms  else  maxcol    
       IJ <- expand.grid(j=1:nocol, i=1:norow)
grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow=norow,ncol=nocol)))   
     GG <- list()
# for (i in xs ) gg[[i]] <-pe_pdf(linear_3, term=i, title=i)
for (p  in 1:lterms) 
    {
      GG[[terms[[p]]]] <- pe_pdf(model, term=terms[[p]], title=terms[[p]],...)
      print(GG[[terms[[p]]]], vp=define_region(IJ$i[p], IJ$j[p]))
    }
}
