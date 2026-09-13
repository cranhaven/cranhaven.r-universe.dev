#***********************************************************************************
#
# Build a Formula Based on Data for modgam() Function
# Copyright (C) 2016, The University of California, Irvine
#
# This library is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this library??? if not, <http://www.gnu.org/licenses/>.
#
#*******************************************************************************
toformula <- function(formula,data,m="adjusted",surv = FALSE, span=0.5,degree=2,smooth = TRUE,offset='')
{
  add = TRUE
  if(missing(formula)){ ## If no formula specified
    names = names(data)
    if(surv) response = paste("Surv(",names[1],",",names[2],")")  ## response
    else response = paste(names[1])
    ## smooth term
    coords = paste("lo(",names[2+surv],",",names[3+surv],",span=",span,",degree=",degree,")")
    if(m=="adjusted"){ ## adjusted variables
      adjust = paste(names[-(1:(surv+3))],collapse=" + ")
      if(smooth)
        fmla = paste(response,paste(coords,"+",adjust),sep="~")
      else
        fmla = paste(response,adjust,sep="~")
    }else{
      if(smooth)
        fmla = paste(response,coords,sep="~")
      else
        fmla = paste(response,"1",sep="~")
    }
  }else{ ## If formula specified, then add span and degree in the smooth term
    fm = Reduce(paste, deparse(formula))
    parts = unlist(strsplit(fm,"\\~"))
    mf = terms(formula)
    parts2 = attr(mf,"term.labels")
    lo.index = grep("lo\\([[:print:]]+\\)",parts2)
    if(!is.null(attr(mf,"offset"))){ 
      offset = as.character(attr(mf,"variables")[attr(mf,"offset")+1])
      add = FALSE
    }
    if(smooth){
      parts.2 = parts2[lo.index]
      start <- gregexpr("\\(",parts.2)[[1]]; end <-rev(gregexpr("\\)",parts.2)[[1]]) 
      psub <- substr(parts.2,start[1]+1,end[1]-1)
      split = paste(unlist(strsplit(psub,"\\([^()]*\\)(*SKIP)(*F)|\\h*,\\h*", perl=T))[1:2],collapse=",")
      parts.2= paste("lo(",split,",span=",span,",degree=",degree,")")
      parts.2=c(parts.2,parts2[-lo.index])
      parts2 <- paste(parts.2,collapse=" + ")
    }else{
      parts2 <- parts2[-lo.index]
      if(length(parts2)>0)
        parts2 <- paste(parts2,collapse=" + ")
      else parts2 = "1"
    }
    fmla = paste(parts[1],parts2,sep=" ~ ")
  }
  if(offset!=''){ ## add offset
    if(add) offset = paste("offset(",offset,")")
    fmla =paste(fmla,offset,sep = "+")  
  }
  as.formula(fmla)
}