# Part of the SMITIDstruct R package.
# Copyright (C) 2018 Jean-François Rey <jean-francois.rey@inra.fr>
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation, Inc.,i
# 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
#


#' plotDiversity.pDistance
#' @description plot Mean Pairwise Distance for an host viralpop over time
#' @param host an Host object
#' @param lvpop a ViralPopSet object
#' @export
plotDiversity.pDistance <- function(host, lvpop){
  
    host_pD <- getDiversity.pDistance(host, lvpop)
    
    if(nrow(host_pD) > 0) {
        if( is.timestamp(host_pD$time[1]) ) host_pD$time <- getDate(host_pD$time)

        ggplot(host_pD, environment=environment(),mapping = aes_string(x='time', y='p_distance', group='1')) +
            geom_point(color= "blue") +
            geom_line(color="steelblue") + 
            labs(title=paste("Mean Pairwise Distance for host",host@ID)) +
            theme(plot.title = element_text(size="25",face="bold", hjust=0.5))
    }
    else ggplot(data.frame()) + geom_blank()
}

#' plotDiversity.sfs
#' @description plot Allele frequency spetrum for an host viralpop over time
#' @param host an Host object
#' @param lvpop an ViralPopSet object
#' @export
plotDiversity.sfs <- function(host, lvpop) {
    host_sfs <- getDiversity.sfs(host,lvpop)
    
    if(length(host_sfs) > 0) {
        toplot <- data.frame(Times=numeric(), Freq=numeric(), time=character(),stringsAsFactors = FALSE)
    
        for( i in 1:length(host_sfs)) {
          df <- data.frame(table(host_sfs[[i]]), stringsAsFactors = FALSE)
          colnames(df) <- c("Times","Freq")
          #toplot <- rbind(toplot,cbind(df,time=rep(names(host_sfs[i]),nrow(df))))
          toplot <- rbind(toplot,data.frame(df,time=rep(names(host_sfs[i]),nrow(df)), stringsAsFactors=FALSE))
        }
        if( is.timestamp(as.numeric(toplot$time[1])) ) toplot$time <- getDate(as.numeric(toplot$time))
    
        ggplot(data=toplot, aes_string(x='Times', y='Freq')) +
            geom_bar(stat="identity", fill="steelblue") + 
            geom_text(aes_string(label="Freq"), vjust=1.6, color="white", size=3.5) +
            ggtitle(paste("Allele Frequency Spectrum for host",host@ID)) +
            facet_grid(. ~ time, drop=TRUE, scales="free_x") +
            theme(plot.title = element_text(size="25",face="bold", hjust=0.5))
            #theme_minimal() 
    }
    else ggplot(data.frame()) + geom_blank()
}
