#' PCA of the estimated parameters
#'
#' This function applys a PCA to the estimate parameters (using function "prcomp" with \code{center = TRUE} and \code{scale. = TRUE}). Then uses the ggbiplot function to plot the biplot.
#'
#' @param paramEstimadosFinal The estimate parameters, in the following order: a11,a12,a13, a21, a22,a23, ...a(D-1)1,a(D-1)2,a(D-1)3,tau. Where D is the number of bacterial especies present in the matrix \code{especie}.
#' @param names Vector with the name of the bacteria. The component i has the name of the bacteria i, with i=1,...,D. The bacteria in the las position of the vector is the bacteria used as reference in the alr transformation.
#' @param E Number of bacteria available.
#'
#' @return Returns a list with the PCA biplot, the variance explained of each Principal Component and an object of class "prcomp" with the PCA. In the biplot, "a" denotes the intercept, "b" denotes the parameter that give information about the importance of the bacteria in defining herself in  the next time point and "c" denotes denotes the parameter that give information about the importance of the rest of the community in defining the bacteria in  the next time point.
#'
#' @examples
#'
#'set.seed(123)
#'especie=t(gtools::rdirichlet(10,c(1,3,1,2,4)))
#'names=c("Bact1","Bact2","Bact3","Bact4","Bact5")
#'tau1=0.4
#'parms1= cbind(c(0.1,0.2,0.4,0.6),c(-0.2,0.1,0.1,0.3),c(0.3,0.2,0.3,0.5))
#'paramEstimadosFinal=c(as.vector( t(parms1)),tau1)
#'
#'PCAbiplot(paramEstimadosFinal,names,5)
#'
#' @export
#'
#'

#    CoDaLoMic. Compositional Models to Longitudinal Microbiome Data.
#    Copyright (C) 2024  Irene Creus Martí
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as
# published by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

PCAbiplot<-function(paramEstimadosFinal,names,E){

H=length(paramEstimadosFinal)

params=paramEstimadosFinal[-H]
MatrizParametros=matrix(0,E-1,3)
MatrizParametros[1,]=params[1:3]
for (i in 1:(E-2)){
  MatrizParametros[i+1,]=params[(1+3*i):(3+3*i)]
}

rownames(MatrizParametros)=names[-length(names)]
colnames(MatrizParametros)=c(" a "," b "," c ")

new=data.frame(MatrizParametros)


#perform PCA
pc <- stats::prcomp(new,center = TRUE, scale = TRUE)

pc.sum<-summary(pc)
Var.Explained=pc.sum$importance[2,]

#visualize results of PCA in biplot
#install_github("vqv/ggbiplot")
g <- ggbiplot::ggbiplot(pc,
              obs.scale = 1,
              var.scale = 1,labels=c(rownames(new)))


df314 <- data.frame(x = c(1:length(Var.Explained)), y = Var.Explained)
x=df314$x
y=df314$y


var<-ggplot2::ggplot(df314, ggplot2::aes(x = x, y = y)) +
  ggplot2:: geom_line() +
  ggplot2::xlab("Principal Component") +
  ggplot2::ylab("Variance Explained") +
  ggplot2::ylim(0, 1)

fin<-list(g,var,pc)
names(fin)<-c("PCA_Biplot","VarianceExplained","PCA")

return(fin)
}




