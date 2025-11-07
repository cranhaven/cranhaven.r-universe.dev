mass.FEM <- function(FEMbasis) {
#  MAS.FEM produces the mass matrix containing integrals of products of
#  nodal functions.
#
#Input: FEMbasis is a List object produced by function makenodes.
#    It contains:
#        ORDER     ... The order of the element (1 or 2)
#        NODES     ... Coordinates of node points
#        NODEINDEX ... indices of node points for each element
#        JVEC      ... Jacobian of the affine transformation of each
#                      element to the master element
#
#Output: K0: the NNOD by NNOD matrix of sums of products of nodal basis
#        functions.
#        For each element i, the integral of the product
#        of the j'th and k'th shape functions over the i'th element is
#        computed.  Then that value is the
#        (NODEINDEX(i,j),NODEINDEX(i,k))'th entry of the i'th elemental
#        mass matrix.

#  Last modified 19 November 2021 by Jim Ramsay.

#  retrieve arrays from FEMbasis

order     <- FEMbasis$params$order
nodes     <- FEMbasis$params$nodes
nodeindex <- FEMbasis$params$nodeindex
Jvec      <- FEMbasis$params$J

nele  <- dim(nodeindex)[[1]]
nnod  <- dim(nodes)[[1]]

if (order ==2) {
    #  the integrals of products of basis functions for master element:

    K0M <- matrix( c( 6,  0, -1, -4, -1,  0,
                     0, 32,  0, 16, -4, 16,
                    -1,  0,  6,  0, -1, -4,
                    -4, 16,  0, 32,  0, 16,
                    -1, -4, -1,  0,  6,  0,
                     0, 16, -4, 16,  0, 32), ncol=6, nrow=6, byrow=T)/360

    #  assemble the mass matrix


   K0 <- matrix(0,nrow=nnod,ncol=nnod)
   for (el in 1:nele) {
   	    ind <- nodeindex[el,]
        K0[ind,ind] <- K0[ind,ind] + K0M * Jvec[el]
   }

} else if (order == 1) {
    #  the integrals of products of basis functions for master element:

    K0M <- matrix( c( 2,  1,  1,
                     1,  2,  1,
                     1 , 1,  2), ncol=3, nrow=3, byrow=T) / 24

    #  assemble the mass matrix

    K0 <- matrix(0,nrow=nnod,ncol=nnod)
    for (el in 1:nele) {
    	 ind <- nodeindex[el,]
         K0[ind,ind] <- K0[ind,ind] + K0M * Jvec[el]
    }
} else {
	stop("ORDER not 1 or 2")
}

K0

}
