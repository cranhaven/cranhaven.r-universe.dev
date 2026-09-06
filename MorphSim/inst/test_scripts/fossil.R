### small helper function to map the fossil object onto the tree that
### same way as for the transisitions

## saved in terms of the descendant tip.
# First get the branch number by using $edge. Can determine the parent and then order
# number is the branch number. Add this to f object and call it ape.branch

f.morpho <- f
f.morpho$ape.branch <-NA
f.morpho$specimen <- NA
f.morpho$specimen <- seq(1,length(f.morpho$sp))

state_at_fossils <- matrix(nrow = length(f.morpho$sp), ncol = trait.num)
rownames(state_at_fossils) <- f.morpho$specimen


for ( i in 1:length(f$edge)){
child <- f$edge[i]
f.morpho$ape.branch[i] <- which(t$edge[,2] == child)
}



for ( tr.num in 1:trait.num){


##go through each fossil for trait n

for (spec in 1:length(f.morpho$specimen)){

branch <- f.morpho$ape.branch[[spec]]
parent <- tree.ordered$edge[branch,1]
current_state <- state_at_nodes[[which(rownames(state_at_nodes) == parent),tr.num]]

## does this branch have any changes?

if (!(f.morpho$ape.branch[spec]  %in% transition_history[[tr.num]][[1]])){
  state_at_fossils[spec,tr.num] <- current_state
  } else

    position_fossil <- f.morpho$hmin[spec]  * br.rates
    changes <- which(transition_history[[tr.num]][[1]] == f.morpho$ape.branch[spec])
    changes_along_edge <- transition_history[[tr.num]][changes,]

    ## remove changes after fossil occurance
    later <- which(changes_along_edge[,3] > position_fossil)
    changes_along_edge <- changes_along_edge[-later,]


   if ( length(changes_along_edge[,1]) == 0) {
     state_at_fossils[spec,tr.num] <- current_state
   } else {
      tran <- which(changes_along_edge$hmin == max(changes_along_edge$hmin))
      state_at_fossils[spec,tr.num] <- changes_along_edge[tran,2]
     }

}

}




