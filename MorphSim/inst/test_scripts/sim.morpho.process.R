
sim.morpho.process <- function(tree = NULL, time.tree= NULL, br.rates = NULL,
                       k = 2, trait.num = 2){



  # check that a tree is provided
  if (is.null(tree) && is.null(time.tree)) stop(cat("Must provide a tree object"))
  # check that there are more then two states
  if (k < 2) stop(cat("Trait data must have more than 1 state"))



  ## if provided with time tree, need to transform branches in genetic distance
  ## rates can be a single value or a vector for each branch
  if (is.null(tree) && !is.null(time.tree)){
    tree <- time.tree

    if(is.null(br.rates)){
      print("No branch rate provide, using default of 0.1 for all branches")
      tree$edge.length <- time.tree$edge.length * 0.1
    } else {
      tree$edge.length <- time.tree$edge.length * br.rates
    }
  }



  ### for a symmetric Q matrix (default?)
  Q <- symmetric.Q.matrix(k)
  trait.num <- trait.num

  states <- as.character(c(0:(k-1)))

  # reorder branches in the phylogeny
  #tree.ordered <- ape::reorder.phylo(tree, "postorder")
  tree.ordered <- reorder(tree)
  edge <- tree.ordered$edge
  num.nodes <- max(edge)
  num.tips <- ape::Ntip(tree.ordered)

  ##reorder the nodes on the time tree to match the format of the genetic distance tree
  if (!is.null(time.tree)) {
    time.tree.order <- reorder(time.tree)
  } else {time.tree.order = NULL}

  edge <- tree.ordered$edge
  num.nodes <- max(edge)
  num.tips <- ape::Ntip(tree.ordered)


  ### for (trait in 1:length(triat.num))


  # container for the transitions
  transitions <- matrix(ncol = 4, nrow= 0)
  colnames(transitions) <- c("edge", "state", "hmin", "hmax")

  # collect what branches have been "done"
  sampled_branches <-  matrix(ncol=2, nrow = 0)
  colnames(sampled_branches) <- c("parent", "child")
  num_tips <- ape::Ntip(tree.ordered)

for (branches in 1:num_tips){

trace_node <- tree.ordered$tip.label[branches]
tip_path <- find_path_to_tip(tree.ordered, trace_node)
full_tip_path <- tip_path

# how many rows are the same?
num_rows_to_remove <- merge(sampled_branches, tip_path)

if (!nrow(num_rows_to_remove) == 0){
num_rows <- length(tip_path[,1])
num_rows_to_rem <- length(num_rows_to_remove[,1]) + 1
tip_path <- tip_path[num_rows_to_rem:num_rows,]

}
# if intger convert into a matrix again, easier for the rest of the set up
if (!is.matrix(tip_path)){
  tip_path <- matrix(tip_path, nrow=1, ncol = 2)
}
### get the edge numbers
edges <- c()
for(p in 1:length(tip_path[,1])){
  for (t in 1:nrow(tree.ordered$edge)) {
    if (identical(tree.ordered$edge[t, ], as.vector(tip_path[p,]))) {
      edges <- c(edges, t)
      #print(t)
    }
  }
}

### get the edge numbers for full path
full_edges <- c()
for(p in 1:length(full_tip_path[,1])){
  for (t in 1:nrow(tree.ordered$edge)) {
    if (identical(tree.ordered$edge[t, ], as.vector(full_tip_path[p,]))) {
      full_edges <- c(full_edges, t)
      #print(t)
    }
  }
}


# can use this to say if this branch has already been sampled ignore it
sampled_branches <- rbind(sampled_branches, tip_path)



# Is the process starting from the root of an internal node?
if(as.vector(tip_path[1,1]) == ape::Ntip(tree.ordered) + 1){

# define the node labels
parent <- as.integer(edge[, 1])
child <- as.integer(edge[, 2])

#identify the root
root <- as.integer(parent[!match(parent, child, 0)][1])

if(!exists("root.state")){
 # simulate the root state
root.state <- sample(states, trait.num, replace = TRUE, prob = rep(1/k, k)) #later prob would need to take into account for bf
# set the root state as current state. This will be updated
}
current_state <- as.numeric(root.state)

} else{
#### if we are starting from an internal node we need to get the state at that node
parent_node_here <- as.vector(tip_path[1,1])
associated_edge_length <- which(tree.ordered$edge[,2] == parent_node_here)



# was the a transition along this branch, if not go back to the node before
# is this the root? if so set root state
matrix_row <- which(transitions[,"edge"] == associated_edge_length)

if (length(matrix_row) == 1){
last_transition <- max(which(transitions[,"edge"] == associated_edge_length))
current_state <- as.numeric(transitions[last_transition,"state"])

} else {
  # what edges do we have transitions for ?
  transitions_edges <- unique(as.numeric(as.vector(sort(transitions[,"edge"]))))
  associated_edge_length <-  intersect(transitions_edges, full_edges)
if (length(associated_edge_length)  == 0){
  current_state <- as.numeric(root.state)
} else {
  associated_edge_length <- max(associated_edge_length)
  last_transition <- max(which(transitions[,"edge"] == associated_edge_length))
  current_state <- as.numeric(transitions[last_transition,"state"])
}

  #}
}
# get the rate of change acorss the matrix

}
current_rate <- Q[(current_state+ 1),(current_state+1)] * -1


# calculate the total distance in the evolutionary process
blength <- 0
for ( bl in 1:length(edges)){
blength <- blength +  tree.ordered$edge.length[edges][bl]
}

## rate is taken from the Qmatrix
rand = rpois(1, blength*current_rate)

if (rand > 0 ){
# get the minmum changes as we need to restart the process as the rate may change. For a
# symetic matrix however we have all the changes now.

# for an asymetric matrix
#h = min(runif(rand, min = 0, max = blength))

# for a symetric matrix
h = runif(rand, min = 0, max = blength)
h <- sort(h)

P  <- ape::matexpo(Q * blength)

# calculated the number of changes there for we need to ensure that a new state is sampled.
state_changes <- c()
new_state <- current_state
for (r in 1:rand){
while(as.numeric(new_state == current_state) ){
new_state <- sample(states, 1, replace = TRUE, prob = P[,(current_state+1)])
}
  state_changes <- rbind(state_changes, new_state)
  current_state <- as.numeric(new_state)
  #print(current_state)

}


## if we have more than one edge we need to determine which branch and where along
# the branch the transitions occurred
if (length(edges) == 1){
  for (p in 1:length(h)){
    position <- h[p]
    exact_branch_position <- h[p]
    add_t <- c(as.numeric(edges[1]), state_changes[p], as.numeric(exact_branch_position), as.numeric(exact_branch_position))
    transitions <- rbind(transitions, add_t)
  }

} else{

## create matrix of the transitions on the correct branch length
for (p in 1:length(h)){
  position <- h[p]
  br_len <- 0
for ( e in 1:length(edges)){
  br_len <- br_len + tree.ordered$edge.length[edges[e]]
if (position < br_len){
  if (e > 1){
  exact_branch_position <- h[p] - sum(tree.ordered$edge.length[1:(e-1)])
  } else {
    exact_branch_position <- h[p]
  }
  add_t <- c(edges[e], state_changes[p], exact_branch_position, exact_branch_position)
  transitions <- rbind(transitions, add_t)
  break
}
}
}

}
}

}

  return(transitions)
}

##### to get from the tip to the root of the tree
find_path_to_tip <- function(tree, tip) {
  # Ensure the tip is valid
  if (!tip %in% tree$tip.label) {
    stop("The specified tip is not found in the tree.")
  }

  # Get the edge matrix and root node
  edge <- tree$edge
  root <- ape::Ntip(tree) + 1

  # the node number of the tip
  tip_node <- which(tree$tip.label == tip)

  # Start from the tip node and move upward toward the root
  path <- tip_node
  current_node <- tip_node

  # loop until reaching the root node
  while (current_node != root) {
    # find the parent of the current node
    parent_node <- edge[edge[, 2] == current_node, 1]
    path <- c(parent_node, path)
    current_node <- parent_node
  }
  length(path)-1
  output <- matrix(nrow =length(path)-1, ncol=2)
  colnames(output) <- c("parent", "child")

  for (i in 1:length(path)-1){
    output[i,] <- c(path[i], path[i+1])
  }

  return(output)
}



