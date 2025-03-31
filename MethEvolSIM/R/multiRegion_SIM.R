#' @title singleStructureGenerator
#' @importFrom R6 R6Class
#'
#' @description
#' an R6 class representing a single genomic structure
singleStructureGenerator <-
  R6::R6Class("singleStructureGenerator",
              portable = FALSE,
              private = list(
                ## @field my_combiStructure Private attribute: Object of class combiStructureGenerator containing it
                my_combiStructure = NULL,
                ## @field combiStructure_index Private attribute: Index in Object of class combiStructureGenerator
                combiStructure_index = NULL,
                ## @field alpha_pI Private attribute: Model parameter for sampling island equilibrium frequencies
                alpha_pI = 0.1,
                ## @field beta_pI Private attribute: Model parameter for sampling island equilibrium frequencies
                beta_pI = 1,
                ## @field alpha_mI Private attribute: Model parameter for sampling island equilibrium frequencies
                alpha_mI = 0.1,
                ## @field beta_mI Private attribute: Model parameter for sampling island equilibrium frequencies
                beta_mI = 0.5,
                ## @field alpha_pNI Private attribute: Model parameter for sampling non-island equilibrium frequencies
                alpha_pNI = 0.1,
                ## @field beta_pNI Private attribute: Model parameter for sampling non-island equilibrium frequencies
                beta_pNI = 1,
                ## @field alpha_mNI Private attribute: Model parameter for sampling non-island equilibrium frequencies
                alpha_mNI = 0.5,
                ## @field beta_mNI Private attribute: Model parameter for sampling non-island equilibrium frequencies
                beta_mNI = 0.1,
                ## @field globalState Private attribute: Structure's favored global state: "M" for methylated (island structures) / "U" for unmethylated (non-island structures)
                globalState = NULL,
                ## @field eqFreqs Private attribute: Structure's methylation state equilibrium frequencies (for unmethylated, partially methylated and methylated)
                eqFreqs = NULL,

                ## @description
                ## Private method: Sample equilibrium frequencies
                ##
                ## This function samples the object's eqFreqs based on the object's globalState.
                ## Cases when the sample_eqFreqs method is called:
                ##
                ## 1. **During Initialization:** If equilibrium frequencies (eqFreqs) are not provided during the initialization of the object, this method is automatically called to set eqFreqs based on the object's globalState.
                ##
                ## 2. **In IWE_evol():** The method is called within the IWE_evol() function to sample new equilibrium frequencies for the structure.
                ##
                ## @return A numeric vector representing equilibrium frequencies for unmethylated, partially methylated, and methylated states.
                ##
                sample_eqFreqs = function() {
                  if (private$globalState == "M") {
                    p <- stats::rbeta(1, private$alpha_pNI, private$beta_pNI)
                    m <- stats::rbeta(1, private$alpha_mNI, private$beta_mNI) * (1 - p)
                    u <- 1 - p - m; if (u == 1) p = m = 0
                  } else if (private$globalState == "U") {
                    p <- stats::rbeta(1, private$alpha_pI, private$beta_pI)
                    m <- stats::rbeta(1, private$alpha_mI, private$beta_mI) * (1 - p)
                    u <- 1 - p - m; if (u == 1) p = m = 0
                  } else {
                    stop("Invalid globalState")
                  }
                  # Normalize u, p, m so that their sum is 1
                  total <- sum(u, p, m)
                  u <- u / total
                  p <- p / total
                  m <- m / total
                  return(c(u, p, m))
                },
                ## @field seq Private attribute: Encoded sequence of CpGs methylation state: 1 for unmethylated, 2 for partially-methylated, 3 for methylated
                seq = NULL,
                ## @field siteR Private attribute: Encoded sequence of CpGs site SSEi rate: 1 for Ri_values[1], 2 for Ri_values[2] and 3 for Ri_values[3]
                siteR = NULL,
                ## @field neighbSt Private attribute: Encoded sequence of CpGs site neighbor state: as in mapNeighbSt_matrix
                neighbSt = NULL,
                ## @field mapNeighbSt_matrix Private attribute: Matrix encoding neighbor state
                ## Map from neighbor state to numerical coding #########################
                #### #### Rows represent left neighbor state (u,p,m)
                #### #### columns represent right neighbor state (u,p,m)
                #### ####     [,1] [,2] [,3]
                #### ####[1,]    1    2    3
                #### ####[2,]    4    5    6
                #### ####[3,]    7    8    9
                mapNeighbSt_matrix = matrix(c(1L:9L), byrow = TRUE, nrow = 3),
                ## @description
                ## Private method: Get next singleStructureGenerator object in my_combiStructure object
                ## @return next singleStructureGenerator object if it exists, NULL if it does not exist
                get_nextStr = function(){
                  if (is.null(private$combiStructure_index)) return(NULL)
                  if (private$combiStructure_index == private$my_combiStructure$get_singleStr_number()) return(NULL)
                  if (private$combiStructure_index < private$my_combiStructure$get_singleStr_number()){
                    private$my_combiStructure$get_singleStr(private$combiStructure_index + 1)
                  }
                },
                ## @description
                ## Private method: Get previous singleStructureGenerator object in my_combiStructure object
                ## @return previous singleStructureGenerator object if it exists, NULL if it does not exist
                get_prevStr = function(){
                  if (is.null(private$combiStructure_index)) return(NULL)
                  if (private$combiStructure_index == 1) return(NULL)
                  if (private$combiStructure_index > 1){
                    private$my_combiStructure$get_singleStr(private$combiStructure_index - 1)
                  }
                },
                ## @description
                ## Private method: Update $neighbSt of a CpG position's neighbors within singleStructureGenerator object
                ##
                ## changed $seq state and updates $neighbSt for the neighbors of the changed position
                ##
                ## @param position position index of the $seq change
                ##
                ## @return NULL
                ##
                update_intraStr_neighbSt = function(position){
                  if (!is.numeric(position) || length(position) != 1 || position != floor(position)) {
                    stop("'position' must be one integer index value")
                  }
                  if( position < 1 || position > length(private$seq)){
                    stop("'position' value must be within $seq length")
                  }

                  if (length(private$seq)== 1){ # cases with length 1
                    private$neighbSt[position] <<- private$mapNeighbSt_matrix[private$seq[1],private$seq[1]]
                  } else { # cases with length > 1
                    if (position == 1){
                      # position 1 has only one neighbor: position 2
                      if(length(private$seq) < position + 2){ # if position 2 has no right neighbor, it counts position 1 as both neighbors
                        private$neighbSt[position + 1] <<- private$mapNeighbSt_matrix[private$seq[position], private$seq[position]]
                      } else {
                        private$neighbSt[position + 1] <<- private$mapNeighbSt_matrix[private$seq[position], private$seq[position + 2]]
                      }
                    } else if (position == 2){
                      # position 1 has only one neighbor: position 2
                      private$neighbSt[position-1] <<- private$mapNeighbSt_matrix[private$seq[position], private$seq[position]]
                      if (position != length(private$seq)){
                        if (length(private$seq) < position + 2){
                          # position 3 has only one neighbor: position 2
                          private$neighbSt[position+1] <<- private$mapNeighbSt_matrix[private$seq[position], private$seq[position]]
                        } else {
                          private$neighbSt[position+1] <<- private$mapNeighbSt_matrix[private$seq[position], private$seq[position+2]]
                        }
                      }
                    } else if (position == length(private$seq)){
                      if (length(private$seq) < 3){
                        # position n-1 has only one neighbor: position n
                        private$neighbSt[position-1] <<- private$mapNeighbSt_matrix[private$seq[position], private$seq[position]]
                      } else {
                        private$neighbSt[position-1] <<- private$mapNeighbSt_matrix[private$seq[position-2], private$seq[position]]
                      }
                    } else if (position == length(private$seq)-1){
                      #last counts the only neighbor as both neighbors
                      private$neighbSt[position+1] <<- private$mapNeighbSt_matrix[private$seq[position], private$seq[position]]
                      private$neighbSt[position-1] <<- private$mapNeighbSt_matrix[private$seq[position-2], private$seq[position]]
                    } else {
                      # intermediate positions
                      private$neighbSt[position-1] <<- private$mapNeighbSt_matrix[private$seq[position - 2], private$seq[position]]
                      private$neighbSt[position+1] <<- private$mapNeighbSt_matrix[private$seq[position], private$seq[position+2]]
                    }
                  }
                },
                ## @description
                ## Private method: Update $neighbSt of a CpG position's neighbors within and between singleStructureGenerator objects
                ##
                ## This fuction takes in the position index of a CpG site that
                ## changed $seq state and updates $neighbSt for the neighbors of the changed position
                ##
                ## @param position position index of the $seq change
                ##
                ## @return NULL
                ##
                update_neighbSt = function(position){
                  if (!is.numeric(position) || length(position) != 1 || position != floor(position)) {
                    stop("'position' must be one integer index value")
                  }
                  if( position < 1 || position > length(private$seq)){
                    stop("'position' value must be within $seq length")
                  }
                  if (is.null(private$combiStructure_index) || private$my_combiStructure$get_singleStr_number() == 1){ ## Case of isolated singleStructure instance
                    private$update_intraStr_neighbSt(position)
                  } else { ## Case of singleStructure instances within combiStructure
                    
                    if (length(private$seq) == 1){ ## Cases with length 1
                      # Update leftNeighbSt
                      if (!is.null(private$get_prevStr())){
                        private$get_prevStr()$update_interStr_lastNeighbSt(private$get_prevStr()$get_seq2ndButLastPos(), private$seq[position])
                      }
                      # Update rightNeighbSt
                      if (!is.null(private$get_nextStr())){
                        private$get_nextStr()$update_interStr_firstNeighbSt(private$seq[position], private$get_nextStr()$get_seq2ndPos())
                      }
                      
                    } else { ## cases with length > 1
                      
                      if (position == 1){
                        # Update leftNeighbSt
                        if (!is.null(private$get_prevStr())){
                          private$get_prevStr()$update_interStr_lastNeighbSt(private$get_prevStr()$get_seq2ndButLastPos(), private$seq[position])
                        }
                        # Update rightNeighbSt
                        if(length(private$seq) < position + 2){
                          if (!is.null(private$get_nextStr())){
                            private$neighbSt[position + 1] <- private$mapNeighbSt_matrix[private$seq[position], private$get_nextStr()$get_seqFirstPos()]
                          } else {
                            private$neighbSt[position + 1] <- private$mapNeighbSt_matrix[private$seq[position], private$seq[position]]
                          }
                        } else {
                          private$neighbSt[position + 1] <- private$mapNeighbSt_matrix[private$seq[position], private$seq[position+2]]
                        }
                      } else if (position == 2){
                        # Update leftNeighbSt
                        if (!is.null(private$get_prevStr())){
                          private$neighbSt[position - 1] <- private$mapNeighbSt_matrix[private$get_prevStr()$get_seqLastPos(), private$seq[position]]
                        } else {
                          private$neighbSt[position - 1] <- private$mapNeighbSt_matrix[private$seq[position], private$seq[position]]
                        }
                        # Update rightNeighbSt
                        if(length(private$seq) < position + 2){
                          if(position == length(private$seq)){ # second position is also last
                            if (!is.null(private$get_nextStr())){ # if there is another structure to the right
                              private$get_nextStr()$update_interStr_firstNeighbSt(private$seq[position], private$get_nextStr()$get_seq2ndPos())
                            }
                          } else {# second position is also previous to last
                            if (!is.null(private$get_nextStr())){
                              private$neighbSt[position + 1] <- private$mapNeighbSt_matrix[private$seq[position], private$get_nextStr()$get_seqFirstPos()]
                            } else {
                              private$neighbSt[position + 1] <- private$mapNeighbSt_matrix[private$seq[position], private$seq[position]]
                            }
                          }
                        } else { # second position is not last or previous to last
                          private$neighbSt[position + 1] <- private$mapNeighbSt_matrix[private$seq[position], private$seq[position+2]]
                        }
                      } else if (position == length(private$seq)){ 
                        # Update leftNeighbSt (always position 3 or higher, so it has two left neighbors)
                        private$neighbSt[position - 1] <- private$mapNeighbSt_matrix[private$seq[position-2], private$seq[position]]
                        # Update rightNeighbSt
                        if(!is.null(private$get_nextStr())){
                          private$get_nextStr()$update_interStr_firstNeighbSt(private$seq[position], private$get_nextStr()$get_seq2ndPos())
                        }
                        
                      } else if (position == length(private$seq)-1){
                        # Update leftNeighbSt (always position 3 or higher, so it has two left neighbors)
                        private$neighbSt[position - 1] <- private$mapNeighbSt_matrix[private$seq[position-2], private$seq[position]]
                        # Update rightNeighbSt
                        if(!is.null(private$get_nextStr())){
                          private$neighbSt[position + 1] <- private$mapNeighbSt_matrix[private$seq[position], private$get_nextStr()$get_seqFirstPos()]
                        } else {
                          private$neighbSt[position + 1] <- private$mapNeighbSt_matrix[private$seq[position], private$seq[position]]
                        }
                      } else {
                        private$neighbSt[position - 1] <- private$mapNeighbSt_matrix[private$seq[position-2], private$seq[position]]
                        private$neighbSt[position + 1] <- private$mapNeighbSt_matrix[private$seq[position], private$seq[position+2]]
                      }
                    } ## End of cases with length > 1
        
                  } ## End of case of singleStructure instances within combiStructure
                },
                ## @field alpha_Ri Private attribute: Model parameter for gamma distribution shape to initialize the 3 $Ri_values
                alpha_Ri = 0.1,
                ## @field iota Private attribute: Model parameter for gamma distribution expected value to initialize the 3 $Ri_values
                iota = 0.3,
                ## @field Ri_values Private attribute: Vector containing 3 Ri rate values
                Ri_values = NULL,
                ## @description
                ## Private method: Initialize $Ri_values
                ##
                ## This fuction uses $iota and $alpha_Ri to compute the 3 Ri_values
                ##
                ## @return A list with the 3 Ri_values
                ##
                init_Ri_values = function(){
                  # Values under alpha_Ri 1e-2 lead to quantile values so close to 0 that they are rounded to 0
                  # To prevent that:
                  private$alpha_Ri <- max(private$alpha_Ri, 1e-2)
                  # Very small values of iota lead to error in integrate for Ri3 due to a very small probability of the tail
                  # To prevent that:
                  private$iota <- max(private$iota, 1e-2)
                  
                  # Set the values that divide the gamma distribution in 3 equal probability categories
                  qGamma_oneThird <- stats::qgamma(1/3, shape = private$alpha_Ri, scale= private$iota / private$alpha_Ri)
                  qGamma_twoThird <- stats::qgamma(2/3, shape = private$alpha_Ri, scale= private$iota / private$alpha_Ri)

                  # Calculate the center of gravity of each gamma distribution category
                  Ri1 <- stats::integrate(function(x) x*stats::dgamma(x, shape =  private$alpha_Ri, scale =  private$iota / private$alpha_Ri) * 3, 0, qGamma_oneThird)$value
                  Ri2 <- stats::integrate(function(x) x*stats::dgamma(x, shape =  private$alpha_Ri, scale =  private$iota / private$alpha_Ri) * 3, qGamma_oneThird, qGamma_twoThird)$value
                  Ri3 <- stats::integrate(function(x) x*stats::dgamma(x, shape =  private$alpha_Ri, scale =  private$iota / private$alpha_Ri) * 3, qGamma_twoThird, Inf)$value
                  return(c(Ri1, Ri2, Ri3))
                },
                ## @field Rc_values Private attribute: Vector containing 2 Rc rate values
                Rc_values = NULL,
                ## @description
                ## Private method: Initialize $Rc_values
                ##
                ## This fuction uses $iota to compute the 2 Rc_values
                ##
                ## @return A list with the 2 Rc_values
                ##
                init_Rc_values = function(){
                  Rcl <- (1 - private$iota) / 2
                  Rcr <- (1 - private$iota) / 2
                  return(list(Rcl = Rcl, Rcr= Rcr))
                },
                ## @field Qi Private attribute: Rate matrix for the independent SSE process
                Qi = NULL,
                ## @description
                ## Private method: Set $Qi
                ##
                ## This fuction is used:
                ##
                ## 1. **During Initialization:** During the initialization of the object, this method is automatically called to set $Qi.
                ##
                ## 2. **In IWE_evol():** The method is called within the IWE_evol() to update $Qi after new $eqFreqs are sampled.
                ##
                ## @return NULL
                ##
                set_Qi = function(){
                  ## Extract methylation state frequencies
                  u <- private$eqFreqs[1]
                  p <- private$eqFreqs[2]
                  m <- private$eqFreqs[3]

                  ## Create the rate matrices for SSEind
                  ## Matrix without R
                  Qi0 <- matrix(c(-p-m, p, m, u, -u-m, m, u, p, -u-p), nrow = 3, byrow = TRUE)
                  ## Matrices with the 3 R values
                  Qi1 <- Qi0 * private$Ri_values[1]
                  Qi2 <- Qi0 * private$Ri_values[2]
                  Qi3 <- Qi0 * private$Ri_values[3]
                  ## Save rate matrices in a list, and validate them
                  private$Qi <<- list(Qi1, Qi2, Qi3)
                },
                ## @field Qc Private attribute: Rate matrix for the correlated SSE process
                Qc = NULL,
                ## @description
                ## Private method: Set $Qc
                ##
                ## This fuction is used to initialize the singleStructure object
                ##
                ## @return NULL
                ##
                set_Qc = function(){
                  ##### Matrices for SSEcor for neighbors state as coded in mapNeighbSt_matrix
                  Rcl <- private$Rc_values$Rcl
                  Rcr <- private$Rc_values$Rcr
                  #### #### uu = 1
                  Qc1 <- matrix(c(0, 0, 0, Rcl + Rcr, -Rcl - Rcr, 0, Rcl + Rcr, 0, -Rcl - Rcr), nrow = 3, byrow = TRUE)
                  #### #### up = 2
                  Qc2 <- matrix(c(-Rcr, Rcr, 0, Rcl , -Rcl, 0, Rcl, Rcr, -Rcl - Rcr), nrow = 3, byrow = TRUE)
                  #### #### um = 3
                  Qc3 <- matrix(c(-Rcr, 0, Rcr, Rcl , -Rcl - Rcr, Rcr, Rcl, 0, -Rcl), nrow = 3, byrow = TRUE)
                  #### #### pu = 4
                  Qc4 <- matrix(c(- Rcl, Rcl, 0, Rcr , -Rcr, 0, Rcr, Rcl, -Rcl - Rcr), nrow = 3, byrow = TRUE)
                  #### #### pp = 5
                  Qc5 <- matrix(c(-Rcl - Rcr, Rcl + Rcr, 0, 0, 0, 0, 0, Rcl + Rcr, -Rcl - Rcr), nrow = 3, byrow = TRUE)
                  #### #### pm = 6
                  Qc6 <- matrix(c(-Rcl - Rcr, Rcl, Rcr, 0, -Rcr, Rcr, 0, Rcl, -Rcl), nrow = 3, byrow = TRUE)
                  #### #### mu = 7
                  Qc7 <- matrix(c(-Rcl, 0, Rcl, Rcr , -Rcl - Rcr, Rcl, Rcr, 0, -Rcr), nrow = 3, byrow = TRUE)
                  #### #### mp = 8
                  Qc8 <- matrix(c(-Rcl - Rcr, Rcr, Rcl, 0, -Rcl, Rcl, 0, Rcr, -Rcr), nrow = 3, byrow = TRUE)
                  #### #### mm = 9
                  Qc9 <- matrix(c(-Rcl - Rcr, 0, Rcl + Rcr, 0, -Rcl - Rcr, Rcl + Rcr, 0, 0, 0), nrow = 3, byrow = TRUE)
                  private$Qc <<- list(Qc1, Qc2, Qc3, Qc4, Qc5, Qc6, Qc7, Qc8, Qc9)
                },
                ## @field Q Private attribute: Rate matrix for the complete SSE process
                Q = NULL,
                ## @description
                ## Private method: Set $Q
                ##
                ## This fuction is used:
                ##
                ## 1. **During Initialization:** During the initialization of the object, this method is automatically called to set $Q.
                ##
                ## 2. **In IWE_evol():** The method is called within the IWE_evol() to update $Q after new $Qi is updated with the new $eqFreqs.
                ##
                ## @return NULL
                ##
                set_Q = function() {
                  # Initialize a list to store the results
                  private$Q <<- list()

                  for (i in 1:length(private$Qi)) {
                    # Initialize a list to store the results for this Ri
                    Qi_results <- list()

                    for (j in 1:length(private$Qc)) {
                      # Sum the rate matrices element-wise
                      result_matrix <- private$Qi[[i]] + private$Qc[[j]]

                      # Add the result to the list for this Ri
                      Qi_results[[j]] <- result_matrix
                    }

                    # Add list of results for corresponding Ri to the result list
                    private$Q[[i]] <<- Qi_results

                  }
                },
                ## @field ratetree Private attribute: Cumulative Rate of Change Binary Tree
                ## This will contain a list of K vectors of length 1, 2, 4,.., N=2^(K-1), where
                ## 2^(K-1) >= length($seq) > 2^(K-2).
                ## The first length($seq) entries of ratetree[[K]][1:length($seq)] will be the rates
                ## of a change at that sequence positions, and the ratetree[[K]][length($seq):N]
                ## are all 0.
                ## ratetree[[1]] will sum(ratetree[[K]]) contain the total rate of changes in the sequence.
                ## ratetree[[2]] will contain the sum of the values in the first half of ratetree[[K]] and
                ## the sum of the values in the second half of ratetree[[K]].
                ## The four values in ratetree[[3]] will be the sums of the values in each quarter of the values
                ## in ratetree[[K]], and so on.
                ratetree = NULL,
                ## @description
                ## Private method: Update $ratetree after a CpG position's change of methylation state
                ##
                ## This fuction takes in the position index of a CpG site that
                ## changed $seq state and the new rate of change and updates $ratetree
                ##
                ## @param position position index of the $seq change
                ## @param rate new rate of change
                ##
                ## @return NULL
                ##
                update_ratetree = function(position, rate) {
                  ## check_ratetree("beginning of update_ratetree")
                  K <- length(private$ratetree)
                  private$ratetree[[K]][position] <<- rate
                  if (K > 1){
                    for(k in (K-1):1) {
                      position <- floor((position-1)/2)+1
                      private$ratetree[[k]][position] <<- sum(private$ratetree[[k+1]][c(2*position-1,
                                                                                        2*position)])
                    }
                  }
                  ## check_ratetree("end of update_ratetree")
                 },

                ## @description
                ## Private method: checkes whether $ratetree is consitent
                ##
                ## @param s a string that can be used to indicate where in the code the test failed
                ##
                check_ratetree = function(s) {
                  K <- length(private$ratetree)
                  if (K > 1){
                      for(k in (K-1):1) {
                          for(i in 1:length(private$ratetree[[k]])) {
                              if(abs(private$ratetree[[k]][i] - sum(private$ratetree[[k+1]][c(2*i-1, 2*i)])) >
                                 1e-8*max(private$ratetree[[k+1]][c(2*i-1, 2*i)])) {
                                  stop(paste(paste(private$ratetree,collapse="\n"),
                                             "multiregion ratetree inconsistent at", s, k, i))
                              }
                          }
                      }
                  }
                },

                ## @description
                ## Private method: Update $ratetree after a CpG position's change of methylation state
                ##
                ## This fuction takes in the position index of a CpG site that
                ## changed $seq state and the new rate of change and updates $ratetree
                ##
                ## @param testing default FALSE. TRUE for enabling testing output
                ##
                ## @return $seq index if testing FALSE and additional sampled value if testing TRUE
                ##
                choose_random_seqpos = function(testing=FALSE) {
                  ## return a random sequence position that is chosen with a probability that is
                  ## proportional to the change rates stored in private$ratetree
                  r  <- runif(n = 1, min = 0, max = private$ratetree[[1]][1])
                  s <- r
                  i <- 1
                  K <- length(private$ratetree)
                  if (K > 1){
                    for(k in 2:K) {
                      i <- 2*i-1
                      if(s > private$ratetree[[k]][i]) {
                        s <- s - private$ratetree[[k]][i]
                        i <- i+1
                      }
                    }
                  } else {
                    i = 1
                  }
                  if(testing){
                    return(c(i, r))
                  }
                  i
                },
                ## @description
                ## Private method: Sample a number of changes in time interval of length dt
                ##
                ## This fuction takes in the length of the time interval (dt) and
                ## samples a number of changing state events from a Poisson distribution
                ## with rate given by the total rate of change of the singleStructure object
                ## over dt time
                ##
                ## @param dt length of time interval
                ##
                ## @return sampled number of changes
                ##
                choose_number_of_changes = function(dt) {
                  ## choose a number of changes to happen in the next time interval of the short length dt
                  if (private$ratetree[[1]][1] < 0){
                    saveRDS(private$ratetree, "rateTreeError.rds")
                    rateMatrices <- list(Q = private$Q,
                                         Qi = private$Qi,
                                         Qc = private$Qc)
                    saveRDS(rateMatrices, "rateMatricesError.rds")
                    stop("negative total rate. Rate tree in file rateTreeError and rate matrices in rateMatricesError")
                  }
                  rpois(1, private$ratetree[[1]][1] * dt)
                },
                
                ## @description
                ## Private Method. To update ratetree from another singleStructure instance
                ##
                ## @param nextStr default FALSE. Set to TRUE to update next structure's $ratetree
                ## @param prevStr default FALSE. Set to TRUE to update previous structure's $ratetree
                ##
                ## @return NULL
                update_ratetree_betweenStr = function(nextStr = FALSE, prevStr = FALSE) {
                  if(nextStr == FALSE && prevStr == FALSE){
                    stop("one of the arguments 'nextStr' or 'prevStr' needs to be TRUE")
                  }
                  if(nextStr){
                    if(!is.null(private$get_nextStr())){
                      siteR <- private$get_nextStr()$get_siteR(index = 1)
                      neighbSt <- private$get_nextStr()$get_neighbSt(index = 1)
                      state <- private$get_nextStr()$get_seq()[1]
                      new_rate <- abs(private$get_nextStr()$get_Q(siteR, neighbSt, state, state))
                      private$get_nextStr()$update_ratetree_otherStr(position = 1, rate = new_rate)
                    }
                  }
                  if(prevStr){
                    if(!is.null(private$get_prevStr())){
                      last_index <- length(private$get_prevStr()$get_seq())
                      siteR <- private$get_prevStr()$get_siteR(index = last_index)
                      neighbSt <- private$get_prevStr()$get_neighbSt(index = last_index)
                      state <- private$get_prevStr()$get_seq()[last_index]
                      new_rate <- abs(private$get_prevStr()$get_Q(siteR, neighbSt, state, state))
                      private$get_prevStr()$update_ratetree_otherStr(position = last_index, rate = new_rate)
                    }
                  }
                },
                
                ## @description
                ## Private Method. To update ratetree within and between structures
                ##
                ## @param index. Numerical. Positions that change state.
                ##
                ## @return NULL
                update_ratetree_allCases = function(index) {
                  for (i in index){
                    for(j in max(i-1, 1):min(i+1, length(private$seq))) {
                      private$update_ratetree(j, abs(private$Q[[private$siteR[j]]][[private$neighbSt[j]]][private$seq[j],private$seq[j]]))
                      if (!is.null(private$my_combiStructure)){
                        if (j == 1){
                          update_ratetree_betweenStr(prevStr = TRUE)
                        }
                        if (j == length(private$seq)){
                          update_ratetree_betweenStr(nextStr = TRUE)
                        }
                      }
                    }
                  }
                }
              ),
              public = list(
                #' @field testing_output Public attribute: Testing output for initialize
                testing_output = NULL,
                #' @description
                #' Public method: Initialization of $neighbSt
                #'
                #' This fuction initiates each CpG position $neighbSt as encoded in $mapNeighbSt_matrix
                #' 
                #' It uses $update_neighbSt which updates for each sequence index, the neighbSt of left and right neighbors
                #' This means that it updates position 2, then 1 and 3, then 2 and 4.. 
                #' Therefore, if the combiStructure instance has several singleStr instances within and the first has length 1,
                #' the $neighbSt of that position of the first singleStr instance is initialized when the method is called from the second singleStr instance
                #' 
                #' Positions at the edge of the entire simulated sequence use
                #' their only neighbor as both neighbors.
                #'
                #' @return NULL
                init_neighbSt = function(){
                  for (i in 1:length(private$seq)){
                    private$update_neighbSt(i)
                  }
                },
                #' @description
                #' Public method: Initialization of $ratetree
                #'
                #' This function initializes $ratetree
                #'
                #' @return NULL
                initialize_ratetree = function() {
                  K  <- ceiling(log(length(private$seq), 2)) + 1
                  for (k in 1:K) private$ratetree[[k]] <<- rep(0.0, 2^(k-1))
                  for (i in 1:length(private$seq)) {
                    private$ratetree[[K]][i] <<- abs(private$Q[[private$siteR[i]]][[private$neighbSt[i]]][private$seq[i],private$seq[i]])
                    ### AND NOTE THAT THE WAY THIS IS NOW CHOSEN MEANS THAT THE CHOSEN SITE SHOULD REALLY CHANGE (!) THE STATE
                  }
                  # Control for length of seq is 1, K = 1, k is 0
                  if(K > 1){
                    for (k in (K-1):1) {
                      for(i in 1:length(private$ratetree[[k]])) private$ratetree[[k]][i] <<- sum(private$ratetree[[k+1]][c(2*i-1, 2*i)])
                    }
                  }
                },
                #' @description
                #' Create a new singleStructureGenerator object.
                #'
                #' Note that this object is typically generated withing a combiStructureGenerator object.
                #'
                #' @param globalState Character. Structure's favored global state: "M" for methylated (island structures) / "U" for unmethylated (non-island structures).
                #' @param n Numerical Value. Number of CpG positions
                #' @param eqFreqs Default NULL. When given: numerical vector with structure's methylation state equilibrium frequencies (for unmethylated, partially methylated and methylated)
                #' @param combiStr Default NULL. When initiated from combiStructureGenerator: object of class combiStructureGenerator containing it
                #' @param combiStr_index Default NULL. When initiated from combiStructureGenerator: index in Object of class combiStructureGenerator
                #' @param params Default NULL. When given: data frame containing model parameters
                #' @param testing Default FALSE. TRUE for writing in public field of new instance $testing_output
                #' @return A new `singleStructureGenerator` object.
                initialize = function(globalState, n, eqFreqs = NULL, combiStr = NULL, combiStr_index = NULL,  params = NULL, testing = FALSE) {
                  if (!is.character(globalState)) {
                    stop("globalState must be a character")
                  }
                  # Check if globalState is valid
                  if (!(globalState %in% c("M", "U"))) {
                    stop("globalState must be 'M' or 'U'")
                  }
                  if (!is.numeric(n)) {
                    stop("n must be a numeric value")
                  }
                  if (!length(n)==1){
                    stop("n must be of length 1")
                  }
                  if(!is.null(params)){
                    private$alpha_pI <- params$alpha_pI
                    private$beta_pI <- params$beta_pI
                    private$alpha_mI <- params$alpha_mI
                    private$beta_mI <- params$beta_mI
                    private$alpha_pNI <- params$alpha_pNI
                    private$beta_pNI <- params$beta_pNI
                    private$alpha_mNI <- params$alpha_mNI
                    private$beta_mNI <- params$beta_mNI
                    private$alpha_Ri <- params$alpha_Ri
                    private$iota <- params$iota
                  }
                  private$globalState <- globalState
                  if(is.null(eqFreqs)){
                    private$eqFreqs <- private$sample_eqFreqs()
                  } else {
                    if(!(is.numeric(eqFreqs) && length(eqFreqs) == 3 && abs(sum(eqFreqs) -1) < 1e-8)){
                      stop("if 'eqFreqs' is not NULL, provide a numeric vector of 3 frequencies ")
                    }
                    private$eqFreqs <- eqFreqs
                  }
                  private$seq <- sample(1L:3L, size = n, prob = private$eqFreqs, replace = TRUE)
                  private$siteR <- sample(1L:3L, size = n, replace = TRUE)
                  private$my_combiStructure <- combiStr
                  private$combiStructure_index <- combiStr_index
                  private$Ri_values <- private$init_Ri_values()

                  private$Rc_values <- private$init_Rc_values()
                  private$set_Qi()
                  private$set_Qc()
                  private$set_Q()
                  #undebug(self$init_neighbSt)
                  #debug(self$SSE_evol)
                  if(is.null(private$my_combiStructure)){
                    self$init_neighbSt()
                    #debug(self$initialize_ratetree)
                    self$initialize_ratetree()
                  }
                },
                #' @description
                #' Public method: Set my_combiStructure. Assigns given combi instance to private field my_combiStructure
                #' 
                #' @param combi instance of combiStructureGenerator
                #'
                #' @return NULL
                set_myCombiStructure = function(combi) private$my_combiStructure <- combi,
                #' @description
                #' Public method: Get object's methylation state sequence
                #'
                #' Encoded with 1 for unmethylated, 2 for partially methylated and 3 for methylated
                #'
                #' @return vector with equilibrium frequencies of unmethylated, partially methylated and methylated
                get_seq = function() private$seq,
                #' @description
                #' Public method: Get first sequence position methylation state
                #'
                #' @return numerical encoding of first position's methylation state
                get_seqFirstPos = function () private$seq[1],
                #' @description
                #' Public method: Get second sequence position methylation state
                #'
                #' @return numerical encoding of second position's methylation state. NULL if position does not exist
                get_seq2ndPos = function () {
                  if (length(private$seq)==1){ # if singleStr contains only one position
                    if (is.null(private$get_nextStr())) NULL # and if there is no other singleStr to the the right, return NULL
                    else private$get_nextStr()$get_seqFirstPos() # if there is other singleStr to the right, return seq state first position
                  } else { # if singleStr contains more then one position, return the seq state of second position
                    private$seq[2]
                  }
                },
                #' @description
                #' Public method: Get first sequence position methylation state
                #'
                #' @return numerical encoding of first position's methylation state
                get_seqLastPos = function() private$seq[length(private$seq)],
                #' @description
                #' Public method: Get second but last sequence position methylation state
                #'
                #' @return numerical encoding of second but last position's methylation state. NULL if position does not exist
                get_seq2ndButLastPos = function () {
                  if (length(private$seq)==1){ # if singleStr contains only one position
                    if (is.null(private$get_prevStr())) NULL # and there is no other singleStr to the left, return NULL
                    else private$get_prevStr()$get_seqLastPos() # if there is other singleStr to the left, return seq state of last position
                  } else { # if singleStr contains more than one position, return the seq state of position previous to last
                    private$seq[length(private$seq)-1]
                  }
                },
                #' @description
                #' Public method: Get index in object of class combiStructureGenerator
                #'
                #' @return index in object of class combiStructureGenerator
                get_combiStructure_index = function() private$combiStructure_index,
                #' @description
                #' Public method: Update neighbSt of next singleStructureGenerator object within combiStructureGenerator object
                #'
                #' This function is used when the last $seq position of a singleStructureGenerator object
                #' changes methylation state to update the neighbSt position
                #'
                #' @param leftNeighb_seqSt $seq state of left neighbor (left neighbor is in previous singleStructureGenerator object)
                #' @param rightNeighb_seqSt $seq state of right neighbor
                #'
                #' @return NULL
                update_interStr_firstNeighbSt = function(leftNeighb_seqSt, rightNeighb_seqSt) {
                  
                  # Check if leftNeighb_seqSt is NULL
                  if (is.null(leftNeighb_seqSt)) {
                    stop("Error: argument 'leftNeighb_seqSt' cannot be NULL")
                  }
                  
                  # Check if leftNeighb_seqSt is 1, 2, or 3
                  if (!leftNeighb_seqSt %in% c(1, 2, 3)) {
                    stop("Error: argument 'leftNeighb_seqSt' must be 1, 2, or 3")
                  }
                  
                  # Check if rightNeighb_seqSt is NULL or 1, 2, or 3
                  if (!is.null(rightNeighb_seqSt) && !rightNeighb_seqSt %in% c(1, 2, 3)) {
                    stop("Error: argument 'rightNeighb_seqSt' must be NULL, 1, 2, or 3")
                  }
                  
                  if(!is.null(rightNeighb_seqSt)){
                    private$neighbSt[1] <<- private$mapNeighbSt_matrix[leftNeighb_seqSt, rightNeighb_seqSt]
                  } else { # if there is no right neighbour use left neighb as both neighbors
                    private$neighbSt[1] <<- private$mapNeighbSt_matrix[leftNeighb_seqSt, leftNeighb_seqSt]
                  }
                },
                #' @description
                #' Public method: Update neighbSt of previous singleStructureGenerator object within combiStructureGenerator object
                #'
                #' @param leftNeighb_seqSt $seq state of right neighbor (left neighbor is in next singleStructureGenerator object)
                #' @param rightNeighb_seqSt $seq state of right neighbor
                #'
                #' @return NULL
                update_interStr_lastNeighbSt = function(leftNeighb_seqSt, rightNeighb_seqSt){
                  
                  # Check if rightNeighb_seqSt is NULL
                  if (is.null(rightNeighb_seqSt)) {
                    stop("Error: argument 'rightNeighb_seqSt' cannot be NULL")
                  }
                  
                  # Check if rightNeighb_seqSt is 1, 2, or 3
                  if (!rightNeighb_seqSt %in% c(1, 2, 3)) {
                    stop("Error: argument 'rightNeighb_seqSt' must be 1, 2, or 3")
                  }
                  
                  # Check if leftNeighb_seqSt is NULL or 1, 2, or 3
                  if (!is.null(leftNeighb_seqSt) && !leftNeighb_seqSt %in% c(1, 2, 3)) {
                    stop("Error: argument 'leftNeighb_seqSt' must be NULL, 1, 2, or 3")
                  }
                  position <- length(private$seq)
                  if(!is.null(leftNeighb_seqSt)){
                    private$neighbSt[position] <<- private$mapNeighbSt_matrix[leftNeighb_seqSt, rightNeighb_seqSt]
                  } else { # if there is no left neighbour use right neighb as both neighbors
                    private$neighbSt[position] <<- private$mapNeighbSt_matrix[rightNeighb_seqSt, rightNeighb_seqSt]
                  }
                },
                #' @description
                #' Public method: Get object's equilibrium Frequencies
                #'
                #' @return vector with equilibrium frequencies of unmethylated, partially methylated and methylated
                get_eqFreqs = function() private$eqFreqs,

                #' @description
                #' Public method. Simulate how CpG dinucleotide methylation state changes due to the SSE process
                #' along a time step of length dt
                #'
                #' @param dt time step length.
                #' @param testing logical value for testing purposes. Default FALSE.
                #'
                #' @return default NULL. If testing TRUE it returns a list with the debugNov3.outnumber of events sampled and a
                #' dataframe with the position(s) affected, new state and old methylation state.
                #'
                SSE_evol = function(dt, testing = FALSE) {
                    if (testing){
                        event_number <- 0
                        SSE_evolInfo <- data.frame(
                            position = integer(),
                            old_St = integer(),
                            new_St = integer()
                        )
                    }
                    ## get a number of changes to happen in the next time interval of the short length dt
                    M <- private$choose_number_of_changes(dt)
                    if (M>0){
                        for(m in 1:M) {
                          if(private$ratetree[[1]][1] >= 1e-8){ # control for the case in which previous
                            # events m in 1:M have lead the singleStr to update rates of change so that
                            # they are 0
                            
                            i <- private$choose_random_seqpos()
                            if (testing){
                              event_number <- M
                              position <- i
                              old_St <- private$seq[i]
                            }
                            
                            # assign new sequence position state with probability given by the relative rates of changing to each of the 2 other states
                            private$seq[i] <<- sample(1:3, size=1, prob=sapply(Q[[private$siteR[i]]][[private$neighbSt[i]]][private$seq[i],], max, 0))
                            
                            if (testing){
                              new_St <- private$seq[i]
                              SSE_evolInfo <- rbind(SSE_evolInfo, data.frame(position, old_St, new_St))
                            }
                            
                            # update neighbSt and ratetree (both methods update at neighbouring singleStructure instances if i is 1st or last position)
                            private$update_neighbSt(i)
                            private$update_ratetree_allCases(index = i)
                          }
                        }
                    }
                    if (testing){
                        return(list(event_number = event_number,
                                    SSE_evolInfo = SSE_evolInfo))
                    }
                },
                
                #' @description
                #' Public Method. Get a transition matrix
                #'
                #' @param old_eqFreqs numeric vector with 3 frequency values (for old u, p and m)
                #' @param new_eqFreqs numeric vector with 3 frequency values (for new u, p and m)
                #' @param info character string to indicate where the method is being called
                #' @param testing logical value for testing purposes. Default FALSE.
                #'
                #' @return transMat. The transition matrix. If testing = TRUE it returns a list.
                #' If there was a change in the equilibrium frequencies the list contains the following 7 elements, if not it contains the first 3 elements:
                #' \describe{
                #'       \item{\code{transMat}}{transition matrix}
                #'       \item{\code{case}}{The applied case.}
                #' }
                #'
                #'
                #' @details Given a tripple of old equilibrium frequencies and new equilibrium frequencies, generates the corresponding transition matrix.
                #'
                get_transMat = function(old_eqFreqs, new_eqFreqs, info, testing = FALSE) {
                  if(!is.character(info)) stop("Argument 'info' needs to be a character string")
                  if (!is.numeric(old_eqFreqs) || !is.numeric(new_eqFreqs) || length(old_eqFreqs) != 3 || length(new_eqFreqs) != 3) {
                    stop(paste("Arguments 'old_eqFreqs' and 'new_eqFreqs'  must be numeric vectors of length 3. Method called from:", info))
                  }
                  if (any(old_eqFreqs < 0 | old_eqFreqs > 1) || any(new_eqFreqs < 0 | new_eqFreqs > 1)) {
                    stop("All elements of 'old_eqFreqs' and 'new_eqFreqs' must be between 0 and 1.")
                  }
                  if (abs(sum(old_eqFreqs) -1) > 1e-8){
                    msg <- paste("Given vector of frequencies does not sum 1.",
                                 "\n sum(old_eqFreqs):", sum(old_eqFreqs), "\n",
                                 "freq u:", old_eqFreqs[1], "\n",
                                 "freq p:", old_eqFreqs[2], "\n",
                                 "freq m:", old_eqFreqs[3], "\n",
                                 "Method called from:", info)
                    warning(msg)
                  }
                  if (abs(sum(new_eqFreqs) -1) > 1e-8){
                    msg <- paste("Given vector of frequencies does not sum 1.",
                                 "\n sum(new_eqFreqs):", sum(new_eqFreqs), "\n",
                                 "freq u:", new_eqFreqs[1], "\n",
                                 "freq p:", new_eqFreqs[2], "\n",
                                 "freq m:", new_eqFreqs[3], "\n",
                                 "Method called from:", info)
                    warning(msg)
                  }
                  
                  u <- old_eqFreqs[1]
                  p <- old_eqFreqs[2]
                  m <- old_eqFreqs[3]
                  if (new_eqFreqs[1] >= u & new_eqFreqs[2] <= p & new_eqFreqs[3] <= m) {
                    case <- "Case 1. u bigger or equal"
                    transMat <- matrix(c(1, 0, 0,
                                         (p-new_eqFreqs[2])/p, new_eqFreqs[2]/p, 0,
                                         (m-new_eqFreqs[3])/m, 0, new_eqFreqs[3]/m),
                                       nrow = 3, byrow = TRUE)
                    if (p == 0){
                      transMat[2,] <- c(1,0,0)
                    }
                    if (m == 0){
                      transMat[3,] <- c(1,0,0)
                    }
                    
                  } else if (new_eqFreqs[2] >= p & new_eqFreqs[1] <= u & new_eqFreqs[3] <= m) {
                    case <- "Case 1. p bigger or equal"
                    transMat <- matrix(c(new_eqFreqs[1]/u, (u-new_eqFreqs[1])/u, 0,
                                         0, 1, 0,
                                         0, (m-new_eqFreqs[3])/m, new_eqFreqs[3]/m),
                                       nrow = 3, byrow = TRUE)
                    if (u == 0){
                      transMat[1,] <- c(0,1,0)
                    }
                    if (m == 0){
                      transMat[3,] <- c(0,1,0)
                    }
                    
                  } else if (new_eqFreqs[3] >= m & new_eqFreqs[2] <= p & new_eqFreqs[1] <= u) {
                    case <- "Case 1. m bigger or equal"
                    transMat <- matrix(c(new_eqFreqs[1]/u, 0, (u-new_eqFreqs[1])/u,
                                         0, new_eqFreqs[2]/p, (p-new_eqFreqs[2])/p,
                                         0, 0, 1),
                                       nrow = 3, byrow = TRUE)
                    if (u == 0){
                      transMat[1,] <- c(0,0,1)
                    }
                    if (p == 0){
                      transMat[2,] <- c(0,0,1)
                    }
                    
                  } else if (new_eqFreqs[1] <= u & new_eqFreqs[2] >= p & new_eqFreqs[3] >= m) { # Check Case 2: 1 new frequency value smaller
                    case <- "Case 2. u smaller"
                    transMat <- matrix(c(new_eqFreqs[1]/u, (new_eqFreqs[2]-p)/u, (new_eqFreqs[3]-m)/u,
                                         0, 1, 0,
                                         0, 0, 1),
                                       nrow = 3, byrow = TRUE)
                  } else if (new_eqFreqs[2] <= p & new_eqFreqs[1] >= u & new_eqFreqs[3] >= m) {
                    case <- "Case 2. p smaller"
                    transMat <- matrix(c(1, 0, 0,
                                         (new_eqFreqs[1]-u)/p, new_eqFreqs[2]/p, (new_eqFreqs[3]-m)/p,
                                         0, 0, 1),
                                       nrow = 3, byrow = TRUE)
                  } else if (new_eqFreqs[3] <= m & new_eqFreqs[2] >= p & new_eqFreqs[1] >= u) {
                    case <- "Case 2. m smaller"
                    transMat <- matrix(c(1, 0, 0,
                                         0, 1, 0,
                                         (new_eqFreqs[1]-u)/m, (new_eqFreqs[2]-p)/m, new_eqFreqs[3]/m),
                                       nrow = 3, byrow = TRUE)
                  }
                  
                  if (testing){
                    list(case = case,
                         transMat = transMat)
                  } else {
                    transMat
                  }
                },

                #' @description
                #' Public Method. Simulate IWE Events
                #'
                #' Simulates how CpG Islands' methylation state frequencies change and simultaneous sites change methylation state
                #' along a branch of length t according to the SSE-IWE model.
                #'
                #' @param testing logical value for testing purposes. Default FALSE.
                #'
                #' @return If testing = TRUE it returns a list.
                #' If there was a change in the equilibrium frequencies the list contains the following 7 elements, if not it contains the first 3 elements:
                #' \describe{
                #'       \item{\code{eqFreqsChange}}{logical indicating if there was a change in the equilibrium frequencies.}
                #'       \item{\code{old_eqFreqs}}{Original equilibrium frequencies before the IWE event.}
                #'       \item{\code{new_eqFreqs}}{New equilibrium frequencies after the IWE event.}
                #'       \item{\code{old_obsFreqs}}{Original observed frequencies before the IWE event.}
                #'       \item{\code{new_obsFreqs}}{New observed frequencies after the IWE event.}
                #'       \item{\code{IWE_case}}{Description of the IWE event case.}
                #'       \item{\code{Mk}}{Transition matrix used for the IWE event.}
                #' }
                #'
                #'
                #' @details The function checks if the methylation equilibrium frequencies (\code{eqFreqs}) and sequence observed
                #' frequencies (\code{obsFreqs}) change after the IWE event. If there is a change in either
                #' frequencies, the corresponding change flag \code{eqFreqsChange}
                #' in the \code{infoIWE} list will be set to \code{TRUE}.
                #'
                IWE_evol = function(testing = FALSE) {
                    ## Extract previous state equilibrium frequencies
                    u <- private$eqFreqs[1]
                    p <- private$eqFreqs[2]
                    m <- private$eqFreqs[3]
                    old_eqFreqs <- c(u,p,m)

                    if (testing){
                                        # compute previous observed methylation frequencies
                        old_obsFreqs <- c(sum(private$seq==1), sum(private$seq==2), sum(private$seq==3))/length(private$seq)
                                        # Save previous rates
                        old_Q <- private$Q
                                        # Initiate changedPos with NULL
                        changedPos <- NULL
                    }

                                        # Sample new methylation frequencies with the structure's global strategie
                    new_eqFreqs <- private$sample_eqFreqs()

                                        # Check if new_eqFreqs are equal to old ones
                    if (identical(old_eqFreqs, new_eqFreqs)) {
                        eqFreqsChange = F

                    } else{
                        eqFreqsChange = T
                        
                        # Set the IWE transition matrix
                        if (testing){
                          test <- self$get_transMat(old_eqFreqs, new_eqFreqs, info = "test within IWE_evol", testing = testing)
                          Mk <- test$transMat
                          IWE_case <- test$case
                        } else {
                          Mk <- self$get_transMat(old_eqFreqs, new_eqFreqs, info = "IWE_evol")
                        }
                        
                        
                        # Change data equilibrium frequencies
                        private$eqFreqs <<- new_eqFreqs
                        # Update Qi and Q
                        private$set_Qi()
                        private$set_Q()

                        if(testing){
                                        # Validate updated rate matrices
                            validationStates <- listRateMatrix_validation(private$Qi, "Qi matrices IWE")
                            listMatrices_validationResults(validationStates)

                            validationStates <- listRateMatrix_validation(private$Q[[1]], "Q matrices IWE R1")
                            listMatrices_validationResults(validationStates)

                            validationStates <- listRateMatrix_validation(private$Q[[2]], "Q matrices IWE R2")
                            listMatrices_validationResults(validationStates)

                            validationStates <- listRateMatrix_validation(private$Q[[3]], "Q matrices IWE R3")
                            listMatrices_validationResults(validationStates)

                                        # Validate transition matrix
                            validationStates <- listTransitionMatrix_validation(list(Mk), "Mk")
                            listMatrices_validationResults(validationStates)

                                        # Validate methylation equilibrium tripple (extra control at IWE)
                            validationStates <- listFreqVector_validation(list(old_eqFreqs, new_eqFreqs), "IWE_evol control old_eqFreqs and new_eqFreqs")
                            listFreqVector_validationResults(validationStates)

                                        # Validate Markov Chain State Transition Property
                            validationStates <- transPropMC_validation(old_eqFreqs, Mk, new_eqFreqs, listName = "transPropMC IWE")
                            transPropMC_validationResults(validationStates)
                        }
                        
                        
                        # Sample the new sequence
                        newseq <- rep(0, length(private$seq))
                        for(i in 1:length(newseq)) {
                          newseq[i] <- sample(1:3, size=1, prob=as.vector(Mk[private$seq[i],]))
                        } 
                        

                        # Update $seq and $neighbSt
                        if(any(private$seq != newseq)){
                            changedPos <- which(private$seq !=newseq)
                            private$seq <<- newseq
                            for (i in changedPos){
                                private$update_neighbSt(i) # method updates neighbSt between singleStr too
                              # Update $ratetree previous or next singleStr
                              if (i == 1){
                                private$update_ratetree_betweenStr(prevStr = TRUE)
                              }
                              if (i == length(private$seq)){
                                private$update_ratetree_betweenStr(nextStr = TRUE)
                              }  
                            }
                        }
                        
                        # Update $ratetree within singleStr
                        self$initialize_ratetree()

                                        #Compute the new_obsFreqs after the IWE event
                        new_obsFreqs <- c(sum(private$seq==1), sum(private$seq==2), sum(private$seq==3))/length(private$seq)
                    }

                    if (testing){
                        if(eqFreqsChange){
                            return(list(combiStructure_index = private$combiStructure_index,
                                        eqFreqsChange = eqFreqsChange,
                                        old_eqFreqs = old_eqFreqs,
                                        new_eqFreqs = new_eqFreqs,
                                        old_obsFreqs = old_obsFreqs,
                                        new_obsFreqs = new_obsFreqs,
                                        IWE_case = IWE_case,
                                        Mk = Mk,
                                        old_Q = old_Q,
                                        new_Q = private$Q,
                                        changedPos = changedPos))
                        } else {
                            return(list(eqFreqsChange = eqFreqsChange,
                                        old_eqFreqs = old_eqFreqs,
                                        new_eqFreqs = new_eqFreqs))
                        }
                    }
                },
                                        
                #' @description
                #' Public Method.
                #' @return Model parameter alpha_pI for sampling island equilibrium frequencies
                get_alpha_pI = function() private$alpha_pI,

                #' @description
                #' Public Method.
                #' @return Model parameter for sampling island equilibrium frequencies
                get_beta_pI = function() private$beta_pI,

                #' @description
                #' Public Method.
                #' @return Model parameter for sampling island equilibrium frequencies
                get_alpha_mI = function() private$alpha_mI,

                #' @description
                #' Public Method.
                #' @return Model parameter for sampling island equilibrium frequencies
                get_beta_mI = function() private$beta_mI,

                                        #sampling strategy
                #' @description
                #' Public Method.
                #' @return Model parameter for sampling non-island equilibrium frequencies
                get_alpha_pNI = function() private$alpha_pNI,

                #' @description
                #' Public Method.
                #' @return Model parameter for sampling non-island equilibrium frequencies
                get_beta_pNI = function() private$beta_pNI,

                #' @description
                #' Public Method.
                #' @return Model parameter for sampling non-island equilibrium frequencies
                get_alpha_mNI = function() private$alpha_mNI,

                #' @description
                #' Public Method.
                #' @return Model parameter for sampling non-island equilibrium frequencies
                get_beta_mNI = function() private$beta_mNI,

                                        # SSE
                #' @description
                #' Public Method.
                #' @return Model parameter for gamma distribution shape to initialize the 3 $Ri_values
                get_alpha_Ri = function() private$alpha_Ri,

                #' @description
                #' Public Method.
                #' @return Model parameter for gamma distribution expected value to initialize the 3 $Ri_values
                get_iota = function() private$iota,

                #' @description
                #' Public Method.
                #' @return The 3 $Ri_values
                get_Ri_values = function() private$Ri_values,
                
                #' @description
                #' Public Method.
                #' 
                #' @param siteR default NULL. Numerical value encoding for the sites rate of independent SSE (1, 2 or 3)
                #' @param neighbSt default NULL. Numerical value encoding for the sites neighbouring state (as in mapNeighbSt_matrix)
                #' @param oldSt default NULL. Numerical value encoding for the sites old methylation state (1, 2 or 3)
                #' @param newSt default NULL. Numerical value encoding for the sites new methylation state (1, 2 or 3)
                #' 
                #' @return With NULL arguments, the list of rate matrices. With non NULL arguments, the corresponding rate of change.
                get_Q = function(siteR = NULL, neighbSt = NULL, oldSt = NULL, newSt = NULL) { 
                  if(all(is.null(siteR), is.null(neighbSt), is.null(oldSt), is.null(newSt))){
                    private$Q
                  } else {
                    private$Q[[siteR]][[neighbSt]][oldSt,newSt]
                  }
                },
                
                #' @description
                #' Public Method.
                #' 
                #' @param index default NULL. Numerical value for the index of the CpG position within the singleStr instance
                #' 
                #' @return with NULL arguments, siteR vector. non NULL arguments, the corresponding siteR
                get_siteR = function(index = NULL){ 
                  if(is.null(index)){
                    private$siteR
                  } else {
                    private$siteR[index]
                  }
                },
                
                #' @description
                #' Public Method.
                #' 
                #' @param index default NULL. Numerical value for the index of the CpG position within the singleStr instance
                #' 
                #' @return with NULL arguments, neighbSt vector. non NULL arguments, the corresponding neighbSt
                get_neighbSt = function(index = NULL) { 
                  if(is.null(index)){
                    private$neighbSt
                  } else {
                    private$neighbSt[index]
                  }
                },
                
                #' @description
                #' Public Method. Update ratetree from another singleStructure instance
                #' 
                #' @param position Numerical value for the index of the CpG position within the singleStr instance
                #' @param rate Rate of change to asign to that position
                #' 
                #' @return NULL
                update_ratetree_otherStr = function(position, rate) { 
                  K <- length(private$ratetree)
                  private$ratetree[[K]][position] <<- rate
                  if (K > 1){
                    for(k in (K-1):1) {
                      position <- floor((position-1)/2)+1
                      private$ratetree[[k]][position] <<- sum(private$ratetree[[k+1]][c(2*position-1,
                                                                                        2*position)])
                    }
                  }
                },
                
                #' @description
                #' Public Method. Get list of matrices for SSE process
                #' 
                #' @param siteR default NULL. Numerical value encoding for the sites rate of independent SSE (1, 2 or 3)
                #' @param oldSt default NULL. Numerical value encoding for the sites old methylation state (1, 2 or 3)
                #' @param newSt default NULL. Numerical value encoding for the sites new methylation state (1, 2 or 3)
                #' 
                #' @return With NULL arguments, the list of SSEi rate matrices. With non NULL arguments, the corresponding rate of change.
                get_Qi = function(siteR = NULL, oldSt = NULL, newSt = NULL){
                  if(is.null(siteR) && is.null(oldSt) && is.null(newSt)){
                    private$Qi
                  } else{
                    private$Qi[[siteR]][oldSt,newSt]
                  }
                },
                
                #' @description
                #' Public Method. Decode methylation state of left neighbor form owns neighbSt
                #' 
                #' @param index Integer index value for the CpG position within the singleStr instance
                #' 
                #' @return decoded methylation state ($seq) of left neighbor (1, 2 or 3 for unmethylated, partially methylated or methylated)
                get_seqSt_leftneighb = function(index){
                  if (is.null(index)) stop("Argument 'index' for CpG position is missing, with no default")
                  if(!(is.numeric(index) && length(index) == 1 && index == floor(index))){
                    stop("Argument 'index' must be one integer index value")
                  }
                  if(!(index >= 1 && index <= length(private$seq))) {
                    stop("Argument 'index' not within sequence length")
                  }
                  # Extract row number (corresponding to $seq state of left neighbor) from the positions neighbSt
                  which(private$mapNeighbSt_matrix == self$get_neighbSt(index), arr.ind = TRUE)[1]
                  #floor((self$get_neighbSt(i)-1) / 3) +1
                },
                
                #' @description
                #' Public Method. Decode methylation state of left neighbor form owns neighbSt
                #' 
                #' @param index Integer index value for the CpG position within the singleStr instance
                #' 
                #' @return decoded methylation state ($seq) of right neighbor (1, 2 or 3 for unmethylated, partially methylated or methylated)
                get_seqSt_rightneighb = function(index){
                  if (is.null(index)) stop("Argument 'index' for CpG position is missing, with no default")
                  if(!(is.numeric(index) && length(index) == 1 && index == floor(index))){
                    stop("Argument 'index' must be one integer index value")
                  }
                  if(!(index >= 1 && index <= length(private$seq))) {
                    stop("Argument 'index' not within sequence length")
                  }
                  which(private$mapNeighbSt_matrix == self$get_neighbSt(index), arr.ind = TRUE)[2]
                  #(self$get_neighbSt(i)-1) %% 3 +1
                },
                
                #' @description
                #' Public Method. Make a singleStructure with the same segment lengths and parameters
                #' as the focal one but where all states are m or u
                #' 
                #' @param state Character value "U" or "M"
                #' @param testing default FALSE. TRUE for testing output
                #' 
                #' @return right neighbSt
                #'
                cftp_all_equal = function(state, testing = FALSE) {
                  n <- length(private$seq)
                  if (state == "U"){
                    #private$globalState <- "U"
                    private$seq <- rep(1L, n)
                  } else if (state == "M"){
                    #private$globalState <- "M"
                    private$seq <- rep(3L, n)
                  } else {
                    stop("Invalid 'state' value. Must be 'U' for unmethylated or 'M' for methylated")
                  }
                  if (testing){
                    private$seq
                  }
                },
                
                #' @description
                #' Public Method. Set the methylation state of a sequence position and update the neighbor's neighbSt. It does NOT update RATETREE 
                #' 
                #' @param index Numerical value for the index of the CpG position within the singleStr instance
                #' @param newSt Numerical value encoding for the sites new methylation state (1, 2 or 3)
                #' @param testing default FALSE. TRUE for testing output
                #' 
                #' @return NULL when testing FALSE. Testing output when testing TRUE.
                set_seqSt_update_neighbSt = function(index, newSt, testing = FALSE){
                  if(!(is.numeric(index) && length(index) == 1 && index == floor(index))){
                    stop("'index' must be one number without decimals")
                  }
                  if(!(index >= 1 && index <= length(private$seq))) {
                    stop("'index' not within sequence length")
                  }
                  if(!(newSt %in% c(1, 2, 3))){
                    stop("'newSt' must be 1, 2 or 3 (for unmethylated, partially methylated or methylated)")
                  }
                  private$seq[index] <- newSt
                  private$update_neighbSt(index)
                  if (testing){
                    list(seq = private$seq,
                         neighbSt = private$neighbSt)
                  }
                },
                
                #' @description
                #' Public Method. Resets the sequence states by resampling according to the instance's equilibrium frequencies.
                #' 
                #' @return NULL. The sequence is updated in place.
                reset_seq = function(){
                  n_sites <- length(private$seq)
                  # Resample seq states according to the instance's equilibrium frequencies
                  private$seq <- sample(1L:3L, size = n_sites, prob = private$eqFreqs, replace = TRUE)
                }
              )
              )



#' @title combiStructureGenerator
#' @importFrom R6 R6Class
#'
#' @description
#' an R6 class representing several genomic structures.
#' Each genomic structure contained is an object of class singleStructureGenerator.
#' Note that default clone(deep=TRUE) fails to clone singleStructureGenerator objects contained, use method $copy() instead.
#'
combiStructureGenerator <-
  R6::R6Class("combiStructureGenerator",
              private = list(
                shared_env = new.env(),
                ## @field id Private attribute: unique numeric identifier for the instance.
                id = NULL,
                ## @description
                ## Private method: Generate the next unique ID
                ##
                ## This private method increments the static counter and returns its value,
                ## ensuring each new instance or copy gets a unique ID.
                ##
                ## @return A numeric value representing the next unique ID.
                get_next_id = function() {
                  counter <- private$shared_env$counter
                  if (is.null(counter)) {
                    counter <- 0  # Initialize the counter if it hasn't been used yet
                  }
                  counter <- counter + 1
                  private$shared_env$counter <- counter  # Update the shared environment
                  return(counter)
                },
                ## @field singleStr Private attribute: List containing objects of class singleStructureGenerator
                singleStr = NULL,
                ## @field globalState Private attribute: Vector with each singleStructureGenerator object favored global state
                singleStr_globalState = NULL,
                ## @field mu Private attribute: Model parameter for the rate of the IWE evolutionary process (per island and branch length).
                mu = 0.1,
                ## @field IWE_rate Private attribute: Total rate of the IWE evolutionary process for the combiStructureObject.
                IWE_rate = NULL,
                ## @description
                ## Private Method: Compute the total rate of IWE evolutionary process.
                ## The total rate of change is given by the rate per CpG island times the number of CpG islands
                ##
                ## @return NULL.
                set_IWE_rate = function(){
                  if (self$get_island_number() != 0){
                    private$IWE_rate <- self$get_island_number() * private$mu
                  } else {
                    private$IWE_rate <- 0
                  }
                },

                ## @description
                ## Private method: Simulate SSE evolution at each short time step in each of the
                ## singleStructureGenerator objects in $singleStr. It samples each object in $singleStr
                ## in random order and calls singleStructureGenerator$SSE_evol()
                ##
                ## @param dt length of time step for SSE evolution.
                ## @param testing default FALSE. True for testing output
                ##
                ## @return default NULL. If testing TRUE it returns testing information
                SSE_evol = function(dt, testing = FALSE){
                    evol_order <- sample(1:self$get_singleStr_number(), self$get_singleStr_number(), replace = FALSE)
                    if (!testing){
                        for (i in evol_order) private$singleStr[[i]]$SSE_evol(dt, testing)
                    } else {
                        testing_info <- vector("list", self$get_singleStr_number())
                        for (i in evol_order) testing_info[[i]] <- private$singleStr[[i]]$SSE_evol(dt, testing)
                    }
                    if (testing){
                        return(list(evol_order = evol_order,
                                    testing_info = testing_info))
                    }
                },

                ## @description
                ## Simulate SSE evolutionary process within a given interval length by time steps of length dt.
                ##
                ## @param testing Default FALSE. Logical for testing purposes.
                ## @param dt Length of time steps for SSE_evol.
                ## @param interval_length Length of the total time interval.
                ##
                ## @return Default NULL. If testing = TRUE returns information for testing purposes.
                ##
                ## @details The function simulates SSE events within a specified interval length.
                ##
                interval_evol = function(interval_length, dt, testing = FALSE) {
                    if(testing){
                        interval_evolInfo <- c()
                    }
                                        # Compute number of dt for discretized time steps
                    number_dt <- floor(interval_length / dt)
                    remainder <- interval_length %% dt
                    if(interval_length < dt){
                        if(testing){
                            interval_evolInfo <- paste("SSE evolving in shorter interval_length than dt. Interval length:", interval_length)
                        }
                        private$SSE_evol(dt = interval_length)
                    } else{
                        if(testing){
                            interval_evolInfo <- paste("Number of SSE:", number_dt, ". Interval length:", dt)
                        }
                        for(i in 1:number_dt){
                            private$SSE_evol(dt = dt)
                        }
                        if (dt * number_dt == interval_length){
                            if(testing){
                                interval_evolInfo <- c(interval_evolInfo, "No remainder")
                            }
                        } else{
                            if(testing){
                                interval_evolInfo <- c(interval_evolInfo, paste("Last SSE with remainder dt of length", remainder))
                            }
                            private$SSE_evol(dt = remainder)
                        }
                    }
                    if(testing){interval_evolInfo}
                },

                ## @field IWE_events Private attribute: Information of the IWE events sampled in a tree branch
                IWE_events = NULL,
                ## @field name Private attribute: If evolutionary process (simulated from class treeMultiRegionSimulator) ends in a tree leaf, the name of the leaf
                name = NULL,
                ## @field own_index Private attribute: Own branch index in the tree along which the evolutionary process is simulated (from class treeMultiRegionSimulator)
                own_index = NULL,
                ## @field parent_index Private attribute: Parent branch index in the tree along which the evolutionary process is simulated (from class treeMultiRegionSimulator)
                parent_index = NULL,
                ## @field parent_index Private attribute: Offspring branch index in the tree along which the evolutionary process is simulated (from class treeMultiRegionSimulator)
                offspring_index = NULL,
                ## @field CFTP_info Private attribute: Instance of class cftpStepGenerator
                ## containing the info for the steps for sampling a sequence of methylation states
                ## from the equilibrium (SSEi and SSEc considered, IWE neglected) 
                ## using the CFTP algorithm for a given combiStructureGenerator instance.
                ## Can be retrieved with the combiStructureGenerator$get_CFTP_info()
                CFTP_info = NULL
              ),
              public = list(
                  #' @field testing_output Public attribute: Testing output for initialize
                  testing_output = NULL,
                  
                  #' @description
                  #' Create a new combiStructureGenerator object.
                  #'
                  #' Note that this object can be generated within a treeMultiRegionSimulator object.
                  #'
                  #' @param infoStr A data frame containing columns 'n' for the number of sites, and 'globalState' for the favoured global methylation state.
                  #' If initial equilibrium frequencies are given the dataframe must contain 3 additional columns: 'u_eqFreq', 'p_eqFreq' and 'm_eqFreq'
                  #' @param params Default NULL. When given: data frame containing model parameters.
                  #' @param testing Default FALSE. TRUE for writing in public field of new instance $testing_output
                  #'
                  #' @return A new `combiStructureGenerator` object.
                  initialize = function (infoStr, params = NULL, testing = FALSE){
                      private$id <- private$get_next_id()  # Assign a unique private ID and update the shared counter (private attribute $shared_env)
                      
                      # Initialize the private attributes to store stingleStructureGenerator instances and their corresponding globalState
                      private$singleStr <- list()
                      private$singleStr_globalState <- c()
        
                      # Initialize the singleStructure instances
                      for (i in 1:nrow(infoStr)) {
                        
                        u_length <- infoStr[i, "n"]
                        private$singleStr_globalState[i] <- infoStr[i, "globalState"]
                        
                        if(all(c("u_eqFreq", "p_eqFreq", "m_eqFreq") %in% colnames(infoStr))){
                          eqFreqs <- c(infoStr$u_eqFreq[i], infoStr$p_eqFreq[i], infoStr$m_eqFreq[i])
                        } else{
                          eqFreqs <- NULL
                        }
                        
                        private$singleStr[[i]] <- singleStructureGenerator$new(globalState = private$singleStr_globalState[i],
                                                                               n = u_length,
                                                                               eqFreqs = eqFreqs,
                                                                               combiStr = self ,combiStr_index = i,
                                                                               params = params)
                                                                               # if testing needs to be passed on to the singleStr initialize,
                                                                               # add testing = testing as argument 
                      } 
                      ## Initialice the neighbSt encoding for each singleStr
                      for (i in 1:length(private$singleStr)){
                        private$singleStr[[i]]$init_neighbSt()
                      }
                      
                      ## Initialice the rate_tree for each singleStr
                      # Note that this step needs the previous one run for all the structures
                      for (i in 1:length(private$singleStr)){
                        private$singleStr[[i]]$initialize_ratetree()
                      }
                      if(!is.null(params)){
                        private$mu <- params$mu
                      }
                      private$set_IWE_rate()
                      #undebug(self$cftp_apply_events)
                      #undebug(self$get_highest_rate)
                      
                  },
                  #' @description
                  #' Public method: Get one singleStructureGenerator object in $singleStr
                  #'
                  #' @param i index of the singleStructureGenerator object in $singleStr
                  #'
                  #' @return the singleStructureGenerator object in $singleStr with index i
                  get_singleStr = function(i) private$singleStr[[i]],


                  #' @description
                  #' Public method: Get number of singleStructureGenerator objects in $singleStr
                  #'
                  #' @return number of singleStructureGenerator object contained in $singleStr
                  get_singleStr_number = function () length(private$singleStr),
                  
                  #' @description
                  #' Public method: Get number of sites in all singleStructureGenerator objects
                  #'
                  #' @return number of sites in all singleStructureGenerator objects 
                  get_singleStr_siteNumber = function () {
                    singleStr_site_n <- c()
                    for(str in 1:length(private$singleStr)){
                      singleStr_site_n[str] <- length(private$singleStr[[str]]$get_seq())
    
                    }
                    singleStr_site_n
                  },

                  #' @description
                  #' Public method: Get number of singleStructureGenerator objects in $singleStr with $globalState "U" (CpG islands)
                  #'
                  #' @return number of singleStructureGenerator in $singleStr objects with $globalState "U" (CpG islands)
                  get_island_number = function (){
                      indexes <- which(private$singleStr_globalState == "U")
                  if (length(indexes) == 0){
                    return(0)
                  } else {
                    return(length(indexes))
                  }
                },

                #' @description
                #' Public method: Get index of singleStructureGenerator objects in $singleStr with $globalState "U" (CpG islands)
                #'
                #' @return index of singleStructureGenerator objects in $singleStr with $globalState "U" (CpG islands)
                get_island_index = function() which(private$singleStr_globalState == "U"),


                #' @description
                #' Public method: Set information of the IWE events sampled in a tree branch
                #'
                #' @param a value to which IWE_events should be set
                #'
                #' @return NULL
                set_IWE_events = function(a) private$IWE_events <- a,


                #' @description
                #' Public method: Get information of the IWE events sampled in a tree branch
                #'
                #' @return information of the IWE events sampled in a tree branch
                get_IWE_events = function() private$IWE_events,


                #' @description
                #' Public method: Set the name of the leaf if evolutionary process
                #' (simulated from class treeMultiRegionSimulator) ends in a tree leaf.
                #'
                #' @param a value to which name should be set
                #'
                #' @return NULL
                set_name = function(a) {private$name <- a},


                #' @description
                #' Public method: Get the name of the leaf if evolutionary process
                #' (simulated from class treeMultiRegionSimulator) ended in a tree leaf.
                #'
                #' @return Name of the leaf if evolutionary process
                #' (simulated from class treeMultiRegionSimulator) ended in a tree leaf.
                #' For iner tree nodes return NULL
                get_name = function() {private$name},


                #' @description
                #' Public method: Set own branch index in the tree
                #' along which the evolutionary process is simulated
                #' (from class treeMultiRegionSimulator).
                #'
                #' @return NULL
                get_own_index =  function() {private$own_index},


                #' @description
                #' Public method: Get own branch index in the tree
                #' along which the evolutionary process is simulated
                #' (from class treeMultiRegionSimulator).
                #'
                #' @param i index of focal object
                #'
                #' @return Own branch index in the tree
                #' along which the evolutionary process is simulated
                #' (from class treeMultiRegionSimulator).
                set_own_index =  function(i) {private$own_index <- i},


                #' @description
                #' Public method: Get parent branch index in the tree
                #' along which the evolutionary process is simulated
                #' (from class treeMultiRegionSimulator).
                #'
                #' @return Parent branch index in the tree
                #' along which the evolutionary process is simulated
                #' (from class treeMultiRegionSimulator).
                get_parent_index = function() {private$parent_index},


                #' @description
                #' Public method: Set parent branch index in the tree
                #' along which the evolutionary process is simulated
                #' (from class treeMultiRegionSimulator).
                #'
                #' @param i set parent_index to this value
                #'
                #' @return NULL
                set_parent_index = function(i) {private$parent_index <- i},


                #' @description
                #' Public method: Get offspring branch index in the tree
                #' along which the evolutionary process is simulated
                #' (from class treeMultiRegionSimulator).
                #'
                #' @return Offspring branch index in the tree
                #' along which the evolutionary process is simulated
                #' (from class treeMultiRegionSimulator).
                get_offspring_index = function() {private$offspring_index},


                #' @description
                #' Public method: Set offspring branch index in the tree
                #' along which the evolutionary process is simulated
                #' (from class treeMultiRegionSimulator).
                #'
                #' @param i set offspring_index to this value
                #'
                #' @return NULL
                set_offspring_index = function(i) {private$offspring_index <- i},


                #' @description
                #' Public method: Add offspring branch index in the tree
                #' along which the evolutionary process is simulated
                #' (from class treeMultiRegionSimulator).
                #'
                #' @param i index to be added
                #'
                #' @return NULL
                add_offspring_index = function(i) {
                  if(is.null(private$offspring_index)) {
                    private$offspring_index <- i
                  } else {
                    private$offspring_index <- c(private$offspring_index, i)
                  }
                },

                #' @description
                #' Public method.
                #'
                #' @return Model parameter for the rate of the IWE evolutionary process (per island and branch length).
                get_mu = function() private$mu,
                
                #' @description
                #' Public method. Get the unique ID of the instance
                #'
                #' @return A numeric value representing the unique ID of the instance.
                get_id = function() {
                  return(private$id)
                },
                #' @description
                #' Public method. Set the unique ID of the instance
                #' 
                #' @param id integer value to identificate the combiStructure instance
                #'
                #' @return A numeric value representing the unique ID of the instance.
                set_id = function(id) {
                  private$id <- id
                },
                
                #' @description
                #' Public method. Get the counter value from the shared environment between instances of combiStructureGenerator class
                #'
                #' @return Numeric counter value.
                get_sharedCounter = function(){
                  private$shared_env$counter
                },
                
                #' @description
                #' Public method. Reset the counter value of the shared environment between instances of combiStructureGenerator class
                #'
                #' @return NULL
                reset_sharedCounter = function(){
                  private$shared_env$counter <- 0
                },

                #' @description
                #' Public method: Clone each singleStructureGenerator object in $singleStr
                #'
                #' @param singStrList object to be cloned
                #'
                #' @return NULL
                set_singleStr = function(singStrList){
                    private$singleStr <- lapply(singStrList, function(singleStr) {
                        singleStr$clone(deep = TRUE)
                    })
                },

                #' @description
                #' Public method: Clone combiStructureGenerator object and all singleStructureGenerator objects in it
                #'
                #' @return cloned combiStructureGenerator object
                copy = function(){
                    new_obj <- self$clone()
                    new_obj$set_id(private$get_next_id())
                    new_obj$set_singleStr(private$singleStr)
                    for (str in 1:length(private$singleStr)){
                      new_obj$get_singleStr(str)$set_myCombiStructure(new_obj)
                    }
                    return(new_obj)
                },

                #' @description
                #' Simulate CpG dinucleotide methylation state evolution along a tree branch.
                #' The function samples the IWE events on the tree branch and simulates the
                #' evolution through the SSE and IWE processes.
                #'
                #' @param dt Length of SSE time steps.
                #' @param testing Default FALSE. TRUE for testing purposes.
                #' @param branch_length Length of the branch.
                #'
                #' @return Default NULL. If testing = TRUE it returns information for testing purposes.
                #'
                #' @details
                #' It handles both cases where IWE events are sampled or not sampled within the branch.
                #'
                branch_evol = function(branch_length, dt, testing = FALSE) {
                    branchEvolInfo <- list()
                                        # Initialize IWE_times as an empty vector
                                        #branchEvolInfo$IWE_times <- c()

                    if(private$IWE_rate == 0){
                        branchEvolInfo$IWE_event <- FALSE
                        self$set_IWE_events("Simulation without IWE events.")
                        SSE_intervals <- branch_length
                                        # Sequence evolution along the branch
                        private$interval_evol(interval_length = branch_length, dt = dt)
                    } else {
                                        # Draw IWE times
                        IWE_t <- stats::rexp(1, private$IWE_rate)
                        if (IWE_t >= branch_length) { # If there is no IWE sampled in the tree branch
                            branchEvolInfo$IWE_event <- FALSE
                                        # Save IWE_events
                            self$set_IWE_events(FALSE)
                            branchEvolInfo$IWE_times <- IWE_t
                            SSE_intervals <- branch_length
                                        # Sequence evolution along the branch
                            private$interval_evol(interval_length = branch_length, dt = dt)
                        } else{ # If there is at least 1 IWE sampled in the tree branch
                            branchEvolInfo$IWE_event <- TRUE
                            while (IWE_t < branch_length) {
                                branchEvolInfo$IWE_times <- c(branchEvolInfo$IWE_times, IWE_t)
                                IWE_t <- IWE_t + stats::rexp(1, private$IWE_rate)
                            }
                                        # Sample islands to apply IWE_events
                            if(length(self$get_island_index()) == 1){
                                branchEvolInfo$islands <- rep(self$get_island_index(), length(branchEvolInfo$IWE_times))
                            } else {
                                branchEvolInfo$islands <- sample(self$get_island_index(), length(branchEvolInfo$IWE_times), replace = TRUE)
                            }
                                        # Save islands and IWE times
                            self$set_IWE_events(list(islands = branchEvolInfo$islands,
                                                     times = branchEvolInfo$IWE_times))
                                        # Compute the branch intervals for SSE evolution
                            if (length(branchEvolInfo$IWE_times)==1){
                                SSE_intervals <- c(branchEvolInfo$IWE_times, branch_length-branchEvolInfo$IWE_times)
                            } else if (length(branchEvolInfo$IWE_times) > 1){
                                SSE_intervals <- c(branchEvolInfo$IWE_times[1], diff(branchEvolInfo$IWE_times), branch_length - branchEvolInfo$IWE_times[length(branchEvolInfo$IWE_times)])
                            }
                            branchEvolInfo$SSE_intervals <- SSE_intervals
                            branchEvolInfo$infoIWE <- list()
                                        # Sequence evolution along the branch intervals followed by IWEs
                            for (i in 1:(length(SSE_intervals)-1)){
                                private$interval_evol(interval_length = SSE_intervals[i], dt = dt)
                                branchEvolInfo$infoIWE[[i]] <- private$singleStr[[branchEvolInfo$islands[i]]]$IWE_evol(testing = testing)
                            }
                                        # Sequence evolution along the last branch interval
                            private$interval_evol(interval_length = SSE_intervals[length(SSE_intervals)], dt = dt)
                        }
                    }
                    if(testing){
                        return(branchEvolInfo)
                    }
                },

                #' @description
                #' Public Method. Gets the highest rate among all singleStructureGenerator objects for CFTP.
                #' 
                #' @return Highest rate value.
                get_highest_rate = function() {
                  CFTP_highest_rate <- 0 # ensure minimum value of 0
                  for(str in 1:length(private$singleStr)){
                    CFTP_highest_rate <- max(CFTP_highest_rate,
                                             max(c(unlist(private$singleStr[[str]]$get_Qi()), 
                                                  (1-private$singleStr[[str]]$get_iota())/2)))
    
                  }
                  CFTP_highest_rate
                },

                #' @description
                #' Public Method. Sets a cftpStepGenerator instance asthe CFTP info.
                #' 
                #' @param CFTP_instance CFTP info.
                #'
                #' @return NULL
                set_CFTP_info = function(CFTP_instance) {
                  if(class(CFTP_instance)[1] != "cftpStepGenerator") stop("Argument 'CFTP_instance' must be of class cftpStepGenerator")
                  private$CFTP_info <- CFTP_instance
                },  

                #' @description
                #' Public Method. Gets the CFTP info.
                #' 
                #' @return CFTP info.
                get_CFTP_info = function () {
                  private$CFTP_info
                },

                #' @description
                #' Public Method. Applies the CFTP events.
                #' 
                #' @param testing default FALSE. TRUE for testing output
                #' 
                #' @return NULL when testing FALSE. Testing output when testing TRUE.
                cftp_apply_events = function(testing = FALSE) {
                  
                  # Check that CFTP events have already been generated
                  if (length(private$CFTP_info$CFTP_event) < 1) stop("No CFTP events generated yet.")
                  
                  # Check that lists containing CFTP info have correct number of vectors
                  # Calculate the correct length according to the number of registered steps
                  correct_length <- private$CFTP_info$number_steps / private$CFTP_info$steps_perVector
                  if(!all(length(private$CFTP_info$CFTP_event)==correct_length,
                          length(private$CFTP_info$CFTP_chosen_singleStr)==correct_length,
                          length(private$CFTP_info$CFTP_chosen_site)==correct_length,
                          length(private$CFTP_info$CFTP_random)==correct_length)){
                    stop("Not all lists with CFTP info have correct length")
                  }
                  
                  if (testing) {
                    # Set list to save acceptance/rejection as T or F
                    event_acceptance <- list()
                    # Set list to save the rate of the chosen site (j) at each CFTP step (v,k)
                    r_jk <- list()
                    # Set list to save the applied case 
                    applied_event <- list()
                  }
                  
                  # Apply the CFTP_events from -n to 1 (past to present) to the combiStructure
                  # Iterate over each vector in the list in reverse order (from past to present)
                  for (v in length(private$CFTP_info$CFTP_event):1) {
                    
                    if (testing) {
                      event_acceptance[[v]] <- logical(length(private$CFTP_info$CFTP_event[[v]]))
                      r_jk[[v]] <- rep(NA, length(private$CFTP_info$CFTP_event[[v]]))
                      applied_event[[v]] <- rep(NA, length(private$CFTP_info$CFTP_event[[v]]))
                    }
                    
                    # Apply events in reverse order (from past to present)
                    for (k in length(private$CFTP_info$CFTP_event[[v]]):1) {
                      
                      # Get the chosen singleStr (i) and chosen site (j)
                      i <- private$CFTP_info$CFTP_chosen_singleStr[[v]][k]
                      j <- private$CFTP_info$CFTP_chosen_site[[v]][k]
                      
                      # Get the site's old methylation state
                      oldSt <- private$singleStr[[i]]$get_seq()[j]
                      
                      # If the event is of type SSEi, set the new methylation state
                      # as the sampled event (1 unmethylated, 2 partially methylated, 3 methylated)
                      if (private$CFTP_info$CFTP_event[[v]][k] < 4) {  
                        
                        # Get the site's proposed new methylation state
                        newSt <- private$CFTP_info$CFTP_event[[v]][k]
                        
                        if (oldSt != newSt) {
                          
                          # Get the site's encoding for SSEi Rate (R1, R2 or R3)
                          siteR <- private$singleStr[[i]]$get_siteR(j)
                          # Get the site's encoding of neighbor states
                          neighbSt <- private$singleStr[[i]]$get_neighbSt(j)
                          # Get the site's rate of SSEi
                          r <- private$singleStr[[i]]$get_Qi(siteR = siteR, oldSt = oldSt, newSt = newSt)
                          
                          # If the rate is higher than the threshold, change the site's methylation state
                          if (r / private$CFTP_info$CFTP_highest_rate > private$CFTP_info$CFTP_random[[v]][k]) {
                            private$singleStr[[i]]$set_seqSt_update_neighbSt(j, newSt)
                            
                            if (testing) {
                              event_acceptance[[v]][k] <- TRUE
                              r_jk[[v]][k] <- r
                              applied_event[[v]][k] <- paste("SSEi", newSt, sep = "_")
                            }
                          } else if (testing) {
                            r_jk[[v]][k] <- r
                          }
                        }
                        
                        # If the sampled event is of type SSEc
                      } else {
                        # Set the event rate as Rc/2 for each event copy left or copy right
                        r <- (1 - private$singleStr[[i]]$get_iota()) / 2
                        
                        # If the rate is higher than the threshold, change the site's methylation state
                        if (r / private$CFTP_info$CFTP_highest_rate > private$CFTP_info$CFTP_random[[v]][k]) {
                          
                          # Copy left neighbor's methylation state and update neighbors encoding of neighbouring states
                          if (private$CFTP_info$CFTP_event[[v]][k] == 4) {
                            private$singleStr[[i]]$set_seqSt_update_neighbSt(j, private$singleStr[[i]]$get_seqSt_leftneighb(index = j))
                            if (testing) {
                              event_acceptance[[v]][k] <- TRUE
                              r_jk[[v]][k] <- r
                              applied_event[[v]][k] <- "SSEc_left"
                            }
                            
                            # Copy right neighbor's methylation state and update neighbors encoding of neighbouring states
                          } else {
                            private$singleStr[[i]]$set_seqSt_update_neighbSt(j, private$singleStr[[i]]$get_seqSt_rightneighb(index = j))
                            if (testing) {
                              event_acceptance[[v]][k] <- TRUE
                              r_jk[[v]][k] <- r
                              applied_event[[v]][k] <- "SSEc_right"
                            }
                          }
                        } else if (testing) {
                          r_jk[[v]][k] <- r
                        }
                      }
                    }
                  }
                  
                  if (testing) {
                    return(list(CFTP_chosen_singleStr = private$CFTP_info$CFTP_chosen_singleStr,
                                CFTP_chosen_site = private$CFTP_info$CFTP_chosen_site,
                                CFTP_event = private$CFTP_info$CFTP_event,
                                CFTP_random = private$CFTP_info$CFTP_random,
                                event_acceptance = event_acceptance,
                                applied_event = applied_event,
                                r_jk = r_jk,
                                r_m = private$CFTP_info$CFTP_highest_rate))
                  }
                },
                
                #' @description
                #' Public Method. Applies the CFTP algorithm to evolve a structure and checks for convergence by comparing methylation states.
                #' 
                #' This method generates CFTP steps until the methylation sequences of the current structure and a cloned structure become identical across all singleStr instances or a step limit is reached. If the step limit is exceeded, an approximation method is applied to finalize the sequence.
                #' 
                #' @param steps minimum number of steps to apply (default 10000).
                #' @param step_limit maximum number of steps before applying an approximation method 
                #'        (default 327680000 corresponding to size of CFTP info of approx 6.1 GB). 
                #'        If this limit is reached, the algorithm stops and an approximation is applied.
                #' @param testing logical. If TRUE, returns additional testing output including the current structure, the cloned structure, the counter value, total steps, and the chosen site for the CFTP events. Default is FALSE.
                #' 
                #' @return NULL when testing is FALSE. If testing is TRUE, returns a list with:
                #'   - `self`: the current object after applying the CFTP algorithm.
                #'   - `combi_m`: a deep cloned object with applied CFTP events.
                #'   - `counter`: the number of iterations performed.
                #'   - `total_steps`: the number of steps applied by the CFTP algorithm.
                #'   - `CFTP_chosen_site`: the site selected during the CFTP event application.
                cftp = function(steps = 10000, step_limit = 327680000, testing = FALSE) {
                  private$CFTP_info <- cftpStepGenerator$new(singleStr_number = self$get_singleStr_number(), 
                                                             singleStr_siteNumber = self$get_singleStr_siteNumber(), 
                                                             CFTP_highest_rate = self$get_highest_rate())
                  
                  # Set a variable to track when the $seq of the 2 combi instances becomes equal
                  equal <- FALSE
                  counter <- 0
                  
                  while(!equal) {
                    
                    # Sample the CFTP steps 
                    private$CFTP_info$generate_events(steps)
                    
                    # Copy (deep clone) the combiStructure instance to generate 
                    # another instance sharing the same CFTP_info 
                    combi_m <- self$copy()
                    
                    # Set the sequences for each singleStr as all m states and all u states
                    for(str in 1:length(private$singleStr)){
                      self$get_singleStr(str)$cftp_all_equal(state = "U")
                      combi_m$get_singleStr(str)$cftp_all_equal(state = "M")
                    }
                    
                    # Update the neighbSt encoding according to the new sequence
                    for(str in 1:length(private$singleStr)){
                      self$get_singleStr(str)$init_neighbSt()
                      combi_m$get_singleStr(str)$init_neighbSt()
                      ### note that rate trees are not updated in combi_u and combi_m 
                    }
                    
                    # Apply the cftp events
                    self$cftp_apply_events() 
                    combi_m$cftp_apply_events() 
                    
                    # Check whether the sequence of methylation states in both
                    # instances is equal across all singleStr instances
                    equal_str <- c()
                    for(str in 1:length(private$singleStr)){
                      equal_str[str] <- all(self$get_singleStr(str)$get_seq() == combi_m$get_singleStr(str)$get_seq())
                    }
                    
                    # Update variable tracking when the $seq of the 2 combi instances becomes equal
                    equal <- all(equal_str)
                    
                    # Double the number of steps
                    steps <- 2*steps
                    
                    if (steps > step_limit && !equal) {
                      message("Steps limit reached. Applying approximation for CFTP.")
                      
                      # Initialize each singleStr sequence of states 
                      # according to its corresponding equilibrium frequencies
                      for (str in 1:length(private$singleStr)){
                        private$singleStr[[str]]$reset_seq()
                      }
                      
                      # Initialice the neighbSt encoding for each singleStr
                      for (str in 1:length(private$singleStr)){
                        private$singleStr[[str]]$init_neighbSt()
                      }
                      
                      # Apply the already generated cftp_events
                      self$cftp_apply_events() 
                      
                      # Set equal to stop iterations
                      equal <- TRUE
                      
                    }
                    
                    # Increase counter value
                    counter <- counter + 1
                  }
                  
                  # Once converged, update ratetree
                  for(str in 1:length(private$singleStr)){
                    self$get_singleStr(str)$initialize_ratetree()
                  }
                  
                  if(testing){
                    return(list(self = self,
                                combi_m = combi_m,
                                counter = counter,
                                total_steps = private$CFTP_info$number_steps,
                                CFTP_chosen_site = private$CFTP_info$CFTP_chosen_site))
                  } 
                }
                )
              )

#' @title cftpStepGenerator
#' @importFrom R6 R6Class
#'
#' @description
#' an R6 class representing the steps for sampling a sequence of methylation states
#' from the equilibrium (SSEi and SSEc considered, IWE neglected) 
#' using the CFTP algorithm for a given combiStructureGenerator instance.
#'
#' It is stored in the private attribute CFTP_info of combiStructureGenerator instances
#' when calling the combiStructureGenerator$cftp() method 
#' and can be retrieved with the combiStructureGenerator$get_CFTP_info()
#'
cftpStepGenerator <- R6::R6Class("cftpStepGenerator",
                                     public = list(
                                       #' @field singleStr_number Public attribute: Number of singleStr instances
                                       singleStr_number = NULL,
                                       #' @field singleStr_siteNumber Public attribute: Number of sites in singleStr instances
                                       singleStr_siteNumber = NULL,
                                       #' @field CFTP_highest_rate Public attribute: CFTP highest rate
                                       CFTP_highest_rate = 0,
                                       
                                       #' @description
                                       #' Create a new instance of class cftpStepGenerator with the info of the corresponding combiStrucutre instance
                                       #'
                                       #' @param singleStr_number Number of singleStr instances
                                       #' @param singleStr_siteNumber Number of sites in singleStr instances
                                       #' @param CFTP_highest_rate CFTP highest rate across all singleStr withing combiStr instance 
                                       #' 
                                       #' @return A new instance of class cftpStepGenerator
                                       initialize = function(singleStr_number, singleStr_siteNumber, CFTP_highest_rate) {
                                         # Initialize a new instance with the info of the corresponding combiStrucutre instance
                                         self$singleStr_number <- singleStr_number
                                         self$singleStr_siteNumber <- singleStr_siteNumber
                                         # Set CFTP_highest_rate to be the highest rate across all singleStr withing combiStr instance
                                         self$CFTP_highest_rate <- CFTP_highest_rate
                                         
                                       },
                                       
                                       #' @field number_steps Public attribute: counter of steps alredy generated
                                       number_steps = 0,
                                       #' @field CFTP_chosen_singleStr Public attribute: list with vectors of equal size with chosen singleStr index at each CFTP step
                                       CFTP_chosen_singleStr = list(),
                                       #' @field CFTP_chosen_site Public attribute: list with vectors of equal size with chosen site index at each CFTP step
                                       CFTP_chosen_site = list(),
                                       #' @field CFTP_event Public attribute: list with vectors of equal size with type of CFTP event at each CFTP step.
                                       #' @description
                                       #' 1: SSEi to unmethylated, 2: SSEi to partially methylated, 3: SSEi to methylated
                                       #' 4: SSEc copy left state, 5: SSEc copy right state
                                       CFTP_event = list(),
                                       #' @field CFTP_random Public attribute: list with vectors of equal size with CFTP threshold at each CFTP step
                                       CFTP_random = list(),
                                       #' @field steps_perVector Public attribute: size of vectors in lists CFTP_chosen_singleStr, CFTP_chosen_site, CFTP_event and CFTP_random 
                                       steps_perVector = NULL,
                                       
                                       #' @description
                                       #' Public Method. Generates the events to apply for CFTP.
                                       #' 
                                       #' @param steps Integer value >=1
                                       #' @param testing default FALSE. TRUE for testing output
                                       #' 
                                       #' @return NULL when testing FALSE. Testing output when testing TRUE.
                                       #'
                                       #' @details
                                       #' The function add steps to the existing ones. 
                                       #' If called several times the given steps need to be higher than the sum of steps generated before.
                                       generate_events = function(steps = 10000, testing = FALSE){
                                         if(!(is.numeric(steps) && length(steps) == 1 && steps == floor(steps) && steps >= 1)){
                                           stop("'index' must be one number >=1 without decimals")
                                         }
                                         
                                         # Set the number of steps per vector
                                         if(is.null(self$steps_perVector)){
                                           self$steps_perVector <- steps
                                         }
                                         
                                         # Check the number of already existing steps
                                         old_steps <- self$number_steps
                                         if (steps <= old_steps){stop("The given number of steps has already been generated")}
                                         
                                         # Get the number of new steps
                                         new_steps <- steps - old_steps
                                         
                                         # Track successful additions
                                         successful_additions <- 0
                                         
                                         # Add the info of the new steps to the existing ones
                                         # handling potential errors (e.g. memmory limits)
                                         tryCatch({
                                           
                                           # Set a variable to store the number of remaining steps to sample 
                                           remaining_steps <- new_steps
                                           
                                           while(remaining_steps > 0){
                                             
                                             # Initialize the vectors to store the CFTP info for the new steps
                                             chosen_singleStr <- integer(length=self$steps_perVector)
                                             chosen_site <- integer(length=self$steps_perVector)
                                             event <- integer(length=self$steps_perVector)
                                             random_threshold <- numeric(length=self$steps_perVector)
                                             
                                             # Generate for each CFTP step the event and location to apply it and a threshold for acceptance/rejection
                                             # For each CFTP step (from past to present)
                                             for(n in self$steps_perVector:1) {
                                               # For generation -n
                                               # Propose 1 site and what may happen to it
                                               chosen_singleStr[n] <- sample(1:self$singleStr_number, 1, prob=self$singleStr_siteNumber)
                                               chosen_site[n] <- sample(1:self$singleStr_siteNumber[chosen_singleStr[n]], 1)
                                               event[n] <- sample(1:5, 1)  ## 1,2,3: go to u, p, m by SSEi ## 4,5: copy left, copy right.                        
                                               # Sample a threshold to accept or reject event
                                               random_threshold[n] <- runif(1) # numerical value between 0 and 1
                                             }
                                             
                                             # Add the info of the new steps to the existing ones
                                             self$CFTP_chosen_singleStr <- append(self$CFTP_chosen_singleStr, list(chosen_singleStr))
                                             self$CFTP_chosen_site <- append(self$CFTP_chosen_site, list(chosen_site))
                                             self$CFTP_event <- append(self$CFTP_event, list(event))
                                             self$CFTP_random <- append(self$CFTP_random, list(random_threshold))
                                             
                                             # Mark successful addition
                                             successful_additions <- successful_additions + 1
                                             
                                             # Update the number of remaining steps to sample 
                                             remaining_steps <- remaining_steps - self$steps_perVector
                                             
                                             # Update the number of already existing steps
                                             self$number_steps <- self$number_steps + self$steps_perVector
                                           }
                                           
                                         }, error = function(e) {
                                           message("Error encountered: ", conditionMessage(e))
                                           
                                           if(successful_additions > 0){
                                             # Calculate the correct length according to the number of registered steps
                                             correct_length <- self$number_steps / self$steps_perVector
                                             
                                             # Adjust all lists to the correct length
                                             self$CFTP_chosen_singleStr <- self$CFTP_chosen_singleStr[1:correct_length]
                                             self$CFTP_chosen_site <- self$CFTP_chosen_site[1:correct_length]
                                             self$CFTP_event <- self$CFTP_event[1:correct_length]
                                             self$CFTP_random <- self$CFTP_random[1:correct_length]
                                             
                                           } else{
                                             stop(paste("CFTP could not converge. Error message:", conditionMessage(e)))
                                           }
                                           
                                           
                                         })
                                         
                                         if(testing){
                                           list(old_steps = old_steps,
                                                new_steps = new_steps)
                                         }
                                       }
                                     ))

#' Split Newick Tree
#'
#' This function splits a tree in Newick format into subtrees.
#' Each subtree unit can be a leaf or another subtree.
#'
#' @param tree A string in Newick format (with or without the ending semicolon).
#' @return A data frame, rows corresponding to each subtree unit and columns
#' $unit: chr (unit name), $brlen: num (unit branch length).
#' @noRd
split_newick <- function(tree) {
  if (!is.character(tree)) {stop("Input tree must be a character string.")}
  s <- strsplit(tree, '')[[1]]
  if(s[1]!="(") stop("newick tree does not begin with '('.")
  # withinU is the index of the within unit separator character ':'
  # it separates unit (leaf/subtree) name and branch length
  withinU <- c()
  # betweenU are the indexes of the between unit separator characters ',', '(' or ')'
  # it separates the units of the current subtree
  betweenU <- 1    # Initialized with unit start '(' index
  parcount <- 1    ## how many more '(' than ')' were there?##################
  for(i in 2:length(s)) { # From the first position after unit start
    if(s[i] == '(') {
      # parcount takes integer values reflecting the depth of the current unit
      # e.g. first level subtree: 1, subtree within first subtree: 2 ..
      # and 0 when the the end of the current subtree is reached ')'
      parcount <- parcount + 1
    } else if(s[i] == ')') {
      parcount <- parcount - 1
      if(parcount == 0) {
        betweenU <- c(betweenU, i)
      }
    } else if(s[i] == ':') {
      if(parcount == 1) { # Saves only withinU separator indexes ':' in first level subtree
        withinU <- c(withinU, i)
      }
    } else if(s[i] == ',') {
      if(parcount == 1) { # Saves only betweenU separator indexes affecting first level subtree
        betweenU <- c(betweenU, i)
      }
    }
  }
  # Create the data frame to save unit names and branch lengths of the current level subrtee
  L <- data.frame("unit"=rep("", length(withinU)),
                  "brlen"=rep(NA, length(withinU)),
                  stringsAsFactors = FALSE)
  for(j in 1:length(withinU)) { # For each unit (tip or subtree)
    #Uid_start is unit name start index
    Uid_start <- betweenU[j]+1 # From next character from unit start '(' look to the right
    while(s[Uid_start] == ' ') {
      Uid_start <- Uid_start+1
    }
    #Uid_start is unit name start index
    Uid_end <- withinU[j]-1 # From within unit separator ':' look to the left
    while(s[Uid_end] == ' ') {
      Uid_end <- Uid_end-1
    }
    #brlen_start is branch length start index
    brlen_start <- withinU[j]+1 # From within unit separator ':' look to the right
    while(s[brlen_start] == ' ') {
      brlen_start <- brlen_start+1
    }
    #brlen_start is branch length end index
    brlen_end <- betweenU[j+1]-1 # From next character from unit separator ', or )' look to the left
    while(s[brlen_end] == ' ') {
      brlen_end <- brlen_end-1
    }
    L[j, 1] <- paste0(s[Uid_start:Uid_end], collapse="")
    L[j, 2] <- as.numeric(paste0(s[brlen_start:brlen_end], collapse=""))
  }
  return(L)
}

#' @title treeMultiRegionSimulator
#' @importFrom R6 R6Class
#'
#' @description
#' an R6 class representing the methylation state of GpGs in different genomic
#' structures in the nodes of a tree.
#'
#' The whole CpG sequence is an object of class combiStructureGenerator.
#' Each genomic structure in it is contained in an object of class singleStructureGenerator.
#'
treeMultiRegionSimulator <- R6Class("treeMultiRegionSimulator",
                             public = list(
                               #' @field testing_output Public attribute: Testing output for initialize
                               testing_output = NULL,
                               #' @field Branch Public attribute: List containing objects of class combiStructureGenerator
                               Branch=NULL,
                               #' @field branchLength Public attribute: Vector with the corresponding branch lengths of each $Branch element
                               branchLength=NULL,

                               #' @description
                               #' Simulate CpG dinucleotide methylation state evolution along a tree.
                               #' The function splits a given tree and simulates evolution along its
                               #' branches. It recursively simulates evolution in all of the subtrees in the given tree
                               #' until the tree leafs
                               #'
                               #' @param Tree String. Tree in Newick format. When called recursivelly it is given the corresponding subtree.
                               #' @param dt Length of SSE time steps.
                               #' @param testing Default FALSE. TRUE for testing purposes.
                               #' @param parent_index Default 1. When called recursivelly it is given the corresponding parent branch index.
                               #'
                               #' @return NULL
                               treeEvol = function(Tree, dt=0.01, parent_index=1, testing=FALSE) {
                                 tl <- split_newick(Tree)    ## list of subtrees and lengths of branches leading to them
                                 if(testing) {
                                   message("List of subtrees and lengths of branches leading to them")
                                   message(tl)
                                   message("Simulation on first branch")
                                 }
                                 for(i in 1:nrow(tl)) {
                                   ni <- length(self$Branch) + 1  ## "new index", that is index of the new branch
                                   self$Branch[[ni]] <<- self$Branch[[parent_index]]$copy()
                                   self$Branch[[ni]]$set_IWE_events(NULL)
                                   self$Branch[[ni]]$set_offspring_index(NULL)
                                   self$Branch[[ni]]$set_own_index(ni)
                                   self$Branch[[ni]]$set_parent_index(parent_index)
                                   self$Branch[[parent_index]]$add_offspring_index(ni)
                                   s <- tl[i, 1]
                                   self$branchLength[ni] <- tl[i, "brlen"]
                                   if(substr(s, 1, 1) == "(") {   ## real subtree
                                     self$Branch[[ni]]$branch_evol(tl[i, "brlen"], dt=dt)
                                     self$treeEvol(Tree = s, dt=dt, parent_index=ni, testing=testing)
                                   } else {   ## branch
                                     self$Branch[[ni]]$set_name(s)
                                     self$Branch[[ni]]$branch_evol(tl[i, "brlen"], dt=dt)
                                   }
                                 }
                               },

                               #' @description
                               #' Create a new treeMultiRegionSimulator object.
                               #' $Branch is a list for the tree branches, its first element represents the tree root.
                               #'
                               #' Note that one of either infoStr or rootData needs to be given. Not both, not neither.
                               #'
                               #' @param rootData combiStructureGenerator object. When given, the simulation uses its parameter values.
                               #' @param tree tree
                               #' @param infoStr  A data frame containing columns 'n' for the number of sites, and 'globalState' for the favoured global methylation state.
                               #' If initial equilibrium frequencies are given the dataframe must contain 3 additional columns: 'u_eqFreq', 'p_eqFreq' and 'm_eqFreq'
                               #' @param params Default NULL. When given: data frame containing model parameters. Note that if rootData is not null, its parameter values are used.
                               #' @param dt length of the dt time steps for the SSE evolutionary process
                               #' @param CFTP Default FALSE. TRUE for calling cftp algorithm to set root state according to model equilibrium (Note that current implementation neglects IWE process).
                               #' @param CFTP_step_limit when CFTP = TRUE, maximum number of steps before applying an approximation method 
                               #'        (default 327680000 corresponding to size of CFTP info of approx 6.1 GB).
                               #' @param testing Default FALSE. TRUE for testing output.
                               #'
                               #' @return A new `treeMultiRegionSimulator` object.
                               initialize = function(infoStr = NULL, rootData = NULL, tree = NULL, params = NULL, dt = 0.01, CFTP = FALSE, CFTP_step_limit = 327680000, testing = FALSE) {
                                 if(is.null(rootData) && is.null(infoStr)) stop("One of the following arguments: 'rootData' or 'infoStr' needs to be given")
                                 if(is.null(tree)) stop("Argument 'tree' is missing with no default.")
                            
                                 self$Branch <- list()
                      
                                 if(!is.null(rootData)){
                                   if (!is.null(infoStr) || !is.null(params)) stop("if 'rootData' is given, 'infoStr' and 'params' need to be NULL")
                                   message(paste("Simulating evolution of given data at root along given tree: ", tree))
                                   self$Branch[[1]] <- rootData$copy()
                                 }
                                 
                                 if(!is.null(params)){
                                   if(!is.data.frame(params) || !all(c("alpha_pI", "beta_pI", "alpha_mI", "beta_mI", "alpha_pNI", "beta_pNI", "alpha_mNI", "beta_mNI", "mu", "alpha_Ri", "iota") %in% colnames(params))){
                                     stop("if 'params' is given, it needs to be a dataframe with column names as in get_parameterValues() output")
                                   }
                                 }
                                 
                                 if(!is.null(infoStr)){
                                   message(paste("Simulating data at root and letting it evolve along given tree: ", tree))
                                   self$Branch[[1]] <- combiStructureGenerator$new(infoStr, params = params)
                                   # if testing needs to be passed on to the combiStr initialize,
                                   # add testing = testing as argument 
                                 }
                                 
                                 if(CFTP){
                                   message("Calling CFTP algorithm for data at root before letting it evolve along given tree.")
                                   if(testing){
                                     self$testing_output <- list()
                                     # Save the sequence of methylation states at 
                                     # the root before calling CFTP
                                     self$testing_output$seq_before_cftp <- c()
                                     for(str in 1:self$Branch[[1]]$get_singleStr_number()) {
                                       self$testing_output$seq_before_cftp <- c(self$testing_output$seq_before_cftp, self$Branch[[1]]$get_singleStr(str)$get_seq())
                                     }
                                     # Call CFTP while saving testing output
                                     self$testing_output$cftp_output <- self$Branch[[1]]$cftp(step_limit = CFTP_step_limit, testing = testing)
                                   } else {
                                     self$Branch[[1]]$cftp(step_limit = CFTP_step_limit)
                                   }
                                 }
                                 
                                 self$branchLength[1] <- NULL
                                 self$Branch[[1]]$set_own_index(1)
                                 
                                 self$treeEvol(Tree=tree, dt = dt, testing=testing)
                               }
                             )
)

