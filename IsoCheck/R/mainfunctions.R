#================================================
# a function that applies a collineation C to a spread spr in order to relabel it.
applyCollineation <- function(C, spr){
  ddd <- dim(spr)
  out <- array(NA, ddd)
  for(i in 1:ddd[3]){
    for(j in 1:ddd[2]){
      out[,j,i] <- (C %*% spr[,j,i])%%2
    }
  }
  return(out)
}
#================================================



#================================================
# a function that converts a spread spr into its bitstring characterization (for checking equivalence)
getBitstrings <- function(spr){
  ddd <- dim(spr)
  n <- ddd[1]
  mult <- 2^(0:(n-1))
  strings <- matrix(0, nrow = ddd[3], ncol = 2^n -1)
  smallests <- rep(NA, ddd[3])
  for(flat in 1:(ddd[3])){
    for(pencil in 1:(ddd[2])){
      ind <- sum(mult * spr[,pencil, flat])
      strings[flat, ind] <- 1
    }
    smallests[flat] <- which.max(strings[flat,] == 1)
  }
  sort_order <- sort(smallests, index.return = TRUE)$ix
  return(strings[sort_order,])
}
#================================================



#================================================
# a function for checking the isomorphism of two spreads (spread1 and spread2)
# Returns a list consisting of two components --- a boolean indicating whether
# or not spread1 is isomorphic to spread2, and a list of isomorphism establishing
# collineations. Note that when returnfirstIEC = TRUE, only the first IEC is
# returned (much faster runtime if they are isomorphic)
checkSpreadIsomorphism <- function(spread1, spread2, returnfirstIEC = FALSE, printstatement = TRUE){
  if(sum(dim(spread1) == dim(spread2)) != 3){
    message("Spreads are not of same dimension.")
    return(FALSE)
  }
  n <- dim(spread1)[1]
  t <- log(dim(spread1)[2] + 1)/log(2)
  ell <- n/t
  mu <- (2^n - 1)/(2^t - 1)
  flatsize <- 2^t-1
  spacesize <- 2^n -1

  # note: for the code to work, the pencils in each flat in the spreads being checked for ismorphism
  # should be arranged such that they are isomorphic to the yates order (basis elements in slots 1,2,4,8,etc.)
  # furthermore, the union of the first ell flats should span PG(n-1,2)
  # the following 2 steps permute within the equivalence class to guarantee these criteria are met.
  spread1 <- .arrange_yates(spread1)
  spread1 <- .arrange_flats_independent(spread1)
  spread2 <- .arrange_yates(spread2)
  spread2 <- .arrange_flats_independent(spread2)


  pencilmappingoptions <- .getpenciloptions(t)


  CxB <- .getCxB(spread1)
  spreadB <- applyCollineation(CxB, spread1)
  bits2 <- getBitstrings(spread2) # get the bitstring characterization of spread2

  flatchoices <- gtools::combinations(mu, ell) # all of the ways to choose ell out of mu flats
  perms <- gtools::permutations(ell,ell) # all of the ways to permute a sequence of ell flats
  basischoices <- gtools::permutations(nrow(pencilmappingoptions),ell, repeats.allowed = TRUE) # all of the ways to choose bases from the ell flats

  count <- 0 # the number of IECs found so far
  validCBys <- list() # a list to collect our CBys corresponding to IECs
  totaloptions <- nrow(flatchoices) * nrow(basischoices) * nrow(perms) # this should be equal to the size of the space to be searched

  # iterate across flat choices for y1...yn
  for(kk in 1:nrow(flatchoices)){
    # first check if this choice of flats form a linearly independent set, if not skip.
    xx <- .getCxB(spread2[,,flatchoices[kk,]])
    if(is.na(xx[1,1]) == FALSE){
      # iterate through the different options for bases of the flats
      for(vv in 1:nrow(basischoices)){
        # iterate through the different permutations
        for(ww in 1:nrow(perms)){
          # create the CBy
          CBy <- matrix(nrow = n, ncol = n)
          for(i in 1:ell){
            for(j in 1:t){
              CBy[,t * (i-1) +j] <- spread2[,pencilmappingoptions[basischoices[vv, i],j], flatchoices[kk, perms[ww,i]]]
            }
          }
          relabelled <- applyCollineation(CBy, spreadB) # relabel spread1 with CBy
          bitsnew <- getBitstrings(relabelled) # get the bitstring characterization
          if(sum(bitsnew != bits2) == 0){ # check equivalence. If equivalent, add to list
            if(returnfirstIEC == TRUE){
              return(list(result = TRUE, IECs = list((CBy %*% CxB) %%2)))
            }
            count <- count + 1
            validCBys[[count]] <- CBy
          }
        }
        message("percent done: ", 100 * round(((kk-1) *nrow(basischoices) * nrow(perms) + (vv)*nrow(perms))  /totaloptions ,4)) # to help keep track of how far along we are
      }
    }
  }
  if(count == 0){
    if(printstatement == TRUE){
      message("The two spreads are not isomorphic.")
    }
    return(list(result = FALSE, IECs = NA))
  }

  IECs <- validCBys
  for(i in 1:length(IECs)){
    IECs[[i]] <- (validCBys[[i]] %*% CxB) %%2
  }
  if(printstatement == TRUE){
    message("The two spreads are isomorphic. For example, one isomorphism establishing collineation is")
    print(IECs[[1]])
  }

  return(list(result = TRUE , IECs = IECs))
}
#================================================



#================================================
# a function that converts a star to its corresponding spread
star_to_spread <- function(star){
  ddd <- dim(star)
  n <- ddd[1]
  t <- log(ddd[2] + 1)/log(2)

  # find nucleus
  df <- suppressMessages(plyr::match_df(data.frame(t(star[,,1])), data.frame(t(star[,,2]))))

  # find dimension of nucleus
  t0 <- log(nrow(df) + 1)/log(2)

  # find basis for nucelus
  if(nrow(df) == 1){
    nucleus <- as.matrix(df)
  }else{
    nucleus <- .rrefmod2(as.matrix(df))[1:t0,]
  }

  alls <- matrix(0, nrow = 2^n, ncol = n)
  for(i in 1:n){
    alls[(floor((0:(2^n-1))/(2^(i-1))) %%2 == 1),i] <- 1
  }
  alls <- alls[-1,]
  newbasis <- nucleus
  for(i in t0:(n - 1)){
    newbasisspan <- t((t(newbasis) %*% t(alls[1:(2^i - 1), 1:i])) %% 2)
    remalls <- suppressMessages(dplyr::anti_join(data.frame(alls), data.frame(newbasisspan)))
    newbasis <- rbind(remalls[1,], newbasis)
  }


  # remove nucleus with collineation

  collineation <- solve(t(newbasis))%%2
  transformed <- applyCollineation(collineation, star)
  spread <- array(NA, c(n - t0, 2^(t - t0) - 1 , ddd[3] ))
  for(i in 1:ddd[3]){
    spread[,,i] <- transformed[1:(n-t0),which(colSums(matrix(transformed[n-t0 + (1:t0), ,i], nrow = t0)) == 0),i]
  }
  return(list(spread, collineation = collineation))
}

# receives two stars star1 and star2 as arrays(n, 2^t-1, mu) and checks their isomorphism. Returns a list containing FALSE is the two stars
# are not isomorphic. If they are isomorphic, it returns a list consisting of T as well
# as a list of IECs from star1 to star2. If returnfirstIEC = TRUE, then only the
# If returnfirstIEC = TRUE the algorithm terminates as soon as the first IEC is found.
#================================================


#================================================
checkStarIsomorphism <- function(star1, star2, returnfirstIEC = FALSE){
  
  if(sum(dim(star1) == dim(star2)) != 3){
    message("Stars are not of same dimension.")
    return(FALSE)
  }
  reduce1 <- star_to_spread(star1)
  reduce2 <- star_to_spread(star2)
  spread1 <- reduce1[[1]]
  spread2 <- reduce2[[1]]
  starcol1 <- reduce1$collineation
  starcol2 <- reduce2$collineation

  isocheck <- checkSpreadIsomorphism(spread1, spread2, returnfirstIEC, FALSE)
  if(isocheck[[1]] == FALSE){
    message("The two stars are not isomorphic.")
    return(list(result = FALSE, IECs = NA))
  }
  else{
    results <- list()
    transafter <- starcol1
    transbeforeinv <- starcol2
    transbefore <- solve(transbeforeinv) %% 2
    for(i in 1:length(isocheck[[1]])){
      fullmat <- diag(dim(star1)[1])
      fullmat[1:(dim(spread1)[1]), 1:(dim(spread1)[1])] <- isocheck[[2]][[i]]
      results[[i]] <- (transbefore %*% fullmat %*% transafter) %%2
    }
    message("The two stars are isomorphic. For example, one isomorphism establishing collineation is")
    print(results[[1]])
    return(list(result = TRUE, IECs = results))
  }
}
#================================================




#================================================
# checks the equivalence of star1 and star2.
checkStarEquivalence <- function(star1, star2){
  
  if(sum(dim(star1) == dim(star2)) != 3){
    message("Stars are not of same dimension.")
    return(FALSE)
  }
  strings1 <- getBitstrings(star1)
  strings2 <- getBitstrings(star2)
  mat <- matrix(log(1:ncol(strings1)), nrow = nrow(strings1), ncol = ncol(strings1), byrow = TRUE)
  if(sum(abs(sort(rowSums(strings1 * mat)) - sort(rowSums(strings2 * mat))) < 0.00001) == nrow(strings1)){
    return(T)
  }
  else{
    return(F)
  }
}
#================================================



#================================================
# checks the equivalence of spread1 and spread2.
checkSpreadEquivalence <- function(spread1, spread2){
  
  if(sum(dim(spread1) == dim(spread2)) != 3){
    message("Spreads are not of same dimension.")
    return(FALSE)
  }
  strings1 <- getBitstrings(spread1)
  strings2 <- getBitstrings(spread2)
  if(sum(strings1 != strings2) == 0){
    return(T)
  }
  return(F)
}
#================================================



#================================================
# printstatements = FALSE allows you to silence the explanation for the failure
is.spread <- function(spr, printstatements = TRUE){
  dims <- dim(spr)
  if(length(dims) != 3){
    if(printstatements == TRUE){
      message("Spread should be given as a 3d binary array")
    }
    return(FALSE)
  }
  
  # weird corner case to address
  if(sum(dims == c(1,1,1)) == 3){
    if(sum(spr != array(1,c(1,1,1))) == 0){
      return(TRUE)
    }
  }
  
  # check if the representation is binary
  if(sum(sort(unique(c(spr))) != c(0,1)) > 0){
    if(printstatements == TRUE){
      message("Spread should be expressed in binary")
    }
    return(FALSE)
  }
  
  t <- log(dims[2] + 1)/log(2)
  
  # check if the flats are of a valid size
  test1 <- floor(t) == t
  if(test1 == FALSE){
    if(printstatements == TRUE){
      message("Invalid size of flats. Should be 2^t-1 for some t.")
    }
    return(FALSE)
  }
  
  # make sure the right number of flats are present
  test2 <- ((2^dims[1] - 1)/(dims[2]) == dims[3])
  if(test2 == FALSE){
    if(printstatements == TRUE){
      message("The number of flats is not equal to (2^n-1)/(2^t-1).")
    }
    return(FALSE)
  }
  
  # ensure that no null effects are included
  for(i in 1:dims[2]){
    for(j in 1:dims[3]){
      if(sum(spr[,i,j]) == 0){
        if(printstatements == TRUE){
          message("Null effects should not be present.")
        }
        return(FALSE)
      }
    }
  }
  
  # check for repeated entries
  bitstrings <- getBitstrings(spr)
  if(max(bitstrings) > 1){
    if(printstatements == TRUE){
      message("Duplicate entries in a flat.")
    }
    return(FALSE)
  }
  
  # make sure the flats partition the space
  if(sum(colSums(bitstrings) != rep(1, 2^(dims[1]) - 1)) != 0){
    if(printstatements == TRUE){
      message("The flats do not form a partition of the space.")
    }
    return(FALSE)
  }
  
  # make sure each flat is actually a flat (is the span of t elements)
  for(i in 1:dims[3]){
    if(max(which(rowSums(.rrefmod2(t(spr[,,i]))) > 0)) != t){
      if(printstatements == TRUE){
        message("Flat ", i, " is not a valid flat.")
      }
      return(FALSE)
    }
  }
  return(TRUE)
}
#================================================


#================================================
# printstatements = FALSE allows you to silence the explanation for the failure
is.star <- function(star, printstatements = TRUE){
  
  dims <- dim(star)
  if(length(dims) != 3){
    if(printstatements == TRUE){
      message("Star should be given as a 3d binary array")
    }
    return(FALSE)
  }
  
  # weird corner case to address
  if(sum(dims == c(1,1,1)) == 3){
    if(sum(star != array(1,c(1,1,1))) == 0){
      return(TRUE)
    }
  }
  
  # check if the representation is binary
  if(sum(sort(unique(c(star))) != c(0,1)) > 0){
    if(printstatements == TRUE){
      message("Star should be expressed in binary")
    }
    return(FALSE)
  }
  
  t <- log(dims[2] + 1)/log(2)
  
  # check if the flats are of a valid size
  test1 <- floor(t) == t
  if(test1 == FALSE){
    if(printstatements == TRUE){
      message("Invalid size of flats. Should be 2^t-1 for some t.")
    }
    return(FALSE)
  }
  
  # ensure that no null effects are included
  for(i in 1:dims[2]){
    for(j in 1:dims[3]){
      if(sum(star[,i,j]) == 0){
        if(printstatements == TRUE){
          message("Null effects should not be present.")
        }
        return(FALSE)
      }
    }
  }
  
  # check for repeated entries
  bitstrings <- getBitstrings(star)
  if(max(bitstrings) > 1){
    if(printstatements == TRUE){
      message("Duplicate entries in a flat.")
    }
    return(FALSE)
  }
  
  inclusioncounts <- colSums(bitstrings)
  
  # check if all effects are included
  if(sum(0 == inclusioncounts) > 0){
    if(printstatements == TRUE){
      message("The elements do not form a cover.")
    }
    return(FALSE)
  }
  
  # check if the only overlap among flats is the nucleus.
  if(length(unique(inclusioncounts)) > 2){
    if(printstatements == TRUE){
      message("Nucleus is not common to all flats or a.")
    }
    return(FALSE)
  }
  
  nucleus_size <- sum(inclusioncounts > 1)
  
  t0 <- log(nucleus_size + 1)/log(2)

  if(t0 == 0){
    message("It is a spread (i.e., trivial star).")
  }
    
  # check if the nucleus is of a valid size
  test1 <- floor(t0) == t0
  if(test1 == FALSE){
    if(printstatements == TRUE){
      message("Invalid size of nucleus. Should be 2^t-1 for some t.")
    }
    return(FALSE)
  }
  
  # make sure each flat is actually a flat (is the span of t elements)
  for(i in 1:dims[3]){
    if(max(which(rowSums(.rrefmod2(t(star[,,i]))) > 0)) != t){
      if(printstatements == TRUE){
        message("Flat ", i, " is not a valid flat.")
      }
      return(FALSE)
    }
  }
  return(TRUE)
}

#=================================================
vectortostring <- function(arry){
  dim_ord = dim(arry)
  if(sum(dim_ord) == 0){
     arry = as.matrix(arry)
     dim_ord = dim(arry)
  }
  n = dim_ord[1]
  k = dim_ord[2]
  if(length(dim_ord)==2){
    dim(arry) <- c(dim_ord,1)
    dim_ord = c(dim_ord,1)
  }
  mu = dim_ord[3]
  str_arry <- array(0,c(dim_ord[-1]))
  
  for(i_mu in 1:mu){
    for(i_k in 1:k){
      
      vec = arry[,i_k,i_mu]
      len_vec = length(vec)
      lett_val = LETTERS[vec*seq(1:len_vec)]
      final_lett_val=NULL;
      for(i_n in 1:length(lett_val)){
        final_lett_val = paste(final_lett_val, lett_val[i_n],sep="")
      }
      str_arry[i_k,i_mu]=final_lett_val
    }
  }
  return(str_arry)
}
#=================================================

stringtovector <- function(string,n){
  ret <- rep(0, n)
  for(i in 1:n){
    ret[i]<- (grepl(LETTERS[i], string)==TRUE)
  }
  return(ret)
}


#================================================
