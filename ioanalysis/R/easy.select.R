easy.select <- function(io){
  if(!"InputOutput" %in% class(io)) stop('Object must be of "InputOutput" class. See ?as.inputoutput()')
  RS_label <- io$RS_label
  selection <- NULL
  continueSelect <- 0
  
  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  
  mm <- quiet(locate.mismatch(io))
  if(length(mm) > 0){
    cat("\nThis is an unbalanced Input-Output system.")
    cat("\nWould you like to see mismatched?")
    userInput <- readline(prompt = "y/n: ")
    if(userInput == "y"){
      n <- length(mm)
      for(i in 1:n){
        cat(paste("\nSector ", RS_label[mm[[i]][[1]][1], 2], 
                  " is in region(s) ", mm[[i]][[2]], 
                  " and not in region(s) ", mm[[i]][[3]], sep = ""))
      }
      cat("\n")
    }
  }
  
  while(TRUE){
    cat("\nPress Esc to force quit")
    cat("\n\n===================")
    cat(  "\n==== MAIN MENU ====")
    cat(  "\n===================\n\n")
    cat("(s) Search using a keyword\n")
    cat("(l) List Indicators\n")
    cat("(v) view and/or remove items from Selection\n")
    cat("(e) Save and Exit")
    userInput <- readline(prompt = "Select Option: ")
    ##############################
    ## Checking for valid input ##
    ##############################
    if(!userInput %in% c("s","l","v","r","e")){cat("Select option from main menu")}
    ########################
    ## Save and Exit Loop ##
    ########################
    if(userInput == "e"){break}
    ####################
    ## Keyword Search ##
    ####################
    if(userInput == "s"){
      while(TRUE){
        cat("\n  ~~~~~~~~~~~~~~~~~~~~~~~~")
        cat("\n  ~~~~ Keyword Search ~~~~")
        cat("\n  ~~~~~~~~~~~~~~~~~~~~~~~~\n")
        cat("\n  (r)  Search Regions then Sectors")
        cat("\n  (s)  Search over all Sectors")
        cat("\n  (m)  Main Menu\n")
        searchSelect <- readline(prompt = "  Search Selection: ")
        #-----------------#
        #- Search Region -#
        #-----------------#
        if(searchSelect == "r"){
          while(TRUE){
            cat("\n    ~~~~~~~~~~~~~~~~~~~~~~~")
            cat("\n    ~~~~ Region Search ~~~~")
            cat("\n    ~~~~~~~~~~~~~~~~~~~~~~~\n\n")
            regionSelect <- readline(prompt = "    Keyword, partial word, or phrase. Non-case-sensitive: ")
            region <- grep(regionSelect, RS_label[, 1], ignore.case = TRUE)
            if(length(region) == 0){
              cat('\n    No regions were matched using "', regionSelect, '"\n', sep = "")
              cat("\n    (r) Search Regions again")
              cat("\n    (k) Keyword Search Menu")
              cat("\n    (m) Main Menu")
              cat("\n    (e) Save and Exit")
              continueSelect <- readline(prompt = "    Choice: ")
              if(continueSelect %in% c("k","m","e")){break}
            }
            else if(length(region) > 0){
              while(TRUE){
                regions <- as.matrix(unique(RS_label[region,1]))
                regionCodes <- paste("  (", seq(dim(regions)[1]), ") ", sep = "")
                cat("\n    Matches:\n")
                cat(paste("\n  ", regionCodes, regions, sep = ""))
                cat("\n\n    Now what?\n")
                cat("\n    (s) Search sectors in matched regions")
                cat("\n    (n) Narrow matched regions")
                cat("\n    (a) Select all sectors of matched regions")
                cat("\n    (r) Search regions again")
                cat("\n    (k) Keyword Search Menu")
                cat("\n    (m) Main Menu")
                cat("\n    (e) Save and Exit")
                continueSelect <- readline(prompt = "    Choice: ")
                # Search sectors
                if("s" %in% continueSelect){
                  while(TRUE){
                    cat("\n      ~~~~~~~~~~~~~~~~~~~~~~~")
                    cat("\n      ~~~~ Sector Search ~~~~")
                    cat("\n      ~~~~~~~~~~~~~~~~~~~~~~~\n\n")
                    sectorSelect <- readline(prompt = "      Keyword, partial word, or phrase. Non-case-sensitive: ")
                    sector <- grep(sectorSelect, RS_label[which(RS_label[, 1] %in% regions), 2], ignore.case = TRUE)
                    if(length(sector) == 0){
                      cat('\n      No sectors were matched using "', sectorSelect, '"\n', sep = "")
                      cat("\n      (s) Search Sectors again from previously matched regions")
                      cat("\n      (r) Search Regions again")
                      cat("\n      (k) Keyword Search Menu")
                      cat("\n      (m) Main Menu")
                      cat("\n      (e) Save and Exit")
                      continueSelect <- readline(prompt = "      Choice: ")
                      if(continueSelect == "s"){} # Nothing
                      if(continueSelect %in% c("r","k","m","e")){break}
                    } else if(length(sector) > 0){
                      sectors <- as.matrix(unique(RS_label[sector, 2]))
                      sectorCodes <- paste("      (", seq(dim(sectors)[1]), ") ", sep = "")
                      cat("\n      Matches:\n")
                      cat(paste("\n", sectorCodes, sectors, sep = ""))
                      if(length(region) > 1){
                        RS <- matrix(RS_label[RS_label[, 1] %in% regions, ], ncol = 2)
                        RS <- matrix(RS[RS[, 2] %in% sectors, ], ncol = 2)
                        for(s in 1:length(sectors)){
                          for(r in 1:length(regions)){
                            if(length(which(RS[, 1] %in% regions[r] & RS[, 2] %in% sectors[s])) == 0){
                              cat("\n\nWARNING:", sectors[s], "is not a sector in the region", regions[r])
                            }
                          }
                        }
                      }
                      cat("\n\n      (c) Choose which sectors to keep from previously matched regions")
                      cat("\n      (a) Select all sectors")
                      cat("\n      (s) Search Sectors again from previously matched regions")
                      cat("\n      (r) Search Regions again")
                      cat("\n      (k) Keyword Search Menu")
                      cat("\n      (m) Main Menu")
                      cat("\n      (e) Save and Exit")
                      continueSelect <- readline(prompt = "      Choice: ")
                      if(continueSelect == "c"){
                        while(TRUE){
                          cat("\n      Which sectors do you want to keep?")
                          cat("\n      For multilple selections use x,y,z")
                          cat("\n      WARNING both c(x,y,z) and x:z are invalid formats")
                          sectorSelect <- as.numeric(unlist(strsplit(readline(prompt = "      Sector Selection: "), split = ",")))
                          selection <- c(selection, which(RS_label[, 1] %in% regions & RS_label[, 2] %in% sectors[sectorSelect]))
                          cat("\n ***** selection made *****\n")
                          cat("\n\n      Now what?\n")
                          cat("\n      (c) Continue adding sectors")
                          cat("\n      (s) Search sectors of previously matched regions")
                          cat("\n      (r) Search regions again")
                          cat("\n      (k) Keyword Search Menu")
                          cat("\n      (m) Main Menu")
                          cat("\n      (e) Save and Exit")
                          continueSelect <- readline(prompt = "      Choice: ")
                          if(continueSelect %in% c("s","r","k","m","e")){break}
                        }
                      }
                      if(continueSelect == "a"){
                        selection <- c(selection, which(RS_label[, 1] %in% regions & RS_label[, 2] %in% sectors))
                        cat("\n ***** selection made *****\n")
                        cat("\n      Now what?\n")
                        cat("\n      (r) Search regions again")
                        cat("\n      (k) Keyword Search Menu")
                        cat("\n      (m) Main Menu")
                        cat("\n      (e) Save and Exit")
                        continueSelect <- readline(prompt = "      Choice: ")
                      }
                      if(continueSelect == "s"){} # Nothing
                      if(continueSelect %in% c("r","k","m","e")){break}
                    }
                  }
                }
                if("n" %in% continueSelect){
                  cat("\n    Keep which regions")
                  cat("\n    For multilple selections use x,y,z")
                  cat("\n    WARNING both c(x,y,z) and x:z are invalid formats")
                  regionSelect <- as.numeric(unlist(strsplit(readline(prompt = "    Region Selection: "), split = ",")))
                  region <- region[regionSelect]
                }
                if("a" %in% continueSelect){
                  selection <- c(selection, which(RS_label[, 1] %in% regions[region]))
                  cat("\n ***** selection made *****\n")
                  cat("\n    Now what?\n")
                  cat("\n    (r) Search regions again")
                  cat("\n    (k) Keyword Search Menu")
                  cat("\n    (m) Main Menu")
                  cat("\n    (e) Save and Exit")
                  continueSelect <- readline(prompt = "    Choice: ")
                }
                if("r" %in% continueSelect){}# Nothing
                if(continueSelect %in% c("m","e","r")){break}
              }
            }
            if(continueSelect %in% c("k","m","e")){break}
          }
        }
        #-----------------#
        #- Search Sector -#
        #-----------------#
        if(searchSelect == "s"){
          while(TRUE){
            cat("\n    ~~~~~~~~~~~~~~~~~~~~~~~")
            cat("\n    ~~~~ Sector Search ~~~~")
            cat("\n    ~~~~~~~~~~~~~~~~~~~~~~~\n\n")
            sectorSelect <- readline(prompt = "    Keyword, partial word, or phrase. Non-case-sensitive: ")
            sector <- grep(sectorSelect, RS_label[, 2], ignore.case = TRUE)
            if(length(sector) == 0){
              cat('\n    No regions were matched using "', sectorSelect, '"\n', sep = "")
              cat("\n    (s) Search Sectors again")
              cat("\n    (k) Keyword Search Menu")
              cat("\n    (m) Main Menu")
              cat("\n    (e) Save and Exit")
              continueSelect <- readline(prompt = "    Choice: ")
              if(continueSelect %in% c("k","m","e")){break}
            } else if(length(sector) > 0){
              while(TRUE){
                sectors <- as.matrix(unique(RS_label[sector, 2]))
                sectorCodes <- paste("    (", seq(dim(sectors)[1]), ") ", sep = "")
                cat("\n    Matches:\n")
                cat(paste("\n", sectorCodes, sectors, sep = ""))
                regions <- unique(RS_label[, 1])
                if(length(regions) > 1){
                  RS <- RS_label[RS_label[, 2] %in% sectors, ]
                  for(s in 1:length(sectors)){
                    for(r in 1:length(regions)){
                      if(length(which(RS[, 1] %in% regions[r] & RS[, 2] %in% sectors[s])) == 0){
                        cat("\n\nWARNING:", sectors[s], "is not a sector in the region", regions[r])
                      }
                    }
                  }
                }
                cat("\n\n    (r) Search regions for matched sectors")
                cat("\n    (n) Narrow matched sectors")
                cat("\n    (a) Select all regions of matched sectors")
                cat("\n    (s) Search sectors again")
                cat("\n    (k) Keyword Search Menu")
                cat("\n    (m) Main Menu")
                cat("\n    (e) Save and Exit")
                continueSelect <- readline(prompt = "    Choice: ")
                if(continueSelect == "r"){
                  while(TRUE){
                    cat("\n      ~~~~~~~~~~~~~~~~~~~~~~~")
                    cat("\n      ~~~~ Region Search ~~~~")
                    cat("\n      ~~~~~~~~~~~~~~~~~~~~~~~\n\n")
                    regionSelect <- readline(prompt = "      Keyword, partial word, or phrase. Non-case-sensitive: ")
                    region <- grep(regionSelect, RS_label[which(RS_label[, 2] %in% sectors), 1], ignore.case = TRUE)
                    if(length(region) == 0){
                      cat('\n      No regions were matched using "', regionSelect, '"\n', sep = "")
                      cat("\n      (r) Search Regions again from previously matched sectors")
                      cat("\n      (s) Search Sectors again")
                      cat("\n      (k) Keyword Search Menu")
                      cat("\n      (m) Main Menu")
                      cat("\n      (e) Save and Exit")
                      continueSelect <- readline(prompt = "    Choice: ")
                      if(continueSelect %in% c("s","k","m","e")){break}
                    } else if(length(region) > 0){
                      regions <- as.matrix(unique(RS_label[region, 1]))
                      regionCodes <- paste("  (", seq(dim(regions)[1]), ") ", sep = "")
                      cat("\n      Matches:\n")
                      cat(paste("\n    ", regionCodes, regions, sep = ""))
                      cat("\n\n      (c) Choose which regions to keep from previously matched sectors")
                      cat("\n      (a) Select all sectors")
                      cat("\n      (r) Search Regions again from previously matched sectors")
                      cat("\n      (s) Search Sectors again")
                      cat("\n      (k) Keyword Search Menu")
                      cat("\n      (m) Main Menu")
                      cat("\n      (e) Save and Exit")
                      continueSelect <- readline(prompt = "      Choice: ")
                      if(continueSelect == "c"){
                        while(TRUE){
                          cat("\n      Which regions do you want to keep?")
                          cat("\n      For multilple selections use x,y,z")
                          cat("\n      WARNING both c(x,y,z) and x:z are invalid formats")
                          regionSelect <- as.numeric(unlist(strsplit(readline(prompt = "      Region Selection: "), split = ",")))
                          selection <- c(selection, which(RS_label[, 1] %in% regions[regionSelect] & RS_label[, 2] %in% sectors))
                          cat("\n ***** selection made *****\n")
                          cat("\n\n      Now what?\n")
                          cat("\n      (c) Continue adding sectors")
                          cat("\n      (r) Search regions again from previously matched sectors")
                          cat("\n      (s) Search sectors again")
                          cat("\n      (k) Keyword Search Menu")
                          cat("\n      (m) Main Menu")
                          cat("\n      (e) Save and Exit")
                          continueSelect <- readline(prompt = "      Choice: ")
                          if(continueSelect %in% c("s","r","k","m","e")){break}
                        }
                      }
                      if(continueSelect == "a"){
                        selection <- c(selection, which(RS_label[, 1] %in% regions & RS_label[, 2] %in% sectors))
                        cat("\n ***** selection made *****\n")
                        cat("\n      Now what?\n")
                        cat("\n      (s) Search Sectorss again")
                        cat("\n      (k) Keyword Search Menu")
                        cat("\n      (m) Main Menu")
                        cat("\n      (e) Save and Exit")
                        continueSelect <- readline(prompt = "      Choice: ")
                      }
                      if(continueSelect == "r"){} # Nothing
                      if(continueSelect %in% c("s","k","m","e")){break}
                    }
                  }
                }
                if(continueSelect == "n"){
                  cat("\n    Keep which sectors")
                  cat("\n    For multilple selections use x,y,z")
                  cat("\n    WARNING both c(x,y,z) and x:z are invalid formats")
                  sectorSelect <- as.numeric(unlist(strsplit(readline(prompt = "    Sector Selection: "), split = ",")))
                  sector <- sector[sectorSelect]
                }
                if(continueSelect == "a"){
                  selection <- c(selection, which(RS_label[, 2] %in% sectors))
                  cat("\n ***** selection made *****\n")
                  cat("\n    Now what?\n")
                  cat("\n    (s) Search sectors again")
                  cat("\n    (k) Keyword Search Menu")
                  cat("\n    (m) Main Menu")
                  cat("\n    (e) Save and Exit")
                  continueSelect <- readline(prompt = "    Choice: ")
                }
                if(continueSelect %in% c("s","k","m","e")){break}
              }
              if(continueSelect %in% c("k","m","e")){break}
            } # one above
          }
        }
        if(searchSelect == "m"){break}
        if(continueSelect %in% c("m", "e")){break}
      }
    }
    #####################
    ## List Indicators ##
    #####################
    if(userInput == "l"){
      while(TRUE){ # Keeping us inside the selection loop
        cat("\n  ~~~~~~~~~~~~~~~~~~~~~~~")
        cat("\n  ~~~~ Select Region ~~~~")
        cat("\n  ~~~~~~~~~~~~~~~~~~~~~~~\n")
        regions <- as.matrix(unique(RS_label[, 1]))
        regionCodes <- paste("  (", seq(dim(regions)[1]), ") ", sep = "")
        cat(paste("\n", regionCodes, regions, sep = ""))
        cat("\nFor multilple selections use x,y,z")
        cat("\nWARNING both c(x,y,z) and x:z are invalid formats")
        cat("\n  (0) Main Menu")
        # regionSelect <- as.numeric(readline(prompt = "  Region Selection: "))
        regionSelect <- as.numeric(unlist(strsplit(readline(prompt = "  Region Selection: "), split = ",")))
        if(0 %in% regionSelect){break}
        if(all(regionSelect %in% 1:length(regions))){
          while(TRUE){ # Keeping us inside loop, allowing for "back"
            region <- regions[regionSelect, 1]
            cat("\n    ~~~~~~~~~~~~~~~~~~~~~~~")
            cat("\n    ~~~~ Select Sector ~~~~")
            cat("\n    ~~~~~~~~~~~~~~~~~~~~~~~\n")
            cat('\n     ', region,'\n', sep = " ")
            regionRow <- which(RS_label[, 1] %in% region)
            sectors <- as.matrix(unique(RS_label[regionRow, 2]))
            sectorCodes <- paste("    (", seq(dim(sectors)[1]), ") ", sep = "")
            cat(paste("\n", sectorCodes, sectors, sep = ""))
            if(length(region) > 1){
              RS <- RS_label[RS_label[, 1] %in% regions, ]
              RS <- RS[RS[, 2] %in% sectors, ]
              for(s in 1:length(sectors)){
                for(r in 1:length(regions)){
                  if(length(which(RS[, 1] %in% regions[r] & RS[, 2] %in% sectors[s])) == 0){
                    cat("\n\nWARNING:", sectors[s], "is not a sector in region", regions[r], "\n")
                  }
                }
              }
            }
            cat("\n  For multilple selections use x,y,z")
            cat("\n  WARNING both c(x,y,z) and x:z are invalid formats")
            cat("\n    (0) Back")
            cat("\n    (-1) Main Menu\n")
            sectorSelect <- as.numeric(unlist(strsplit(readline(prompt = "    Sector Selection: "), split = ",")))
            if(-1 %in% sectorSelect){break}
            selection <- c(selection, which(RS_label[, 1] %in% regions[regionSelect,1] & RS_label[, 2] %in% sectors[sectorSelect,1]))
            cat("\n ***** selection made *****\n")
            if(0 %in% sectorSelect| -1 %in% sectorSelect){
              continueSelect <- "y"
              break
            }
            if(all(sectorSelect %in% 1:length(sectors))){
              cat("\n    Go back to:\n")
              cat("\n    (s) Sector Selection")
              cat("\n    (r) Region Selection")
              cat("\n    (m) Main Menu\n    (e) Save and Exit")
              continueSelect <- readline(prompt = "  Choice: ")
              if(continueSelect %in% c("r", "m", "e")){break}
            }
          }
          if(continueSelect %in% c("m", "e")){break}
          if(-1 %in% sectorSelect){break}
        }
      }
      if(-1 %in% regionSelect){break}
    }
    if(userInput == "v"){
      if(is.null(selection)){
        cat("\n-------------------------------------")
        cat("\n-------------------------------------")
        cat("\n---- No items have been selected ----")
        cat("\n-------------------------------------")
        cat("\n-------------------------------------\n\n")
      } else if(!is.null(selection)){
        while(TRUE){
          selection <- sort(unique(selection))
          view1 <- RS_label[selection, 1]
          view2 <- RS_label[selection, 2]
          viewCodes <- paste("  (", seq(length(view1)), ") ", sep = "")
          cat(paste("\n", viewCodes, view1, " - ", view2, sep = ""))
          cat("\n\n  (r) choose items to remove")
          cat(  "\n  (m) Main Menu")
          cat(  "\n  (e) Save and Exit")
          continueSelect <- readline(prompt = "  Choice: ")
          if(continueSelect == "r"){
            cat("\n  For multilple selections use x,y,z")
            cat("\n  WARNING both c(x,y,z) and x:z are invalid formats")
            selectionSelect <- as.numeric(unlist(strsplit(readline(prompt = "  Sector Selection: "), split = ",")))
            selection <- selection[setdiff(1:length(selection), selectionSelect)]
          }
          if(continueSelect %in% c("m","e")){break}
        }
      }
    }
    if(continueSelect == "e"){break}
  }
  if(!is.null(selection)){
    selection <- sort(unique(selection))
    regions <- as.character(RS_label[selection, 1])
    sectors <- as.character(RS_label[selection, 2])
    selection <- cbind(selection, regions, sectors)
    class(selection) <- "EasySelect"
    selection
  } else if(is.null(selection)){
    cat("\n  No selection made \n ")
  }
}
