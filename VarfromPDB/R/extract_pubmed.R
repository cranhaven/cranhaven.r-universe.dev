extract_pubmed <-
function(query, keyword, localPDB.path = paste(getwd(),"localPDB",sep="/")){ 
    ## extract the RESULTS from abstract
    abs_trim <- function(x){
       # x = pubmed_abs[1]
        if(length(grep("METHODS:",x)) > 0) {
           x.trim <- unlist(strsplit(x,"METHODS:"))[2]
           }else if(length(grep("METHOD:",x)) > 0) {
               x.trim <- unlist(strsplit(x,"METHOD:"))[2]
               }else {
                 x.trim <- x  
        }
        if(length(grep("CONCLUSION",x.trim)) > 0) 
              x.trim <- unlist(strsplit(x.trim,"CONCLUSION*"))[1]     
        return(x.trim)  
    }
    
    # extract the conclusion from abstract
    abs_conclusion <- function(x){
       # x = pubmed_abs[1]
        if(length(grep("CONCLUSIONS:",x)) > 0) {
           x.trim <- unlist(strsplit(x,"CONCLUSIONS:"))[2]
           }else if(length(grep("CONCLUSION:",x)) > 0) {
                 x.trim <- unlist(strsplit(x,"CONCLUSION:"))[2]
                 }else{
                 x.trim <- x  
        }
        return(x.trim)  
    }
####################################
## remove the redundancy strings p.change or c.change
str_rep_check  <- function(x){
    if(length(x) > 1){
       rmlist <- c()
       for(i in 1:(length(x)-1)){
          for(j in (i+1):length(x)){
            if(length(grep(x[i],x[j])) == 1)  rmlist <- c(rmlist,i)
            if(length(grep(x[j],x[i])) == 1)  rmlist <- c(rmlist,j)       
          }    
       }   
    if(length(rmlist) > 0 )    x <- x[-rmlist]
    }
    return(x)
}
    
#################################################3    
# transform the gene alias to gene symbol
gs2hgnc <- function(gene){
   #gene = "A1BGAS"
   gene = toupper(gene)
   gene.approved = gene
   app = hgnc[hgnc$symbol==gene,]
   pre.symbol = hgnc[grep(gene,hgnc$prev_symbol),]
   synonyms = hgnc[grep(gene,hgnc$alias_symbol),]
   if(nrow(app)==1){
      gene.approved = as.character(app$symbol)
      }else if(nrow(pre.symbol)==1){
         pre.symbol.trim = as.character(pre.symbol$prev_symbol)
         pre.symbol.trim = unlist(strsplit(pre.symbol.trim,", "))
         if(length(pre.symbol.trim[pre.symbol.trim==gene]) == 1){
            gene.approved = as.character(pre.symbol$symbol)
            }
          }else if(nrow(pre.symbol) > 1){
            pre.symbol.trim = as.character(pre.symbol$prev_symbol)
            for(j in 1:length(pre.symbol.trim)){
                  pre.symbol.trim.j = unlist(strsplit(pre.symbol.trim[j],", "))
                  if(length(intersect(pre.symbol.trim.j,gene)) > 0){
                  if(intersect(pre.symbol.trim.j,gene) == gene){
                    pre.symbol.j = pre.symbol[j,]
                    gene.approved = as.character(pre.symbol.j$symbol)
                  }}
               }
            }else if(nrow(synonyms) >= 1){
                synonyms.trim = as.character(synonyms$Synonyms)
                for(j in 1:length(synonyms.trim)){
                  synonyms.trim.j = unlist(strsplit(synonyms.trim[j],", "))
                  if(length(intersect(synonyms.trim.j,gene)) > 0){
                  if(intersect(synonyms.trim.j,gene) == gene){
                   synonyms.j = synonyms[j,]
                    gene.approved = as.character(synonyms.j$symbol)
                  }}
               }               
            }
   if(nrow(synonyms) == 0 & nrow(app)==0 & nrow(pre.symbol)==0) { gene.approved = paste("missing(",gene,")",sep="") }        
   return(gene.approved)
}
##########################################################
## variants to genes
## mapping a variant to a gene based on sentence-level concurrence
## esblish a statistic for distance to measure the relationship of a pair of variant-gene
var2gene <- function(text,extr.genes,extr.vars.c,extr.vars.p){
    # text <- pubmed_abs[i]
  #  text <- stri_replace_all_regex(x, "?",  "") 
    text <- stri_replace_all_regex(text, " &gt; ",  "&gt;") 
    text <- stri_replace_all_regex(text, "&gt; ",  "&gt;") 
    text <- stri_replace_all_regex(text, "-&gt;",  "&gt;") 
#    x.trim <- unlist(strsplit(text,"\\:|\\,|\\[|\\]|and"))
#    x.trim.2 <- unlist(strsplit(text,"\\:|\\. |\\[|\\]|and"))
#    x.trim.3 <- unlist(strsplit(text,"\\:|\\. |\\[|\\]"))
    x.trim <- unlist(strsplit(text,"\\:|\\,|and"))
    x.trim.2 <- unlist(strsplit(text,"\\:|\\. |and"))
    x.trim.3 <- unlist(strsplit(text,"\\:|\\. "))
    relations <- c()    

    ## search "()",whether have the format such as "GJB2 (35delG, 176del16, 235delC, 299delAT)"
    parentheses.former <- grep("\\(",x.trim)
    parentheses.last <- grep("\\)",x.trim)
    parentheses.former.sel <- setdiff(parentheses.former,parentheses.last)
    parentheses.last.sel <- setdiff(parentheses.last,parentheses.former)
    if(length(parentheses.former.sel) == length(parentheses.last.sel) & length(parentheses.former.sel) > 0){
       for(b in 1:length(parentheses.former.sel)){
          relations.special.b <- paste(x.trim[parentheses.former.sel[b]:parentheses.last.sel[b]], collapse="")
          relations.special.b.trim <- unlist(strsplit(relations.special.b, "( )|\\(|\\)"))
          relations.special.b.trim <- relations.special.b.trim[relations.special.b.trim !=""]
          relations.special.b.1 <- relations.special.b.trim[is.element(relations.special.b.trim,extr.genes)]
          relations.special.b.2 <- relations.special.b.trim[is.element(relations.special.b.trim,extr.vars.c)]
          if(length(relations.special.b.1) == 0 ){
             relations <- rbind(relations,c("",paste(relations.special.b.2,collapse= ","),"", paste(c("toConfirm", relations.special.b),collapse = ":")))
             }else if(length(relations.special.b.1) != 0 & length(relations.special.b.2) == 0){
                 relations <- rbind(relations,c(paste(relations.special.b.1,collapse= ","), "", "", paste(c("toConfirm",relations.special.b),collapse=":")))
                 }else if(unique(is.element(relations.special.b.1,extr.genes) & is.element(relations.special.b.2,extr.vars.c))){
                    relations <- rbind(relations,c(paste(relations.special.b.1,collapse= ","),paste(relations.special.b.2,collapse= ","),"", relations.special.b))
          }
       }
    }
    relations <- relations[relations[,2] !="",]
    
    
   ## multiple genes, multiple variants: no c.DNA, but have p.change
    if(length(extr.genes) > 0 & length(extr.vars.c)==0 & length(extr.vars.p) > 0){
       if(length(extr.genes) == 1 & length(extr.vars.p) == 1){
          relations <- rbind(relations,c(extr.genes,"",extr.vars.p,"one2one"))
          }else if(length(extr.genes) > 1 & length(extr.vars.p) == 1){
             relaltions.slect = c()
             for(i in extr.genes){
                k <- extr.vars.p
                sentence.m <- x.trim.3[intersect(grep(i,x.trim.3,fixed = TRUE),grep(k,x.trim.3,fixed = TRUE))] 
                relaltions.slect <- rbind(relaltions.slect,c(i,k,length(sentence.m),paste(sentence.m,collapse = ";")))
             }   
             relaltions.hit <- relaltions.slect[relaltions.slect[,3] == max(relaltions.slect[,3]),]
             relations <- rbind(relations,relaltions.hit)         
          }else{
          for(i in extr.genes){
             for(k in extr.vars.p){
                sentence.m <- x.trim.3[intersect(grep(i,x.trim.3,fixed = TRUE),grep(k,x.trim.3,fixed = TRUE))] 
                if(length(sentence.m) >=1)
                   relations <- rbind(relations,c(i,"",k,paste(c("toConfirm", sentence.m),collapse = ":")))
             }
          }
       }
    }

    ## multiple genes, multiple variants:c.DNA and p.change
    if(length(extr.genes) >1){
    for(i in extr.genes){
      if(length(extr.vars.c) == 1 & length(extr.vars.p) == 1){
          sentence.m <- x.trim.3[c(intersect(grep(i,x.trim.3,fixed = TRUE),grep(extr.vars.c,x.trim.3,fixed = TRUE)),
            intersect(grep(i,x.trim.3,fixed = TRUE),grep(extr.vars.p,x.trim.3,fixed = TRUE)))] 
          if(length(sentence.m) >=1)
          relations <- rbind(relations,c(i,extr.vars.c,extr.vars.p,"one2one"))
      }else
      if(length(extr.vars.c) > 0){
        for(j in extr.vars.c){
           s.i <- intersect(grep(i,x.trim,fixed = TRUE),grep(j,x.trim,fixed = TRUE))
           if(length(s.i) ==0 | (length(extr.vars.c) == 1 & length(extr.vars.p) == 1)){
                  s.i.2 <- intersect(grep(i,x.trim.2,fixed = TRUE),grep(j,x.trim.2,fixed = TRUE))
                  if(length(s.i.2)==0) next
           }       
    ## 1. c.DNA,p.change,gene in a sentence. comma seperated
           if(length(s.i) > 0){
              for(m in 1:length(s.i)){
                 sentence.m <- x.trim[s.i]             
           # check the protein-level mutation whether in the sentence
                 if(length(extr.vars.p) > 0){
                      for(k in extr.vars.p){
                        if(length(grep(k,sentence.m,fixed = TRUE)) > 0){
                            relations <- rbind(relations,c(i,j,k,","))
                            }else  if(length(grep(j,relations,fixed = TRUE)) == 0){
                                relations <- rbind(relations,c(i,j,"",","))
                        }    
                      }      
                      ## no mutation at protein level
                    }else if(length(extr.vars.p) == 0){
                         relations <- rbind(relations,c(i,j,"",","))
                 }            
              }
           }
    
    ## 2. c.DNA,p.change,gene in a sentence. ". " seperated
           if(length(s.i) == 0 |(length(extr.vars.c) == 1 & length(extr.vars.p) == 1)){
              for(m in 1:length(s.i.2)){
                 sentence.m <- x.trim.2[s.i.2]             
           # check the protein-level mutation whether in the sentence
                 if(length(extr.vars.p) > 0){
                      for(k in extr.vars.p){
                        if(length(grep(k,sentence.m,fixed = TRUE)) > 0){
                            if(length(extr.vars.c) == 1 & length(extr.vars.p) == 1){
                               relations <- rbind(relations,c(i,j,k,"one2one"))
                               }else{
                                  relations <- rbind(relations,c(i,j,k,"."))
                            }      
                            }else{
                               relations <- rbind(relations,c(i,j,"","."))   
                        }    
                      }      
                   }else if(length(extr.vars.p) == 0){
                         relations <- rbind(relations,c(i,j,"",".")) 
                 }                   
              }
           } 
        }   
      }
    }    
    }
    
    ## one genes, multiple variants: c.DNA and protein change  
    if(length(extr.genes) == 1 & length(extr.vars.c) == 1 & length(extr.vars.p) ==1 ){
       relations <- rbind(relations,c(extr.genes,extr.vars.c,extr.vars.p,"one2one")) 
       }else if(length(extr.genes) == 1 & length(extr.vars.c) == 1 & length(extr.vars.p) ==0 ){
           relations <- rbind(relations,c(extr.genes,extr.vars.c,"","one2one"))
           }else if(length(extr.genes) == 1 & length(extr.vars.c) >= 1 & length(extr.vars.p) ==0 ){
               relations <- rbind(relations,c(extr.genes,paste(extr.vars.c,collapse = "," ),"","one2more")) 
            }else if(length(extr.genes) == 1 & length(extr.vars.c) == 1 & length(extr.vars.p) >1 ){ 
                 relations <- rbind(relations,c(extr.genes,extr.vars.c,paste(extr.vars.p,collapse = ","),"toConfirm")) 
             }else if(length(extr.genes) == 1){
             i <- extr.genes
             if(length(extr.vars.c)>0){
               for(j in extr.vars.c){
                 if(length(extr.vars.p)>0){
                  for(k in extr.vars.p){
                    s.j <- intersect(grep(j,x.trim,fixed = TRUE),grep(k,x.trim,fixed = TRUE))
                    if(length(s.j) ==0 ){
                           s.j.2 <- intersect(grep(j,x.trim.2,fixed = TRUE),grep(k,x.trim.2,fixed = TRUE))
                           if(length(s.j.2)==0) # next
                           relations <- rbind(relations,c(i,j,"",""))
                    }       
             ## 1. c.DNA,p.change,gene in a sentence. comma seperated 
                    if(length(s.j) >=1) { 
                       relations <- rbind(relations,c(i,j,k,",")) 
                       }else if(length(s.j) == 0 & length(s.j.2) >=1) {
                          relations <- rbind(relations,c(i,j,k,"."))  
                      }else if(length(s.j) == 0 & length(s.j.2) == 0){
                              relations <- rbind(relations,c(i,j,"","")) 
                    }     
                    rm(s.j);           
                  }
                 } 
               }     
             }
     }

#    ## check the relations whether have a repeat
   relations <- unique(relations)
## check the "( )"
   if(!is.null(relations)){
     for(g.c in extr.genes){
         if(!is.matrix(relations)) next
             relations.gc <- relations[relations[,1] == g.c,]
         if(is.matrix(relations.gc)){
           if(nrow(relations.gc) > 1){
              if(length(grep("\\(", relations.gc[,4]))>0){
                 if(length(grep("toConfirm",relations.gc[,4]))== 0)
                    relations <- relations[-setdiff(grep(g.c, relations[,1]), grep("\\(",relations[,4])),]
              }  
           }
         }     
     }
   }
   
   if(is.null(relations)){
       relations <- c(paste(extr.genes,collapse= ","), 
              ifelse(length(extr.vars.c) == 0, "", paste(extr.vars.c,collapse= ",")), 
              ifelse(length(extr.vars.p) == 0, "", paste(extr.vars.p,collapse= ",")), paste(c("toConfirm", text),collapse = ":"))
       }else if(is.matrix(relations)){
      ## check the pt change
       pt.check = table(relations[relations[,3] != "",3])      
       pt.check = pt.check[pt.check>1]
       if(length(pt.check) > 0){
           checks = is.element(relations[,3],names(pt.check)) & relations[,4] == "."
           nums.rm = (1:nrow(relations))[checks]
           relations = relations[-nums.rm,]
       }
      
      if(is.matrix(relations)){     
      if(nrow(relations) > length(extr.vars.c)){
          relations.1 <- relations[relations[,4] == ","|relations[,4] == "one2one"|relations[,4] == "one2more",]
          if(is.matrix(relations.1)){
           if(nrow(relations.1) > length(extr.vars.c) & length(unique(relations.1[,2]))==1)
             relations.1 = relations.1[relations.1[,3] != "",]      
          }        
          relations.2 <- relations[relations[,4] == "",]
          mut.c.rep <- table(relations[,2])
          mut.c.rep <- names(mut.c.rep[mut.c.rep>1])
          mut.c.rep <- mut.c.rep[mut.c.rep != ""]
          if(length(mut.c.rep) > 0){
            if(nrow(relations) - length(extr.vars.c) == 1){
              relations <- relations.1
              }else{
                relations.2 <- relations.2[!is.element(relations.2[,2],mut.c.rep),]
                if (is.matrix(relations.2)|length((relations.2)) == 4){
                      relations <- rbind(relations.1,relations.2)
                    }else{
                      relations <- relations.1
                } 
            }         
          } 
          
      ## check relations from "," "." 
        if(!is.null( nrow(relations))){
          if(nrow(relations) > length(extr.vars.c)){     
             relations.1 <- relations[relations[,4] == ",",]
             relations.2 <- relations[relations[,4] == ".",]
             mut.c.rep <- table(relations[,2])
             mut.c.rep <- names(mut.c.rep[mut.c.rep>1])
             mut.c.rep <- mut.c.rep[mut.c.rep != ""]
             if(length(mut.c.rep) > 0){
               if(nrow(relations) - length(extr.vars.c) == 1){
                 relations <- relations.1
                 }else{
                    relations.2 <- relations.2[!is.element(relations.2[,2],mut.c.rep),]
                    if(nrow(relations.2)>0){
                       relations <- rbind(relations.1,relations.2)
                       }else{
                          relations <- relations.1
                    }
               }           
              } 
          }
          }else if(is.null(nrow(relations))){
             if(length(extr.vars.c)>1){
                relations <- c(paste(extr.genes,collapse= ","), paste(extr.vars.c,collapse= ","), paste(extr.vars.p,collapse= ","),
                                          paste(c("toConfirm", text),collapse = ":"))
             }
        }  
       }}   
   }           
    return(relations)
}


#######################################################
ptChange2hgvs <- function(x){
       # x = "D88H"
       # x= "p.L32fs"
       # x = "p.Lys377*"
       # x = "p.Pro155fsX"
       # x = "p.Met847_Glu853dup"
       # x = "p.Lys1048_Glu1054del"
       # x = "p.Ala461Thr"
       
## case1: x = "p.Ala461Thr", p.Met84....
         x.trim <- c()
         if(length(grep("^p\\.[A-Z]+",x)) ==1){ 
            pos.num <- as.numeric(unlist(strsplit(x,"")))            
            pos.num.1 <- (1:length(pos.num))[!is.na(pos.num)][1]            
            pos.nums <- (1:length(pos.num))[!is.na(pos.num)]
            pos.nums.last <- pos.nums[length(pos.nums)]
            if(is.na(pos.num.1)){
               x.trim <- paste(x,"toConform",sep=",")
               }else if(pos.num.1 == 6){
                       if(is.element(substr(x,3,5),aa[,3])){
                             x.trim <- x
                             }else{
                                x.trim <- "check" 
                           }  
                       }else if(pos.num.1 == 4){
         ## case2: x= "p.L32fs"                   
                               if(length(grep("fs",x,ignore.case=T))==1){
                                  x.1 <- unlist(strsplit(x,"fsX|fs|fs\\*"))[1]
                                  x.tail <- substr(x,nchar(x.1)+1,nchar(x))
                                  }else{
                                    x.1 <- x
                               }   
                                  x.hgvs <- unlist(strsplit(x.1,"p\\.|[0-9]+"))
                                  x.hgvs <- x.hgvs[x.hgvs != ""]
                                  x.hgvs.1 <- x.hgvs[is.element(x.hgvs,aa.table[,4])]
                               if(length(x.hgvs.1) == 1){
                                     x.hgvs.1_3_1 <- as.character(unique(aa.table[aa.table[,4] == x.hgvs.1[1],3]))
                                     x.trim <- stri_replace_all_regex(x,paste("p.",x.hgvs.1[1],sep=""),paste("p.",x.hgvs.1_3_1,sep=""))
                                  }                                       
         ## case3: x= "p.L32Afs"                                         
                               if(length(x.hgvs.1) == 2){
                                  x.hgvs.1_3_1 <- as.character(unique(aa.table[aa.table[,4] == x.hgvs.1[1],3]))
                                  x.hgvs.1_3_2 <- as.character(unique(aa.table[aa.table[,4] == x.hgvs.1[2],3]))
                                  x.tail <- substr(x,pos.nums.last+1+1,nchar(x))
                                  x.pos <- substr(x,pos.num.1,pos.nums.last)
                                  x.trim <- paste("p.",x.hgvs.1_3_1,x.pos,x.hgvs.1_3_2,x.tail,sep="")
                              }               
                       }
 ## case4: x= "L32A"                                         
         }else if(length(grep("^p\\.[A-Z]+",x)) ==0){
             if(length(grep("^[A-Z][0-9]+[A-Z]$",x)) ==1){
                x.hgvs <- unlist(strsplit(x,"[0-9]+"))
                x.hgvs.1_3_1 <- as.character(unique(aa.table[aa.table[,4] == x.hgvs[1],3]))
                x.hgvs.1_3_2 <- as.character(unique(aa.table[aa.table[,4] == x.hgvs[2],3]))
                x.pos <- substr(x,2,nchar(x)-1)
                x.trim <- paste("p.",x.hgvs.1_3_1,x.pos,x.hgvs.1_3_2,sep="")     
                }else if(length(grep("^[A-Z][a-z][a-z][0-9]+[A-Z][a-z][a-z]$",x)) ==1){
## case5: Pro453Thr                           
                        x.hgvs <- unlist(strsplit(x,"[0-9]+"))
                        if(unique(is.element(x.hgvs,aa.table[,3])))
                              x.trim <- paste("p.",x,sep="")
                }              
         }  
         if(is.null(x.trim))
                    x.trim <- ""
   return(x.trim)         
}


##################################
    # x is a string, title, background, conclusion, results etc
    # pheno_capture_abs(keyword,pubmed_title[91])
    pheno_capture_abs <- function(keyword,x){
         # x <- pubmed_title[16]
         x.trim <- unlist(strsplit(x," with |\\.|\\:| cause. | associated | mapped 
                    |\\,| due | to | by | was | were | that | of | in | on | is 
                    | are | the | a | an | for | identified | by | using | .ing 
                    | susceptibility| had | featuring | and therefore"))
         x.trim <- unlist(strsplit(x.trim,"with |\\.| cause. |\\,| to | by | was | were 
               |that |of | in | is| are|for |the | susceptibility |had |causing"))
         pheno.extract <-  paste(unique(x.trim[c(grep_split(keyword,x.trim),
                                grep("congenital",x.trim),
                                grep("deficiency",x.trim),
                                grep("susceptibility",x.trim),
                                grep("X-linked",x.trim),
                                grep("dominant",x.trim),
                                grep("autosomal",x.trim),
                                grep("dominant",x.trim),
                                grep("recessive",x.trim),
                                grep("dysplasia",x.trim),
                                grep("familial",x.trim),
                                grep("dystrophy",x.trim),
                                grep("disorder",x.trim),
                                grep("ataxia",x.trim),
                                grep("hereditary",x.trim),
                                grep("Mitochondrial",x.trim),
                                grep("hypoplasia",x.trim),
                                grep("Mitochondrial",x.trim),
                                grep("syndrome",x.trim))]),collapse=";")
         if(pheno.extract == ""){
            x.trim <- unlist(strsplit(x," with "))[2]
            pheno.extract <- unlist(strsplit(x.trim,"\\.| by "))[1]
            pheno.extract <- ifelse(length(grep("mutation",pheno.extract)) > 0, 
                                        pheno.extract[-1],pheno.extract)
            if(is.na(pheno.extract)){
               if(length(grep("X.linked",x)) > 0 ){
                  x.trim <- unlist(strsplit(x," X.linked "))[2]
                  pheno.extract <- paste("X-linked", 
                          unlist(strsplit(x.trim,"\\.| by | in | with "))[1],sep=" ")
               }
               if(length(grep("Y.linked",x)) > 0 ){
                  x.trim <- unlist(strsplit(x,"Y.linked "))[2]
                  pheno.extract <- paste("Y-linked", 
                       unlist(strsplit(x.trim,"\\.| by | in | with "))[1],sep=" ")
               }
               if(length(grep("autosomal dominant",x)) > 0 ){
                  x.trim <- unlist(strsplit(x,"autosomal dominant"))[2]
                  pheno.extract <- paste("autosomal dominant",
                      unlist(strsplit(x.trim,"\\.| by | in | with "))[1],sep=" ")
               }
               if(length(grep("autosomal recessive",x)) > 0 ){
                  x.trim <- unlist(strsplit(x,"autosomal recessive"))[2]
                  pheno.extract <- paste("autosomal recessive",
                      unlist(strsplit(x.trim,"\\.| by | in | with "))[1],sep=" ")
               }
            }
         }   
         pheno.extract <- stri_replace_all_regex(pheno.extract,
              '\\:$|\\]|\\[|^ and|^ | $', '')
         if(length(pheno.extract) >1)
            pheno.extract <-  paste(pheno.extract,collapse=";")
          return(pheno.extract)
    }
    
##############################################################################################
    ## capture the genes and mutations from an abstract
    gene_var_abs <- function(x){
        # x = abs_trim(pubmed_abs[20])
        # x = (pubmed_abs[309])
        x <- stri_replace_all_regex(x, " &gt; ",  "&gt;") 
        x <- stri_replace_all_regex(x, "&gt; ",  "&gt;") 
        x <- stri_replace_all_regex(x, "-&gt;",  "&gt;") 
        x.trim <- unlist(strsplit(x,"( )|\\)|\\(|\\:|\\,|\\[|\\]|\\-associated|\\/"))
        x.trim <- stri_replace_all_regex(x.trim,"\\;$","")
         n_c. <- grep("(^c\\.[0-9]+$|^c\\.$)",x.trim)
         if(length(n_c.) > 0){
           for(i in 1:length(n_c.)){
             n_c.i <- n_c.[i]
             x.trim <- c(x.trim,paste(x.trim[n_c.i], x.trim[n_c.i + 1],sep=""))
             x.trim <- x.trim[-(n_c.i:(n_c.i+1))]
           }  
         }
         n_p. <- grep("^p\\.$",x.trim)
         if(length(n_p.) > 0){
           for(i in 1:length(n_p.)){
              n_p.i <- n_p.[i]
              x.trim <- c(x.trim,paste("p.", x.trim[n_p.i + 1],sep=""))
           }   
              x.trim <- x.trim[-c(n_p.,n_p.+1)]
         }
         n_p.space <- grep("^p\\.[A-Z][0-9]+$",x.trim)
         if(length(n_p.space) > 0){
           for(i in 1:length(n_p.space)){
              n_p.space.i <- n_p.space[i]
              x.trim <- c(x.trim,paste(x.trim[n_p.space.i], x.trim[n_p.space.i + 1],sep=""))
           }   
              x.trim <- x.trim[-c(n_p.space,n_p.space+1)]
         }
             
         x.trim <- stri_replace_all_regex(x.trim, '\\.$', '')
         muts.dna.1 <- x.trim[grep("^c\\..",x.trim)]
         muts.dna.2 <- x.trim[grep("^[ATCG]\\&gt\\;[ATCG]$",x.trim)]   
         muts.dna.3 <- x.trim[grep("^IVS.",x.trim)]  
         muts.dna.4 <- x.trim[grep("[0-9]+ins[ATCG]$",x.trim)]  
         muts.dna.5 <- x.trim[grep("rs[0-9]+",x.trim)] 
         muts.dna.6 <- x.trim[grep("[ATCG]+[0-9]+del",x.trim)]                   
         muts.dna.7 <- x.trim[grep("[0-9]+\\_[0-9]dup*.",x.trim)]                
         muts.dna.8 <- x.trim[grep("[ATCG][0-9]+[ATCG]$",x.trim)]            
         muts.dna.9 <- x.trim[grep("[0-9]+del[ATCG]+",x.trim)]                   
         muts.dna <- unique(c(muts.dna.1,muts.dna.2,muts.dna.3, muts.dna.4,muts.dna.5,muts.dna.6,muts.dna.7,muts.dna.8,muts.dna.9))
     #    if(length(muts.dna) == 0) muts.dna <- ""
         
         #search the AA mutation
         ## p.* format in the first
         muts.pt <- unique(x.trim[grep("^p\\.",x.trim)])
         
         ## if can not find, then AA+numeric+AA
  ##       if(length(muts.pt) == 0 ){
            muts.pt.2 <- unique(x.trim[grep("^[A-Z]+.[0-9]+[A-Z]+$|^[A-Z][0-9]+del$|^[A-Z][a-z][a-z][0-9]+[A-Z][a-z][a-z]$|^[A-Z][a-z][a-z][0-9]+del$|^[A-Z][a-z][a-z][0-9]+[X]$",x.trim)])
          #  muts.pt.2 <- muts.pt.2[-grep("^p\\.",muts.pt.2)]
            muts.pt.2 <- setdiff(muts.pt.2, muts.pt.2[grep("^D[0-9]+S[0-9]+",muts.pt.2)])
            muts.pt.2 <- setdiff(muts.pt.2, muts.pt.2[grep("^D[XY]S[0-9]+",muts.pt.2)])
            muts.pt <- union(muts.pt,muts.pt.2)
            ## check the AA mutation
            if(length(muts.pt.2 ) > 0){
             for( i in muts.pt.2){
                i <- stri_replace_all_regex(i, "p\\.",  "") 
                aa.check = unlist(strsplit(i,'[0-9]+'))
                aa.check = aa.check[aa.check != ""]
                aa.check = unique(is.element(aa.check,aa_ab))
                aa.check = aa.check[order(aa.check)]
                if(!aa.check[1])
                   muts.pt = setdiff(muts.pt,i)
             }
            }
   
         ## check whether the mutation is AA+nmeric+*, premature termanition
            muts.pt.3 <- unique(x.trim[grep("^[A-Z]*.*[0-9]+\\*$|^[dD]elta[A-Z][0-9]+",x.trim)])
            if(length(muts.pt.3 ) > 0){
              for(i in muts.pt.3 ){
                if(length(grep(i,muts.pt)) == 0)
                  muts.pt <- union(muts.pt,muts.pt.3)
              }    
            }
            
         ## remove the STRs, such as D1S498
            if(length(muts.pt) > 0){
                muts.pt <- setdiff(muts.pt, muts.pt[grep("^D[0-9]+S[0-9]+",muts.pt)])
                muts.pt <- setdiff(muts.pt, muts.pt[grep("^D[XY]S[0-9]+",muts.pt)])
            }
     
         ## last, search AA full name mutation
         if(length(muts.pt) == 0 ){
            muts.no <- unlist(lapply(aa_full,function(x.aa) agrep(x.aa,x.trim,value = FALSE)))
            muts.no <- muts.no[order(muts.no)]
            if(length(muts.no) > 0 ){
               muts.pt.4 <- x.trim[muts.no]
               muts.pt <- muts.pt.4
            }            
         }
         muts.pt <- unique(setdiff(muts.pt,muts.dna))
    ## search genes
         genes <- x.trim[grepl("^[A-Z]*.*[A-Z0-9]", x.trim)]
         genes.symbol <- unique(genes[is.element(genes,Approved.Symbol)])
         genes.symbol <- genes.symbol[!is.element(genes.symbol,c("A","C","G","T","I","III","II","IV",
                   "V","VI","VII","VIII","IVV","VV","DNA","RNA","HDR","LOD","WT","MRI"))]
         genes.alias <- unique(genes[is.element(genes,gene.Alias)])
         genes.alias <- genes.alias[nchar(genes.alias)>2]
         genes.alias <- genes.alias[!is.element(genes.alias,c("A","C","G","T","I","III","II","IV",
                   "V","VI","VII","VIII","IVV","VV","DNA","RNA","HDR","LOD","WT","BLAST"))]
         genes.all <- unique(c(genes.symbol,genes.alias))          
         muts.pt <- setdiff(muts.pt,genes.all)
         muts.pt <- str_rep_check(muts.pt)
         muts.dna <- str_rep_check(muts.dna)


#relations resolve
         if(length(genes.all) == 0 & length(muts.dna) == 0 & length(muts.pt) == 0){
            relations <- rep("",4) 
            }else if (length(muts.dna) > 0 | length(muts.pt) > 0){
                  relations <- var2gene(text=x,extr.genes=genes.all,extr.vars.c=muts.dna,extr.vars.p=muts.pt)                       
                   }else if(length(genes.all) > 0 & length(muts.dna) == 0 & length(muts.pt) == 0){
                      if(length( genes.all)==1){
                         relations <- c( genes.all,"","","")  
                         }else{
                             relations <- cbind( genes.all,"","","") 
                      } 
              } else{
                 relations <- c(rep("",3), paste(c("toConfirm", x),collapse = ":"))           
            }       
           return(relations)
    }        


## input HGNC dataset
    if(file.exists(localPDB.path)){
         if(file.exists(paste(localPDB.path,"hgnc_complete_set.txt.gz",sep="/"))){
             hgnc <- paste(localPDB.path,"hgnc_complete_set.txt.gz",sep="/")
             }else{
                 hgnc <- NULL
         }        
        }else{
             hgnc <- NULL
    }     

    if(is.null(hgnc)){
       hgnc <- "ftp://ftp.ebi.ac.uk/pub/databases/genenames/hgnc_complete_set.txt.gz"
       download.path <- paste(getwd(),"localPDB",sep="/")
       if(!file.exists(download.path))
          dir.create(download.path )
       options(timeout = 300)
       if( !file.exists(paste(download.path,"hgnc_complete_set.txt.gz",sep="/")))
          # download.file(hgnc,paste(download.path,"hgnc_complete_set.txt.gz",sep="/"),method="auto")
            curl_download(hgnc,paste(download.path,"hgnc_complete_set.txt.gz",sep="/"))
       hgnc <- paste(download.path,"hgnc_complete_set.txt.gz",sep="/")
    }
    if(substr(hgnc,nchar(hgnc)-1,nchar(hgnc)) == "gz"){
        hgnc <- read.delim(gzfile(hgnc))
        }else{
            hgnc <- read.delim(hgnc)
    }       

    Approved.Symbol <- as.character(hgnc$symbol)
    gene.Synonyms <- unlist(lapply(as.character(hgnc$alias_symbol),function(x) str_trim(unlist(strsplit(x,"|")))))
    gene.Previous.Symbols <- unlist(lapply(as.character(hgnc$prev_symbol),function(x) str_trim(unlist(strsplit(x,"|")))))
    gene.Alias <- c(gene.Synonyms,gene.Previous.Symbols)
    aa.table <- aa
    aa_full = unique(as.character(aa.table[,2]))
    aa_ab = as.character(unique(as.matrix(aa.table[,3:4])))
    
    ## search in PubMed
    pubmed_search <- EUtilsSummary(query, type="esearch",db = "pubmed",retmax=30000)
    pubmed_records <- EUtilsGet(pubmed_search)
    years <- YearPubmed(pubmed_records)
    authors <- Author(pubmed_records)
    first_author <- unlist(lapply(authors,function(x) paste(as.character(x[1,1:2]),collapse=" ")))
    country <- Country(pubmed_records)
    journal <- ISOAbbreviation(pubmed_records)
    pubmed_abs <- AbstractText(pubmed_records)
    pubmed_Affiliation <- Affiliation(pubmed_records)
    pubmed_title <- ArticleTitle(pubmed_records)
    pubmed_PMID <- PMID(pubmed_records)
#    pubmed_abs.trim <- unlist(lapply(pubmed_abs,abs_trim))
    pubmed_conclusion <- unlist(lapply(pubmed_abs,abs_conclusion))
    phenotype_pubmed <- unlist(lapply(pubmed_title, function(x) pheno_capture_abs(keyword,x)))
    phenotype_pubmed[is.na(phenotype_pubmed)] <- unlist(lapply(pubmed_conclusion[is.na(phenotype_pubmed)], function(x) pheno_capture_abs(keyword,x)))
    pheno.info <- cbind(phenotype_pubmed,pubmed_title,journal,years,first_author,country,pubmed_PMID)
    pubmed_captures <- lapply(pubmed_abs,gene_var_abs)
   # for(i in 500:1500) {print(i);gene_var_abs(pubmed_abs[i])}   ## debug the code
    length.pairs <- unlist(lapply(pubmed_captures,function(x) ifelse(is.null(nrow(x)),1,nrow(x))))
    pubmed_merge <- c()
    for(i in 1:length(length.pairs)){
  #  for(i in 1:10){
        pubmed_captures.i <- pubmed_captures[[i]]
        if(is.null(nrow(pubmed_captures.i))){
           pubmed_merge.i <- c(pheno.info[i,],pubmed_captures.i) 
        #   if(length(pubmed_captures.i) !=5) print(c(i,pubmed_captures.i))
           }else if(nrow(pubmed_captures.i) == 0){
                pubmed_merge.i <- c(pheno.info[i,],rep("",4) )                
                }else{
                    pubmed_merge.i <- cbind(matrix(rep(pheno.info[i,],each=nrow(pubmed_captures.i)),nrow=nrow(pubmed_captures.i)),pubmed_captures.i)
        }  
        pubmed_merge <- rbind(pubmed_merge,pubmed_merge.i)
    }
    colnames(pubmed_merge) <- c("Phenotype", "Article_Title", "Journal", "Year", "First_author", "Country", "PMID", "Genes.captured","cdna_change", "p.change","pair.status")
#    pubmed_merge <- pubmed_merge[pubmed_merge[,"Genes.captured"] != "",]
    var_captured_hgvs <- unlist(lapply(pubmed_merge[,"p.change"],ptChange2hgvs))
   # for(i in 1:nrow(pubmed_merge)) {print(c(i,pubmed_merge[i,"p.change"]));ptChange2hgvs(pubmed_merge[i,"p.change"])}   ## test the code
    gene_approved <- rep("",length(var_captured_hgvs))
    gene_approved[is.element(pubmed_merge[,"Genes.captured"],Approved.Symbol)] <- pubmed_merge[is.element(pubmed_merge[,"Genes.captured"],Approved.Symbol),"Genes.captured"]
    gene_approved[!is.element(pubmed_merge[,"Genes.captured"],Approved.Symbol)]  <- unlist(lapply(pubmed_merge[!is.element(pubmed_merge[,"Genes.captured"],Approved.Symbol),"Genes.captured"],gs2hgnc))    
    DNA_change_hgvs <- stri_replace_all_regex(as.character(pubmed_merge[,"cdna_change"]), '\\&gt\\;', '>') 
    pubmed_merge <- cbind(pubmed_merge,gene_approved,DNA_change_hgvs,var_captured_hgvs)
    colnames(pubmed_merge)[(ncol(pubmed_merge)-2):ncol(pubmed_merge)] <- c("Approved.Symbol", "cdna.change.HGVS", "p.change.HGVS")
    pubmed_merge <- pubmed_merge[,c("Phenotype", "Approved.Symbol", "cdna.change.HGVS", "p.change.HGVS", "pair.status", "Article_Title", "Journal", "Year","First_author","Country","PMID","Genes.captured","cdna_change","p.change")]     
    print(c(length(phenotype_pubmed),length(pubmed_title),length(years),length(first_author),length(country),length(pubmed_PMID),nrow(pubmed_merge)))
    return(list(pubmed_merge,cbind(pubmed_PMID,pubmed_title,pubmed_abs)))    
}

